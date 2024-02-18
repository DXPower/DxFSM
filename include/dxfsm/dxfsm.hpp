#ifndef DXFSM_HPP
#define DXFSM_HPP

#include <concepts>
#include <coroutine>
#include <memory>
#include <string_view>
#include <utility>
#include <stdexcept>
#include <type_traits>
#include <functional>
#include <vector>
#include <cassert>
#include <any>
#include <optional>
#include <format>
#include <algorithm>
#include <variant>
#include <ranges>

namespace dxfsm {
namespace detail {
    class AnyPtr {
        std::any m_underlying_any{};

        template<typename T>
        struct DestroyingPtr {
            T* ptr{};
            DestroyingPtr(T* ptr) noexcept : ptr(ptr) { }

            static_assert(std::is_trivially_destructible_v<T>);
        };
        
        template<typename T>
        requires (not std::is_trivially_destructible_v<T>)
        struct DestroyingPtr<T> {
            T* ptr{};

            DestroyingPtr(T* ptr) noexcept : ptr(ptr) { }
            ~DestroyingPtr() noexcept(noexcept(ptr->~T())) {
                if (ptr != nullptr)
                    ptr->~T();
            }

            DestroyingPtr(DestroyingPtr&& move) noexcept : ptr(std::exchange(move.ptr, nullptr)) { }
            DestroyingPtr& operator=(DestroyingPtr&& move) noexcept {
                std::swap(ptr, move.ptr);
                return *this;
            }

            DestroyingPtr(const DestroyingPtr&) {
                assert(false); // Should never be copied
            }

            DestroyingPtr& operator=(const DestroyingPtr&) {
                assert(false); // Should never be copied
            }
        };
    public:
        AnyPtr() = default;
        AnyPtr(std::nullptr_t) { }

        template<typename T>
        AnyPtr(T* ptr) : m_underlying_any(std::in_place_type<DestroyingPtr<T>>, ptr) { }

        // Moves
        AnyPtr(AnyPtr&& move) noexcept = default;
        AnyPtr& operator=(AnyPtr&& move) noexcept = default;

        template<typename T>
        AnyPtr& operator=(T* ptr) {
            m_underlying_any.emplace<DestroyingPtr<T>>(ptr);
            return *this;
        }

        template<typename T>
        T* TryExtract() {
            auto* any_ptr = std::any_cast<DestroyingPtr<T>>(&m_underlying_any);
            return any_ptr ? any_ptr->ptr : nullptr;
        }

        template<typename T>
        const T* TryExtract() const {
            auto* any_ptr = std::any_cast<DestroyingPtr<T>>(&m_underlying_any);
            return any_ptr ? any_ptr->ptr : nullptr;
        }

        void Reset() {
            m_underlying_any.reset();
        }

        void Swap(AnyPtr& other) {
            std::swap(m_underlying_any, other.m_underlying_any);
        }

        bool HasValue() const {
            return m_underlying_any.has_value();
        }

        // Delete copy
        AnyPtr(const AnyPtr&) = delete;
        AnyPtr& operator=(const AnyPtr&) = delete;
    };
}

/// @brief Facilitates transmittal of data into @ref State "States" via a reusable storage block.
/// @details Events are the basic unit of data transfer between @ref State "States".
/// Events can store any data in a type-safe manner, including nothing (similar to `std::any`).
/// States can use `co_await` expressions on the FSM to suspend until they receive an event,
/// such as with @link ReceiveEvent @endlink or @link EmitAndReceive @endlink.
/// This class manages a resizable buffer that automatically expands to the largest
/// data stored.
/// @tparam Id Type to use as the Event Id
template<typename Id>
class Event {
public:
    using Id_t = Id;

    /// @brief Constructs an empty event
    Event() noexcept = default;
    /// @brief Constructs an event that stores an Id without any additional data
    explicit Event(Id id) noexcept : _id(std::move(id)) { }

    /// @brief Constructs an event that stores an Id with an additional object as its stored data
    template<typename T>
    Event(Id id, T&& data) {
        Store(std::move(id), std::forward<T>(data));
    }

    /// @brief Constructs an event that stores an Id as well as constructs the stored data in-place
    /// @tparam T Type to emplace
    /// @param args Arguments to forward to the constructor of @p T
    /// @param type Tag parameter. Use std::in_place_type to create.
    template<typename T, typename... Args >
    Event(Id id, std::in_place_type_t<T> type [[maybe_unused]], Args&&... args) {
        Emplace<T>(std::move(id), std::forward<Args>(args)...);
    }

    Event(Event&& other) noexcept {
        _id = std::exchange(other._id, std::nullopt);
        _capacity = std::exchange(other._capacity, 0u);
        _storage = std::exchange(other._storage, nullptr);
        _any_ptr = std::exchange(other._any_ptr, nullptr);
    }

    Event& operator=(Event&& other) noexcept {
        if (&other != this) {
            this->Swap(other);
        }

        return *this;
    }

    /// @brief Updates this Event's Id and clears its stored data.
    /// @details Equivalent to @ref Event::Store "Store(id)"
    Event& operator=(Id id) {
        Store(std::move(id));
        return *this;
    }

    ~Event() {
        _any_ptr.Reset(); // This will destroy the data in storage
        delete[] _storage;
    }

    /// @brief Updates this Event's Id and clears its stored data.
    void Store(Id id) {
        this->_id = std::move(id);
        _any_ptr.Reset();  // Destroy the object currently living in the buffer by implicitly invoking AnyPtr<T> destructor.
    }

    /// @brief Updates this Event's Id and stores the provided object as its data
    /// @return Reference to the stored object
    /// @details Any previously stored data is destroyed
    template<typename T>
    std::decay_t<T>& Store(Id id, T&& data) {
        _any_ptr.Reset();
        this->Reserve(sizeof(T));

        using Stored_t = std::remove_reference_t<T>;
        Stored_t* ptr = std::construct_at(reinterpret_cast<Stored_t*>(_storage), std::forward<T>(data));

        this->_any_ptr = ptr;
        this->_id = std::move(id);

        return *ptr;
    }

    /// @brief Updates this Event's Id and constructs an object in-place in its storage using the provided arguments
    /// @details Any previously stored data is destroyed
    /// @tparam T Type to construct
    /// @param args Arguments to forward to the constructor of @p T
    /// @returns Reference to the constructed object in the storage
    template<typename T, typename... Args>
    T& Emplace(Id id, Args&&... args) {
        _any_ptr.Reset();
        this->Reserve(sizeof(T));

        T* ptr = std::construct_at(reinterpret_cast<T*>(_storage), std::forward<Args>(args)...);

        this->_id = std::move(id);
        this->_any_ptr = ptr;

        return *ptr;
    }
        
    /// @brief Makes this Event Empty by clearing the Id and destroying stored data. Storage is not deallocated.
    void Clear() {
        this->_id.reset();
        _any_ptr.Reset();
    }

    /// @brief Safely gets the stored object, throwing an exception if the requested type is not stored.
    /// @tparam T The type to extract
    /// @returns Reference to the stored object
    /// @exception std::runtime_error Thrown if this Event is empty, does not store an object, or the type
    /// of the stored object is not @p T.
    template<class T>
    T& Get() {
        T* ptr = _any_ptr.TryExtract<T>();

        if (ptr == nullptr) {
            throw std::runtime_error("Attempt to extract data from Event which does not hold the requested type");
        }

        return *ptr;
    }

    /// @brief Safely gets the stored object, throwing an exception if the requested type is not stored.
    /// @tparam T The type to extract
    /// @returns Reference to the stored object
    /// @exception std::runtime_error Thrown if this Event is empty, does not store an object, or the type
    /// of the stored object is not @p T.
    template<class T>
    const T& Get() const {
        const T* ptr = _any_ptr.TryExtract<T>();

        if (ptr == nullptr) {
            throw std::runtime_error("Attempt to extract data from Event which does not hold the requested type");
        }

        return *ptr;
    }

    /// @brief Safely gets the stored object, returning a `nullptr` if the requested type is not stored.
    /// @tparam T The type to extract
    /// @returns Pointer to the stored object, or `nullptr` if the requested type is not stored.
    template<class T>
    T* GetMaybe() {
        return _any_ptr.TryExtract<T>();
    }

    /// @brief Safely gets the stored object, returning a `nullptr` if the requested type is not stored.
    /// @tparam T The type to extract
    /// @returns Pointer to the stored object, or `nullptr` if the requested type is not stored.
    template<class T>
    const T* GetMaybe() const {
        return _any_ptr.TryExtract<T>();
    }

    /// @brief Unsafely gets the stored object without checking its type.
    /// @details Calling this function is undefined behavior if the stored object is not of type @p T.
    /// @tparam T The type to extract
    /// @returns Reference to the stored object
    template<class T>
    T& GetUnchecked() {
        return *std::launder(reinterpret_cast<T*>(_storage));
    }

    /// @brief Unsafely gets the stored object without checking its type.
    /// @details Calling this function is undefined behavior if the stored object is not of type @p T.
    /// @tparam T The type to extract
    /// @returns Reference to the stored object
    template<class T>
    const T& GetUnchecked() const {
        return *std::launder(reinterpret_cast<const T*>(_storage));
    }

    /// @brief Gets the underlying storage buffer.
    void* Data() {
        return _storage;
    }

    /// @brief Gets the underlying storage buffer.
    const void* Data() const {
        return _storage;
    }

    /// @brief Deletes all allocated data and resets the event to its default, empty state
    void ReleaseStorage() {
        Event dummy{};
        this->Swap(dummy);
    }

    /// @brief Reserves space in the underlying storage.
    /// @details Invalidates pointers to the stored data and the the underlying storage.
    /// If reallocation occurs, the stored data is destroyed <b>and not recreated</b>.
    void Reserve(std::size_t size) {
        if (_capacity < size) {
            if (_any_ptr.HasValue())  // Destroy the stored object if it exists
                _any_ptr.Reset();

            _id.reset();
            _capacity = size;
            delete[] _storage;
            _storage = new std::byte[size];
        }
    }

    /// @brief Returns the maximum size of an object which can be constructed
    /// in the data buffer without reallocation.
    std::size_t Capacity() const { return _capacity; }

    /// @brief Compares the Id of this event.
    bool operator==(const Id& id) const { return _id.has_value() && _id.value() == id; }

    /// @brief Returns true if the event is empty (ie., no Id is stored).
    [[nodiscard]] bool Empty() const { return !_id.has_value(); }

    /// @brief Checks if the event has stored data associated with it
    /// @tparam T If not void, will check if the stored data (if any) matches this type.
    template<typename T = void>
    bool HasData() const {
        if constexpr (std::is_same_v<T, void>) {
            return _any_ptr.HasValue();
        } else {
            return GetMaybe<T>() != nullptr;
        }
    }

    /// @brief Gets the Id of this event.
    const Id& GetId() const { return _id.value(); }

    /// @brief Transfers ownership of the storage to a new Event object with a different Id type.
    /// @details `this` will be left in an empty state and zero capacity.
    template<typename NewId>
    Event<NewId> TransferToIdType(NewId&& new_id) {
        Event<NewId> target{};
        target._id = std::forward<NewId>(new_id);
        target._capacity = std::exchange(_capacity, 0);
        target._storage = std::exchange(_storage, nullptr);
        target._any_ptr = std::exchange(_any_ptr, nullptr);
        _id = std::nullopt;
        
        return target;
    }

    void Swap(Event& other) {
        std::swap(_id, other._id);
        std::swap(_capacity, other._capacity);
        std::swap(_storage, other._storage);
        _any_ptr.Swap(other._any_ptr);
    }

private:
    // Copying not allowed.
    Event(const Event&) = delete;
    Event& operator=(const Event&) = delete;


    std::optional<Id> _id{};
    // Capacity of the data buffer in bytes
    std::size_t _capacity{};
    // Pointer to data buffer
    std::byte* _storage{};
    
    detail::AnyPtr _any_ptr{};

    template<typename T>
    friend class Event;
};

template<typename Id>
void swap(Event<Id>& lhs, Event<Id>& rhs) {
    lhs.Swap(rhs);
}

template<typename StateId, typename EventId>
class FSM;

// Return type of coroutines which represent states.
/// @brief The coroutine handle type that represents discrete states in the FSM
/// @details This class should be the return type of each function intended to be a state in the FSM.
/// This type is automatically constructed when the function is called (part of C++20 Coroutines).
/// Once returned, it should be moved into the FSM via @ref FSM::AddState.
/// The function may be a non-static member, but its signature must be:\n
/// `State<StateId> Func(FSM<StateId, EventId>&, StateId id)`.
/// @tparam StateId The type to use as the @ref State Id
template<typename StateId>
class State {
public:
    /// @brief The type automatically constructed by the C++20 Coroutines system when a
    /// a coroutine returning State is called.
    /// @details This type is automatically managed by C++20 Coroutines. It is of
    /// no importance to an end-user of this library.
    struct promise_type {
        promise_type() = delete; // State must take FSM& and Id parameters

        // Define both member and non-member functions
        template<typename EventId> 
        promise_type(FSM<StateId, EventId>& fsm [[maybe_unused]], StateId id) : id(id) { }
        template<typename Self, typename EventId> 
        promise_type(Self&&, FSM<StateId, EventId>& fsm [[maybe_unused]], StateId id) : id(id) { }

        std::suspend_never initial_suspend() noexcept { return {}; }
        constexpr std::suspend_always final_suspend() noexcept { return {}; }
        State get_return_object() noexcept { return State(this); };
        void unhandled_exception() { 
            self->m_is_abominable = true;
            self->m_coro_handle = nullptr;
            throw;
        }
        void return_void() {
            // State coroutines must never return.
            throw std::runtime_error("State coroutine is not allowed to co_return.");
        }

        State* self;
        StateId id;
    };

private:
    using handle_type = std::coroutine_handle<promise_type>;

    handle_type m_coro_handle{};
    // Need a copy of the ID here in case the coroutine throws an exception, as the promise
    // object gets destroyed. This keeps the State object queryable for its identifying information.
    StateId m_id{};
    std::string m_name{};
    bool m_is_abominable{};

    explicit State(promise_type *p) noexcept 
        : m_coro_handle(handle_type::from_promise(*p)),
          m_id(p->id)
    {
        p->self = this;
    }

public:
    State(State&& other) noexcept 
        : m_coro_handle(std::exchange(other.m_coro_handle, nullptr)),
          m_id(std::exchange(other.m_id, {})),
          m_name(std::exchange(other.m_name, {})),
          m_is_abominable(std::exchange(other.m_is_abominable, false)) 
    {
        m_coro_handle.promise().self = this;
    }

    State& operator=(State&& other) noexcept {
        m_coro_handle = std::exchange(other.m_coro_handle, nullptr);
        m_id = std::exchange(other.m_id, {});
        m_name = std::exchange(other.m_name, {});
        m_is_abominable = std::exchange(other.m_is_abominable, false);
        m_coro_handle.promise().self = this;
        return *this;
    }

    State(const State&) = delete;
    State& operator=(const State&) = delete;

    ~State() {
        if (m_coro_handle)
            m_coro_handle.destroy();
    }

    /// @brief Gets the Id of this State.
    /// @details This id is automatically retrieved from the id parameter when the State coroutine was called.
    StateId Id() const {
        if (m_coro_handle.address() == nullptr && !m_is_abominable) {
            throw std::runtime_error("Attempt to get the ID of a state not associated with a coroutine");
        }

        return m_id;
    }

    /// @brief Sets the human-readable name.
    State& Name(std::string state_name) & {
        if (m_coro_handle.address() == nullptr && !m_is_abominable) {
            throw std::runtime_error("Attempt to set the name of State not associated with a coroutine");
        }

        m_name = std::move(state_name);
        return *this;
    }

    /// @brief Sets the human-readable name.
    State&& Name(std::string state_name) && {
        return std::move(this->Name(state_name));
    }

    /// @brief Gets the human-readable name.
    std::string_view Name() const {
        if (m_coro_handle.address() == nullptr && !m_is_abominable) {
            throw std::runtime_error("Attempt to get the name of a State not associated with a coroutine");
        }

        return m_name;
    }

    /// @brief Gets whether this state is abominable
    /// @details Abominable means that this state was killed due to an exception,
    /// and should be removed and possibly readded by the user.
    bool IsAbominable() const {
        return m_is_abominable;
    }

private:
    handle_type handle() const noexcept { return m_coro_handle; }
    operator handle_type() const noexcept { return m_coro_handle; }

    template<typename S, typename E>
    friend class FSM;
}; // State

namespace detail {
    struct DoneReporterBase {
        virtual ~DoneReporterBase() = default;

        virtual void ReportDone(bool nullify_current_state) const = 0;
    };
}

class ResetToken {
    bool m_should_reset{};

public:
    constexpr ResetToken() noexcept = default;
    constexpr ResetToken(bool should_reset) noexcept : m_should_reset(should_reset) { }

    constexpr bool ShouldReset() const noexcept {
        return m_should_reset;
    }

    constexpr operator bool() const noexcept {
        return m_should_reset;
    }
};

/// @brief The core class for representing a Finite State Machine
/// @details This class needs to be created first so it can be passed
/// to the State coroutines when they are started.
template<typename StateId, typename EventId>
class FSM {
public:
    using State_t = State<StateId>;
    using Event_t = Event<EventId>;
    using StateHandle = typename State_t::handle_type;

    FSM() = default;
    FSM(std::string human_name) : _name(std::move(human_name)) { }

    FSM(const FSM&) = delete;
    /// @todo: Implement FSM move semantics
    FSM(FSM&&) = delete;
    FSM& operator=(const FSM&) = delete;
    /// @todo: Implement FSM move semantics
    FSM& operator=(FSM&&) = delete;

    /// @brief Gets the human-readable name
    std::string_view Name() const { return _name; }

    /// @brief Sets the human-readable name
    FSM& Name(std::string name) { _name = std::move(name); return *this;}

    /// @brief Gets the current state of the FSM, `nullptr` if there is no current state
    /// @details If the previous resumption of the state machine resulted in an abominable state
    /// being created (unhandled exception was thrown from it), then the current state is cleared.
    /// In other words, the current state will never be abominable.
    auto GetCurrentState() const -> const State_t* {
        if (!_state) {
            return nullptr;
        }
        
        // TODO: Rework how this information is stored
        return FindState(_state.promise().id);
    }

    /// @brief Sets the current state of the FSM.
    /// @details The previous state will remain in its prior stage in execution; ie.,
    /// no resumption or restart occurs on the former current state.
    FSM& SetCurrentState(StateId id) {
        auto* state = FindState(id);

        if (state == nullptr)
            throw std::runtime_error(std::format("Attempt to set nonexistent state on FSM '{}'", Name()));
        else if (state->IsAbominable())
            throw std::runtime_error(std::format(
                "Attempt to set abominable state '{}' as current on FSM '{}'",
                state->Name(),
                Name()
            ));
        
        _state = state->handle();
        return *this;
    }

    using HandleOrId = std::variant<StateHandle, StateId>;

    /// @brief Adds a transition that defines `from->to` whenever \p event is sent to this FSM.
    /// @details After this function is called, whenever the current state is \p from, and the FSM
    /// receives an event with the Id \p event, then it will automatically transition the current state
    /// to \p to before execution resumes. Both internally generated and externally generated events
    /// will trigger this transition.\n
    /// The combination of (from, event) forms a unique transition. If such a transition already exists,
    /// then its target \p to will be replaced.
    FSM& AddTransition(HandleOrId from, EventId event, HandleOrId to) {
        auto GetHandle = [this]<typename T>(const T& hoi) -> StateHandle {
            if constexpr (std::is_same_v<T, StateHandle>) {
                (void) this; // Ignore unused warning in this branch
                return hoi;
            } else if constexpr (std::is_same_v<T, StateId>) {
                return FindState(hoi)->handle();
            }
        };

        auto from_handle = std::visit(GetHandle, from);
        auto to_handle = std::visit(GetHandle, to);

        m_transition_table.emplace(
            PartialTransition{
                .from = from_handle,
                .event = std::move(event)
            },
            LocalTransition{to_handle}
        );

        return *this;
    }

    /// @brief Adds a transition that defines `from->to` whenever \p event is sent to this FSM, and the \p to state is located in another FSM.
    /// @details After this function is called, whenever the current state is \p from, and the FSM
    /// receives an event with the Id \p event, then it will automatically transition the current state
    /// to \p to before execution resumes. Both internally generated and externally generated events
    /// will trigger this transition. The originating FSM will become inactive, and the remote FSM 
    /// will become active.\n
    /// The combination of (from, event) forms a unique transition. If such a transition already exists,
    /// then its target \p to will be replaced. This unique table is shared with non-remote transitions.
    /// @warning \p remote_fsm is not aware of such a remote transition existing. Therefore, this transition
    /// may become dangling if \p remote_fsm is moved or destroyed.
    template<typename DStateId, typename DEventId>
    FSM& AddRemoteTransition(HandleOrId from, EventId event, FSM<DStateId, DEventId>& remote_fsm, DStateId to) {
        static_assert(std::is_same_v<EventId, DEventId>, "Remote transition must provide EventId translation if the types do not match.");
        
        auto GetHandle = [this]<typename T>(const T& hoi) -> StateHandle {
            if constexpr (std::is_same_v<T, StateHandle>) {
                (void) this; // Ignore unused warning in this branch
                return hoi;
            } else if constexpr (std::is_same_v<T, StateId>) {
                return FindState(hoi)->handle();
            }
        };

        auto from_handle = std::visit(GetHandle, from);
        auto to_handle = remote_fsm.FindState(to)->handle();

        auto target = std::make_unique<RemoteTransitionTarget<DStateId, DEventId>>(
            remote_fsm,
            to_handle,
            event
        );

        m_transition_table.emplace(
            PartialTransition{
                .from = from_handle,
                .event = std::move(event)
            },
            RemoteTransition{std::move(target)}
        );

        return *this;
    }

    // TODO: readd remove transition
    // Removes transition triggered by event 'onEvent' sent from 'fromState'.
    // Return true if the transition was found and successfully removed.
    // bool removeTransition(StateHandle fromState, SV onEvent)
    // {
    //     auto erased = m_transition_table.erase({fromState, onEvent});
    //     return bool(erased);
    // }

    // bool removeTransition(SV fromState, SV onEvent)
    // {
    //     auto erased = m_transition_table.erase({FindHandle(fromState), onEvent});
    //     return bool(erased);
    // }

    // TODO: readd hasTransition
    // Return true if the FSM knows how to deal with event 'onEvent' sent from state 'fromState'.
    // bool hasTransition(StateHandle fromState, SV onEvent)
    // {
    //     return m_transition_table.contains({fromState, onEvent});
    // }

    // bool hasTransition(SV fromState, SV onEvent)
    // {
    //     return m_transition_table.contains({FindHandle(fromState), onEvent});
    // }

    // TODO: readd targetSTate
    // Finds the target state of 'onEvent' when sent from 'fromState'.
    // Returns an empty string if not found.
    // const std::string& targetState(StateHandle fromState, SV onEvent)
    // {
    //    auto it = m_transition_table.find({fromState, onEvent});
    //    if (it == m_transition_table.end())
    //        return _sharedEmptyString;
    //     else
    //         return it->second.state.promise().name;
    // }

    // const std::string& targetState(SV fromState, SV onEvent)
    // {
    //     return targetState(FindHandle(fromState), onEvent);
    // }

    /// @brief Awaitable type used by @link FSM::EmitAndReceive @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct EmitReceiveAwaitable {
        FSM* self{};
        Event_t* event_return{};

        constexpr bool await_ready() {return false;}
        std::coroutine_handle<> await_suspend(StateHandle) {
            return self->CommonEmittingAwaitSuspend();
        }

        void await_resume() {
            self->m_is_fsm_active = true;
            *event_return = std::move(self->m_event_for_next_resume);
        }
    };

    friend struct EmitReceiveAwaitable;

    /// @brief Can be awaited from a State coroutine to send an event to this FSM whilst suspending the State until an event is sent to it.
    /// @details If \p event is empty, this FSM will be suspended. 
    /// Else, if (current state, \p event) is in the transition table, it will transition to the target state.
    /// Else, if no transition was found, an exception will be thrown. If you wish to send events
    /// to the same current state, use AddTransition to create a circular transition.
    /// @param event The event to be sent and eventually received. It will be transparently replaced with the received event.
    /// @exception std::runtime_error Thrown when \p event is not empty but (current state, \p event)
    /// is not found in the transition table.
    EmitReceiveAwaitable EmitAndReceive(Event_t& event) {
        if (IsResetting() && !event.Empty()) {
            throw std::runtime_error("Cannot send non-empty event from state that is resetting.");
        }

        if (!IsResetting()) {
            m_event_for_next_resume = std::move(event);
        }

        return EmitReceiveAwaitable{this, &event};
    }

    /// @brief Awaitable type used by @link FSM::EmitAndReceiveResettable @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct EmitReceiveResettableAwaitable {
        FSM* self{};
        Event_t* event_return{};

        constexpr bool await_ready() {return false;}
        std::coroutine_handle<> await_suspend(StateHandle from_state) {
            self->m_state_to_reset = from_state;
            return self->CommonEmittingAwaitSuspend();            
        }

        [[nodiscard]] ResetToken await_resume() {
            auto reset_token = self->CommonResettableAwaitResume();

            if (reset_token.ShouldReset()) {
                event_return->Clear();
            } else {
                *event_return = std::move(self->m_event_for_next_resume);
            }

            return reset_token;
        }
    };

    friend struct EmitReceiveResettableAwaitable;

    EmitReceiveResettableAwaitable EmitAndReceiveResettable(Event_t& event) {
        if (IsResetting() && !event.Empty()) {
            throw std::runtime_error("Cannot send non-empty event from state that is resetting.");
        }

        if (!IsResetting()) {
            m_event_for_next_resume = std::move(event);
        }

        return EmitReceiveResettableAwaitable{this, &event};
    }
    

    /// @brief Awaitable type used by @link FSM::ReceiveEvent @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct ReceiveAwaitable {
        FSM* self;
        Event_t* event_return;

        bool await_ready() {
            return false;
        }

        std::coroutine_handle<> await_suspend(StateHandle) {
            return self->CommonAwaitSuspend();
        }

        void await_resume() {
            self->m_is_fsm_active = true;
            *event_return = std::move(self->m_event_for_next_resume);
        }
    };

    friend struct ReceiveAwaitable;

    /// @brief Can be awaited from a State coroutine to suspend it until an event is sent to it.
    /// @param[out] event_out Where the received Event will be written to
    /// @details The use of an out-parameter here allows for the Event's storage to be reused.
    ReceiveAwaitable ReceiveEvent(Event_t& event_out) {
        // Bring the storage for the Event into the FSM so it can be reused by the next
        // call to InsertEvent or EmitAndReceive
        if (!IsResetting()) {
            event_out.Clear(); // Clear the previous event
            m_event_for_next_resume = std::move(event_out);
        }
        return ReceiveAwaitable{this, &event_out};
    }
    
    /// @brief Awaitable type used by @link FSM::ReceiveInitialEvent @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct InitialReceiveAwaitable {
        FSM* self;

        bool await_ready() {
            const Event_t& pending_event = self->m_event_for_next_resume;
            return !pending_event.Empty();
        }

        std::coroutine_handle<> await_suspend(StateHandle) {
            return self->CommonAwaitSuspend();
        }

        [[nodiscard]] Event_t await_resume() {
            self->m_is_fsm_active = true;
            return std::move(self->m_event_for_next_resume);
        }
    };

    friend struct InitialReceiveAwaitable;

    /// @brief Can be awaited from a State coroutine to suspend it until an event is sent to it.
    /// @warning This function should not be used if an Event variable already exists within the state,
    /// as Event storage cannot be reused when it is returned from the await expression.
    /// Improper usage of this function will cause calls to InsertEvent to unnecessarily allocate memory.
    /// @return When this function call is awaited, it will return the received Event.
    InitialReceiveAwaitable ReceiveInitialEvent() {
        return InitialReceiveAwaitable{this};
    }
    
    /// @brief Awaitable type used by @link FSM::IgnoreEvent @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct IgnoreAwaitable {
        FSM* self;

        bool await_ready() {
            const Event_t& pending_event = self->m_event_for_next_resume;
            return !pending_event.Empty();
        }

        std::coroutine_handle<> await_suspend(StateHandle) {
            return self->CommonAwaitSuspend();
        }

        void await_resume() {
            self->m_is_fsm_active = true;
            self->m_event_for_next_resume.Clear();
        }
    };

    friend struct IgnoreAwaitable;

    /// @brief Can be awaited from a State coroutine to ignore the next event sent to this state, suspending
    /// until it is received.
    IgnoreAwaitable IgnoreEvent() {
        return IgnoreAwaitable{this};
    }

    // Adds a state to the state machine without associating any events with it.
    // Returns the index of the vector to which the state was stored.
    /// @brief Adds a state to the FSM.
    /// @details See State for details on how one should be created.
    FSM& AddState(State_t&& state) {
        // TODO: readd
        // if (hasState(state.Name()))
        //     throw std::runtime_error(std::format(
        //         "A state with name '{}' already exists in FSM {}",
        //         state.Name(),
        //         _name
        //     ));

        if (state.handle())
            m_states.push_back(std::move(state));
        else
            throw std::runtime_error(std::format("Attempt to add an empty state to FSM '{}'", Name()));
        
        return *this;
    }

    std::size_t NumStates() const { return m_states.size(); }

    /// @brief Sends an event to a suspended FSM, potentially triggering a transition.
    /// @details If (cur state, event id) exists in the transition table, then a transition
    /// will be triggered to the target state and the event will be sent to that state and resume.
    /// Otherwise, the event is sent to the current state and it is resumed.\n\n
    /// \p event_initializer is called with a reference to the stored event object.
    /// The callable should use Event's member functions to set the id or store data.
    /// If the event is empty after returning, then the FSM will not be resumed;
    /// this can be used to reserve space in the stored event without triggering resumption.\n\n
    /// <b>Exception Safety</b>: If an exception escapes any of the states eventually resumed
    /// by this function call, that State will be marked "abominable", then the exception will
    /// be rethrown from that call. The abominable state should be removed and readded before
    /// it is resumed, otherwise it results in undefined behavior.
    /// @todo Detect no current state, empty event, and abominable states in resumption
    template<std::invocable<Event_t&> F>
    FSM& InsertEvent(F&& event_initializer) {
        std::forward<F>(event_initializer)(m_event_for_next_resume);

        // Treat this event as one that could change the state
        const Transitioner& potential_transition = GetTransitioner(m_event_for_next_resume);
        std::coroutine_handle<> state_to_resume = potential_transition.Perform(*this);
    
        // If an exception is thrown while the state is running, catch it to 
        // always cleanup
        try {
            state_to_resume.resume();
        } catch (...) {
            potential_transition.ReportDone(*this, true);
            throw;
        }

        return *this;
    }

    /// \brief Shortcut for the function overload of InsertEvent which sets the Id of
    /// the event without storing associated data.
    FSM& InsertEvent(EventId id)  {
        return InsertEvent([&id](Event_t& ev) {
            ev.Store(std::move(id));
        });
    }

    /// \brief Shortcut for the function overload of InsertEvent which sets the Id of
    /// the event and stores associated data
    template<typename T>
    FSM& InsertEvent(EventId id, T&& obj) {
        return InsertEvent([&id, &obj](Event_t& ev) {
            ev.Store(std::move(id), std::forward<T>(obj));
        });
    }

    /// \brief Shortcut for the function overload of InsertEvent which sets the Id of
    /// the event and constructs the associated data in-place
    template<typename T, typename... Args>
    FSM& EmplaceEvent(EventId id, Args&&... args) {
        return InsertEvent([&id, &args...](Event_t& ev) {
            ev.template Emplace<T>(std::move(id), std::forward<Args>(args)...);
        });
    }

    bool HasState(StateId id) const {
        return std::ranges::find(m_states, id, &State_t::Id) != m_states.end();
    }

    /// @brief Returns a range that contains all abominable states.
    auto GetAbominableStates() const {
        return m_states | std::views::filter([](const State_t& s) {
            return s.IsAbominable();
        });
    }

    /// @brief Removes all states marked as abominable.
    void RemoveAbominableStates() {
        std::erase_if(m_states, [](const State_t& s) {
            return s.IsAbominable();
        });
    }

    /// @brief Moves out the stored Event object within the FSM
    /// @details In most cases, this will be an empty event object with
    /// a non-zero capacity. However, this event may be non-empty if 
    /// a State never received the event due to an uncaught exception
    /// during an reset.
    Event_t ExtractStoredEvent() {
        m_transition_after_reset = nullptr;
        return std::move(m_event_for_next_resume);
    }

    /// @brief Resends a stored Event, useful for recovering from an 
    /// exception during a reset
    /// @details Has no effect if there is no stored event.
    /// @exception User-defined Has the same exception rules as @ref InsertEvent
    bool ResendStoredEvent() {
        if (m_event_for_next_resume.Empty() || m_transition_after_reset == nullptr)
            return false;

        try {
            m_transition_after_reset->Perform(*this).resume();
        } catch (...) {
            m_transition_after_reset->ReportDone(*this, true);
            throw;
        }

        return true;
    }

    /// @brief Returns true if any State coroutine is currently running.
    bool IsActive() const { return m_is_fsm_active; }


    // Callback for debugging and writing log. It is called when the state of
    // the fsm whose name is in the first argument is about
    // to change from 'from' to 'to' because the from is sending
    // event 'event'.
    std::function<
        void(const FSM& fsm, 
             const State_t& from,
             const Event_t& event,
             const State_t& to
        )
    > logger;

private:
    std::string _name;       // Name of the FSM (for information only)
    StateHandle _state = nullptr; // Current state (for information only)

    State_t* FindState(StateId id) {
        auto it = std::ranges::find(m_states, id, &State_t::Id);
        return it != m_states.end() ? &*it : nullptr;
    }

    const State_t* FindState(StateId id) const {
        auto it = std::ranges::find(m_states, id, &State_t::Id);
        return it != m_states.end() ? &*it : nullptr;
    }

    struct LocalTransitionTarget {
        StateHandle state{};
    };

    struct RemoteTransitionTargetBase {
        virtual ~RemoteTransitionTargetBase() = default;

        virtual std::coroutine_handle<> TransitionOnRemote(FSM& originating_fsm) const = 0;
        virtual void ReportDone(bool nullify_current_state) const = 0;
    };

    // D... = Destination
    template<typename DStateId, typename DEventId>
    struct RemoteTransitionTarget final : RemoteTransitionTargetBase {
        using DFSM_t = FSM<DStateId, DEventId>;

        DFSM_t* target_fsm{};
        typename DFSM_t::Transitioner local_transitioner{};
        typename DFSM_t::StateHandle target_state{};
        DEventId translated_event_id;

        RemoteTransitionTarget(DFSM_t& target_fsm, typename DFSM_t::StateHandle target_state, DEventId translated_id)
            : target_fsm(&target_fsm),
              local_transitioner(typename DFSM_t::LocalTransition{target_state}),
              target_state(target_state),
              translated_event_id(translated_id)
        { }

        std::coroutine_handle<> TransitionOnRemote(FSM& originating_fsm) const override {
            // Translate the event to the EventId type of the target FSM,
            // then store it into the target FSM
            auto& originating_event = originating_fsm.m_event_for_next_resume;
            auto& target_event = target_fsm->m_event_for_next_resume;

            target_event = originating_event.TransferToIdType(DEventId(translated_event_id));

            const bool different_states = target_fsm->_state != target_state;

            if (!target_fsm->IsResetting() && target_fsm->ShouldSendResetOnTransition() && different_states) {
                target_fsm->m_transition_after_reset = &local_transitioner;

                target_fsm->_state = target_fsm->m_state_to_reset;
                return target_fsm->m_state_to_reset;
            } else {
                return local_transitioner.Perform(*target_fsm);
            }
        }
        
        void ReportDone(bool nullify_current_state) const override {
            if (nullify_current_state) {
                target_fsm->_state = nullptr;
            }

            target_fsm->m_state_to_reset = nullptr;
            target_fsm->m_is_fsm_active = false;
        }
    };

    template<typename DStateId, typename DEventId>
    friend struct RemoteTransitionTarget;

    using RemoteTransitionTargetPtr = std::unique_ptr<RemoteTransitionTargetBase>;

    struct LocalTransition {
        std::coroutine_handle<> state_to_resume{};
    };

    struct RemoteTransition {
        RemoteTransitionTargetPtr remote_target{};
    };

    struct NullTransition { };

    class Transitioner {
        std::variant<LocalTransition, RemoteTransition, NullTransition> m_data{};

    public:
        Transitioner(NullTransition) : m_data(std::in_place_type<NullTransition>) { }
        Transitioner(LocalTransition local) : m_data(local) { }
        Transitioner(RemoteTransition&& remote) : m_data(std::move(remote)) { }

        std::coroutine_handle<> Perform(FSM& originating) const {
            return std::visit([&originating, this]<typename T>(const T& o) -> std::coroutine_handle<> {
                if constexpr (std::is_same_v<T, LocalTransition>) {
                    const LocalTransition& local = o;
                    const bool different_states = originating._state != local.state_to_resume;

                    // Trigger a reset if needed
                    if (!originating.IsResetting() && originating.ShouldSendResetOnTransition() && different_states) {
                        originating.m_transition_after_reset = this;
                        originating._state = originating.m_state_to_reset;
                        return originating.m_state_to_reset;
                    } else {
                        // If we get here while resetting, that means the reset is done and can be cleared
                        if (originating.IsResetting()) {
                            originating.m_transition_after_reset = nullptr;
                            originating.m_state_to_reset = nullptr;
                        }

                        originating._state = StateHandle::from_address(local.state_to_resume.address());
                        return local.state_to_resume;
                    }
                } else if constexpr (std::is_same_v<T, RemoteTransition>) {
                    const RemoteTransitionTargetBase& remote = *o.remote_target;
                    return remote.TransitionOnRemote(originating);
                } else if constexpr(std::is_same_v<T, NullTransition>) {
                    return originating._state;
                }
            }, m_data);
        }

        void ReportDone(FSM& originating, bool nullify_current_state) const {
            return std::visit([&originating, nullify_current_state]<typename T>(const T& o) {
                if constexpr (std::is_same_v<T, LocalTransition> || std::is_same_v<T, NullTransition>) {
                    if (nullify_current_state) {
                        originating._state = nullptr;
                    }

                    originating.m_state_to_reset = nullptr;
                    originating.m_is_fsm_active = false;
                } else if constexpr(std::is_same_v<T, RemoteTransition>) {
                    const RemoteTransitionTargetBase& remote = *o.remote_target;
                    remote.ReportDone(nullify_current_state);
                }
            }, m_data);

        }

        bool IsRemote() const {
            return std::holds_alternative<RemoteTransition>(m_data);
        }

        bool IsNull() const {
            return std::holds_alternative<NullTransition>(m_data);
        }
    };

    const Transitioner& GetTransitioner(const Event_t& event) const noexcept {
        if (auto it = m_transition_table.find({_state, event.GetId()}); it != m_transition_table.end())
            return it->second;
        else
            return null_transition; // No coded state transition, return null transition
    }

    // The 'from' and 'event' of a transition
    struct PartialTransition {
        StateHandle from;
        EventId event;

        bool operator==(const PartialTransition& rhs) const = default;
    };

    struct PartialTransitionHash
    {
        std::size_t operator() (const PartialTransition& t) const noexcept {
            std::size_t lhs = std::hash<void*>()(t.from.address());

            // Activate ADL
            using std::hash;
            std::size_t rhs = hash<EventId>()(t.event);

            // Combine hashes
            return lhs ^ (rhs + 0x517cc1b727220a95 + (lhs << 6) + (lhs >> 2));
        }
    };

    bool IsResetting() const {
        return m_transition_after_reset != nullptr;
    }

    bool ShouldResumeRespondWithReset() const {
        return m_state_to_reset != nullptr && m_transition_after_reset != nullptr;
    }

    bool ShouldSendResetOnTransition() const {
        return m_state_to_reset != nullptr;
    }

    // If currently resetting, this will return the stored post-reset transition.
    // Else, it will suspend entirely.
    std::coroutine_handle<> CommonAwaitSuspend() {
        m_is_fsm_active = false;

        if (!IsResetting()) {
            return std::noop_coroutine();
        }

        // If we get here while resetting, the reset is complete and the state
        // has suspended. Clear the reset info.
        const Transitioner* transitioner = std::exchange(m_transition_after_reset, nullptr);
        m_state_to_reset = nullptr;

        return transitioner->Perform(*this);
    }

    // If currently resetting, this will resume the stored post-reset transition.
    // Else, it will check the pending event and perform its transitioner
    std::coroutine_handle<> CommonEmittingAwaitSuspend() {
        m_is_fsm_active = false;

        const Event_t& pending_event = m_event_for_next_resume;
        
        // If this state sends an empty event and we're not resetting,
        // then suspend the FSM
        if (pending_event.Empty() && !IsResetting()) {
            return std::noop_coroutine();
        }

        const Transitioner& transitioner = IsResetting()
            ? *m_transition_after_reset
            : GetTransitioner(pending_event);

        return transitioner.Perform(*this);
    }

    ResetToken CommonResettableAwaitResume() {
        m_is_fsm_active = true;

        if (ShouldResumeRespondWithReset()) {
            return true;
        } else {
            m_state_to_reset = nullptr;
            return false;
        }
    }

    // Transition table in format {from-state, event} -> to-state
    // That is, an event sent from from-state will be routed to to-state.
    std::unordered_map<PartialTransition, Transitioner, PartialTransitionHash> m_transition_table{};

    std::vector<State_t> m_states;
    StateHandle m_state_to_reset{};
    const Transitioner* m_transition_after_reset{};

    // Shared null_transition instance
    inline static const Transitioner null_transition{NullTransition{}};

    // True if the FSM is running, false if suspended.
    bool m_is_fsm_active{};

    // Temporary event storage to be captured by Awaitable on resume
    Event_t m_event_for_next_resume{};
}; // FSM

} // namespace dxfsm
#endif // DXFSM_HPP
