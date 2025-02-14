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
#include <cassert>
#include <any>
#include <optional>
#include <format>
#include <algorithm>
#include <variant>
#include <ranges>
#include <unordered_set>

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

        using Stored_t = std::remove_cvref_t<T>;
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

template<typename StateId>
class State;

namespace detail {
    struct DoneReporterBase {
        virtual ~DoneReporterBase() = default;

        virtual void ReportDone(bool nullify_current_state) const = 0;
    };

    class RemoteTransitionTargetBase {
    public:
        virtual ~RemoteTransitionTargetBase() = default;

        virtual std::coroutine_handle<> TransitionOnRemote(void* originating_fsm_ptr) const = 0;
        virtual void ReportDone(bool nullify_current_state) const = 0;
        virtual bool RebindTransition() = 0;
        virtual bool IsTransitionDangling() const = 0;
    };

    template<typename, typename, typename, typename>
    class RemoteTransitionTarget;

    using RemoteTransitionTargetPtr = std::unique_ptr<RemoteTransitionTargetBase>;

    template<typename StateId>
    class StateCoro;

    // This awaiter lets the FSM be marked as active on the very
    // first event it receives.
    struct InitialStateAwaitable {
        bool* fsm_active{};

        constexpr bool await_ready() const { return false; }
        constexpr void await_suspend(std::coroutine_handle<>) const { }
        void await_resume() {
            *fsm_active = true;
        }
    };

    template<typename StateId>
    struct StatePromiseType {
        using handle_type = std::coroutine_handle<StatePromiseType>;
        using State_t = State<StateId>;

        // This type is the same as StoredState below
        std::pair<const StateId, StateCoro<StateId>>* self{};
        bool* fsm_active{};

        StatePromiseType() = delete; // State must take FSM& and Id parameters

        // Define both member and non-member functions
        template<typename EventId, typename... Args> 
        StatePromiseType(FSM<StateId, EventId>& fsm, StateId id, Args&&...) {
            self = &fsm.EmplaceState(std::move(id), handle_type::from_promise(*this));
            fsm_active = &fsm.m_is_fsm_active;
        }

        template<typename Self, typename EventId, typename... Args> 
        StatePromiseType(Self&&, FSM<StateId, EventId>& fsm, StateId id, Args&&...) 
            : StatePromiseType(fsm, id) { }

        constexpr InitialStateAwaitable initial_suspend() noexcept { return {fsm_active}; }
        constexpr std::suspend_always final_suspend() noexcept { return {}; }

        State<StateId> get_return_object() noexcept {
            return State<StateId>(*self);
        }

        void unhandled_exception() { 
            self->second.m_coro_handle = nullptr;
            throw;
        }
    };
        
    template<typename StateId>
    class StateCoro {
        using handle_type = std::coroutine_handle<StatePromiseType<StateId>>;
        handle_type m_coro_handle{};

    public:
        StateCoro(handle_type handle) : m_coro_handle(handle) { }

        StateCoro(const StateCoro&) = delete;
        StateCoro(StateCoro&&) = delete;
        StateCoro& operator=(const StateCoro&) = delete;
        StateCoro& operator=(StateCoro&&) = delete;

        ~StateCoro() {
            if (m_coro_handle)
                m_coro_handle.destroy();
        }

        /// @brief Gets whether this state is abominable
        /// @details Abominable means that this state was killed due to an exception,
        /// and should be removed and possibly readded by the user.
        bool IsAbominable() const {
            return m_coro_handle == nullptr;
        }

    private:
        handle_type Handle() const noexcept { return m_coro_handle; }
        operator handle_type() const noexcept { return m_coro_handle; }

        template<typename S, typename E>
        friend class dxfsm::FSM;

        template<typename>
        friend struct StatePromiseType;

        template<typename, typename, typename, typename>
        friend class detail::RemoteTransitionTarget;
    };

    template<typename StateId>
    using StateMap = std::unordered_map<StateId, StateCoro<StateId>>;

    template<typename StateId>
    using StoredState = StateMap<StateId>::value_type;
}

template<typename StateId>
class State {
    const detail::StoredState<StateId>* m_state{};

    explicit State(const detail::StoredState<StateId>& state) : m_state(&state) { }

public:
    using promise_type = detail::StatePromiseType<StateId>;

    StateId Id() const {
        return m_state->first;
    }

    bool IsAbominable() const {
        return m_state->second.IsAbominable();
    }

    template<typename S>
    friend struct detail::StatePromiseType;

    template<typename S, typename E>
    friend class FSM;
};

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

template<typename StateId, typename EventId, bool IsConst = false>
class Transition {
    using FSM_t = FSM<StateId, EventId>;
    using StoredTransition_t = FSM<StateId, EventId>::TransitionTable_t::value_type;
    using CST = std::conditional_t<IsConst, const StoredTransition_t, StoredTransition_t>;

    const FSM_t* m_fsm{};
    CST* m_transition{};

    Transition(const FSM_t& fsm, CST& transition)
        : m_fsm(&fsm), m_transition(&transition) { }
public:
    using Guard_t = std::function<typename FSM_t::GuardSig>;

    const StateId& From() const {
        return m_transition->first.from;
    }

    const EventId& Event() const {
        return m_transition->first.event;
    }

    // TODO: implement To id getter

    bool IsRemote() const {
        return m_transition->second.IsRemote();
    }

    bool IsDangling() const {
        return m_transition->second.IsDangling(*m_fsm);
    }

    Guard_t& Guard() {
        return m_transition->second.Guard();
    }

    const Guard_t& Guard() const {
        return m_transition->second.Guard();
    }

    void Guard(Guard_t&& guard) requires (not IsConst) {
        m_transition->second.Guard(std::move(guard));
    }

    template<typename, typename>
    friend class FSM;
};

template<typename StateId, typename EventId>
using CTransition = Transition<StateId, EventId, true>;

/// @brief The core class for representing a Finite State Machine
/// @details This class needs to be created first so it can be passed
/// to the State coroutines when they are started.
template<typename StateId, typename EventId>
class FSM {
public:
    using State_t = State<StateId>;
    using Event_t = Event<EventId>;

    using GuardSig = bool(const Event_t&);

private:
    using StateCoro_t = detail::StateCoro<StateId>;
    using StateHandle = typename StateCoro_t::handle_type;
    using StoredState_t = detail::StoredState<StateId>;

    struct LocalTransition {
        std::coroutine_handle<> state_to_resume{};
        StateId id_to_resume{};
    };

    struct RemoteTransition {
        detail::RemoteTransitionTargetPtr remote_target{};
    };

    struct NullTransition { };

    class Transitioner {
    public:
        std::variant<LocalTransition, RemoteTransition, NullTransition> m_data{};
        std::function<GuardSig> m_guard{};

    public:
        Transitioner() : Transitioner(NullTransition{}) { }
        Transitioner(NullTransition) : m_data(std::in_place_type<NullTransition>) { }
        Transitioner(LocalTransition local) : m_data(local) { }
        Transitioner(RemoteTransition&& remote) : m_data(std::move(remote)) { }

        void Guard(std::function<GuardSig> func) { m_guard = std::move(func); }

        std::function<GuardSig>& Guard() { return m_guard; }
        const std::function<GuardSig>& Guard() const { return m_guard; }

        std::coroutine_handle<> Perform(FSM& originating) const {
            return std::visit([&originating, this]<typename T>(const T& o) -> std::coroutine_handle<> {
                if constexpr (std::is_same_v<T, LocalTransition>) {
                    const LocalTransition& local = o;
                    const bool different_states = originating.m_cur_state == nullptr
                        || originating.m_cur_state->second.Handle() != local.state_to_resume;

                    // Trigger a reset if needed
                    if (!originating.IsResetting() && originating.ShouldSendResetOnTransition() && different_states) {
                        originating.m_transition_after_reset = this;
                        originating.m_cur_state = originating.m_state_to_reset.promise().self;
                        return originating.m_state_to_reset;
                    } else {
                        // If we get here while resetting, that means the reset is done and can be cleared
                        if (originating.IsResetting()) {
                            originating.m_transition_after_reset = nullptr;
                            originating.m_state_to_reset = nullptr;
                        }

                        State_t original_state(*originating.m_cur_state);
                        originating.m_cur_state = StateHandle::from_address(local.state_to_resume.address()).promise().self;
                        
                        if (originating.m_transition_observer) {
                            assert(!originating.m_event_for_next_resume.Empty());
                            originating.m_transition_observer(originating, original_state, State_t(*originating.m_cur_state), originating.m_event_for_next_resume);
                        }
                        
                        return local.state_to_resume;
                    }
                } else if constexpr (std::is_same_v<T, RemoteTransition>) {
                    const detail::RemoteTransitionTargetBase& remote = *o.remote_target;
                    return remote.TransitionOnRemote(&originating);
                } else if constexpr(std::is_same_v<T, NullTransition>) {
                    return originating.m_cur_state->second.Handle();
                }
            }, m_data);
        }

        void ReportDone(FSM& originating, bool nullify_current_state) const {
            return std::visit([&originating, nullify_current_state]<typename T>(const T& o) {
                if constexpr (std::is_same_v<T, LocalTransition> || std::is_same_v<T, NullTransition>) {
                    if (nullify_current_state) {
                        originating.m_cur_state = nullptr;
                    }

                    originating.m_state_to_reset = nullptr;
                    originating.m_is_fsm_active = false;
                } else if constexpr(std::is_same_v<T, RemoteTransition>) {
                    const detail::RemoteTransitionTargetBase& remote = *o.remote_target;
                    remote.ReportDone(nullify_current_state);
                }
            }, m_data);
        }

        bool Rebind(FSM& originating) {
            return std::visit([&originating]<typename T>(T& o) {
                if constexpr (std::is_same_v<T, LocalTransition>) {
                    LocalTransition& local = o;
                    
                    if (const StoredState_t* state = originating.FindStoredState(local.id_to_resume)) {
                        local.state_to_resume = state->second.Handle();
                        return true;
                    } else {
                        local.state_to_resume = nullptr;
                    }
                } else if constexpr(std::is_same_v<T, RemoteTransition>) {
                    detail::RemoteTransitionTargetBase& remote = *o.remote_target;
                    return remote.RebindTransition();
                }

                return false;
            }, m_data);
        }

        bool IsRemote() const {
            return std::holds_alternative<RemoteTransition>(m_data);
        }

        bool IsNull() const {
            return std::holds_alternative<NullTransition>(m_data);
        }

        bool IsDangling(const FSM& originating) const {
            return std::visit([&originating]<typename T>(const T& o) {
                if constexpr (std::is_same_v<T, LocalTransition>) {
                    const LocalTransition& local = o;
                    const auto state = originating.GetState(local.id_to_resume);

                    return !state.has_value() || state->IsAbominable();
                } else if constexpr(std::is_same_v<T, RemoteTransition>) {
                    const detail::RemoteTransitionTargetBase& remote = *o.remote_target;
                    return remote.IsTransitionDangling();
                } else {
                    return false;
                }
            }, m_data);
        }
    };

    // The 'from' and 'event' of a transition
    struct PartialTransition {
        StateId from;
        EventId event;

        struct Indirect {
            const StateId* from{};
            const EventId* event{};
        };

        bool operator==(const PartialTransition& rhs) const = default;
        bool operator==(const Indirect& rhs) const {
            return from == *rhs.from && event == *rhs.event;
        }

        struct Equal {
            using is_transparent = void;

            bool operator()(const PartialTransition& lhs, const PartialTransition& rhs) const {
                return lhs == rhs;
            }

            bool operator()(const PartialTransition& lhs, const Indirect& rhs) const {
                return lhs == rhs;
            }

            bool operator()(const Indirect& lhs, const PartialTransition& rhs) const {
                return rhs == lhs;
            }
        };

        struct Hash {
            using is_transparent = void;

            std::size_t do_hash(const StateId& from, const EventId& event) const noexcept {
                // Activate ADL
                using std::hash;
                std::size_t from_hash = hash<StateId>()(from);
                std::size_t event_hash = hash<EventId>()(event);

                // Combine hashes
                return from_hash ^ (event_hash + 0x517cc1b727220a95 + (from_hash << 6) + (from_hash >> 2));
            }

            std::size_t operator()(const PartialTransition& t) const noexcept {
                return do_hash(t.from, t.event);
            }

            std::size_t operator()(const Indirect& t) const noexcept {
                return do_hash(*t.from, *t.event);
            }
        };
    };

    using TransitionTable_t = std::unordered_map<
        PartialTransition,
        Transitioner,
        typename PartialTransition::Hash,
        typename PartialTransition::Equal
    >;

public:
    using Transition_t = Transition<StateId, EventId>;
    using CTransition_t = CTransition<StateId, EventId>;

    FSM() = default;

    // Non-moveable, non-copyable
    // This is done because the awaiters and remote transitions hold FSM*.
    // The first can be solved. Want feedback if this is worth it though.
    FSM(const FSM&) = delete;
    FSM(FSM&&) = delete;
    FSM& operator=(const FSM&) = delete;
    FSM& operator=(FSM&&) = delete;

    /// @brief Gets the current state of the FSM, `nullptr` if there is no current state
    /// @details If the previous resumption of the state machine resulted in an abominable state
    /// being created (unhandled exception was thrown from it), then the current state is cleared.
    /// In other words, the current state will never be abominable.
    auto GetCurrentState() const -> std::optional<State_t> {
        if (m_cur_state != nullptr)
            return State_t(*m_cur_state);
        else
            return std::nullopt;
    }

    FSM& SetCurrentState(State_t state) {
        if (state.IsAbominable())
            throw std::runtime_error("Attempt to set abominable state as current on FSM");
        else if (m_is_fsm_active)
            throw std::runtime_error("Cannot change the current state while FSM is active");
        
        const auto* stored_state = state.m_state;

        // If the current state changes while waiting on a resettable awaiter,
        // trigger the reset
        if (m_cur_state != stored_state && m_state_to_reset != nullptr) {
            m_do_one_shot_reset = true;
            m_state_to_reset.resume();
            m_do_one_shot_reset = false;
            m_state_to_reset = nullptr; // Clear if this was set
        }

        if (m_transition_observer) {
            auto cur_state = GetCurrentState();
            if (!cur_state.has_value() || cur_state->m_state != stored_state) {
                m_transition_observer(*this, cur_state, State_t(*stored_state), Event_t());
            }
        }

        m_cur_state = stored_state;
        return *this;
    }

    /// @brief Sets the current state of the FSM.
    /// @details The previous state will remain in its prior stage in execution; ie.,
    /// no resumption or restart occurs on the former current state.
    FSM& SetCurrentState(StateId id) {
        const StoredState_t* state = FindStoredState(id);

        if (state == nullptr)
            throw std::runtime_error("Attempt to set nonexistent state on FSM");
        
        return SetCurrentState(State_t(*state));
    }

    FSM& SetTransitionObserver(std::function<void(const FSM&, std::optional<State_t>, State_t, const Event_t&)> observer) {
        m_transition_observer = std::move(observer);
        return *this;
    }

    /// @brief Adds a transition that defines `from->to` whenever \p event is sent to this FSM.
    /// @details After this function is called, whenever the current state is \p from, and the FSM
    /// receives an event with the Id \p event, then it will automatically transition the current state
    /// to \p to before execution resumes. Both internally generated and externally generated events
    /// will trigger this transition.\n
    /// The combination of (from, event) forms a unique transition. If such a transition already exists,
    /// then its target \p to will be replaced.
    Transition_t AddTransition(StateId from, EventId event, StateId to) {
        auto to_handle = [&]() -> StateHandle {
            if (const StoredState_t* state = FindStoredState(to)) {
                return state->second.Handle();
            } else {
                return nullptr;
            }
        }();

        auto res = m_transition_table.emplace(
            PartialTransition{
                .from = std::move(from),
                .event = std::move(event)
            },
            LocalTransition{to_handle, to}
        );

        return Transition_t(*this, *res.first);
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
    Transition_t AddRemoteTransition(
        StateId from,
        EventId event,
        FSM<DStateId, DEventId>& remote_fsm,
        DStateId to) {
        static_assert(std::is_same_v<EventId, DEventId>, "Remote transition must provide EventId translation if the types do not match.");

        return AddRemoteTransition(from, event, remote_fsm, to, event);
    }

    template<typename DStateId, typename DEventId>
    Transition_t AddRemoteTransition(
        StateId from_state,
        EventId from_event,
        FSM<DStateId, DEventId>& remote_fsm,
        DStateId to_state,
        DEventId translated_event) {
        
        using Remote_t = FSM<DStateId, DEventId>;

        auto to_handle = [&]() -> Remote_t::StateHandle {
            if (const auto* state = remote_fsm.FindStoredState(to_state)) {
                return state->second.Handle();
            } else {
                return nullptr;
            }
        }();

        auto target = std::make_unique<detail::RemoteTransitionTarget<StateId, EventId, DStateId, DEventId>>(
            remote_fsm,
            to_handle,
            to_state,
            translated_event
        );

        auto res = m_transition_table.emplace(
            PartialTransition{
                .from = std::move(from_state),
                .event = std::move(from_event)
            },
            RemoteTransition{std::move(target)}
        );

        return Transition_t(*this, *res.first);
    }

    auto GetTransitions() {
        return m_transition_table | std::views::transform([this](auto& kv) {
            return Transition_t(*this, kv);
        });
    }

    auto GetTransitions() const {
        return m_transition_table | std::views::transform([this](const auto& kv) {
            return CTransition_t(*this, kv);
        });
    }

    std::optional<Transition_t> GetTransition(const StateId& from, const EventId& event) {
        typename PartialTransition::Indirect key{&from, &event};
        auto it = m_transition_table.find(key);

        if (it != m_transition_table.end()) {
            return Transition_t(*this, *it);
        } else {
            return std::nullopt;
        }
    }

    std::optional<CTransition_t> GetTransition(const StateId& from, const EventId& event) const {
        typename PartialTransition::Indirect key{&from, &event};
        auto it = m_transition_table.find(key);

        if (it != m_transition_table.end()) {
            return Transition_t(*this, *it);
        } else {
            return std::nullopt;
        }
    }

    bool RemoveTransition(StateId from, EventId event) {
        typename PartialTransition::Indirect key{&from, &event};
        auto it = m_transition_table.find(key);

        if (it != m_transition_table.end()) {
            m_transition_table.erase(it);
            return true;
        } else {
            return false;
        }
    }
   
    bool RemoveTransition(const Transition_t& transition) {
        typename PartialTransition::Indirect key{&transition.From(), &transition.Event()};
        auto it = m_transition_table.find({transition.From(), transition.Event()});

        if (it != m_transition_table.end()) {
            m_transition_table.erase(it);
            return true;
        } else {
            return false;
        }
    }

    /// @brief Awaitable type used by @link FSM::EmitAndReceive @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct EmitReceiveAwaitable {
        FSM* self{};
        Event_t* event_source{};
        Event_t* event_dest{}; // This is usually the same as event_source
        std::coroutine_handle<> next_state{};

        bool await_ready() {
            auto res = self->CommonEmittingAwaitReady(std::move(*event_source));

            // Save the next_state in the awaiter in-case we suspend,
            // that way it can return the coroutine_handle from there
            next_state = res.next_state;
            return !res.should_suspend;
        }

        std::coroutine_handle<> await_suspend(StateHandle) {
            self->m_is_fsm_active = false;
            return next_state;
        }

        void await_resume() {
            self->m_is_fsm_active = true;
            *event_dest = std::move(self->m_event_for_next_resume);
            self->m_event_for_next_resume.Clear();
        }
    };

    friend struct EmitReceiveAwaitable;

    /// @brief Can be awaited from a State coroutine to send an event to this FSM whilst suspending the State until an event is sent to it.
    /// @details If \p event is empty, this FSM will be suspended. 
    /// Else, if (current state, \p event) is in the transition table, it will transition to the target state.
    /// If no transition was found, then the target state will be the current state.
    /// @param event The event to be sent and eventually received. It will be transparently replaced with the received event.
    [[nodiscard]] EmitReceiveAwaitable EmitAndReceive(Event_t& event) {
        if ((IsResetting() || m_do_one_shot_reset) && !event.Empty()) {
            throw std::runtime_error("Cannot send non-empty event from state that is resetting.");
        }

        if (!event.Empty() && !m_event_for_next_resume.Empty()) {
            throw std::runtime_error("Cannot send an event before receiving any events");
        }

        if (event.Empty()) {
            TakeLargestEventStorage(std::move(event));
        }

        // Handle the case for when we use EmitAndReceive before receiving an event
        if (m_event_for_next_resume.Empty()) [[likely]]
            return EmitReceiveAwaitable{this, &event, &event};
        else [[unlikely]]
            return EmitReceiveAwaitable{this, &m_event_for_next_resume, &event};
    }

    /// @brief Awaitable type used by @link FSM::EmitAndReceiveResettable @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct EmitReceiveResettableAwaitable {
        FSM* self{};
        Event_t* event_source{};
        Event_t* event_dest{}; // Usually the same as event_source
        std::coroutine_handle<> next_state{};

        bool await_ready() {
            if (not self->IsResetting() && not self->m_do_one_shot_reset) {
                // Save the current state to reset so the transitioner can detect a reset
                self->m_state_to_reset = self->m_cur_state->second.Handle();
            }

            auto res = self->CommonEmittingAwaitReady(std::move(*event_source));

            if (res.should_suspend) {
                // Save these variables on suspend for await_suspend to pick up
                next_state = res.next_state;
                return false;
            } else {
                // Clear the fact that this state may be reset if we don't suspend
                self->m_state_to_reset = nullptr;
                return true;
            }
        }

        std::coroutine_handle<> await_suspend(StateHandle from_state) {
            if (!next_state) {
                self->m_state_to_reset = from_state;
            }

            self->m_is_fsm_active = false;
            return next_state;          
        }

        [[nodiscard]] ResetToken await_resume() {
            auto reset_token = self->CommonResettableAwaitResume();

            if (reset_token.ShouldReset()) {
                event_dest->Clear();
            } else {
                *event_dest = std::move(self->m_event_for_next_resume);
                self->m_event_for_next_resume.Clear();
            }

            return reset_token;
        }
    };

    friend struct EmitReceiveResettableAwaitable;

    /// @brief Can be awaited from a State coroutine to send an event to this FSM
    /// whilst suspending the State until an event is sent to it, resetting if a transition occurs instead.
    /// @details If \p event is empty, this FSM will be suspended. 
    /// Else, if (current state, \p event) is in the transition table, it will transition to the target state.
    /// If the target state is a different, non-remote state, then a reset will be triggered. 
    /// If no transition was found, then the target state will be the current state.\n\n
    /// Awaiting the result of this function call will return a @ref ResetToken that will indicate whether a reset is happening.
    /// This can be used to clean up the variables of a @ref State coroutine.
    /// @param event The event to be sent and eventually received. It will be transparently replaced with the received event.
    [[nodiscard]] EmitReceiveResettableAwaitable EmitAndReceiveResettable(Event_t& event) {
        if ((IsResetting() || m_do_one_shot_reset) && !event.Empty()) {
            throw std::runtime_error("Cannot send non-empty event from state that is resetting.");
        }

        if (!event.Empty() && !m_event_for_next_resume.Empty()) {
            throw std::runtime_error("Cannot send an event before receiving any events");
        }

        if (m_event_for_next_resume.Empty()) [[likely]] {
            return EmitReceiveResettableAwaitable{this, &event, &event};
        } else {
            return EmitReceiveResettableAwaitable{this, &m_event_for_next_resume, &event};
        }
    }
    

    /// @brief Awaitable type used by @link FSM::ReceiveEvent @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct ReceiveAwaitable {
        FSM* self;
        Event_t* event_return;

        bool await_ready() {
            return self->CommonAwaitReady();
        }

        std::coroutine_handle<> await_suspend(StateHandle) {
            return self->CommonAwaitSuspend();
        }

        void await_resume() {
            self->m_is_fsm_active = true;
            *event_return = std::move(self->m_event_for_next_resume);
            self->m_event_for_next_resume.Clear();
        }
    };

    friend struct ReceiveAwaitable;

    /// @brief Can be awaited from a State coroutine to suspend it until an event is sent to it.
    /// @param[out] event_out Where the received Event will be written to
    /// @details The use of an out-parameter here allows for the Event's storage to be reused.
    [[nodiscard]] ReceiveAwaitable ReceiveEvent(Event_t& event_out) {
        // Bring the storage for the Event into the FSM so it can be reused by the next
        // call to InsertEvent or EmitAndReceive
        if (!IsResetting()) {
            event_out.Clear(); // Clear the previous event
            TakeLargestEventStorage(std::move(event_out));
        }

        return ReceiveAwaitable{this, &event_out};
    }
    
    /// @brief Awaitable type used by @link FSM::ReceiveEventResettable @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct ReceiveResettableAwaitable {
        FSM* self;
        Event_t* event_return;

        bool await_ready() {
            return self->CommonAwaitReady();
        }

        std::coroutine_handle<> await_suspend(StateHandle from_state) {
            self->m_state_to_reset = from_state;
            return self->CommonAwaitSuspend();
        }

        [[nodiscard]] ResetToken await_resume() {
            auto reset_token = self->CommonResettableAwaitResume();

            if (reset_token.ShouldReset()) {
                event_return->Clear();
            } else {
                *event_return = std::move(self->m_event_for_next_resume);
                self->m_event_for_next_resume.Clear();
            }

            return reset_token;
        }
    };

    friend struct ReceiveResettableAwaitable;

    /// @brief Can be awaited from a State coroutine to suspend it until an event is sent to it.
    /// @param[out] event_out Where the received Event will be written to
    /// @details The use of an out-parameter here allows for the Event's storage to be reused.
    [[nodiscard]] ReceiveResettableAwaitable ReceiveEventResettable(Event_t& event_out) {
        // Bring the storage for the Event into the FSM so it can be reused by the next
        // call to InsertEvent or EmitAndReceive
        if (!IsResetting()) {
            event_out.Clear(); // Clear the previous event
            TakeLargestEventStorage(std::move(event_out));
        }
        
        return ReceiveResettableAwaitable{this, &event_out};
    }

    /// @brief Awaitable type used by @link FSM::ReceiveInitialEvent @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct InitialReceiveAwaitable {
        FSM* self;

        bool await_ready() {
            return self->CommonAwaitReady();
        }

        std::coroutine_handle<> await_suspend(StateHandle) {
            return self->CommonAwaitSuspend();
        }

        [[nodiscard]] Event_t await_resume() {
            self->m_is_fsm_active = true;
            Event_t ret = std::move(self->m_event_for_next_resume);
            self->m_event_for_next_resume.Clear();
            return ret;
        }
    };

    friend struct InitialReceiveAwaitable;

    /// @brief Can be awaited from a State coroutine to suspend it until an event is sent to it.
    /// @warning This function should not be used if an Event variable already exists within the state,
    /// as Event storage cannot be reused when it is returned from the await expression.
    /// Improper usage of this function will cause calls to InsertEvent to unnecessarily allocate memory.
    /// @return When this function call is awaited, it will return the received Event.
    [[nodiscard]] InitialReceiveAwaitable ReceiveInitialEvent() {
        return InitialReceiveAwaitable{this};
    }

    /// @brief Awaitable type used by @link FSM::IgnoreEvent @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct IgnoreAwaitable {
        FSM* self;

        bool await_ready() {
            return self->CommonAwaitReady();
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
    [[nodiscard]] IgnoreAwaitable IgnoreEvent() {
        return IgnoreAwaitable{this};
    }

    /// @brief Awaitable type used by @link FSM::IgnoreResettableEvent @endlink
    /// @details This type is automatically managed by C++20 Coroutines, and its definition
    /// is not relevant for end-users of this library.
    struct IgnoreResettableAwaitable {
        FSM* self;

        bool await_ready() {
            return self->CommonAwaitReady();
        }

        std::coroutine_handle<> await_suspend(StateHandle from_state) {
            self->m_state_to_reset = from_state;
            return self->CommonAwaitSuspend();
        }

        [[nodiscard]] ResetToken await_resume() {
            self->m_event_for_next_resume.Clear();
            return self->CommonResettableAwaitResume();
        }
    };

    friend struct IgnoreResettableAwaitable;

    /// @brief Can be awaited from a State coroutine to ignore the next event sent to this state, suspending
    /// until it is received.
    [[nodiscard]] IgnoreResettableAwaitable IgnoreEventResettable() {
        return IgnoreResettableAwaitable{this};
    }

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
    template<std::invocable<Event_t&> F>
    FSM& InsertEvent(F&& event_initializer) {
        if (m_is_fsm_active) [[unlikely]] {
            throw std::runtime_error("Cannot insert event while the FSM is running");
        }

        std::forward<F>(event_initializer)(m_event_for_next_resume);

        // Treat this event as one that could change the state
        const Transitioner* potential_transition = GetTransitioner(m_event_for_next_resume);

        // If transitioner is null, then a guard blocked it
        if (potential_transition == nullptr)
            return *this;

        std::coroutine_handle<> state_to_resume = potential_transition->Perform(*this);
    
        // If an exception is thrown while the state is running, catch it to 
        // always cleanup
        try {
            state_to_resume.resume();
        } catch (...) {
            potential_transition->ReportDone(*this, true);
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

    std::optional<State_t> GetState(StateId id) const {
        if (auto it = m_states.find(id); it != m_states.end())
            return State_t(*it);
        else
            return std::nullopt;
    }

    auto GetStates() const {
        return m_states | std::views::transform([](const StoredState_t& s) {
            return State_t(s);
        });
    }

    bool HasState(StateId id) const {
        return m_states.find(id) != m_states.end();
    }

    std::size_t NumStates() const { return m_states.size(); }

    bool RemoveState(StateId id) {
        const auto erase_it = m_states.find(id);
        
        if (erase_it == m_states.end()) {
            return false;
        } else if (&*erase_it == m_cur_state && m_is_fsm_active) {
            throw std::runtime_error("Cannot delete state while it is running");
        }

        if (&*erase_it == m_cur_state) {
            m_state_to_reset = nullptr;
            m_cur_state = nullptr;
        }

        m_states.erase(erase_it);
        RebindLocalTransitions();
        return true;
    }

    bool RemoveState(State_t state) {
        return RemoveState(state.Id());
    }

    /// @brief Removes all states marked as abominable.
    /// @return The number of states removed
    std::size_t RemoveAbominableStates() {
        return std::erase_if(m_states, [](const StoredState_t& s) {
            return s.second.IsAbominable();
        });
    }

    /// @brief Rebinds the latest state handles of the target IDs of remote transitions
    void RebindRemoteTransitions() {
        for (auto& [from, transitioner] : m_transition_table) {
            if (transitioner.IsRemote())
                transitioner.Rebind(*this);
        }
    }

    /// @brief Removes dangling transitions left after a target state was removed
    /// or was made abominable via an uncaught exception
    std::size_t RemoveDanglingTransitions() {
        return std::erase_if(m_transition_table, [this](const auto& p) {
            return p.second.IsDangling(*this);
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
    /// @returns True if an event was resent.
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

    // // Callback for debugging and writing log. It is called when the state of
    // // the fsm whose name is in the first argument is about
    // // to change from 'from' to 'to' because the from is sending
    // // event 'event'.
    // std::function<
    //     void(const FSM& fsm, 
    //          const State_t& from,
    //          const Event_t& event,
    //          const State_t& to
    //     )
    // > logger;

private:
    struct LocalTransitionTarget {
        StateHandle state{};
    };

    template<typename, typename>
    friend class dxfsm::FSM;

    template<typename, typename, typename, typename>
    friend class detail::RemoteTransitionTarget;

    template<typename>
    friend struct detail::StatePromiseType;

    template<typename, typename, bool>
    friend class Transition;

    const Transitioner* GetTransitioner(const Event_t& event) const noexcept {
        typename PartialTransition::Indirect key{&m_cur_state->first, &event.GetId()};
        if (auto it = m_transition_table.find(key); it != m_transition_table.end()) {
            const auto& guard = it->second.Guard();

            if (!guard || guard(event))
                return &it->second;
            else
                return nullptr; // Guard rejected, return null transition
        } else {
            return &null_transition; // No coded state transition, return null transition
        }
    }
    
    detail::StoredState<StateId>& EmplaceState(StateId id, StateHandle handle) {
        auto state_it = m_states.find(id);

        if (state_it != m_states.end() && !state_it->second.IsAbominable()) {
            throw std::runtime_error("Attempt to add a state with a conflicting id to FSM");
        }

        if (state_it == m_states.end()) {
            state_it = m_states.emplace(
                std::piecewise_construct,
                std::forward_as_tuple(std::move(id)), 
                std::forward_as_tuple(handle)
            ).first;
        } else {
            state_it->second.m_coro_handle = handle;
        }

        RebindLocalTransitions();

        return *state_it;
    }

    StoredState_t* FindStoredState(StateId id) {
        auto it = m_states.find(id);
        return it != m_states.end() ? &*it : nullptr;
    }

    const StoredState_t* FindStoredState(StateId id) const {
        auto it = m_states.find(id);
        return it != m_states.end() ? &*it : nullptr;
    }

    // This does not consider one-shot reset
    bool IsResetting() const {
        return m_transition_after_reset != nullptr;
    }

    bool ShouldResumeRespondWithReset() const {
        return m_transition_after_reset != nullptr;
    }

    bool ShouldSendResetOnTransition() const {
        return m_state_to_reset != nullptr;
    }

    /// @brief Rebinds transitions
    void RebindLocalTransitions() {
        for (auto& [from, transitioner] : m_transition_table) {
            if (not transitioner.IsRemote())
                transitioner.Rebind(*this);
        }
    }

    // If currently resetting, this will return the stored post-reset transition.
    // Else, it will suspend entirely.
    std::coroutine_handle<> CommonAwaitSuspend() {
        m_is_fsm_active = false;

        if (not IsResetting() || m_do_one_shot_reset) {
            return std::noop_coroutine();
        }

        // If we get here while resetting, the reset is complete and the state
        // has suspended. Clear the reset info.
        const Transitioner* transitioner = std::exchange(m_transition_after_reset, nullptr);
        m_state_to_reset = nullptr;

        return transitioner->Perform(*this);
    }

    // If currently resetting, this will always return false.
    // Otherwise, this will return true if there is an event ready
    bool CommonAwaitReady() {
        return not IsResetting() && not m_do_one_shot_reset && not m_event_for_next_resume.Empty();
    }

    // If currently resetting, this will resume the stored post-reset transition.
    // Else, it will check the pending event and perform its transitioner
    auto CommonEmittingAwaitReady(Event_t&& pending_event) {
        struct Result {
            std::coroutine_handle<> next_state{};
            bool should_suspend{};
        };

        // If this state sends an empty event and we're not resetting,
        // then suspend the FSM.
        // Also check if the FSM has consumed the prior event, which is only
        // false if EmitAndReceive() or the resettable version is called first.
        if (pending_event.Empty() && m_event_for_next_resume.Empty() && not IsResetting()) {
            return Result{std::noop_coroutine(), true};
        }

        // If this is a one-shot reset, suspend the FSM
        if (m_do_one_shot_reset) {
            return Result{std::noop_coroutine(), true};
        }

        const Transitioner* const transitioner = [&] {
            if (IsResetting()) {
                return m_transition_after_reset;
            } else if (!m_event_for_next_resume.Empty()) [[unlikely]] {
                // If there is a pending event, receive it without emitting
                // This will only happen if emitting an empty event is the first
                // thing a coroutine does (no receive first).
                return &null_transition;
            } else {
                return GetTransitioner(pending_event);
            }
        }();

        // Transitioner is only ever null if a guard blocked it from firing
        if (transitioner == nullptr) {
            return Result{std::noop_coroutine(), true};
        }

        if (!IsResetting()) {
            // Store the event into the FSM to be picked up by the transitioner
            // Gate by !IsResetting() so we don't overwrite the original event
            // that triggered the reset
            m_event_for_next_resume = std::move(pending_event);
        }

        // Save cur_state here because transitioner.Perform() may update m_cur_state
        const auto cur_state = m_cur_state->second.Handle();

        Result res{};
        res.next_state = transitioner->Perform(*this);
        res.should_suspend = cur_state != res.next_state;
        return res;
    }

    ResetToken CommonResettableAwaitResume() {
        m_is_fsm_active = true;

        if (ShouldResumeRespondWithReset()) {
            return true;
        } else if (m_do_one_shot_reset) {
            return true;
        } else {
            m_state_to_reset = nullptr;
            return false;
        }
    }

    // Takes the capacity of the input event if it is larger
    // then the stored event capacity,
    // but only if the stored event is empty.
    void TakeLargestEventStorage(Event_t&& in) {
        if (!m_event_for_next_resume.Empty())
            return;

        if (in.Capacity() > m_event_for_next_resume.Capacity()) {
            m_event_for_next_resume = std::move(in);
        }
    }

    const StoredState_t* m_cur_state{};
    
    // Transition table in format {from-state, event} -> to-state
    // That is, an event sent from from-state will be routed to to-state.
    TransitionTable_t m_transition_table{};
    detail::StateMap<StateId> m_states{};

    const Transitioner* m_transition_after_reset{};
    StateHandle m_state_to_reset{};
    bool m_do_one_shot_reset{};

    // True if the FSM is running, false if suspended.
    bool m_is_fsm_active{};

    // Temporary event storage to be captured by Awaitable on resume
    Event_t m_event_for_next_resume{};

    std::function<void(const FSM&, std::optional<State_t>, State_t, const Event_t&)> m_transition_observer;

    // Shared null_transition instance
    inline static const Transitioner null_transition{NullTransition{}};
}; // FSM

namespace detail {
    // O... = Origin, D... = Destination
    template<typename OStateId, typename OEventId, typename DStateId, typename DEventId>
    class RemoteTransitionTarget final : public RemoteTransitionTargetBase {
        using DFSM_t = FSM<DStateId, DEventId>;

        DFSM_t* target_fsm{};
        typename DFSM_t::Transitioner local_transitioner{};
        typename DFSM_t::StateHandle target_state{};
        DStateId id_to_resume{};
        DEventId translated_event_id;

    public:
        RemoteTransitionTarget(
            DFSM_t& target_fsm,
            typename DFSM_t::StateHandle target_state,
            DStateId id_to_resume,
            DEventId translated_id)
            : target_fsm(&target_fsm),
              local_transitioner(typename DFSM_t::LocalTransition{target_state}),
              target_state(target_state),
              id_to_resume(std::move(id_to_resume)),
              translated_event_id(std::move(translated_id))
        { }

        std::coroutine_handle<> TransitionOnRemote(void* originating_fsm_ptr) const override {
            auto& originating_fsm = *static_cast<FSM<OStateId, OEventId>*>(originating_fsm_ptr);
            // Translate the event to the EventId type of the target FSM,
            // then store it into the target FSM
            auto& originating_event = originating_fsm.m_event_for_next_resume;
            auto& target_event = target_fsm->m_event_for_next_resume;

            target_event = originating_event.TransferToIdType(DEventId(translated_event_id));

            const bool different_states = target_fsm->m_cur_state == nullptr
                || target_fsm->m_cur_state->second.Handle() != target_state;

            if (!target_fsm->IsResetting() && target_fsm->ShouldSendResetOnTransition() && different_states) {
                target_fsm->m_transition_after_reset = &local_transitioner;

                target_fsm->m_cur_state = target_fsm->m_state_to_reset.promise().self;
                return target_fsm->m_state_to_reset;
            } else {
                return local_transitioner.Perform(*target_fsm);
            }
        }
        
        void ReportDone(bool nullify_current_state) const override {
            if (nullify_current_state) {
                target_fsm->m_cur_state = nullptr;
            }

            target_fsm->m_state_to_reset = nullptr;
            target_fsm->m_is_fsm_active = false;
        }

        bool RebindTransition() override {
            if (const auto* state = target_fsm->FindStoredState(id_to_resume)) {
                target_state = state->second.Handle();
                local_transitioner = typename DFSM_t::LocalTransition{target_state};

                return true;
            } else {
                target_fsm = nullptr;
                local_transitioner = {};
            }

            return false;
        }

        bool IsTransitionDangling() const override {
            const auto state = target_fsm->GetState(id_to_resume);
            return !state.has_value() || state->IsAbominable();
        }
    };
}

} // namespace dxfsm
#endif // DXFSM_HPP
