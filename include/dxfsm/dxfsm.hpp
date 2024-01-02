#ifndef DXFSM_HPP
#define DXFSM_HPP

#include <coroutine>
#include <memory>
#include <string_view>
#include <utility>
#include <stdexcept>
#include <type_traits>
#include <functional>
#include <vector>
#include <cassert>
#include <atomic>
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

// Generic reusable Event class.
// An object of this type hold its identity in a string_view
// and data in a byte buffer. Hence an event object can be reused
// by replacing the identity with a new one and storing the data
// of the new event in the buffer. If the buffer is too small
// for the data, it will be extended a bit like std::vector does.
// The buffer never shrinks but can be reset to zero lenght like std::vector.
template<typename Id>
class Event {
public:
    using Id_t = Id;

    Event() noexcept = default;
    explicit Event(Id id) noexcept : _id(std::move(id)) { }

    template<typename T>
    Event(Id id, T&& data) {
        Store(std::move(id), std::forward<T>(data));
    }

    template<typename T, typename... Args >
    Event(Id id, std::in_place_type_t<T> type [[maybe_unused]], Args&&... args) {
        Emplace<T>(std::move(id), std::forward<Args>(args)...);
    }

    // Moves
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

    // Shorthand for updating an event to a new ID without accidental move-assignment,
    // which would cause the internal buffer to be replaced.
    Event& operator=(Id id) {
        Store(std::move(id));
        return *this;
    }

    ~Event() {
        // If *_any_ptr contains an AnyPtr<T> object which points to the buffer,
        // the object living in the buffer will be destroyed at the destructor of AnyPtr<T>
        _any_ptr.Reset();
        delete[] _storage;
    }

    // Constructs a new event without associated data
    void Store(Id id) {
        this->_id = std::move(id);
        _any_ptr.Reset();  // Destroy the object currently living in the buffer by implicitly invoking AnyPtr<T> destructor.
    }

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

    template<typename T, typename... Args>
    T& Emplace(Id id, Args&&... args) {
        _any_ptr.Reset();
        this->Reserve(sizeof(T));

        T* ptr = std::construct_at(reinterpret_cast<T*>(_storage), std::forward<Args>(args)...);

        this->_id = std::move(id);
        this->_any_ptr = ptr;

        return *ptr;
    }
        
    void Clear() {
        this->_id.reset();
        _any_ptr.Reset();
    }

    // If the type of the object stored in the buffer is not T, an exception will be thrown.
    // So you can not accidentally read the data in a wrong format.
    template<class T>
    T& Get() {
        T* ptr = _any_ptr.TryExtract<T>();

        if (ptr == nullptr) {
            throw std::runtime_error("Attempt to extract data from Event which does not hold the requested type");
        }

        return *ptr;
    }

    template<class T>
    const T& Get() const {
        const T* ptr = _any_ptr.TryExtract<T>();

        if (ptr == nullptr) {
            throw std::runtime_error("Attempt to extract data from Event which does not hold the requested type");
        }

        return *ptr;
    }

    template<class T>
    T* GetMaybe() {
        return _any_ptr.TryExtract<T>();
    }

    template<class T>
    const T* GetMaybe() const {
        return _any_ptr.TryExtract<T>();
    }

    // Reinterprets the data buffer as an object of type T.
    template<class T>
    T& GetUnchecked() {
        return *std::launder(reinterpret_cast<T*>(_storage));
    }

    // Reinterprets the data buffer as an object of type T.
    template<class T>
    const T& GetUnchecked() const {
        return *std::launder(reinterpret_cast<const T*>(_storage));
    }

    // Returns pointer to the data buffer
    void* Data() {
        return _storage;
    }

    const void* Data() const {
        return _storage;
    }

    // Deletes all allocated data and resets the event to its default, empty state
    void ReleaseStorage() {
        Event dummy{};
        this->Swap(dummy);
    }

    // Reserves space for event data. The existing data may be wiped out.
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

    // Returns the maximum size of an object which can be constructed
    // in the data buffer without reallocation.
    std::size_t Capacity() const { return _capacity; }

    // Returns true if the name of the event == other
    bool operator==(const Id& id) const { return _id.has_value() && _id.value() == id; }

    // Returns true if the event is empty (i.e. _id is not set)
    [[nodiscard]] bool Empty() const { return !_id.has_value(); }

    // Checks if the event has stored data associated with it
    template<typename T = void>
    bool HasData() const {
        if constexpr (std::is_same_v<T, void>) {
            return _any_ptr.HasValue();
        } else {
            return GetMaybe<T>() != nullptr;
        }
    }

    const Id& GetId() const { return _id.value(); }

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
}; // Event

template<typename Id>
void swap(Event<Id>& lhs, Event<Id>& rhs) {
    lhs.Swap(rhs);
}

template<typename StateId, typename EventId>
class FSM;

// Return type of coroutines which represent states.
template<typename StateId>
struct State {
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

    using handle_type = std::coroutine_handle<promise_type>;

    // State has one-to-one correspondence to its coroutine handle
    handle_type handle() const noexcept { return m_coro_handle; }

    // State has one-to-one correspondence to its coroutine handle
    operator handle_type() const noexcept { return m_coro_handle; }

    StateId Id() const {
        if (m_coro_handle.address() == nullptr && !m_is_abominable) {
            throw std::runtime_error("Attempt to get the ID of a state not associated with a coroutine");
        }

        return m_id;
    }

    // Sets human-readable name for the state.
    State& Name(std::string state_name) & {
        if (m_coro_handle.address() == nullptr && !m_is_abominable) {
            throw std::runtime_error("Attempt to set the name of State not associated with a coroutine");
        }

        m_name = std::move(state_name);
        return *this;
    }

    // Sets human-readable name for the state.
    State&& Name(std::string state_name) && {
        return std::move(this->Name(state_name));
    }

    // Gets the human-readable name for the state.
    std::string_view Name() const {
        if (m_coro_handle.address() == nullptr && !m_is_abominable) {
            throw std::runtime_error("Attempt to get the name of a State not associated with a coroutine");
        }

        return m_name;
    }

    // Abominable means that this state was killed due to an exception,
    // and should be removed or readded by the user.
    bool IsAbominable() const {
        return m_is_abominable;
    }

    // Move constructors.
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

    ~State() {
        if (m_coro_handle)
            m_coro_handle.destroy();
    }
private:
    // A state is move-only
    State(const State&) = delete;
    State& operator=(const State&) = delete;

    explicit State(promise_type *p) noexcept 
        : m_coro_handle(handle_type::from_promise(*p)),
          m_id(p->id)
    {
        p->self = this;
    }

    handle_type m_coro_handle{};
    // Need a copy of the ID here in case the coroutine throws an exception, as the promise
    // object gets destroyed. This keeps the State object queryable for its identifying information.
    StateId m_id{};
    std::string m_name{};
    bool m_is_abominable{};
}; // State

namespace detail {
    struct DoneReporterBase {
        virtual ~DoneReporterBase() = default;

        virtual void ReportDone() = 0;
    };
}

// Finite State Machine class
template<typename StateId, typename EventId>
class FSM {
public:
    using State_t = State<StateId>;
    using Event_t = Event<EventId>;
    using StateHandle = typename State_t::handle_type;

    // Gives the FSM a human-readable name.
    FSM() = default;
    FSM(std::string fsm_name) : _name(std::move(fsm_name)) { }

    // Delete copies and moves
    // TODO: implement moves
    FSM(const FSM&) = delete;
    FSM(FSM&&) = delete;
    FSM& operator=(const FSM&) = delete;
    FSM& operator=(FSM&&) = delete;

    // Get the human-readable name of the FSM
    std::string_view Name() const { return _name; }

    // Set the human-readable name of the FSM
    FSM& Name(std::string name) { _name = std::move(name); return *this;}

    // Returns the name of the target state of the latest transition.
    auto GetCurrentState() const -> const State_t* {
        if (!_state) {
            return nullptr;
        }
        
        // TODO: Rework how this information is stored
        return FindState(_state.promise().id);
    }

    FSM& SetCurrentState(StateId id) {
        auto* state = FindState(id);

        if (state == nullptr)
            throw std::runtime_error(std::format("Attempt to set nonexistent state on FSM '{}'", Name()));
        
        _state = state->handle();
        return *this;
    }

    using HandleOrId = std::variant<StateHandle, StateId>;

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
            LocalTransitionTarget{
                .state = to_handle
            }
        );

        return *this;
    }

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

        auto target = std::make_unique<RemoteTransitionTarget<DStateId, DEventId>>();

        target->target_fsm = &remote_fsm;
        target->target_state_handle = to_handle;
        target->translated_event_id = event;

        m_transition_table.emplace(
            PartialTransition{
                .from = from_handle,
                .event = std::move(event)
            },
            std::move(target)
        );

        return *this;
    }

    // // Adds transition from state 'from' to state 'to' on event 'onEvent' which lives in FSM 'targetFSM'.
    // // targetFSM==nullptr means this FSM, so the 4th argument can be omitted if every state
    // // refers to the same FSM.
    // // Returns true if {from, onEvent} pair has not been routed previously.
    // // Returns false if an existing destination is replaced with '{to, targetFSM}'.
    // // Typically should return true unless you deliberately modify the state machine on the fly.

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

    struct EmitReceiveAwaitable {
        FSM* self;
        Event_t* event_return{};

        constexpr bool await_ready() {return false;}
        std::coroutine_handle<> await_suspend(StateHandle fromState) {
            const Event_t& pending_event = self->m_event_for_next_resume;
            
            // If a state emits an empty event all states will remain suspended.
            // Consequently, the FSM will stopped. It can be restarted by calling SendEvent()
            if (pending_event.Empty()) {
                self->m_is_fsm_active.store(false, std::memory_order::seq_cst);
                return std::noop_coroutine();
            }

            std::optional<TransitionResult> transition = self->PerformTransition();

            if (!transition.has_value()) {
                throw std::runtime_error(std::format(
                    "FSM '{}' can't find transition from state '{}' for given event. Please fix the transition table",
                    self->Name(), self->FindState(fromState.promise().id)->Name()
                ));
            }

            if (transition->IsRemote()) {
                self->m_is_fsm_active.store(false, std::memory_order::seq_cst);
            }

            return transition->state_to_resume;
        }

        void await_resume() {
            *event_return = std::move(self->m_event_for_next_resume);
        }
    };

    friend struct EmitReceiveAwaitable;

    EmitReceiveAwaitable EmitAndReceive(Event_t& e) {
        m_event_for_next_resume = std::move(e);
        return EmitReceiveAwaitable{this, &e};
    }

    // A simplified form of EmitAndReceive that only waits for an event,
    // and does not have the emission logic needed for transmission.
    // Uses an out parameter instead of a return.
    struct ReceiveAwaitable {
        FSM* self;
        Event_t* event_return;

        bool await_ready() {
            // const Event_t& pending_event = self->m_event_for_next_resume;
            // return !pending_event.Empty();
            return false;
        }

        void await_suspend(StateHandle) {
            // If there is no event ready to be read, suspend the entire FSM
            self->m_is_fsm_active.store(false, std::memory_order::seq_cst);
        }

        void await_resume() {
            self->m_is_fsm_active.store(true, std::memory_order::seq_cst);
            *event_return = std::move(self->m_event_for_next_resume);
        }
    };

    friend struct ReceiveAwaitable;

    // Returns an awaitable which gives the next event sent to the awaiting state coroutine.
    // This takes an out parameter to allow for reuse of the Event object.
    ReceiveAwaitable ReceiveEvent(Event_t& out) {
        out.Clear(); // Clear the previous event
        // Bring the storage for the Event into the FSM so it can be reused by the next
        // call to SendEvent or EmitAndReceive
        m_event_for_next_resume = std::move(out);
        return ReceiveAwaitable{this, &out};
    }
    
    // A simplified form of EmitAndReceive that only waits for an event,
    // and does not have the emission logic needed for transmission.
    // Uses a return instead of an out parameter.
    struct InitialReceiveAwaitable {
        FSM* self;

        bool await_ready() {
            const Event_t& pending_event = self->m_event_for_next_resume;
            return !pending_event.Empty();
        }

        void await_suspend(StateHandle) {
            // If there is no event ready to be read, suspend the entire FSM
            self->m_is_fsm_active.store(false, std::memory_order::seq_cst);
        }

        [[nodiscard]] Event_t await_resume() {
            self->m_is_fsm_active.store(true, std::memory_order::seq_cst);
            return std::move(self->m_event_for_next_resume);
        }
    };

    friend struct InitialReceiveAwaitable;

    InitialReceiveAwaitable ReceiveInitialEvent() {
        return InitialReceiveAwaitable{this};
    }

    
    // Waits for an event and discards its contents whilst allowing for 
    // storage reuse
    struct IgnoreAwaitable {
        FSM* self;

        bool await_ready() {
            const Event_t& pending_event = self->m_event_for_next_resume;
            return !pending_event.Empty();
        }

        void await_suspend(StateHandle) {
            // If there is no event ready to be read, suspend the entire FSM
            self->m_is_fsm_active.store(false, std::memory_order::seq_cst);
        }

        void await_resume() {
            self->m_is_fsm_active.store(true, std::memory_order::seq_cst);
            self->m_event_for_next_resume.Clear();
        }
    };

    friend struct IgnoreAwaitable;

    // Waits for an event and discards its contents whilst allowing for 
    // storage reuse
    IgnoreAwaitable IgnoreEvent() {
        return IgnoreAwaitable{this};
    }

    // Adds a state to the state machine without associating any events with it.
    // Returns the index of the vector to which the state was stored.
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

    // Kick off a suspended state machine by sending an event.
    // If (current state, event id) is in the transition table,
    // then it is sent to the next_state as defined by the transition.
    // Else, it is sent to the current_state.
    FSM& InsertEvent(Event_t&& e) {
        // Store the event in the FSM, which will get moved by the next Awaitable resumed
        m_event_for_next_resume = std::move(e);

        // Treat this event as one that could change the state
        std::optional<TransitionResult> potential_transition = PerformTransition();

        // // potential_transition has_value() means that a transition was found and
        // // potential_transition is the coroutine to be resumed.
        // // Else, no transition occured, send the event to the current state.
        std::coroutine_handle<> state_to_resume = potential_transition.has_value() ?
            potential_transition->state_to_resume :
            _state;

        // If an exception is thrown while the state is running, catch it to 
        // always cleanup
        try {
            state_to_resume.resume();
        } catch (...) {
            if (potential_transition.has_value() && potential_transition->IsRemote()) {
                potential_transition->remote_done_reporter->ReportDone();
            } else {
                m_is_fsm_active.store(false, std::memory_order::seq_cst);
            }
            throw;
        }

        return *this;
    }

    bool HasState(StateId id) const {
        return std::ranges::find(m_states, id, &State_t::Id) != m_states.end();
    }

    auto GetAbominableStates() const {
        return m_states | std::views::filter([](const State_t& s) {
            return s.IsAbominable();
        });
    }

    void RemoveAbominableStates() {
        std::erase_if(m_states, [](const State_t& s) {
            return s.IsAbominable();
        });
    }

    // Returns true if the FSM is running and false if all states
    // are suspended and waiting for an event.
    bool IsActive() const { return m_is_fsm_active; }
    const std::atomic<bool>& GetActiveFlag() const { return m_is_fsm_active; }


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

    void TrySetActive() {
        bool is_started = false;
        if (!m_is_fsm_active.compare_exchange_strong(is_started, true, std::memory_order::seq_cst)) {
            throw std::runtime_error(std::format("Attempt to insert event into FSM '{}' while it is running", Name()));
        }
    }

    struct RemoteDoneReporter final : detail::DoneReporterBase {
        FSM* self{};

        RemoteDoneReporter(FSM& self) : self(&self) { }

        void ReportDone() override {
            self->m_is_fsm_active.store(false, std::memory_order::seq_cst);
        }
    };

    struct LocalTransitionTarget {
        StateHandle state{};
    };

    struct RemoteTransitionTargetBase {
        virtual ~RemoteTransitionTargetBase() = default;

        virtual std::coroutine_handle<> TryTransitionOnRemote(FSM& originating_fsm) const = 0;
        virtual auto MakeExceptionReporter() const -> std::unique_ptr<detail::DoneReporterBase> = 0;
    };

    // D... = Destination
    template<typename DStateId, typename DEventId>
    struct RemoteTransitionTarget final : RemoteTransitionTargetBase {
        using DFSM_t = FSM<DStateId, DEventId>;

        DFSM_t* target_fsm{};
        typename DFSM_t::StateHandle target_state_handle{};
        DEventId translated_event_id;

        std::coroutine_handle<> TryTransitionOnRemote(FSM& originating_fsm) const override {
            target_fsm->TrySetActive();
            
            // Translate the event to the EventId type of the target FSM,
            // then store it into the target FSM
            auto& originating_event = originating_fsm.m_event_for_next_resume;
            auto& target_event = target_fsm->m_event_for_next_resume;

            target_event = originating_event.template TransferToIdType(DEventId(translated_event_id));

            target_fsm->_state = target_state_handle;

            originating_fsm.m_is_fsm_active.store(false, std::memory_order::seq_cst);
            return target_state_handle;
        }

        auto MakeExceptionReporter() const -> std::unique_ptr<detail::DoneReporterBase> override {
            return std::make_unique<typename DFSM_t::RemoteDoneReporter>(*target_fsm);
        }
    };

    template<typename DStateId, typename DEventId>
    friend struct RemoteTransitionTarget;

    using RemoteTransitionTargetPtr = std::unique_ptr<RemoteTransitionTargetBase>;
    using TransitionTarget = std::variant<LocalTransitionTarget, RemoteTransitionTargetPtr>;

    struct TransitionResult {
        std::coroutine_handle<> state_to_resume{};
        std::unique_ptr<detail::DoneReporterBase> remote_done_reporter{};

        bool IsRemote() const {
            return remote_done_reporter != nullptr;
        }
    };

    std::optional<TransitionResult> PerformTransition() noexcept {
        const Event_t& pending_event = m_event_for_next_resume;
        const TransitionTarget* transition;

        if (auto it = m_transition_table.find({_state, pending_event.GetId()}); it != m_transition_table.end())
            transition = &it->second;
        else
            return std::nullopt; // No coded state transition, return empty handle to signify this

        return std::visit([this]<typename T>(const T& o) -> TransitionResult {
            if constexpr (std::is_same_v<T, LocalTransitionTarget>) {
                const LocalTransitionTarget& local = o;
                this->_state = local.state;

                return {
                    .state_to_resume = this->_state,
                    .remote_done_reporter = nullptr
                };
            } else if constexpr(std::is_same_v<T, RemoteTransitionTargetPtr>) {
                const RemoteTransitionTargetBase& remote = *o;
                auto remote_handle = remote.TryTransitionOnRemote(*this);

                return {
                    .state_to_resume = remote_handle,
                    .remote_done_reporter = remote.MakeExceptionReporter()
                };
            }
        }, *transition);
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


    // Transition table in format {from-state, event} -> to-state
    // That is, an event sent from from-state will be routed to to-state.
    std::unordered_map<PartialTransition, TransitionTarget, PartialTransitionHash> m_transition_table;

    // All coroutines which represent the states in the state machine
    std::vector<State_t> m_states;

    // True if the FSM is running, false if suspended.
    std::atomic<bool> m_is_fsm_active = false;

    // Temporary event storage to be captured by Awaitable on resume
    Event_t m_event_for_next_resume{};
}; // FSM

} // namespace dxfsm
#endif // DXFSM_HPP
