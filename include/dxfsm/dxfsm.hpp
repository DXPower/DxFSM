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

namespace dxfsm {
namespace detail {
    class AnyPtr {
        std::any m_underlying_any{};

        template<typename T>
        struct DestroyingPtr {
            T* ptr{};

            DestroyingPtr(T* ptr) noexcept : ptr(ptr) { }

            ~DestroyingPtr() requires std::is_trivially_destructible_v<T> = default;
            
            ~DestroyingPtr() noexcept(noexcept(ptr->~T()))
            requires (not std::is_trivially_destructible_v<T>) 
            { ptr->~T(); }
        };
    public:
        AnyPtr() = default;
        AnyPtr(std::nullptr_t) { }

        template<typename T>
        AnyPtr(T* ptr) : m_underlying_any(DestroyingPtr(ptr)) { }

        // Moves
        AnyPtr(AnyPtr&& move) noexcept = default;
        AnyPtr& operator=(AnyPtr&& move) noexcept = default;

        template<typename T>
        T* TryExtract() {
            auto* any_ptr = std::any_cast<DestroyingPtr<T>>(&m_underlying_any);
            return any_ptr ? any_ptr->ptr : nullptr;
        }

        template<typename T>
        const T* TryExtract() const {
            auto* any_ptr = std::any_cast<DestroyingPtr<T>*>(&m_underlying_any);
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

static const std::string _sharedEmptyString{};


// Generic reusable Event class.
// An object of this type hold its identity in a string_view
// and data in a byte buffer. Hence an event object can be reused
// by replacing the identity with a new one and storing the data
// of the new event in the buffer. If the buffer is too small
// for the data, it will be extended a bit like std::vector does.
// The buffer never shrinks but can be reset to zero lenght like std::vector.
template<typename Id>
struct Event {
    using Id_t = Id;

    Event() noexcept = default;
    Event(Id id) noexcept : _id(std::move(id)) { }

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
        this->Swap(other);
        return *this;
    }

    ~Event() {
        // If *_any_ptr contains an AnyPtr<T> object which points to the buffer,
        // the object living in the buffer will be destroyed at the destructor of AnyPtr<T>
        _any_ptr.Reset();
        delete[] _storage;
    }

    // Constructs a new event without associated data
    void Store(Id id)
    {
        this->_id = std::move(id);
        _any_ptr.Reset();  // Destroy the object currently living in the buffer by implicitly invoking AnyPtr<T> destructor.
    }

    template<typename T>
    std::remove_reference_t<T>& Store(Id id, T&& data) {
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
        return std::launder(reinterpret_cast<T*>(_storage));
    }

    // Reinterprets the data buffer as an object of type T.
    template<class T>
    const T& GetUnchecked() const {
        return std::launder(reinterpret_cast<const T*>(_storage));
    }

    // Returns pointer to the data buffer
    void* Data() {
        return _storage;
    }

    const void* Data() const {
        return _storage;
    }

    // Deletes all allocated data and resets the event to its default, empty state
    void ReleaseStorage()
    {
        this->swap({});
    }

    // Reserves space for event data. The existing data may be wiped out.
    void Reserve(std::size_t size) {
        if (_capacity < size) {
            if (_any_ptr.HasValue())  // Destroy the stored object if it exists
                _any_ptr.Reset();

            _id.reset();
            _capacity = size;
            delete [] _storage;
            _storage = new std::byte[size];
        }
    }

    // Returns the maximum size of an object which can be constructed
    // in the data buffer without reallocation.
    std::size_t Capacity() const { return _capacity; }

    // Returns true if the name of the event == other
    bool operator==(Id id) const { return _id.has_value() && _id.value() == id; }

    // Returns true if the event is empty (i.e. _id is not set)
    [[nodiscard]] bool Empty() const { return !_id.has_value(); }

    // Checks if the event has stored data associated with it
    bool HasData() const { return _any_ptr.HasValue(); }

    Id GetId() const { return _id.value(); }

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
}; // Event

template<typename Id>
inline void swap(Event<Id>& lhs, Event<Id>& rhs) {
    lhs.Swap(rhs);
}

template<typename StateId, typename EventId>
class FSM;

// Return type of coroutines which represent states.
template<typename StateId>
struct State {
    struct InfoView {
        StateId id;
        std::string_view name;
    };

    struct promise_type {
        // Awaitable returned when the coroutine is first constructed
        struct InitialAwaitable {
            promise_type* self = nullptr;
            constexpr bool await_ready() {return false;}
            void await_suspend(std::coroutine_handle<promise_type>) {}
            void await_resume() { self->is_started = true; } // The state was resumed from initial_suspend
        };

        promise_type() = delete; // State must take FSM& and Id parameters

        // Define both member and non-member functions
        template<typename EventId> 
        promise_type(FSM<StateId, EventId>& fsm [[maybe_unused]], StateId id) : id(id) { }
        template<typename Self, typename EventId> 
        promise_type(Self&&, FSM<StateId, EventId>& fsm [[maybe_unused]], StateId id) : id(id) { }

        InitialAwaitable initial_suspend() noexcept { return InitialAwaitable{this}; }
        constexpr std::suspend_always final_suspend() noexcept { return {}; }
        State get_return_object() noexcept { return State(this); };
        void unhandled_exception() { throw; }
        void return_void() {
            // State coroutines must never return.
            throw std::runtime_error("State coroutine is not allowed to co_return.");
        }

        InfoView GetInfoView() const {
            return InfoView{
                .id = id,
                .name = name
            };
        }

        StateId id;
        std::string name;

        // Only false before fsm.start() is called after the state has been added
        bool is_started = false;
    };

    using handle_type = std::coroutine_handle<promise_type>;

    // State has one-to-one correspondence to its coroutine handle
    handle_type handle() const noexcept { return m_coro_handle; }

    // State has one-to-one correspondence to its coroutine handle
    operator handle_type() const noexcept { return m_coro_handle; }

    StateId Id() const {
        if (m_coro_handle.address() == nullptr) {
            throw std::runtime_error("Attempt to get the ID of a state not associated with a coroutine");
        }

        return m_coro_handle.promise().id;
    }

    // Sets human-readable name for the state.
    State& Name(std::string state_name) & {
        if (m_coro_handle.address() == nullptr) {
            throw std::runtime_error("Attempt to set the name of State not associated with a coroutine");
        }

        m_coro_handle.promise().name = std::move(state_name);
        return *this;
    }

    // Sets human-readable name for the state.
    State&& Name(std::string state_name) && {
        return std::move(this->Name(state_name));
    }

    // Gets the human-readable name for the state.
    std::string_view Name() const {
        if (m_coro_handle.address() == nullptr) {
            throw std::runtime_error("Attempt to get the name of a State not associated with a coroutine");
        }

        return m_coro_handle.promise().name;
    }

    // False if the state is still waiting in initial_suspend.
    // True if the initial await has been resumed, typically by calling dxfsm::start())
    bool IsStarted() const {
        return m_coro_handle.promise().is_started;
    }

    // Move constructors.
    State(State&& other) noexcept : m_coro_handle(std::exchange(other.m_coro_handle, nullptr)) {}

    State& operator=(State&& other) noexcept {
        m_coro_handle = std::exchange(other.m_coro_handle, nullptr);
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

    explicit State(promise_type *p) noexcept : m_coro_handle(handle_type::from_promise(*p)) {}

    handle_type m_coro_handle;
}; // State

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
    FSM& Name(std::string name) { _name = std::move(name); }

    // The event that was sent in the latest transition
    // const Event& latestEvent() const { return _event; }

    // Returns the name of the target state of the latest transition.
    auto GetCurrentState() const -> std::optional<typename State_t::InfoView> {
        if (!_state) {
            return std::nullopt;
        }
        
        return _state.promise().GetInfoView();
    }

    // // Sets the current state. The next event will come to this state.
    // FSM& setState(const State& state)
    // {
    //     _state = state.handle();
    //     return *this;
    // }

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
            TransitionTarget{
                .state = to_handle,
                .fsm = this
            }
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

    // // Returns a vector of transition triplets {from-state, on-event, to-state}
    // std::vector<std::array<SV, 3>> getTransitions() const
    // {
    //     std::vector<std::array<SV, 3>> vecResult(m_transition_table.size());
    //     for (std::size_t i = 0; const auto& [fromStateOnEvent, toState] : m_transition_table) {
    //         auto& triple = vecResult[i++];
    //         triple[0] = fromStateOnEvent.first.promise().name;
    //         triple[1] = fromStateOnEvent.second;
    //         triple[2] = toState.state.promise().name;
    //     }
    //     return vecResult;
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

    // struct Awaitable
    // {
    //     FSM* self;
    //     constexpr bool await_ready() {return false;}
    //     std::coroutine_handle<> await_suspend(StateHandle fromState)
    //     {
    //         const Event& onEvent = self->latestEvent();
    //         // If a state emits an empty event all states will remain suspended.
    //         // Consequently, the FSM will stopped. It can be restarted by calling sendEvent()
    //         if (onEvent.isEmpty()) {
    //             self->m_is_fsm_active.store(false, std::memory_order_relaxed);
    //             return std::noop_coroutine();
    //         }

    //         std::optional<StateHandle> next_state = self->DoPotentialTransition(onEvent);

    //         if (!next_state.has_value()) {
    //             throw std::runtime_error("FSM '" + self->name() + "' can't find transition from state '" +
    //                                      std::string(fromState.promise().name) +
    //                                      "' on event '" + std::string(onEvent.name()) + "'.\nPlease fix the transition table.");
    //         }

    //         return *next_state;
    //     }

    //     Event await_resume()
    //     {
    //         if (self->_event.isEmpty())
    //             throw std::runtime_error("FSM '" + self->name() +  "': An empty event has been sent to state " + self->currentState());
    //         return std::move(self->_event);
    //     }
    // };

    // friend struct Awaitable;

    // Emits the given event and returns an awaitable which gives
    // the next event sent to the awaiting state coroutine.
    // Awaitable emitAndReceive(Event* e)
    // {
    //     this->_event = std::move(*e);
    //     return Awaitable{this};
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
                self->m_is_fsm_active.store(false, std::memory_order_relaxed);
                return std::noop_coroutine();
            }

            std::optional<StateHandle> next_state = self->DoPotentialTransition();

            if (!next_state.has_value()) {
                throw std::runtime_error(std::format(
                    "FSM '{}' can't find transition from state '{}' for given event. Please fix the transition table",
                    self->Name(), fromState.promise().name
                ));
            }

            return *next_state;
        }

        void await_resume() {
            // if (self->_event.isEmpty())
            //     throw std::runtime_error("FSM '" + self->name() +  "': An empty event has been sent to state " + self->currentState());
            *event_return = std::move(self->m_event_for_next_resume);
        }
    };

    friend struct EmitReceiveAwaitable;

    EmitReceiveAwaitable EmitAndReceive(Event_t& e) {
        if (e.Empty()) {
            throw std::runtime_error(std::format(
                "FSM '{}' got invalid empty event",
                this->Name()
            ));
        }

        m_event_for_next_resume = std::move(e);
        return EmitReceiveAwaitable{this, &e};
    }

    // A simplified form of EmitAndReceive that only waits for an event,
    // and does not have the emission logic needed for transmission
    struct ReceiveAwaitable {
        FSM* self;

        bool await_ready() {
            const Event_t& pending_event = self->m_event_for_next_resume;
            return !pending_event.Empty();
        }

        void await_suspend(StateHandle) {
            // If there is no event ready to be read, suspend the entire FSM
            self->m_is_fsm_active.store(false, std::memory_order_relaxed);
        }

        [[nodiscard]] Event_t await_resume() {
            self->m_is_fsm_active.store(true, std::memory_order_relaxed);
            return std::move(self->m_event_for_next_resume);
        }
    };

    friend struct ReceiveAwaitable;

    // Returns an awaitable which gives the next event sent to the awaiting state coroutine.
    ReceiveAwaitable ReceiveEvent() {
        return ReceiveAwaitable{this};
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

    // // Alias for the above.
    // FSM& operator<<(State&& state)
    // {
    //     addState(std::move(state));
    //     return *this;
    // }

    // // Returns reference to the state object at the given index.
    // const State& getStateAt(std::size_t index) { return m_states.at(index); }

    std::size_t NumStates() const { return m_states.size(); }

    // Resumes all newly added states since the previous call to Start
    FSM& Start() {
        for (auto& state : m_states) {
            // Resume only if the coroutine is still suspended in initial_suspend.
            if (!state.IsStarted())
                state.handle().resume();
        }

        return *this;
    }

    // Kick off the state machine by sending the event.
    // It it sent to the state which
    // is either the state where the FSM left off when it was
    // suspended last time or the state which has been explicitly
    // set by calling setState().
    // TODO: add exception to check if it's currently running
    FSM& SendEvent(Event_t&& e) {
        // if (!_state.promise().bIsStarted)
        //     throw std::runtime_error("FSM('" + _name + "'): sendEvent("+std::string(pEvent->name())+") can not resume state "+
        //                              _state.promise().name+" because it has not been started. Call first fsm.start() to activate all states.");
        if (m_is_fsm_active) {
            throw std::runtime_error(std::format("Attempt to send event to FSM '{}' while it is running", Name()));
        }

        // Store the event in the FSM, which will get moved by the next Awaitable resumed
        m_event_for_next_resume = std::move(e);

        // Treat this event as one that could change the state
        auto next_state = DoPotentialTransition();

        // next_state has_value() means that a transition was found and next_state is
        // the coroutine to be resumed
        if (next_state.has_value()) {
            next_state->resume();
        } else {
            // Else, no transition occured, send the event to the current state
            _state.resume();
        }

        return *this;
    }

    bool HasState(StateId id) const {
        return std::ranges::find(m_states, id, &State_t::Id) != m_states.end();
    }

    // Returns true if the FSM is running and false if all states
    // are suspended and waiting for an event.
    bool IsActive() const { return m_is_fsm_active; }
    const std::atomic<bool>& GetActiveFlag() const { return m_is_fsm_active; }


    // Callback for debugging and writing log. It is called when the state of
    // the fsm whose name is in the first argument is about
    // to change from 'fromState' to 'toState' because the fromState is sending
    // event 'onEvent'.
    std::function<
        void(const FSM& fsm, 
             typename State_t::InfoView from,
             const Event_t& event,
             typename State_t::InfoView to
        )
    > logger;

private:
    std::string _name;       // Name of the FSM (for information only)
    // Event _event;       // The latest event
    StateHandle _state = nullptr; // Current state (for information only)

    State_t* FindState(StateId id) {
        auto it = std::ranges::find(m_states, id, &State_t::Id);
        return it != m_states.end() ? &*it : nullptr;
    }

    const State_t* FindState(StateId id) const {
        auto it = std::ranges::find(m_states, id, &State_t::Id);
        return it != m_states.end() ? &*it : nullptr;
    }

    std::optional<StateHandle> DoPotentialTransition() {
        Event_t& pending_event = m_event_for_next_resume;

        TransitionTarget to;

        if (auto it = m_transition_table.find({_state, pending_event.GetId()}); it != m_transition_table.end())
            to = it->second;
        else
            // No coded state transition, return empty handle to signify this
            return std::nullopt;

        auto from_state = _state;
        // Typically the event is being sent to a state owned by this FSM (i.e. self).
        // However, it may also be going to a state owned by another FSM.
        // The destination FSM is in TransitionTarget struct together with the state handle.
        if (to.fsm == this) {  // The target state lives in this FSM.
            _state = to.state;

            if (logger)
                logger(*this, from_state.promise().GetInfoView(), pending_event, to.state.promise().GetInfoView());

            m_is_fsm_active.store(true, std::memory_order_relaxed);
            return to.state;
        } else { // The target state lives in another FSM.
            // Note: self FSM will suspend and self->state remains in the state where
            //       it left off when to.fsm took over.
            to.fsm->_state = to.state; // to.fsm will resume.
            // Move the event to the target FSM. The event of the target FSM should be empty.

            if (logger)
                logger(*this, from_state.promise().GetInfoView(), pending_event, to.state.promise().GetInfoView());

            // TODO: convert to exception
            assert(to.fsm->m_event_for_next_resume.Empty());
            to.fsm->m_event_for_next_resume = std::move(pending_event);

            // Self is suspended and to.fsm is resumed.
            m_is_fsm_active.store(false, std::memory_order_relaxed);
            to.fsm->m_is_fsm_active.store(true, std::memory_order_relaxed);

            return to.state;
        }
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

    // Target state of a transition (i.e. go to the 'state' which belongs in 'fsm')
    struct TransitionTarget
    {
        StateHandle state = nullptr;
        FSM* fsm = nullptr;
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
