#ifndef DXFSM_HPP
#define DXFSM_HPP

#include <coroutine>
#include <memory>
#include <sstream>
#include <iomanip>
#include <string_view>
#include <utility>
#include <stdexcept>
#include <type_traits>
#include <functional>
#include <vector>
#include <array>
#include <concepts>
#include <unordered_map>
#include <initializer_list>
#include <assert.h>
#include <atomic>
#include <any>
#include <optional>

namespace dxfsm {

// Find out the cache line length.
#ifdef __cpp_lib_hardware_interference_size
using std::hardware_constructive_interference_size;
#else  // Make a reasonable guess.
constexpr std::size_t hardware_constructive_interference_size = 64;
#endif

static const std::string _sharedEmptyString{};

template <class T>
concept Trivial = (std::is_trivially_destructible_v<T> || std::is_same_v<T, void>);

// Generic reusable Event class.
// An object of this type hold its identity in a string_view
// and data in a byte buffer. Hence an event object can be reused
// by replacing the identity with a new one and storing the data
// of the new event in the buffer. If the buffer is too small
// for the data, it will be extended a bit like std::vector does.
// The buffer never shrinks but can be reset to zero lenght like std::vector.
struct Event {
    Event() noexcept = default;
    Event(Event&& other) noexcept
    {
        _name = std::exchange(other._name, "");
        _capacity = std::exchange(other._capacity, 0u);
        _data = std::exchange(other._data, nullptr);
        _anyPtr = std::exchange(other._anyPtr, nullptr);
    }

    Event& operator=(Event&& other) noexcept
    {
        if (this != &other) {
            this->clear();
            _name = std::exchange(other._name, "");
            _capacity = std::exchange(other._capacity, 0u);
            _data = std::exchange(other._data, nullptr);
            _anyPtr = std::exchange(other._anyPtr, nullptr);
        }
        return *this;
    }

    ~Event()
    {
        // If *_anyPtr contains an AnyPtr<T> object which points to the buffer,
        // the object living in the buffer will be destroyed at the destructor of AnyPtr<T>
        _anyPtr.reset();
        delete [] _data;
    }

    // Constructs a new object of type T into the data block using placement new.
    template <class T = void, class... Args>
    T* construct(std::string_view name, Args&&... args)
    {
        static_assert(!(std::is_same_v<T, void> && sizeof...(Args) > 0),
                      "Void event must not take constructor arguments.");
        if (_anyPtr && _anyPtr->has_value())
            *_anyPtr = std::any();  // Destroy the object currently living in the buffer by implicitly invoking AnyPtr<T> destructor.
        if constexpr (std::is_same_v<T, void>) {
            this->_name = name;
            void* p = this->data();
            return p;
        } else {
            this->reserve(sizeof(T));
            ::new (this->_data) T{std::forward<Args>(args)...};
            this->_name = name;
            T* p = this->dataAs<T>();
            // Store typed pointer into a type erased std::any object.
            // Note that dynamic memory will not be allocated for AnyPtr<T> object due to Small Buffer Optimization.
            _anyPtr->emplace<AnyPtr<T>>(p);
            return p;
        }
    }

    // Construct from a reference to an object of target type. Uses default copy / move constructor.
    template <class T>
    std::decay_t<T>* construct(std::string_view name, T&& t)
    {
        using TT = std::decay_t<T>;
        if (_anyPtr && _anyPtr->has_value())
            *_anyPtr = std::any();  // Destroy the object currently living in the buffer by implicitly invoking AnyPtr<T> destructor.
        this->reserve(sizeof(TT));
        ::new (this->_data) TT{std::forward<T>(t)};
        this->_name = name;
        TT* p = this->dataAs<TT>();
        _anyPtr->emplace<AnyPtr<TT>>(p);
        return p;
    }

    // Destroys the object pointed by _data unless the type T is
    // void or T is trivially destructible.
    // After this call, the event will be empty.
    template<class T = void>
    void destroy(T* = nullptr)
    {
        if (_anyPtr && _anyPtr->has_value())
            *_anyPtr = std::any();  // Destroy the object currently living in the buffer by implicitly invoking AnyPtr<T> destructor.
        this->_name = "";
    }

    // Reinterprets the data buffer as an object of type T.
    template<class T>
    T* dataAs()
    {
        return std::launder(reinterpret_cast<T*>(this->data()));
    }

    template<class T>
    const T* dataAs() const
    {
        return std::launder(reinterpret_cast<const T*>(this->data()));
    }

    // Allows you to get a pointer to the payload of type T using syntax "event >> p" where T* p;
    // Returns reference to the payload object so you can also use the result directly without
    // dereferening p. For example: "auto x = (event >> p) + 1;" means "event >> p; auto x = *p + 1;"
    // If the type of the object stored in the buffer is not T, an exception will be thrown.
    // So you can not accidentally read the data in a wrong format.
    template<class T>
    T& operator>>(T*& p)
    {
        p = this->safeCast<T>();
        return *p;
    }

    // Returns pointer to the data buffer
    void* data()
    {
        return _data;
    }

    const void* data() const
    {
        return _data;
    }

    // Releases the data allocated from the heap and empties the name.
    void clear()
    {
        if (!_data)
            return;

        _anyPtr.reset(); // Destroy the object in the buffer, if any.
        _name = "";
        _capacity = 0;
        delete [] _data;
        _data = nullptr;
    }

    // Reserves space for event data. The existing data may be wiped out.
    void reserve(std::size_t size)
    {
        if (_capacity < size) {
            if (!_anyPtr)  // Make a new empty AnyPtr object
                _anyPtr = std::make_unique_for_overwrite<std::any>();
            else if (_anyPtr->has_value())
                *_anyPtr = std::any{};   // Destroy the object in the buffer, if any.
            _name = "";
            _capacity = size;
            delete [] _data;
            _data = new std::byte[size];
        }
    }

    // Returns the maximum size of an object which can be constructed
    // in the data buffer without reallocation.
    std::size_t capacity() const { return _capacity; }

    // Returns true if the name of the event == other
    bool isEqual(const std::string_view& other) const { return (_name.compare(other) == 0); }

    // Returns true if the event is empty (i.e. name string is not set)
    bool isEmpty() const { return _name.empty(); }

    // Checks if the event has data in the buffer.
    bool hasData() const { return (_anyPtr && _anyPtr->has_value()); }

    // Returns the name of the event as a string_view.
    std::string_view name() const { return _name; }

    // The same as above but as a string.
    std::string nameAsString() const { return std::string(_name); }

private:
    // Copying not allowed.
    Event(const Event&) = delete;
    Event& operator=(const Event&) = delete;

    // A typed pointer to the object in the storage space.
    // When AnyPtr is destroyed, the object in the storage is also destroyed.
    template <class T>
    struct AnyPtr
    {
        AnyPtr(T* p = nullptr) : ptr(p) {}
        T* ptr;
        ~AnyPtr()
        {
            if (ptr)
                ptr->~T();
        }
    };

    // No need for an explicit destructor call if T is trivially destructible.
    template <Trivial T>
    struct AnyPtr<T>
    {
        AnyPtr(T* p = nullptr) : ptr(p) {}
        T* ptr;
    };

    // Get pointer to the data buffer if T is the type of the object in the buffer.
    // Otherwise, throw an exception.
    template <class T>
    T* safeCast()
    {
        try {
            if (_anyPtr && _anyPtr->has_value())
                return std::any_cast<AnyPtr<T>&>(*_anyPtr).ptr;
            else
                throw std::runtime_error("dxfsm::Event does not contain data so data pointer can not be returned.");
        }
        catch (const std::bad_any_cast&) {
            throw std::runtime_error("Attempt to store pointer to the object in dxfsm::Event into a variable of wrong type.");
        }
        return nullptr;
    }

    // Name of the object store in the data buffer.
    // Should be empty if and only if there is no stored object.
    std::string_view _name = "";
    // Capacity of the data buffer in bytes
    std::size_t _capacity = 0;
    // Pointer to data buffer
    std::byte* _data = nullptr;
    // An std::any object which contains an object of type AnyPtr<T> where T is the type
    // of the object living in the buffer. T = void if there is no object in the buffer.
    std::unique_ptr<std::any> _anyPtr;
}; // Event

// Returns true if the name of the event is sv.
inline bool operator==(const Event& e, std::string_view sv)
{
    return e.isEqual(sv);
}

inline bool operator==(std::string_view sv, const Event& e)
{
    return e.isEqual(sv);
}

// Returns a string which contains pointer p in hex format.
inline std::string asHex(void* p)
{
    std::stringstream ss;
    ss << std::hex << std::showbase << p << std::noshowbase << std::dec;
    return ss.str();
}

inline std::string asHex(std::coroutine_handle<> h)
{
    return asHex(h.address());
}

// Return type of coroutines which represent states.
struct State
{
    struct promise_type
    {
        struct InitialAwaitable
        {
            promise_type* self = nullptr;
            constexpr bool await_ready() {return false;}
            void await_suspend(std::coroutine_handle<promise_type>) {}
            void await_resume() { self->bIsStarted = true; } // The state was resumed from initial_suspend
        };

        promise_type()
        {   // Initialize the name as the hex address.
            // The user may choose to replace it later with a better name.
            // Note that typically a string whose length is <= 22 is subject to
            // short string optimization so a hex address does not allocate from the heap.
            void* addr = std::coroutine_handle<promise_type>::from_promise(*this).address();
            name = asHex(addr);
        }
        InitialAwaitable initial_suspend() noexcept { return InitialAwaitable{this}; }
        constexpr std::suspend_always final_suspend() noexcept { return {}; }
        State get_return_object() noexcept { return State(this); };
        void unhandled_exception() { throw; }
        void return_void()
        {
            // State coroutines must never return.
            throw std::runtime_error("State coroutine '" + name + "' is not allowed to co_return.");
        }

        std::string name;
        // false if the state is waiting at initial_suspend,
        // true if the state has been resumed from the initial_suspend.

        bool bIsStarted = false;
    }; // promise_type

    using handle_type = std::coroutine_handle<promise_type>;

    // Returns the handle to the state coroutine.
    handle_type handle() const noexcept { return coro_handle_; }

    // State has one-to-one correspondence to its coroutine handle
    operator handle_type() const noexcept { return coro_handle_; }

    // Sets human-readable name for the state.
    State&& setName(std::string stateName)
    {
        if (!stateName.empty())
            coro_handle_.promise().name = std::move(stateName);
        return std::move(*this);
    }

    // Alias for setName
    State&& operator=(std::string stateName)
    {
        return std::move(setName(std::move(stateName)));
    }

    // Returns the name of the state coroutine.
    const std::string& getName() const
    {
        return coro_handle_.promise().name;
    }

    // False if the state is still waiting in initial_suspend.
    // True if the initial await has been resumed /typically by calling dxfsm::start())
    bool isStarted() const
    {
        return coro_handle_.promise().bIsStarted;
    }

    // Move constructors.
    State(State&& other) noexcept : coro_handle_(std::exchange(other.coro_handle_, nullptr)) {}

    State& operator=(State&& other) noexcept
    {
        coro_handle_ = std::exchange(other.coro_handle_, nullptr);
        return *this;
    }

    ~State() {
        if (coro_handle_)
            coro_handle_.destroy();
    }
private:
    // A state is move-only
    State(const State&) = delete;
    State& operator=(const State&) = delete;

    explicit State(promise_type *p) noexcept : coro_handle_(handle_type::from_promise(*p)) {}

    handle_type coro_handle_;
}; // State

// A state can be presented either as a name string or as a coroutine handle.
template<class T>
concept StateType = std::convertible_to<T, std::string_view> || std::convertible_to<T, typename State::handle_type>;

class FSM;

// Type for setting transition {from-state, on-event} --> {to-state of targetFSM}
template <class FROM, class EVENT, class TO>
struct Transition
{
    Transition(const FROM& f, const EVENT& e, const TO& t, FSM* tf) : from(f), event(e), to(t), targetFSM(tf) {}
    const FROM& from;
    const EVENT& event;
    const TO& to;
    FSM* targetFSM;
};

// A helper which allows you to add transitions with syntax
// "fsm << transition(from1, event1, to1) << transition(from2, event2, to2) << ..."
template <StateType FROM, std::convertible_to<std::string_view> EVENT, StateType TO>
inline auto transition(const FROM& from, const EVENT& event, const TO& to, FSM* targetFSM = nullptr)
{
    return Transition{from, event, to, targetFSM};
}

// A helper which allows you to remove transitions with syntax
// "fsm >> transition(from1, event1) >> transition(from2, event2) >> ..."
template <StateType FROM, std::convertible_to<std::string_view> EVENT>
inline auto transition(const FROM& from, const EVENT& event)
{
    return Transition{from, event, nullptr, nullptr};
}


// Finite State Machine class
class FSM {
public:
    using StateHandle = typename State::handle_type;
    using SV = std::string_view;

    // Gives the FSM a human-readable name.
    // If the name is empty, use hex address of the FSM object.
    FSM(std::string fsmName) : _name(std::move(fsmName))
    {
        if (_name.empty())  // If the user did not provide a name, use a dummy one.
            _name = asHex(this);
    };

    FSM()  { _name = asHex(this); };
    FSM(const FSM&) = delete;
    FSM& operator=(const FSM&) = delete;
    ~FSM() = default;

    // Returns the name of the FSM
    const std::string& name() const { return _name; }

    // The event that was sent in the latest transition
    const Event& latestEvent() const { return _event; }

    // Returns the name of the target state of the latest transition.
    const std::string& currentState() const { return _state ? _state.promise().name : _sharedEmptyString; }

    // Sets the current state. The next event will come to this state.
    FSM& setState(const State& state)
    {
        _state = state.handle();
        return *this;
    }

    FSM& setState(SV stateName)
    {
        _state = findHandle(stateName);
        if (!_state)
            throw std::runtime_error("FSM('" + _name + "'): setState() did not find the requested state '" + std::string(stateName) + "'");
        return *this;
    }

    // Adds transition from state 'from' to state 'to' on event 'onEvent' which lives in FSM 'targetFSM'.
    // targetFSM==nullptr means this FSM, so the 4th argument can be omitted if every state
    // refers to the same FSM.
    // Returns true if {from, onEvent} pair has not been routed previously.
    // Returns false if an existing destination is replaced with '{to, targetFSM}'.
    // Typically should return true unless you deliberately modify the state machine on the fly.
    bool addTransition(StateHandle from, SV onEvent, StateHandle to, FSM* targetFSM = nullptr)
    {
        targetFSM = targetFSM ? targetFSM : this;
        return _mapTransitionTable.insert_or_assign({from, onEvent}, TransitionTarget{to, targetFSM}).second;
    }

    // The same as above but the states are identified by their names (i.e. strings)
    bool addTransition(SV fromState, SV onEvent, SV toState, FSM* targetFSM = nullptr)
    {
        targetFSM = targetFSM ? targetFSM : this;
        StateHandle fromHandle = this->findHandle(fromState);
        if (!fromHandle)
            throw std::runtime_error("FSM('" + _name + "'): addTransition() did not find the requested source state '" + std::string(fromState) + "'.");
        StateHandle toHandle = targetFSM->findHandle(toState);
        if (!toHandle)
            throw std::runtime_error("FSM('" + _name + "'): addTransition() did not find the requested target state'" + std::string(toState) + "'.");

        return addTransition(fromHandle, onEvent, toHandle, targetFSM);
    }

    bool addTransition(StateHandle fromHandle, SV onEvent, SV toState, FSM* targetFSM = nullptr)
    {
        targetFSM = targetFSM ? targetFSM : this;
        StateHandle toHandle = targetFSM->findHandle(toState);
        if (!toHandle)
            throw std::runtime_error("FSM('" + _name + "'): addTransition() did not find the requested target state'" + std::string(toState) + "'.");
        return addTransition(fromHandle, onEvent, toHandle, targetFSM);
    }

    bool addTransition(SV fromState, SV onEvent, StateHandle toHandle, FSM* targetFSM = nullptr)
    {
        targetFSM = targetFSM ? targetFSM : this;
        StateHandle fromHandle = this->findHandle(fromState);
        if (!fromHandle)
            throw std::runtime_error("FSM('" + _name + "'): addTransition() did not find the requested source state '" + std::string(fromState) + "'.");
        return addTransition(fromHandle, onEvent, toHandle, targetFSM);
    }

    // A shortcut for writing "fsm << transition(from, event, to)" instead of "fsm.addTransition(from, event, to)"
    template <StateType FROM, std::convertible_to<std::string_view> EVENT, StateType TO>
    FSM& operator<<(const Transition<FROM,EVENT,TO>& fromEventTo)
    {
        addTransition(fromEventTo.from, fromEventTo.event, fromEventTo.to, fromEventTo.targetFSM);
        return *this;
    }

    // Removes transition triggered by event 'onEvent' sent from 'fromState'.
    // Return true if the transition was found and successfully removed.
    bool removeTransition(StateHandle fromState, SV onEvent)
    {
        auto erased = _mapTransitionTable.erase({fromState, onEvent});
        return bool(erased);
    }

    bool removeTransition(SV fromState, SV onEvent)
    {
        auto erased = _mapTransitionTable.erase({findHandle(fromState), onEvent});
        return bool(erased);
    }

    // A shortcut for writing "fsm >> transition(from, event)" instead of "fsm.removeTransition(from, event)"
    template <StateType FROM, std::convertible_to<std::string_view> EVENT, class TO>
    FSM& operator>>(const Transition<FROM,EVENT,TO>& fromEventTo)
    {
        removeTransition(fromEventTo.from, fromEventTo.event);
        return *this;
    }

    // Return true if the FSM knows how to deal with event 'onEvent' sent from state 'fromState'.
    bool hasTransition(StateHandle fromState, SV onEvent)
    {
        return _mapTransitionTable.contains({fromState, onEvent});
    }

    bool hasTransition(SV fromState, SV onEvent)
    {
        return _mapTransitionTable.contains({findHandle(fromState), onEvent});
    }

    // Returns a vector of transition triplets {from-state, on-event, to-state}
    std::vector<std::array<SV, 3>> getTransitions() const
    {
        std::vector<std::array<SV, 3>> vecResult(_mapTransitionTable.size());
        for (std::size_t i = 0; const auto& [fromStateOnEvent, toState] : _mapTransitionTable) {
            auto& triple = vecResult[i++];
            triple[0] = fromStateOnEvent.first.promise().name;
            triple[1] = fromStateOnEvent.second;
            triple[2] = toState.state.promise().name;
        }
        return vecResult;
    }

    // Finds the target state of 'onEvent' when sent from 'fromState'.
    // Returns an empty string if not found.
    const std::string& targetState(StateHandle fromState, SV onEvent)
    {
       auto it = _mapTransitionTable.find({fromState, onEvent});
       if (it == _mapTransitionTable.end())
           return _sharedEmptyString;
        else
            return it->second.state.promise().name;
    }

    const std::string& targetState(SV fromState, SV onEvent)
    {
        return targetState(findHandle(fromState), onEvent);
    }

    struct Awaitable
    {
        FSM* self;
        constexpr bool await_ready() {return false;}
        std::coroutine_handle<> await_suspend(StateHandle fromState)
        {
            const Event& onEvent = self->latestEvent();
            // If a state emits an empty event all states will remain suspended.
            // Consequently, the FSM will stopped. It can be restarted by calling sendEvent()
            if (onEvent.isEmpty()) {
                self->_bIsActive.store(false, std::memory_order_relaxed);
                return std::noop_coroutine();
            }

            std::optional<StateHandle> next_state = self->DoPotentialTransition(onEvent);

            if (!next_state.has_value()) {
                throw std::runtime_error("FSM '" + self->name() + "' can't find transition from state '" +
                                         std::string(fromState.promise().name) +
                                         "' on event '" + std::string(onEvent.name()) + "'.\nPlease fix the transition table.");
            }

            return *next_state;
        }

        Event await_resume()
        {
            if (self->_event.isEmpty())
                throw std::runtime_error("FSM '" + self->name() +  "': An empty event has been sent to state " + self->currentState());
            return std::move(self->_event);
        }
    };

    friend struct Awaitable;

    // Emits the given event and returns an awaitable which gives
    // the next event sent to the awaiting state coroutine.
    Awaitable emitAndReceive(Event* e)
    {
        this->_event = std::move(*e);
        return Awaitable{this};
    }

    struct InitialAwaitable
    {
        FSM* self;
        constexpr bool await_ready() {return false;}
        void await_suspend(StateHandle) {}
        Event await_resume()
        {
            self->_bIsActive.store(true, std::memory_order_relaxed);
            if (self->_event.isEmpty())
                throw std::runtime_error("FSM '" + self->name() + "': An empty event has been sent to state " + self->currentState());
            return std::move(self->_event);
        }
    };

    friend struct InitialAwaitable;

    // Returns an awaitable which gives the next event sent to the awaiting state coroutine.
    InitialAwaitable getEvent()
    {
        return InitialAwaitable{this};
    }

    // Adds a state to the state machine without associating any events with it.
    // Returns the index of the vector to which the state was stored.
    std::size_t addState(State&& state)
    {
        if (hasState(state.getName()))
            throw std::runtime_error("A state with name '" + state.getName() + "' already exists in FSM " + _name);

        if (state.handle())
            _vecStates.push_back(std::move(state));
        else
            throw std::runtime_error("Attempt to add an invalid state to FSM " + _name);
        return _vecStates.size() - 1;
    }

    // Alias for the above.
    FSM& operator<<(State&& state)
    {
        addState(std::move(state));
        return *this;
    }

    // Returns reference to the state object at the given index.
    const State& getStateAt(std::size_t index) { return _vecStates.at(index); }

    // Returns the number of states in the FSM.
    std::size_t numberOfStates() const { return _vecStates.size(); }

    // Get the states going from the initial suspension.
    FSM& start()
    {
        for (auto& state : _vecStates) {
            // Resume only if the coroutine is still suspended in initial_suspend.
            if (!state.isStarted())
                state.handle().resume();
        }
        return *this;
    }

    // Kick off the state machine by sending the event.
    // It it sent to the state which
    // is either the state where the FSM left off when it was
    // suspended last time or the state which has been explicitly
    // set by calling setState().
    FSM& sendEvent(Event* pEvent)
    {
        if (!_state.promise().bIsStarted)
            throw std::runtime_error("FSM('" + _name + "'): sendEvent("+std::string(pEvent->name())+") can not resume state "+
                                     _state.promise().name+" because it has not been started. Call first fsm.start() to activate all states.");

        _event = std::move(*pEvent);

        // Treat this event as one that could change the state
        DoPotentialTransition(_event);
        // _state will have updated in the above function call if it was a transition
        // else, this event will be sent to the previous state
        _state.resume();

        return *this;
    }


    // Find the state based on the name. Throws if not found.
     const State& findState(SV name) const
     {
        // Find the name from the list of states
        for (const State& state : _vecStates)
            if (state.getName() == name)
                return state;
        throw std::runtime_error("FSM('" + _name + "'): findState() did not find the requested name '" + std::string(name) + "'");
     }

    // Finds the state vector index where the state with the given name lives. Throws if not found.
     std::size_t findIndex(SV name) const
     {
         for (std::size_t i = 0; i < _vecStates.size(); ++i)
             if (_vecStates[i].getName() == name)
                 return i;
        throw std::runtime_error("FSM('" + _name + "'): findIndex() did not find the requested name '" + std::string(name) + "'");
     }

     // Returns true if the given state is registered in the fsm.
     bool hasState(SV name) const
     {
        // Find the name from the list of states
        for (const State& state : this->_vecStates)
            if (state.getName() == name)
                return true;
         return false;
     }

    // Returns true if the FSM is running and false if all states
    // are suspended and waiting for an event.
    const std::atomic<bool>& isActive() const { return _bIsActive; }

    // Callback for debugging and writing log. It is called when the state of
    // the fsm whose name is in the first argument is about
    // to change from 'fromState' to 'toState' because the fromState is sending
    // event 'onEvent'.
    std::function<void(const std::string& fsm, const std::string& fromState, const Event& onEvent, const std::string& toState)> logger;

private:
    std::string _name;       // Name of the FSM (for information only)
    Event _event;       // The latest event
    StateHandle _state = nullptr; // Current state (for information only)

    // Find the handle based on the name. Returns nullptr if not found.
    StateHandle findHandle(SV name) const
    {
    // Find the name from the list of states
    for (const State& state : _vecStates)
        if (state.getName() == name)
            return state.handle();
        return nullptr;
    }

    std::optional<StateHandle> DoPotentialTransition(const Event& onEvent) {
        TransitionTarget to;

        if (auto it = _mapTransitionTable.find({_state, onEvent.name()}); it != _mapTransitionTable.end())
            to = it->second;
        else
            // No coded state transition, return empty handle to signify this
            return std::nullopt;

        auto fromState = _state;
        // Typically the event is being sent to a state owned by this FSM (i.e. self).
        // However, it may also be going to a state owned by another FSM.
        // The destination FSM is in TransitionTarget struct together with the state handle.
        if (to.fsm == this) {  // The target state lives in this FSM.
            _state = to.state;

            if (logger)
                logger(name(), fromState.promise().name, onEvent, to.state.promise().name);

            _bIsActive.store(true, std::memory_order_relaxed);
            return to.state;
        } else { // The target state lives in another FSM.
            // Note: self FSM will suspend and self->state remains in the state where
            //       it left off when to.fsm took over.
            to.fsm->_state = to.state; // to.fsm will resume.
            // Move the event to the target FSM. The event of the target FSM should be empty.
            assert(to.fsm->_event.isEmpty());
            to.fsm->_event = std::move(_event);

            if (logger)
                logger(name()+"-->"+to.fsm->name(), fromState.promise().name, to.fsm->_event, to.state.promise().name);

            // Self is suspended and to.fsm is resumed.
            _bIsActive.store(false, std::memory_order_relaxed);
            to.fsm->_bIsActive.store(true, std::memory_order_relaxed);

            return to.state;
        }
    }

    // Hashes a coroutine handle
    struct HandleHash
    {
        std::size_t operator() (const StateHandle& h) const noexcept {
            return std::hash<void*>()(h.address());
        }
    };

    // Hash {state, event} - pair
    struct PairHash
    {
        std::size_t operator() (const std::pair<StateHandle, SV>& p) const noexcept {
            // Note: you could possibly do better than xor. See
            // https://stackoverflow.com/questions/5889238/why-is-xor-the-default-way-to-combine-hashes
            return HandleHash{}(p.first) ^ std::hash<SV>()(p.second);
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
    std::unordered_map<std::pair<StateHandle,SV>, TransitionTarget, PairHash> _mapTransitionTable;

    // All coroutines which represent the states in the state machine
    std::vector<State> _vecStates;

    // True if the FSM is running, false if suspended.
    std::atomic<bool> _bIsActive = false;
}; // FSM

} // namespace dxfsm
#endif // DXFSM_HPP
