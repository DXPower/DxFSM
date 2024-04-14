# Overview

DxFSM is a C++20 library for building Finite State Machines out of
[Coroutines](https://en.cppreference.com/w/cpp/language/coroutines).
Each State is represented by a coroutine, which leverages their innate ability to implicitly keep...
well... state! Inside each State, you can await to signify where events can be received. This also
means that you can create simple FSMs within a single State coroutine function!

The Event is a type-safe `std::any`-like type which stores an ID and any additional data you so please.
The library intelligently shares a single Event object for all State coroutines in order to minimize the
need for allocations. If you know the maximum size of the data you need to send to a state, then
no dynamic allocations are required to trigger events once the state machine is initialized.

The library as a whole is fully exception safe. This means that you can throw exceptions from within
your State coroutines without affecting the operation of the FSM. When this happens, the coroutine will be
destroyed, and the FSM will unset the current state and then propagate the exception
back to the original event trigger.

## What Are Coroutines?
### A very brief introduction

Put simply, a coroutine is a function that can arbitrarily suspend its execution at any point.
A suspended coroutine may then be resumed at any time by an external agent.

DxFSM leverages this to suspend a function (which represents a State) when it needs to wait for an event.
The pseudo-code for this looks like:

```c++
coroutine MyState() {
    while (true) {
        Event ev = await GetNextEvent();
        
        if (ev.id == A) {
            // Do something if we receive an A event
        } else if (ev.id == B) {
            // Do something if we receive a B event
        }
    }
}
```

An *await* statement will pause the coroutine until the WaitForNextEvent() *awaiter* returns a result.
Once ready, the MyState coroutine will resume with an Event object returned into the `ev` variable.

For example, walking through MyState's execution (the numbering will make sense shortly):

**1.** MyState() starts<br/>
**2.** Execution enters the while loop, and calls `await GetNextEvent()`<br/>
**3.** No events have been sent yet, so the coroutine is suspended <br/>
**5.** Event B is received (sent externally), so the coroutine is resumed with `ev.id == B<br/>
**6.** The second if statement is entered<br/>
**7.** The coroutine loops back to the await statement and suspends itself once again, ready for another event.<br/>

Then, the controller of this State sends it an event. Consider the following pseudo-code for initializing
a State then sending it an event:

```c++
State s1 = MyState();
s1.SendEvent(B);

print("Done!");
```
**0.** This creates the MyState coroutine and runs steps 1-3 above<br/>
**4.** After the coroutine suspends for the first time, the external code regains control, allowing it to
send the event B. Steps 5-7 are then executed.<br/>
**8.** After the coroutine suspends itself again after the first event, the code is ready to move on and prints "Done!".<br/>

A full state machine consists of one or more of these coroutines which receive and process
events in an infinite loop.

### Coroutines in C++20

C++20 Coroutines are notoriously complex. A major design goal of this library was to hide as much of this
complexity as possible from the user. Below are the basic definitions you will need to understand this library.

C++'s definition of a coroutine is any function which contains `co_await`, `co_yield`, or `co_return` in its body.
`co_await` is the only one relevant to this library, as DxFSM's States cannot return nor yield values.

When you call a coroutine, the language will call `new` to allocate memory for the local storage of the coroutine.
This storage is what lets it save progress and later resume from the same point. This storage is freed when
the coroutine ends.

Within the body of the coroutine, the compiler generates the *promise*, which is used to suspend the
coroutine, select a new coroutine to continue, and submit yielded/returned results
(the latter of which is not relevant). The promise is implicitly interacted with via the `co_*` keywords.

The return type of the coroutine is called the *handle*, which can be used to resume the coroutine once suspended,
or to retrieve yield/return values (the latter of which is not relevant again). The return type is explicitly
interacted with (usually via member functions). The base abstraction for this provided by the language is
`std::coroutine_handle`, which provides the very basic `.resume()` member function.

## DxFSM Basic Use

The library provides three fundamental types: FSM, State, and Event. The general flow of operations is:

1. Construct the FSM
2. Add states/transitions
3. Set the current state of the FSM
4. Send events to the FSM (repeat)

Assuming the functions `First`, and `Second` are State coroutine functions, this is what those
steps may look like:

```c++
enum class StateId { F, S };
enum class EventId { Tick };

int main() {
    dxfsm::FSM<StateId, EventId> fsm{};
    
    // States are added to the FSM simply by invoking the coroutine with the proper arguments
    State<StateId> first  = First(fsm, StateId::F);
    State<StateId> second = Second(fsm, StateId::S);
    
    fsm.AddTransition(StateId::F, EventId::Tick, StateId::S)
       .AddTransition(StateId::S, EventId::Tick, StateId::F);
       .SetCurrentState(first); // StateId::F also accepted here
    
    // Send events
    fsm.InsertEvent(EventId::Tick); // (1)
    fsm.InsertEvent(EventId::Tick); // (2)
    fsm.InsertEvent(EventId::Tick); // (3)
}
```

For each event the FSM receives, it will look at the transition table for (A) the current state id,
and (B) the event id. If that combination exists, then the FSM will send to the event to the target state
of the transition, resuming it. Otherwise, it sends the event to the current state.

Let's say that each time `First` gets an event, it will print "Bing". When `Second` gets an event, it will
print "Bang". The `InsertEvent` calls will do the following:

1. `(F, Tick)` is in the transition table, with a target state of `S`. Thus, `Second` becomes the current state,
   and it receives the event. "Bang" is printed.
2. `(S, Tick)` is in the transition table. Following the same logic as (1), "Bing" is printed.
3. "Bang" is printed.

Thus, the output is: "BangBingBang";

We can implement these two state functions like so:

```c++
using FSM = dxfsm::FSM<StateId, EventId>;
using State = dxfsm::State<StateId>; // FSM::State_t also valid

State First(FSM& fsm, StateId id) {
    while (true) {
        co_await fsm.IgnoreEvent();
        std::cout << "Bing";
    }
}

State Second(FSM& fsm, StateId id) {
    while (true) {
        co_await fsm.IgnoreEvent();
        std::cout << "Bang";
    }
}
```

Initially, we set up some helper aliases to write our state machine. Then, we create our coroutines `First`
and `Second`. Each one is a coroutine because of the use of the `co_await` keyword. This means that when
they are called with the proper arguments (`FSM&` and `StateId`, which are always required), it will
create the coroutine and automatically register it with the FSM.

Inside each state coroutine, we loop with `co_await fsm.IgnoreEvent()`. This is a very simple way to
always do an action no matter what event we receive. That thing we do: print something.

The return value `State` is automatically generated by the library. This is a non-owning handle that
can report the status of the given state.

Alternatively, we can use the `id` parameter to combine these states into a single function:

```c++
State Ticker(FSM& fsm, StateId id) {
    while (true) {
        co_await fsm.IgnoreEvent();
        std::cout << (id == StateId::F ? "Bing" : "Bang");
    }
}

int main() {
    // Create the FSM...
    State first  = Ticker(fsm, A);
    State second = Ticker(fsm, B);
    // Add the transitions...
}
```

## Simple Example

Consider a 4-way interaction controlled by a traffic light with two thruways — Jane Street and Doe Avenue.
The green light will remain on until the 30 cars pass through it, 5 cars are waiting on the red light, or
an ambulance approaches the red light.

Let's define two states: `JaneGreen` and `DoeGreen`, and four events: 
`GreenCarPass`, `RedCarWaiting`, `RedAmbulanceApproaching`, and `Toggle`.
The `GreenCarPass` and `RedCarWaiting` events will store an `int` to say how many cars passed or began waiting.
Because the two states (green on Jane or green on Doe) are symmetric, we can use the same State coroutine
function for both. 

```c++
#include <dxfsm/dxfsm.hpp>
#include <iostream>

enum class StateId { JaneGreen, DoeGreen };
enum class EventId { GreenCarPass, RedCarWaiting, RedAmbulanceApproaching, Toggle };

using FSM = dxfsm::FSM<StateId, EventId>;
using State = FSM::State_t;
using Event = FSM::Event_t;

State TrafficState(FSM& fsm, StateId, std::string_view street_name) {
    Event ev = co_await fsm.ReceiveInitialEvent();
    
    int cars_waiting = 0;
    int cars_passed = 0;
    
    while (true) {
        using enum EventId;

        if (ev == Toggle) {
            // The green light just started for this state. 
            // Reset the counts, then wait for the next event.
            cars_waiting = 0;
            cars_passed = 0;
            ev.Clear();
            
            std::cout << street_name << " turned green!\n";
        } else if (ev == RedAmbulanceApproaching) {
            std::cout << "An ambulance is approaching the red!\n";
            ev.Store(Toggle); // Trigger an event to switch the lights
        } else {
            if (ev == GreenCarPass) {
                cars_passed += ev.Get<int>();
                std::cout << cars_passed << " cars have passed through the green\n";
            } else {
                cars_waiting += ev.Get<int>();
                std::cout << cars_waiting << " cars are waiting at the red\n";
            }

            if (cars_passed >= 30 || cars_waiting >= 5) {
                // Hit the car limit, trigger an event to switch the lights
                ev.Store(Toggle);
            } else {
                // Otherwise, clear the event to wait for the next event
                ev.Clear();
            }
        }
        
        co_await fsm.EmitAndReceive(ev);
    }
}

int main() {
    using enum StateId;
    using enum EventId;
    
    FSM fsm{};
    TrafficState(fsm, JaneGreen, "Jane Street");
    TrafficState(fsm, DoeGreen, "Doe Avenue");
    
    fsm.AddTransition(JaneGreen, Toggle, DoeGreen)
       .AddTransition(DoeGreen, Toggle, JaneGreen);
    
    fsm.SetCurrentState(JaneGreen);
    std::cout << "Starting out with Jane Street green\n";

    fsm.InsertEvent(GreenCarPass, 20)
       .InsertEvent(GreenCarPass, 15)         // Toggle triggered here
       .InsertEvent(RedCarWaiting, 3)
       .InsertEvent(GreenCarPass, 8)
       .InsertEvent(RedCarWaiting, 2)         // Toggle triggered here
       .InsertEvent(RedAmbulanceApproaching); // Toggle triggered here
       
   // You can also force the lights to toggle externally by sending the event:
   std::cout << "Forcing the lights to switch...\n";
   fsm.InsertEvent(Toggle);
}
```

This example will output:
```
Starting out with Jane Street green
20 cars have passed through the green
35 cars have passed through the green
Jane Street is turning red!
Doe Avenue turned green!
3 cars are waiting at the red
8 cars have passed through the green
5 cars are waiting at the red
Doe Avenue is turning red!
Jane Street turned green!
An ambulance is approaching the red!
Jane Street is turning red!
Doe Avenue turned green!
Forcing the lights to switch...
Doe Avenue is turning red!
Jane Street turned green!
```


## DxFSM Concepts

### FSM

`dxfsm::FSM<StateId, EventId>` is the primary point of interaction with DxFSM.
`FSM` controls all of its states, contains the transition table, and facilitates the sending of
and waiting for events. The two template arguments define the types which uniquely identify States and Events.
These are primarily used to query the transition table and manage the current state.

### Event

`dxfsm::Event<EventId>` events are the basic unit of transitions and data transmission to-and-from states. All events store
an id, which unique identifies it for the FSM to detect transitions. An event can also be queried for its
id to perform different actions. `EventId` may be any type that is comparable by `operator==` and
hashable by a specialization of `std::hash`.

Alongside the id, an event can store any additional data within a growable buffer it controls.
This buffer will expand to the size of the largest data it has ever stored. The FSM will automatically
reuse a single Event's buffer between all the states to remove the need for allocations while it is being used.

The main functions you will use to set up and query Events are:

- `event.Store(id [, Object]) -> Object&` — Sets the id and optionally stores an object in its data buffer
- `event.Emplace<T>(id [, args...]) -> T&` — Sets the id and constructs T in the data buffer using the supplied arguments
- `event.Id() -> EventId` — Gets the id of this event
- `event.HasData<T>() -> bool` — Queries whether this event stores T
- `event.Get<T>() -> T&` — Gets the data stored in the event, throwing an exception if it doesn't store T
- `event.GetMaybe<T>() -> T*` — Gets the data stored in the event, return a nullptr if it doesn't store T
- `event.GetUnchecked<T>() -> T&` — Gets the data stored in the event without checking if it actually stores T

The FSM provides two main functions for externally triggering events:

- `fsm.InsertEvent(id [, Object])` — Calls `Store` on the shared Event before sending it to the next state
- `fsm.EmplaceEvent<T>(id [, args...])` — Calls `Emplace` on the shared Event before sending it to the next state

These functions will compare the FSM's current state and the id of the Event to determine which State should be
resumed and the Event sent to.

From within a State, you can use `co_await` to indicate points where Events will be received:

- `co_await fsm.ReceiveEvent(Event&);` — Suspends this State until an event is received, transparently reusing
  and replacing provided Event object.
- `Event ev = co_await fsm.ReceiveInitialEvent();` — Recommended to use in the beginning of the coroutine function.
  This brings the shared Event object into local scope in a concise manner. However, if you use this after already
  receiving events then you may trigger unnecessary allocations (as multiple Event objects will be "active").
- `co_await fsm.IgnoreEvent();` — Suspends this State until it receives any event. It is not recommended to use
  this with any other Event awaitables, as the FSM will not be able to reuse the Event object the State previously received.

Additionally, a State can also trigger an Event on its own accord. This Event can perform transitions; if no
transition exists in the transition table, then the Event will be sent back to the State that emitted it.

`co_await fsm.EmitAndReceive(Event&);` will send the provided Event to the FSM, and transparently replace
it with the next Event this State receives. If the Event is empty, then the FSM will be suspended.

### State

States are the individual coroutines that can be suspended/resumed based on the transmission of Events.
Once an Event is received, the State may perform additional actions such as calling functions, triggering new
events, waiting for another event, throwing exceptions, or even modifying the FSM.

All State coroutines must take the following form:

```c++
using FSM = dxfsm::FSM<StateId, EventId>;
using State = FSM::State_t; // dxfsm::State<StateId> valid as well

State FunctionName(FSM& fsm, StateId id [, args...]) {
  ...
}
```

When a State coroutine is called, it will automatically register itself with the `fsm` parameter using
the `id` provided. The `fsm` parameter must be a non-const lvalue reference.
Additional arguments may also be provided by the author's choice.
The FSM will presume ownership of the internal State coroutine; 
thus, it is recommended that any reference/pointer
parameters provided outlive the lifetime of the FSM object.

`StateId` may be any type that is comparable by `operator==` and
hashable by a specialization of `std::hash`.

The `dxfsm::State<StateId>` return type is what enables this functionality and is also required. However,
its value may be ignored by the caller as it is *non-owning*. This handle can be used to query for the
State's id or whether an exception was thrown from it via `state.Id()` and `state.IsAbominable()` respectively.

The current state of the FSM can be set while the FSM is idle via `fsm.SetCurrentState(StateId)`
or `fsm.SetCurrentState(State<StateId>)`. The current state may also change due to transitions from Events.

The FSM also provides an API to manage states:

- `fsm.GetState(StateId) -> std::optional<State<StateId>>` — Returns a State if one exists for the provided Id
- `fsm.GetStates() -> (iterable range of State<StateId>)`
- `fsm.RemoveState(StateId / State<StateId>) -> bool` — Returns false if the State did not exist to be removed.
- `fsm.RemoveAbominableStates()` — Removes all states where `state.IsAbominable()` is true.

All `dxfsm::State<StateId>` handles will be invalidated if the State is removed from the FSM.
Any action that results in the resumption of a State that does not exist in the FSM will
result in **undefined behavior**.

### Resets

DxFSM provides *resettable* awaitables as analogs to most of the Event awaitables:

- `dxfsm::ResetToken should_reset = co_await fsm.ReceiveEventResettable(Event&);`
- `dxfsm::ResetToken should_reset = co_await fsm.EmitAndReceiveEventResettable(Event&);`
- `dxfsm::ResetToken should_reset  = co_await fsm.IgnoreEventResettable();`

These perform the same action as their non-resettable equivalents, with one additional
functionality. If the current state changes while waiting on a resettable awaitable,
then a "reset" is triggered. The reset resumes the from-state of the transition,
but does not allow it to send further events.
The to-state of the transition will be resumed like normal once the from-state suspends
after its reset.

The `ResetToken` returned from the `co_await` statement (as shown above)
evaluates to true when a reset is occurring.

Resets are primarily useful for States where you wish to gather multiple events
in sequence, and then clear the sequence back to the beginning on any transition.
This allows you to create "nested" States, as each State coroutine can itself contain
multiple stages which you can intelligently switch between. This can help organize
closely related States or event responses together.

### Exceptions

DxFSM is fully exception-safe and can handle user-thrown exceptions at any point
during execution.

A state will become *abominable* if an exception escapes the coroutine function.
When this happens, the State will still exist in the FSM, but will no longer associated
with a C++ coroutine storage.
DxFSM will capture this exception and rethrow it to the original external caller 
that sent the event to the FSM — this will be the call to `fsm.InsertEvent` or `fsm.EmplaceEvent`.

Any action that results in the resumption of an 
abominable state will result in **undefined behavior**.
However, you can call the State coroutine again with the same id to re-add it to the FSM, which will
transparently update existing `dxfsm::State<StateId>` handles and existing, non-remote transitions.
Remote transitions need to be *rebound* by calling `fsm.RebindRemoteTransitions()` on the
FSM where they originate from.

Abominable States and Dangling Transitions
(transitions where the to-state is abominable or nonexistent)
can be removed via `fsm.RemoveAbominableStates()` 
and `fsm.RemoveDanglingTransitions()` respectively.

#### Library-Generated Exceptions

DxFSM uses exceptions to communicate misuse of the API.
Some cases of misuse, however, intentionally do not throw an exception.
This was done to keep exceptions off the primary API as much as possible.

TODO: Document where exceptions can be thrown from


