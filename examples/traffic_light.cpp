#include <dxfsm/dxfsm.hpp>
#include <iostream>

enum class StateId { JaneGreen, DoeGreen };
enum class EventId { GreenCarPass, RedCarWaiting, RedAmbulanceApproaching, Toggle };

using enum StateId;
using enum EventId;
using FSM = dxfsm::FSM<StateId, EventId>;
using State = FSM::State_t;
using Event = FSM::Event_t;

State TrafficState(FSM& fsm, StateId, std::string_view street_name) {
    Event ev = co_await fsm.ReceiveInitialEvent();
    
    int cars_waiting = 0;
    int cars_passed = 0;
    
    while (true) {
        if (ev == Toggle) {
            std::cout << street_name << " turned green!\n";
            ev.Clear();
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
        
        bool toggling_lights = co_await fsm.EmitAndReceiveResettable(ev);

        if (toggling_lights) {
            // We are transitioning away from this state,
            // which means we're turning red, 
            // and the car counts should be reset.
            cars_waiting = 0;
            cars_passed = 0;
            std::cout << street_name << " is turning red!\n";
            co_await fsm.ReceiveEvent(ev);
        }
    }
}

int main() {
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