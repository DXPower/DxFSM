#include <dxfsm/dxfsm.hpp>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_vector.hpp>

using namespace std::string_view_literals;

namespace {
    // CycleOut event will carry a brightness to set
    enum class ExternalEventId_e { CycleIn, CycleOut };

    template<bool UseInternalEventId, int Counter>
    struct Light {
        using StateId = std::string_view;
        
        enum class EventId_e { CycleIn, CycleOut };
        using EventId = std::conditional_t<UseInternalEventId, EventId_e, ExternalEventId_e>;

        using FSM = dxfsm::FSM<StateId, EventId>;
        using State = dxfsm::State<StateId>;
        using Event = dxfsm::Event<EventId>;

        FSM fsm;

        bool light_on{};
        int current_brightness{};
        
        std::vector<int> light_sequence{};

        Light(std::string name, StateId on_state, StateId off_state) {
            fsm.Name(name);

            fsm.AddState(StateOn(fsm, on_state).Name("ON"))
               .AddState(StateOff(fsm, off_state).Name("OFF"));

            fsm.AddTransition(on_state, EventId::CycleIn, off_state)
               .AddTransition(off_state, EventId::CycleIn, on_state);

            // Start the FSM by bringing it into the off state
            fsm.SetCurrentState(on_state).InsertEvent(Event(EventId::CycleIn));
        }

        State StateOn(FSM& fsm, StateId id) {
            Event event = co_await fsm.ReceiveInitialEvent();

            while (true) {
                light_on = true;

                if (event == EventId::CycleOut) {
                    current_brightness = event.template Get<int>();
                }

                light_sequence.push_back(current_brightness);

                event.Clear();
                co_await fsm.EmitAndReceive(event);
            }
        }

        State StateOff(FSM& fsm, StateId id) {
            while (true) {
                co_await fsm.IgnoreEvent();
                light_on = false;
                light_sequence.push_back(0);
            }
        }
    };
}

TEST_CASE("Light Local Test") {
    using Light_t = Light<true, 0>;

    Light_t light("LightFSM", "StateOn", "StateOff");
    light.current_brightness = 37;

    using EventId = Light_t::EventId;
    light.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn));
    light.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn));
    light.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn));
    light.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn));

    CHECK_THAT(light.light_sequence, Catch::Matchers::Equals(std::vector({0, 37, 0, 37, 0})));
    CHECK(light.fsm.GetCurrentState()->Id() == "StateOff");
}

TEST_CASE("Light Remote Test Internal Types") {
    using Red = Light<false, 0>;
    using Green = Light<false, 1>;
    using Blue = Light<false, 2>;

    Red red("RedFSM", "RedOn", "RedOff");
    Green green("GreenFSM", "GreenOn", "GreenOff");
    Blue blue("BlueFSM", "BlueOn", "BlueOff");
    red.current_brightness = 64;
    blue.current_brightness = 36;
    green.current_brightness = 28;

    using EventId = ExternalEventId_e;

    red.fsm.AddRemoteTransition("RedOn", EventId::CycleOut, green.fsm, "GreenOn"sv);
    green.fsm.AddRemoteTransition("GreenOn", EventId::CycleOut, blue.fsm, "BlueOn"sv);
    blue.fsm.AddRemoteTransition("BlueOn", EventId::CycleOut, red.fsm, "RedOn"sv);


    red.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn));
    red.fsm.InsertEvent(dxfsm::Event(EventId::CycleOut));

    // light.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn));
    // light.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn));
    // light.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn));

    // CHECK_THAT(light.light_sequence, Catch::Matchers::Equals(std::vector({0, 37, 0, 37, 0})));
    // CHECK(light.fsm.GetCurrentState()->Id() == "StateOff");
}