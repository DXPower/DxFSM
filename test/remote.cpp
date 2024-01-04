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

            fsm.AddState(StateOn(fsm, on_state).Name(std::string(on_state)))
               .AddState(StateOff(fsm, off_state).Name(std::string(off_state)));

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

                    if (current_brightness == 0xDEAD) {
                        throw std::runtime_error("DEAD!");
                    }
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

TEST_CASE("Remote Transitions with External Event IDs (No ID conversion)", "[remote][advanced][exceptions]") {
    using Red = Light<false, 0>;
    using Green = Light<false, 1>;
    using Blue = Light<false, 2>;

    Red red("RedFSM", "RedOn", "RedOff");
    Green green("GreenFSM", "GreenOn", "GreenOff");
    Blue blue("BlueFSM", "BlueOn", "BlueOff");
    red.current_brightness = 64;
    green.current_brightness = 28;
    blue.current_brightness = 36;

    using EventId = ExternalEventId_e;

    red.fsm.AddRemoteTransition("RedOn", EventId::CycleOut, green.fsm, "GreenOn"sv);
    green.fsm.AddRemoteTransition("GreenOn", EventId::CycleOut, blue.fsm, "BlueOn"sv);
    blue.fsm.AddRemoteTransition("BlueOn", EventId::CycleOut, red.fsm, "RedOn"sv);

    SECTION("Normal Operation") {
        CHECK(red.fsm.GetCurrentState()->Name() == "RedOff");
        CHECK(green.fsm.GetCurrentState()->Name() == "GreenOff");
        CHECK(blue.fsm.GetCurrentState()->Name() == "BlueOff");

        red.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn)); // Turns red on
        red.fsm.InsertEvent(dxfsm::Event(EventId::CycleOut, 50)); // Sends 50 brightness to green

        CHECK(red.current_brightness == 64);
        CHECK(green.current_brightness == 50);
        CHECK(blue.current_brightness == 36);
        CHECK(red.fsm.GetCurrentState()->Name() == "RedOn");
        CHECK(green.fsm.GetCurrentState()->Name() == "GreenOn");
        CHECK(blue.fsm.GetCurrentState()->Name() == "BlueOff");

        green.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn)); // Turns green off
        green.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn)); // Turns green on
        red.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn)); // Turns red off
        red.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn)); // Turns red on
        green.fsm.InsertEvent(dxfsm::Event(EventId::CycleOut, 75)); // Sends 75 brightness to blue

        CHECK(red.current_brightness == 64);
        CHECK(green.current_brightness == 50);
        CHECK(blue.current_brightness == 75);
        CHECK(red.fsm.GetCurrentState()->Name() == "RedOn");
        CHECK(green.fsm.GetCurrentState()->Name() == "GreenOn");
        CHECK(blue.fsm.GetCurrentState()->Name() == "BlueOn");

        blue.fsm.InsertEvent(dxfsm::Event(EventId::CycleOut, 49)); // Sends 49 brightness to red

        CHECK(red.current_brightness == 49);
        CHECK(green.current_brightness == 50);
        CHECK(blue.current_brightness == 75);
        CHECK(red.fsm.GetCurrentState()->Name() == "RedOn");
        CHECK(green.fsm.GetCurrentState()->Name() == "GreenOn");
        CHECK(blue.fsm.GetCurrentState()->Name() == "BlueOn");

        red.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn)); // Turns red off
        red.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn)); // Turns red on

        CHECK_THAT(red.light_sequence, Catch::Matchers::Equals(std::vector({0, 64, 0, 64, 49, 0, 49})));
        CHECK_THAT(green.light_sequence, Catch::Matchers::Equals(std::vector({0, 50, 0, 50})));
        CHECK_THAT(blue.light_sequence, Catch::Matchers::Equals(std::vector({0, 75})));
    }

    SECTION("Exception Safety") {
        red.fsm.InsertEvent(dxfsm::Event(EventId::CycleIn)); // Turns red on
        CHECK_THROWS_WITH(red.fsm.InsertEvent(dxfsm::Event(EventId::CycleOut, 0xDEAD)), "DEAD!");

        auto CheckFailedStates = [&]<typename S, typename E>(const dxfsm::FSM<S, E>& fsm, std::string_view expected_id = "") {
            std::vector<const dxfsm::State<S>*> failed_states{};
            std::ranges::copy(fsm.GetAbominableStates() | std::views::transform([](const auto& s) {
                return &s;
            }), std::back_inserter(failed_states));

            if (expected_id == "") {
                CHECK(failed_states.size() == 0);
            } else {
                REQUIRE(failed_states.size() == 1);
                CHECK(failed_states[0]->Id() == expected_id);
                CHECK(failed_states[0]->IsAbominable());
            }
        };

        CheckFailedStates(red.fsm);
        CheckFailedStates(green.fsm, "GreenOn");
        CheckFailedStates(blue.fsm);

        CHECK_FALSE(red.fsm.IsActive());
        CHECK_FALSE(green.fsm.IsActive());
        CHECK_FALSE(blue.fsm.IsActive());
    }
}