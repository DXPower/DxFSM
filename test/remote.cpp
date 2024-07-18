#include <dxfsm/dxfsm.hpp>
#include "common.hpp"

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

        Light(StateId on_state, StateId off_state) {
            StateOn(fsm, on_state);
            StateOff(fsm, off_state);

            fsm.AddTransition(on_state, EventId::CycleIn, off_state);
            fsm.AddTransition(off_state, EventId::CycleIn, on_state);

            // Start the FSM by bringing it into the off state
            fsm.SetCurrentState(on_state).InsertEvent(EventId::CycleIn);
        }

        State StateOn(FSM& fsm, StateId) {
            CheckActive(fsm, true);

            Event event = co_await fsm.ReceiveInitialEvent();

            while (true) {
                CheckActive(fsm, true);
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

        State StateOff(FSM& fsm, StateId) {
            while (true) {
                CheckActive(fsm, true);
                co_await fsm.IgnoreEvent();
                light_on = false;
                light_sequence.push_back(0);
            }
        }
    };
}

TEST_CASE("Light Local Test") {
    using Light_t = Light<true, 0>;

    Light_t light("StateOn", "StateOff");
    light.current_brightness = 37;

    using EventId = Light_t::EventId;
    light.fsm.InsertEvent(EventId::CycleIn);
    light.fsm.InsertEvent(EventId::CycleIn);
    light.fsm.InsertEvent(EventId::CycleIn);
    light.fsm.InsertEvent(EventId::CycleIn);

    CHECK_THAT(light.light_sequence, Catch::Matchers::Equals(std::vector({0, 37, 0, 37, 0})));
    CHECK(light.fsm.GetCurrentState()->Id() == "StateOff");
}

TEST_CASE("Remote Transitions with External Event IDs (No ID conversion)", "[remote][advanced][exceptions]") {
    using Red = Light<false, 0>;
    using Green = Light<false, 1>;
    using Blue = Light<false, 2>;

    Red red("RedOn", "RedOff");
    Green green("GreenOn", "GreenOff");
    Blue blue("BlueOn", "BlueOff");
    red.current_brightness = 64;
    green.current_brightness = 28;
    blue.current_brightness = 36;

    using EventId = ExternalEventId_e;

    red.fsm.AddRemoteTransition("RedOn"sv, EventId::CycleOut, green.fsm, "GreenOn"sv);
    green.fsm.AddRemoteTransition("GreenOn"sv, EventId::CycleOut, blue.fsm, "BlueOn"sv);
    blue.fsm.AddRemoteTransition("BlueOn"sv, EventId::CycleOut, red.fsm, "RedOn"sv);

    CheckActive(red.fsm, false);
    CheckActive(green.fsm, false);
    CheckActive(blue.fsm, false);

    SECTION("Normal Operation") {
        CHECK(red.fsm.GetCurrentState()->Id() == "RedOff");
        CHECK(green.fsm.GetCurrentState()->Id() == "GreenOff");
        CHECK(blue.fsm.GetCurrentState()->Id() == "BlueOff");

        red.fsm.InsertEvent(EventId::CycleIn); // Turns red on
        red.fsm.InsertEvent([](auto& ev) { ev.Store(EventId::CycleOut, 50); }); // Sends 50 brightness to green

        CHECK(red.current_brightness == 64);
        CHECK(green.current_brightness == 50);
        CHECK(blue.current_brightness == 36);
        CHECK(red.fsm.GetCurrentState()->Id() == "RedOn");
        CHECK(green.fsm.GetCurrentState()->Id() == "GreenOn");
        CHECK(blue.fsm.GetCurrentState()->Id() == "BlueOff");

        green.fsm.InsertEvent(EventId::CycleIn); // Turns green off
        green.fsm.InsertEvent(EventId::CycleIn); // Turns green on
        red.fsm.InsertEvent(EventId::CycleIn); // Turns red off
        red.fsm.InsertEvent(EventId::CycleIn); // Turns red on
        green.fsm.InsertEvent([](auto& ev) { ev.Store(EventId::CycleOut, 75); }); // Sends 75 brightness to blue

        CHECK(red.current_brightness == 64);
        CHECK(green.current_brightness == 50);
        CHECK(blue.current_brightness == 75);
        CHECK(red.fsm.GetCurrentState()->Id() == "RedOn");
        CHECK(green.fsm.GetCurrentState()->Id() == "GreenOn");
        CHECK(blue.fsm.GetCurrentState()->Id() == "BlueOn");

        blue.fsm.InsertEvent([](auto& ev) { ev.Store(EventId::CycleOut, 49); }); // Sends 49 brightness to red

        CHECK(red.current_brightness == 49);
        CHECK(green.current_brightness == 50);
        CHECK(blue.current_brightness == 75);
        CHECK(red.fsm.GetCurrentState()->Id() == "RedOn");
        CHECK(green.fsm.GetCurrentState()->Id() == "GreenOn");
        CHECK(blue.fsm.GetCurrentState()->Id() == "BlueOn");

        red.fsm.InsertEvent(EventId::CycleIn); // Turns red off
        red.fsm.InsertEvent(EventId::CycleIn); // Turns red on

        CHECK_THAT(red.light_sequence, Catch::Matchers::Equals(std::vector({0, 64, 0, 64, 49, 0, 49})));
        CHECK_THAT(green.light_sequence, Catch::Matchers::Equals(std::vector({0, 50, 0, 50})));
        CHECK_THAT(blue.light_sequence, Catch::Matchers::Equals(std::vector({0, 75})));
    }

    SECTION("Exception Safety") {
        red.fsm.InsertEvent(EventId::CycleIn); // Turns red on
        CHECK_THROWS_WITH(red.fsm.InsertEvent([](auto& ev) { ev.Store(EventId::CycleOut, 0xDEAD); }), "DEAD!");

        auto CheckFailedStates = [&]<typename S, typename E>(const dxfsm::FSM<S, E>& fsm, std::string_view expected_id = "") {
            auto failed_states = GetAbominableStates(fsm);

            if (expected_id == "") {
                CHECK(failed_states.size() == 0);
            } else {
                REQUIRE(failed_states.size() == 1);
                CHECK(failed_states[0].Id() == expected_id);
                CHECK(failed_states[0].IsAbominable());
            }
        };

        CheckFailedStates(red.fsm);
        CheckFailedStates(green.fsm, "GreenOn");
        CheckFailedStates(blue.fsm);

        CHECK_FALSE(red.fsm.IsActive());
        CHECK_FALSE(green.fsm.IsActive());
        CHECK_FALSE(blue.fsm.IsActive());

        REQUIRE(red.fsm.GetCurrentState().has_value());
        CHECK(red.fsm.GetCurrentState()->Id() == "RedOn");
        CHECK_FALSE(green.fsm.GetCurrentState().has_value());
        REQUIRE(blue.fsm.GetCurrentState().has_value());
        CHECK(blue.fsm.GetCurrentState()->Id() == "BlueOff");
    }
    
    CheckActive(red.fsm, false);
    CheckActive(green.fsm, false);
    CheckActive(blue.fsm, false);
}