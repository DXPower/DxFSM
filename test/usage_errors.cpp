#include <dxfsm/dxfsm.hpp>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <catch2/generators/catch_generators.hpp>

namespace {
    using StateId = std::string_view;
    using EventId = std::string_view;

    using FSM_t = dxfsm::FSM<StateId, EventId>;
    using State_t = FSM_t::State_t;

    State_t Main(FSM_t& fsm, StateId id, bool& throw_on_resume, bool& delete_on_resume, std::string_view& state_to_set) {
        while (true) {
            co_await fsm.IgnoreEvent();

            if (throw_on_resume) {
                throw std::runtime_error("Throw!");
            } else if (delete_on_resume) {
                fsm.RemoveState(id);
            } else if (!state_to_set.empty()) {
                fsm.SetCurrentState(state_to_set);
            }
        }
    }

    State_t Resettable(FSM_t& fsm, StateId, bool emit_mode) {
        FSM_t::Event_t event = co_await fsm.ReceiveInitialEvent();
        (void) co_await fsm.IgnoreEventResettable();

        event.Store("Blah", 123);

        while (true) {
            if (emit_mode) {
                co_await fsm.EmitAndReceive(event);
            } else {
                (void) co_await fsm.EmitAndReceiveResettable(event);
            }
        }
    }
}

TEST_CASE("Basic usage error checking", "[basic][exceptions][removing]") {
    using namespace Catch::Matchers;

    FSM_t fsm{};

    bool throw_on_resume = false;
    bool delete_on_resume = false;
    std::string_view state_to_set = "";

    Main(fsm, "State", throw_on_resume, delete_on_resume, state_to_set);
    fsm.SetCurrentState("State");

    SECTION("Duplicate state check") {
        CHECK_THROWS_WITH(
            Main(fsm, "State", throw_on_resume, delete_on_resume, state_to_set),
            ContainsSubstring("conflicting id")
        );
    }

    SECTION("Set non-existent current state") {
        CHECK_THROWS_WITH(fsm.SetCurrentState("Null"), ContainsSubstring("nonexistent"));
    }

    SECTION("Set abominable state") {
        throw_on_resume = true;
        CHECK_THROWS_WITH(fsm.InsertEvent("Event"), Equals("Throw!"));
        CHECK_THROWS_WITH(fsm.SetCurrentState("State"), ContainsSubstring("abominable"));
    }

    SECTION("Set state while active") {
        Main(fsm, "State2", throw_on_resume, delete_on_resume, state_to_set);
        state_to_set = "State2";
        CHECK_THROWS_WITH(fsm.InsertEvent("Event"), ContainsSubstring("FSM is active"));
    }

    SECTION("Remove current state while running") {
        delete_on_resume = true;
        CHECK_THROWS_WITH(fsm.InsertEvent("Event"), ContainsSubstring("while it is running"));
    }

    SECTION("Emit non-empty event while resetting") {
        auto resettable_state = Resettable(fsm, "Resettable", GENERATE(true, false));
        fsm.AddTransition("Resettable", "Transition", "State");
        fsm.SetCurrentState(resettable_state);
        fsm.InsertEvent("event"); // Start the state with the initial event, setting up the reset

        SECTION("Reset from event transition") {
            CHECK_THROWS_WITH(fsm.InsertEvent("Transition"), ContainsSubstring("non-empty event"));
        }

        SECTION("Reset from SetCurrentState") {
            CHECK_THROWS_WITH(fsm.SetCurrentState("State"), ContainsSubstring("non-empty event"));
        }
    }
}