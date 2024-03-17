#include <dxfsm/dxfsm.hpp>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

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
}

TEST_CASE("Basic usage error checking", "[basic][exceptions][removing]") {
    using namespace Catch::Matchers;

    FSM_t fsm{};
    bool throw_on_resume = false;
    bool delete_on_resume = false;
    std::string_view state_to_set = "";

    fsm.AddState(Main(fsm, "State", throw_on_resume, delete_on_resume, state_to_set));
    fsm.SetCurrentState("State");

    SECTION("Duplicate state check") {
        CHECK_THROWS_WITH(
            fsm.AddState(Main(fsm, "State", throw_on_resume, delete_on_resume, state_to_set)),
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
        fsm.AddState(Main(fsm, "State2", throw_on_resume, delete_on_resume, state_to_set)),
        state_to_set = "State2";
        CHECK_THROWS_WITH(fsm.InsertEvent("Event"), ContainsSubstring("FSM is active"));
    }

    SECTION("Remove nonexistent state") {
        CHECK_THROWS_WITH(fsm.RemoveState("Not here"), ContainsSubstring("nonexistent"));
    }

    SECTION("Remove current state while running") {
        delete_on_resume = true;
        CHECK_THROWS_WITH(fsm.InsertEvent("Event"), ContainsSubstring("while it is running"));
    }
}