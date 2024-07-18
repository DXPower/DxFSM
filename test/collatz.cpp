#include <dxfsm/dxfsm.hpp>
#include "common.hpp"

#include <algorithm>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <catch2/matchers/catch_matchers_vector.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

using namespace dxfsm;

namespace {
    enum class EventId {
        Start,
        ProcessValue,
        Finish
    };

    enum class StateId {
        Start,
        Processing,
        Finish
    };

    struct InvalidValue : std::runtime_error {
        int value{};

        InvalidValue(int value)
            : std::runtime_error(std::format("Invalid value in Collatz sequence: {}", value)),
              value(value)
        { }
    };
}

struct CollatzFsm {
    using Event_t = Event<EventId>;
    using State_t = State<StateId>;
    using FSM_t = FSM<StateId, EventId>;

    FSM_t fsm;
    
    std::vector<int> sequence{};
    std::vector<EventId> event_ids{};

    CollatzFsm() {
        StateStart(fsm, StateId::Start);
        StateProcess(fsm, StateId::Processing);
        StateFinish(fsm, StateId::Finish);

        fsm.AddTransition(StateId::Start, EventId::ProcessValue, StateId::Processing);
        fsm.AddTransition(StateId::Processing, EventId::ProcessValue, StateId::Processing);
        fsm.AddTransition(StateId::Processing, EventId::Finish, StateId::Finish);
        fsm.AddTransition(StateId::Finish, EventId::Start, StateId::Start);

        fsm.SetCurrentState(StateId::Start);
    }

    State_t StateStart(FSM_t& fsm, StateId) {
        // Specifically test using EmitAndReceive at beginning of loop
        Event_t event{};

        while (true) {
            co_await fsm.EmitAndReceive(event);
            sequence.clear();
            event_ids.clear();

            event_ids.push_back(event.GetId());
            
            auto start_value = event.Get<int>();

            event.Store(EventId::ProcessValue, start_value);
        }
    }

    State_t StateProcess(FSM_t& fsm, StateId) {
        // Specifically test ReceiveEvent at top then EmitAndReceive at end of loop
        Event event = co_await fsm.ReceiveInitialEvent();

        while (true) {
            event_ids.push_back(event.GetId());

            auto& cur_value = event.Get<int>();

            sequence.push_back(cur_value);

            if (cur_value == 1) {
                // Replace the current event with a Finish event
                event = EventId::Finish;
            } else if (cur_value > 1) {
                // Change the value in the current event in-place
                if (cur_value % 2 == 0) {
                    cur_value /= 2;
                } else {
                    cur_value = 3 * cur_value + 1;
                }
            } else {
                throw InvalidValue(cur_value);
            }

            co_await fsm.EmitAndReceive(event);
        }
    }

    State_t StateFinish(FSM_t& fsm, StateId) {
        Event_t event{};

        while (true) {
            // Swallow previous event allocations (just to test ReceiveEvent)
            co_await fsm.ReceiveEvent(event);
            event_ids.push_back(event.GetId());
        }
    }
};

struct InvalidValueMatcher : Catch::Matchers::MatcherGenericBase {
    int expected{};

    InvalidValueMatcher(int expected) : expected(expected) { }

    bool match(const InvalidValue& given) const {
        return given.value == expected;
    }

    std::string describe() const override {
        return std::format("InvalidValue equals: {}", expected);
    }
};

TEST_CASE("Collatz FSM", "[basic]") {
    CollatzFsm collatz{};

    CollatzFsm::Event_t event(EventId::Start, 15);
    collatz.fsm.InsertEvent(EventId::Start, 15);

    CHECK_FALSE(collatz.fsm.IsActive());

    auto odds = collatz.sequence | std::views::filter([](int x) { return x % 2 != 0; });
    std::vector<int> odds_vec{};
    std::ranges::copy(odds, std::back_inserter(odds_vec));

    std::vector odds_answers = {15,23,35,53,5,1};

    CHECK_THAT(odds_vec, Catch::Matchers::Equals(odds_answers));
}

TEST_CASE("Collatz FSM States/Transitions", "[basic][transition_api][state_api]") {
    CollatzFsm collatz{};
    using Transition_t = CollatzFsm::FSM_t::Transition_t;

    std::vector<Transition_t> transitions{};
    std::ranges::copy(collatz.fsm.GetTransitions(), std::back_inserter(transitions));

    auto HasTransition = [&](StateId from, EventId event) {
        auto found = collatz.fsm.GetTransition(from, event);
        bool result = true;

        result &= found.has_value();
        result &= std::ranges::find_if(transitions, [&](const Transition_t& t) {
            return t.From() == from && t.Event() == event;
        }) != transitions.end();

        if (result) {
            result &= found->From() == from;
            result &= found->Event() == event;
        }

        return result;
    };

    CHECK(HasTransition(StateId::Start, EventId::ProcessValue));
    CHECK(HasTransition(StateId::Processing, EventId::ProcessValue));
    CHECK(HasTransition(StateId::Processing, EventId::Finish));
    CHECK(HasTransition(StateId::Finish, EventId::Start));

    CHECK_FALSE(HasTransition(StateId::Start, EventId::Finish));
    CHECK_FALSE(HasTransition(StateId::Processing, EventId::Start));
    CHECK_FALSE(HasTransition(StateId::Finish, EventId::ProcessValue));

    CHECK(collatz.fsm.RemoveTransition(StateId::Start, EventId::ProcessValue));
    CHECK_FALSE(collatz.fsm.RemoveTransition(StateId::Start, EventId::Finish));

    CHECK_FALSE(HasTransition(StateId::Start, EventId::ProcessValue));

    CHECK(collatz.fsm.HasState(StateId::Start));
    CHECK(collatz.fsm.GetState(StateId::Start).has_value());
    CHECK(collatz.fsm.RemoveState(StateId::Start));
    CHECK_FALSE(collatz.fsm.HasState(StateId::Start));
    CHECK_FALSE(collatz.fsm.GetState(StateId::Start).has_value());
    CHECK_FALSE(collatz.fsm.RemoveState(StateId::Start));

    CHECK(collatz.fsm.GetTransition(StateId::Finish, EventId::Start)->IsDangling());
}

TEST_CASE("Collatz FSM Exceptions", "[advanced][exceptions]") {
    CollatzFsm collatz{};

    int invalid_value{};

    SECTION("Zero") {
        invalid_value = 0;
    }

    SECTION("Negative") {
        invalid_value = -37;
    }

    CHECK_THROWS_MATCHES(
        collatz.fsm.InsertEvent(EventId::Start, invalid_value),
        InvalidValue, 
        InvalidValueMatcher(invalid_value)
    );
    CHECK_FALSE(collatz.fsm.IsActive());

    std::vector<CollatzFsm::State_t> failed_states = GetAbominableStates(collatz.fsm);

    REQUIRE(failed_states.size() == 1);
    CHECK(failed_states[0].Id() == StateId::Processing);
    CHECK(failed_states[0].IsAbominable());
    CHECK(collatz.fsm.GetTransition(StateId::Start, EventId::ProcessValue)->IsDangling());
    CHECK(collatz.fsm.GetTransition(StateId::Processing, EventId::ProcessValue)->IsDangling());

    using Catch::Matchers::ContainsSubstring;
    CHECK_THROWS_WITH(collatz.fsm.SetCurrentState(StateId::Processing), ContainsSubstring("Attempt to set abominable state"));
    CHECK_FALSE(collatz.fsm.GetCurrentState().has_value());
    
    collatz.fsm.RemoveAbominableStates();
    CHECK(GetAbominableStates(collatz.fsm).size() == 0);
}