#include <dxfsm/dxfsm.hpp>

#include <algorithm>
#include <array>
#include <ranges>
#include <iostream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>

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

template<>
struct std::hash<EventId> {
    std::size_t operator()(const EventId& id) {
        using U = std::underlying_type_t<EventId>;
        return std::hash<U>{}(static_cast<U>(id));
    }
};

struct CollatzFsm {
    using Event_t = Event<EventId>;
    using State_t = State<StateId>;
    using FSM_t = FSM<StateId, EventId>;

    FSM_t fsm{"CollatzFsm"};
    
    std::vector<int> sequence{};
    std::vector<EventId> event_ids{};

    CollatzFsm() {
        fsm
            .AddState(StateStart(fsm, StateId::Start).Name("Start"))
            .AddState(StateProcess(fsm, StateId::Processing).Name("Processing"))
            .AddState(StateFinish(fsm, StateId::Finish).Name("Finish"));

        fsm
            .AddTransition(StateId::Start, EventId::ProcessValue, StateId::Processing)
            .AddTransition(StateId::Processing, EventId::ProcessValue, StateId::Processing)
            .AddTransition(StateId::Processing, EventId::Finish, StateId::Finish)
            .AddTransition(StateId::Finish, EventId::Start, StateId::Start);

        fsm.SetCurrentState(StateId::Start).Start();
    }

    State_t StateStart(FSM_t& fsm, StateId id) {
        std::cout << "Hello" << std::endl;

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

    State_t StateProcess(FSM_t& fsm, StateId id) {
        // Specifically test ReceiveEvent at top then EmitAndReceive at end of loop
        Event event = co_await fsm.ReceiveInitialEvent();

        while (true) {
            event_ids.push_back(event.GetId());

            auto& cur_value = event.Get<int>();

            sequence.push_back(cur_value);

            if (cur_value == 1) {
                // Replace the current event with a Finish event
                // event.Store(EventId::Finish);
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

    State_t StateFinish(FSM_t& fsm, StateId id) {
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
    collatz.fsm.InsertEvent(std::move(event));

    auto odds = collatz.sequence | std::views::filter([](int x) { return x % 2 != 0; });
    std::array odds_answers = {15,23,35,53,5,1};

    REQUIRE(std::ranges::equal(odds, odds_answers));
}

TEST_CASE("Collatz FSM Exceptions", "[advanced][exceptions]") {
    CollatzFsm collatz{};

    int invalid_value{};

    SECTION("Zero") {
        // Event(EventId::Start, 0)
        invalid_value = 0;
        // CHECK_THROWS_MATCHES(collatz.fsm.InsertEvent(), InvalidValue, InvalidValueMatcher(0));
    }

    SECTION("Negative") {
        // CHECK_THROWS_MATCHES(collatz.fsm.InsertEvent(Event(EventId::Start, 0)));
        invalid_value = -37;
    }

    Event e(EventId::Start, invalid_value);

    CHECK_THROWS_MATCHES(collatz.fsm.InsertEvent(std::move(e)), InvalidValue, InvalidValueMatcher(invalid_value));
    CHECK_FALSE(collatz.fsm.IsActive());
}