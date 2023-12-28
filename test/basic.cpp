#include <dxfsm/dxfsm.hpp>

#include <algorithm>
#include <array>
#include <ranges>
#include <iostream>
#include <catch2/catch_test_macros.hpp>

using namespace dxfsm;

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

        Event event = co_await fsm.ReceiveEvent();

        while (true) {
            sequence.clear();
            event_ids.clear();

            event_ids.push_back(event.GetId());
            
            auto start_value = event.Get<int>();

            event.Store(EventId::ProcessValue, start_value);
            co_await fsm.EmitAndReceive(event);
        }
    }

    State_t StateProcess(FSM_t& fsm, StateId id) {
        Event event = co_await fsm.ReceiveEvent();

        while (true) {
            event_ids.push_back(event.GetId());

            auto& cur_value = event.Get<int>();

            sequence.push_back(cur_value);

            if (cur_value == 1) {
                // Replace the current event with a Finish event
                event.Store(EventId::Finish);
            } else {
                // Change the value in the current event in-place
                if (cur_value % 2 == 0) {
                    cur_value /= 2;
                } else {
                    cur_value = 3 * cur_value + 1;
                }
            }

            co_await fsm.EmitAndReceive(event);
        }
    }

    State_t StateFinish(FSM_t& fsm, StateId id) {
        Event_t event{};

        while (true) {
            event = co_await fsm.ReceiveEvent();
            event_ids.push_back(event.GetId());
        }
    }
};

TEST_CASE("Collatz FSM", "[basic][events][names]") {
    CollatzFsm collatz{};

    CollatzFsm::Event_t event(EventId::Start, 15);
    collatz.fsm.SendEvent(std::move(event));

    auto odds = collatz.sequence | std::views::filter([](int x) { return x % 2 != 0; });
    std::array odds_answers = {15,23,35,53,5,1};

    REQUIRE(std::ranges::equal(odds, odds_answers));
}