#include <dxfsm/dxfsm.hpp>

#include <algorithm>
#include <ranges>
#include <iostream>
#include <catch2/catch_test_macros.hpp>

using namespace dxfsm;

struct CollatzFsm {
    FSM fsm{"CollatzFsm"};
    
    std::vector<int> sequence{};
    std::vector<std::string> event_names{};

    CollatzFsm() {
        fsm << StateStart(fsm).Name("StateStart")
            << StateProcess(fsm).Name("StateProcess")
            << StateFinish(fsm).Name("StateFinish");

        fsm << transition("StateStart", "ProcessValue", "StateProcess")
            << transition("StateProcess", "ProcessValue", "StateProcess")
            << transition("StateProcess", "Finish", "StateFinish")
            << transition("StateFinish", "Start", "StateStart");

        fsm.setState("StateStart").start();
    }

    State StateStart(FSM& fsm) {
        std::cout << "Hello" << std::endl;

        Event event = co_await fsm.getEvent();

        while (true) {
            sequence.clear();
            event_names.clear();

            event_names.push_back(std::string(event.name()));
            
            auto start_value = event.Get<int>();

            event.Store("ProcessValue", start_value);
            co_await fsm.emitAndReceive2(event);
        }
    }

    State StateProcess(FSM& fsm) {
        Event event = co_await fsm.getEvent();

        while (true) {
            event_names.push_back(std::string(event.name()));

            auto& cur_value = event.Get<int>();

            sequence.push_back(cur_value);

            if (cur_value == 1) {
                // Replace the current event with a Finish event
                event.Store("Finish");
            } else {
                // Change the value in the current event in-place
                if (cur_value % 2 == 0) {
                    cur_value /= 2;
                } else {
                    cur_value = 3 * cur_value + 1;
                }
            }

            event = co_await fsm.emitAndReceive(&event);
        }
    }

    State StateFinish(FSM& fsm) {
        Event event = co_await fsm.getEvent();

        while (true) {
            event_names.push_back(std::string(event.name()));

            event = co_await fsm.getEvent();
        }
    }
};

TEST_CASE( "Collatz FSM", "[basic][events][names]" ) {
    CollatzFsm collatz{};

    Event event;
    event.Store("Start", 15);
    collatz.fsm.sendEvent(&event);

    auto odds = collatz.sequence | std::views::filter([](int x) { return x % 2 != 0; });
    std::array odds_answers = {15,23,35,53,5,1};

    REQUIRE(std::ranges::equal(odds, odds_answers));
}