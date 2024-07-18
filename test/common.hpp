#pragma once

#include <dxfsm/dxfsm.hpp>
#include <vector>

template<typename StateId, typename EventId>
auto GetAbominableStates(const dxfsm::FSM<StateId, EventId>& fsm) {
    std::vector<dxfsm::State<StateId>> states{};
    std::ranges::copy(fsm.GetStates() | std::views::filter([](const auto& s) {
        return s.IsAbominable();
    }), std::back_inserter(states));

    return states;
}

template<typename StateId, typename EventId>
void CheckActive(const dxfsm::FSM<StateId, EventId>& fsm, bool expectation) {
    if (expectation && !fsm.IsActive())
        throw std::runtime_error("Expected the FSM to be active");
    else if (!expectation && fsm.IsActive())
        throw std::runtime_error("Expected the FSM to be idle");
}