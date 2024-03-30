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