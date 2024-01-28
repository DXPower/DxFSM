#include <dxfsm/dxfsm.hpp>

#include <ostream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_vector.hpp>

using namespace dxfsm;

namespace {
    enum class EventId {
        OuterStep,
        InnerStep,
        InnerNext,
        Throw
    };

    using Event_t = Event<EventId>;

    using StateId = std::string_view;
    using State_t = State<std::string_view>;
    using FSM_t = FSM<StateId, EventId>;

    enum class StageId {
        A, B, C, D
    };

    struct Stage {
        StateId state_id{};
        StageId stage_id{};
        int data{};

        bool operator==(const Stage& rhs) const = default;

        friend std::ostream& operator<<(std::ostream& out, const Stage& s) {
            char stage_id{};

            switch (s.stage_id) {
                case StageId::A: stage_id = 'A';
                case StageId::B: stage_id = 'B';
                case StageId::C: stage_id = 'C';
                case StageId::D: stage_id = 'D';
            }
            return out << std::format("State: {}; Stage: {}; Data: {};", s.state_id, stage_id, s.data);
        }
    };

    struct Resets {
        FSM_t fsm{"ResetsFSM"};

        std::vector<Stage> stages{};
        bool throw_on_reset{};

        Resets() {
            // fsm.AddState(State_t &&state)
            fsm
                .AddState(Cycler(fsm, "One").Name("OneState"))
                .AddState(Cycler(fsm, "Two").Name("TwoState"))
                .AddState(Cycler(fsm, "Three").Name("ThreeState"));

            fsm
                .AddTransition("One", EventId::OuterStep, "Two")
                .AddTransition("Two", EventId::OuterStep, "Three")
                .AddTransition("Three", EventId::OuterStep, "One");

            fsm.SetCurrentState("One").InsertEvent(Event(EventId::InnerStep, 1));
        }

        State_t Cycler(FSM_t& fsm, StateId state_id) {
            Event_t event = co_await fsm.ReceiveInitialEvent();
            ResetToken reset_token{};

            while (true) {
            Escape:
                for (StageId stage_id : {StageId::A, StageId::B, StageId::C, StageId::D}) {
                    if (reset_token) {
                        if (throw_on_reset)
                            throw std::runtime_error("Exception during reset");

                        reset_token = {};
                        event.Store(EventId::InnerStep, -1);
                        goto Escape;
                    } else if (event == EventId::InnerStep || event == EventId::OuterStep) {
                        stages.push_back({state_id, stage_id, event.Get<int>()});
                        event.Clear();
                        reset_token = co_await fsm.EmitAndReceiveResettable(event);
                    } else if (event == EventId::InnerNext) {
                        // This will trigger a transition to another state and thus a reset
                        event.Store(EventId::OuterStep, 0xFACE);
                        reset_token = co_await fsm.EmitAndReceiveResettable(event);
                    } else if (event == EventId::Throw) {
                        throw std::runtime_error(event.Get<std::string>());
                    }
                }

                event.Store(EventId::OuterStep, 0xDEAD);
                co_await fsm.EmitAndReceive(event);
            }
        }

        Stage CurrentStage() const {
            return stages.back();
        }
    };
}

TEST_CASE("Resettable States", "[advanced][resets]") {
    using enum StageId;

    Resets resets{};
    int counter = 2;

    CHECK(resets.CurrentStage() == Stage{"One", A, 1});

    SECTION("Externally triggered reset") {
        SECTION("State One") {
            resets.fsm.InsertEvent(Event(EventId::InnerStep, counter++));
            resets.fsm.InsertEvent(Event(EventId::InnerStep, counter++));

            CHECK(resets.CurrentStage() == Stage{"One", C, 3});

            resets.fsm.InsertEvent(Event(EventId::OuterStep, counter++));

            CHECK_THAT(resets.stages, Catch::Matchers::Equals(std::vector{
                Stage{"One", A, 1},
                Stage{"One", B, 2},
                Stage{"One", C, 3},
                Stage{"One", A, -1},
                Stage{"Two", A, 4},
            }));

        }
    }
}