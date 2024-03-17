#include <dxfsm/dxfsm.hpp>

#include <ostream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_vector.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

using namespace dxfsm;
using namespace std::string_view_literals;

namespace {
    enum class EventId {
        OuterStep,
        InnerStep,
        InnerNext,
        Jump,
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
        bool throw_after_jump{};

        Resets() {
            // fsm.AddState(State_t &&state)
            fsm
                .AddState(Cycler(fsm, "One"))
                .AddState(Cycler(fsm, "Two"))
                .AddState(Cycler(fsm, "Three"));

            fsm
                .AddTransition("One", EventId::OuterStep, "Two")
                .AddTransition("Two", EventId::OuterStep, "Three")
                .AddTransition("Three", EventId::OuterStep, "One");

            fsm.SetCurrentState("One").InsertEvent(EventId::InnerStep, 1);
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
                    } else if (event == EventId::Jump) {
                        stages.push_back({state_id, stage_id, event.Get<int>()});
                        
                        if (throw_after_jump) {
                            throw std::runtime_error("Jump throw");
                        }

                        event.Clear();
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

TEST_CASE("Resettable States", "[advanced][resets][exceptions]") {
    using enum StageId;

    Resets resets{};
    int counter = 2;

    CHECK(resets.CurrentStage() == Stage{"One", A, 1});
    resets.fsm.InsertEvent(EventId::InnerStep, counter++);
    resets.fsm.InsertEvent(EventId::InnerStep, counter++);
    CHECK(resets.CurrentStage() == Stage{"One", C, 3});

    resets.fsm.InsertEvent(EventId::OuterStep, counter++);
    CHECK(resets.fsm.GetCurrentState()->Id() == "Two");


    resets.fsm.InsertEvent(EventId::InnerStep, counter++);
    resets.fsm.InsertEvent(EventId::OuterStep, counter++);
    resets.fsm.InsertEvent(EventId::InnerNext);

    CHECK_THAT(resets.stages, Catch::Matchers::Equals(std::vector{
        Stage{"One", A, 1},
        Stage{"One", B, 2},
        Stage{"One", C, 3},
        Stage{"One", A, -1},
        Stage{"Two", A, 4},
        Stage{"Two", B, 5},
        Stage{"Two", A, -1},
        Stage{"Three", A, 6},
        Stage{"Three", A, -1},
        Stage{"One", B, 0xFACE}
    }));

    resets.throw_on_reset = true;

    using Catch::Matchers::Equals;
    CHECK_THROWS_WITH(
        resets.fsm.InsertEvent(EventId::OuterStep, counter++),
        Equals("Exception during reset")
    );

    std::vector<const State_t*> failed_states{};
    std::ranges::copy(resets.fsm.GetAbominableStates() | std::views::transform([](const auto& s) {
        return &s;
    }), std::back_inserter(failed_states));

    CHECK(resets.fsm.GetCurrentState() == nullptr);
    REQUIRE(failed_states.size() == 1);
    CHECK(failed_states[0]->IsAbominable());
    CHECK(failed_states[0]->Id() == "One");
}

TEST_CASE("Resettable States with Remote Transitions", "[advanced][resets][remote][exceptions]") {
    using enum StageId;

    Resets ra{};
    Resets rb{};
    Resets rc{};

    for (std::string_view state : {"One", "Two", "Three"}) {
        ra.fsm.AddRemoteTransition(state, EventId::Jump, rb.fsm, "One"sv);
        rb.fsm.AddRemoteTransition(state, EventId::Jump, rc.fsm, "One"sv);
        rc.fsm.AddRemoteTransition(state, EventId::Jump, ra.fsm, "One"sv);
    }

    int counter = 2;
    ra.fsm.InsertEvent(EventId::InnerStep, counter++);
    ra.fsm.InsertEvent(EventId::InnerStep, counter++);
    ra.fsm.InsertEvent(EventId::Jump, counter++);

    rb.fsm.InsertEvent(EventId::InnerStep, counter++);
    rc.fsm.InsertEvent(EventId::InnerStep, counter++);
    rb.fsm.InsertEvent(EventId::Jump, counter++);
    rc.fsm.InsertEvent(EventId::InnerStep, counter++);
    rc.fsm.InsertEvent(EventId::Jump, counter++);
    
    rb.throw_after_jump = true;

    CHECK_THROWS_WITH(
        ra.fsm.InsertEvent(EventId::Jump, counter++),
        Catch::Matchers::Equals("Jump throw")
    );

    CHECK(ra.fsm.GetAbominableStates().empty());
    CHECK(rc.fsm.GetAbominableStates().empty());
    CHECK(ra.fsm.GetCurrentState()->Id() == "One");
    CHECK(rc.fsm.GetCurrentState()->Id() == "One");

    std::vector<const State_t*> failed_states{};
    std::ranges::copy(rb.fsm.GetAbominableStates() | std::views::transform([](const auto& s) {
        return &s;
    }), std::back_inserter(failed_states));

    CHECK(rb.fsm.GetCurrentState() == nullptr);
    REQUIRE(failed_states.size() == 1);
    CHECK(failed_states[0]->IsAbominable());
    CHECK(failed_states[0]->Id() == "One");
}

TEST_CASE("Resettable States Resend Event", "[advanced][resets][exceptions]") {
    using enum StageId;

    Resets resets{};
    int counter = 2;

    resets.throw_on_reset = true;
    resets.fsm.InsertEvent(EventId::InnerStep, counter++);
    
    using Catch::Matchers::Equals;
    CHECK_THROWS_WITH(
        resets.fsm.InsertEvent(EventId::OuterStep, counter++),
        Equals("Exception during reset")
    );

    CHECK(resets.fsm.GetCurrentState() == nullptr);

    resets.fsm.ResendStoredEvent();

    REQUIRE(resets.fsm.GetCurrentState() != nullptr);
    CHECK(resets.fsm.GetCurrentState()->Id() == "Two");

    CHECK_THAT(resets.stages, Catch::Matchers::Equals(std::vector{
        Stage{"One", A, 1},
        Stage{"One", B, 2},
        Stage{"Two", A, 3}
    }));
}

TEST_CASE("Resettable States Remote Transitions Do Not Reset Local", "[advanced][resets]") {
    using enum StageId;

    Resets ra{};
    Resets rb{};

    ra.fsm.AddRemoteTransition("One"sv, EventId::Jump, rb.fsm, "Three"sv);

    int counter = 2;

    ra.throw_on_reset = true;
    ra.fsm.InsertEvent(EventId::InnerStep, counter++);

    SECTION("Remote reset without exception") {
        CHECK_NOTHROW(ra.fsm.InsertEvent(EventId::Jump, counter++));

        REQUIRE(ra.fsm.GetCurrentState() != nullptr);
        CHECK(ra.fsm.GetCurrentState()->Id() == "One");

        REQUIRE(rb.fsm.GetCurrentState() != nullptr);
        CHECK(rb.fsm.GetCurrentState()->Id() == "Three");

        CHECK_THAT(ra.stages, Catch::Matchers::Equals(std::vector{
            Stage{"One", A, 1},
            Stage{"One", B, 2},
        }));

        CHECK_THAT(rb.stages, Catch::Matchers::Equals(std::vector{
            Stage{"One", A, 1},
            Stage{"One", A, -1},
            Stage{"Three", A, 3},
        }));
    }

    SECTION("Remote reset with exception, resend event on remote") {
        rb.throw_on_reset = true;

        using Catch::Matchers::Equals;
        CHECK_THROWS_WITH(
            ra.fsm.InsertEvent(EventId::Jump, counter++),
            Equals("Exception during reset")
        );

        // RA wasn't affected
        REQUIRE(ra.fsm.GetCurrentState() != nullptr);
        CHECK(ra.fsm.GetCurrentState()->Id() == "One");

        // RB threw exception and became abominable
        CHECK(rb.fsm.GetCurrentState() == nullptr);

        std::vector<const State_t*> failed_states{};
        std::ranges::copy(rb.fsm.GetAbominableStates() | std::views::transform([](const auto& s) {
            return &s;
        }), std::back_inserter(failed_states));

        REQUIRE(failed_states.size() == 1);
        CHECK(failed_states[0]->IsAbominable());
        CHECK(failed_states[0]->Id() == "One");

        rb.fsm.ResendStoredEvent();

        REQUIRE(rb.fsm.GetCurrentState() != nullptr);
        CHECK(rb.fsm.GetCurrentState()->Id() == "Three");

        CHECK_THAT(ra.stages, Catch::Matchers::Equals(std::vector{
            Stage{"One", A, 1},
            Stage{"One", B, 2},
        }));

        // RB threw exception during reset so it shouldn't have the -1 data
        CHECK_THAT(rb.stages, Catch::Matchers::Equals(std::vector{
            Stage{"One", A, 1},
            Stage{"Three", A, 3},
        }));
    }
}

namespace {
    enum class ActionType {
        Received,
        ReceivedReset,
        Ignored,
        IgnoredReset,
    };

    struct Action {
        StateId state{};
        ActionType action{};
        int event_data{};

        friend std::ostream& operator<<(std::ostream& out, const Action& a) {
            auto action_str = [=] {
                switch (a.action) {
                case ActionType::Received: return "Received";
                case ActionType::ReceivedReset: return "ReceivedReset";
                case ActionType::Ignored: return "Ignored";
                case ActionType::IgnoredReset: return "IgnoredReset";
                }
            }();

            out << std::format("{{S: {}; A: {}; E: {}}}", a.state, action_str, a.event_data);
            return out;
        }

        bool operator==(const Action& rhs) const = default;
    };

    struct OtherResets {
        FSM_t fsm{};
        std::vector<Action> actions{};

        OtherResets() {
            fsm
                .AddState(StateFunc(fsm, "One"))
                .AddState(StateFunc(fsm, "Two"))
                .AddTransition("One", EventId::InnerStep, "One")
                .AddTransition("Two", EventId::InnerStep, "Two")
                .AddTransition("One", EventId::Jump, "Two")
                .AddTransition("Two", EventId::Jump, "One");

            fsm.SetCurrentState("One");
        }

        State_t StateFunc(FSM_t& fsm, StateId id) {
            Event_t event{};

            while (true) {
                auto should_reset = co_await fsm.ReceiveEventResettable(event);

                if (!should_reset) {
                    actions.push_back({id, ActionType::Received, event.Get<int>()});
                } else {
                    actions.push_back({id, ActionType::ReceivedReset, -1});
                }

                should_reset = co_await fsm.IgnoreEventResettable();

                if (!should_reset) {
                    actions.push_back({id, ActionType::Ignored, -1});
                } else {
                    actions.push_back({id, ActionType::IgnoredReset, -1});
                }
            }
        }
    };
}

TEST_CASE("Other Resettable Awaitables", "[advanced][resets][rebinding][removing]") {
    OtherResets resets{};

    int counter = 1;
    resets.fsm.InsertEvent(EventId::InnerStep, counter++);
    resets.fsm.InsertEvent(EventId::InnerStep, counter++);
    resets.fsm.InsertEvent(EventId::Jump, counter++);
    resets.fsm.InsertEvent(EventId::Jump, counter++);

    // Test removing states with resets involved
    resets.fsm.RemoveState("One"); // Current state at this point
    CHECK(resets.fsm.GetCurrentState() == nullptr);

    resets.fsm.AddState(resets.StateFunc(resets.fsm, "One"));
    resets.fsm.SetCurrentState("One");

    // This should move it to Ignored then ReceivedReset
    resets.fsm.InsertEvent(EventId::InnerStep, counter++);
    
    resets.fsm.RemoveState("One"); // Current state at this point
    CHECK(resets.fsm.GetCurrentState() == nullptr);

    resets.fsm.AddState(resets.StateFunc(resets.fsm, "One"));
    resets.fsm.SetCurrentState("One");

    resets.fsm.InsertEvent(EventId::InnerStep, counter++);
    resets.fsm.InsertEvent(EventId::InnerStep, counter++);

    // Removing non-current state doesn't affect current state
    resets.fsm.RemoveState("Two");
    REQUIRE(resets.fsm.GetCurrentState() != nullptr);
    CHECK(resets.fsm.GetCurrentState()->Id() == "One");

    resets.fsm.AddState(resets.StateFunc(resets.fsm, "Two"));

    resets.fsm.InsertEvent(EventId::Jump, counter++);

    CHECK_THAT(resets.actions, Catch::Matchers::Equals(std::vector{
        Action{"One", ActionType::Received, 1},
        Action{"One", ActionType::Ignored, -1},
        Action{"One", ActionType::ReceivedReset, -1},
        Action{"Two", ActionType::Received, 3},
        Action{"Two", ActionType::IgnoredReset, -1},
        Action{"One", ActionType::Ignored, -1},
        // One was removed and readded here
        Action{"One", ActionType::Received, 5},
        // One was removed and readded here
        Action{"One", ActionType::Received, 6},
        Action{"One", ActionType::Ignored, -1},
        // Two was removed and readded here
        Action{"One", ActionType::ReceivedReset, -1},
        Action{"Two", ActionType::Received, 8},

    }));
}