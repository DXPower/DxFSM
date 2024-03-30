#include <dxfsm/dxfsm.hpp>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <catch2/matchers/catch_matchers_vector.hpp>

namespace {
    enum class FromStates {
        FM, FT
    };

    enum class ToStates {
        TM, TT, TU
    };

    enum class FromEvents {
        FE, FR
    };

    enum class ToEvents {
        TE, TR
    };

    struct From {
        using FSM = dxfsm::FSM<FromStates, FromEvents>;
        using State = FSM::State_t;
        using Event = FSM::Event_t;

        FSM fsm{};
        std::vector<FromStates> states{};
        bool throw_on_resume{};

        From() {
            // Intentionally add transition before states to test rebinding on AddState
            fsm
                .Name("FromFSM")
                .AddTransition(FromStates::FM, FromEvents::FE, FromStates::FT);

            Main(fsm, FromStates::FM);
            Main(fsm, FromStates::FT);

            fsm.SetCurrentState(FromStates::FM);
        }

        void ReaddTarget() {
            Main(fsm, FromStates::FT);
            fsm.SetCurrentState(FromStates::FM);
        }

        State Main(FSM& fsm, FromStates id) {
            while (true) {
                co_await fsm.IgnoreEvent();

                if (throw_on_resume) {
                    throw std::runtime_error("Throw!");
                }

                states.push_back(id);
            }
        }
    };

    struct To {
        using FSM = dxfsm::FSM<ToStates, ToEvents>;
        using State = FSM::State_t;
        using Event = FSM::Event_t;

        FSM fsm{};
        std::vector<ToStates> states{};
        bool throw_on_resume{};

        To() {
            // Intentionally add transition before states to test rebinding on AddState
            fsm
                .Name("ToFSM")
                .AddTransition(ToStates::TM, ToEvents::TE, ToStates::TT);

                Main(fsm, ToStates::TM);
                Main(fsm, ToStates::TT);

            fsm.SetCurrentState(ToStates::TM);
        }

        void ReaddTarget() {
            Main(fsm, ToStates::TT);
            fsm.SetCurrentState(ToStates::TM);
        }

        State Main(FSM& fsm, ToStates id) {
            while (true) {
                co_await fsm.IgnoreEvent();

                if (throw_on_resume) {
                    throw std::runtime_error("Throw!");
                }

                states.push_back(id);
            }
        }
    };
}   

TEST_CASE("Dangling local transition", "[exceptions][advanced][rebinding][state_api][transition_api]") {
    From from{};
    from.throw_on_resume = true;

    using Catch::Matchers::Equals;
    CHECK_THROWS_WITH(from.fsm.InsertEvent(FromEvents::FE), Equals("Throw!"));

    REQUIRE(from.fsm.GetState(FromStates::FT).has_value());
    CHECK(from.fsm.GetState(FromStates::FT)->IsAbominable());

    bool remove_abominable_states = GENERATE(false, true);
    if (remove_abominable_states) {
        from.fsm.RemoveAbominableStates();
    }

    auto abominable_state = from.fsm.GetState(FromStates::FT);

    if (remove_abominable_states) {
        CHECK_FALSE(abominable_state.has_value());
    } else {
        REQUIRE(abominable_state.has_value());
        CHECK(abominable_state->IsAbominable());
    }

    SECTION("Readd state") {
        from.throw_on_resume = false;
        from.ReaddTarget();

        CHECK_NOTHROW(from.fsm.InsertEvent(FromEvents::FE));

        CHECK_THAT(from.states, Equals(std::vector{FromStates::FT}));

        if (remove_abominable_states) {
            abominable_state = from.fsm.GetState(FromStates::FT);
        }

        REQUIRE(abominable_state.has_value());
        CHECK_FALSE(abominable_state->IsAbominable());
        CHECK(abominable_state->Id() == FromStates::FT);
    }

    SECTION("Delete dangling transition") {
        CHECK(from.fsm.RemoveDanglingTransitions() == 1);
    }
}

TEST_CASE("Dangling remote transition", "[exceptions][advanced][remote][rebinding][removing][state_api][transition_api]") {
    using Catch::Matchers::Equals;

    From from{};
    To to{};
    to.throw_on_resume = true;

    from.fsm.AddRemoteTransition(FromStates::FM, FromEvents::FR, to.fsm, ToStates::TT, ToEvents::TR);

    enum class RemoveStrategy { Exception, Remove, ExceptionRemove };
    auto strat = GENERATE(RemoveStrategy::Exception, RemoveStrategy::ExceptionRemove, RemoveStrategy::Remove);
    bool remote_state_removed = false;

    switch (strat) {
    case RemoveStrategy::Exception:
    case RemoveStrategy::ExceptionRemove:
        CHECK_THROWS_WITH(from.fsm.InsertEvent(FromEvents::FR), Equals("Throw!"));

        REQUIRE(to.fsm.GetState(ToStates::TT).has_value());
        CHECK(to.fsm.GetState(ToStates::TT)->IsAbominable());

        if (strat == RemoveStrategy::ExceptionRemove) {
            to.fsm.RemoveAbominableStates();
            remote_state_removed = true;
        }
        break;
    case RemoveStrategy::Remove:
        to.fsm.RemoveState(ToStates::TT);
        remote_state_removed = true;
        break;
    }

    CHECK(from.fsm.GetTransition(FromStates::FM, FromEvents::FR)->IsDangling());
    
    auto removed_state = to.fsm.GetState(ToStates::TT);

    if (remote_state_removed) {
        CHECK_FALSE(removed_state.has_value());
    } else {
        REQUIRE(removed_state.has_value());
        CHECK(removed_state->IsAbominable());
    }

    SECTION("Readd state and remote rebind") {
        to.throw_on_resume = false;
        to.ReaddTarget();
        from.fsm.RebindRemoteTransitions();

        if (remote_state_removed) {
            removed_state = to.fsm.GetState(ToStates::TT);
        }

        REQUIRE(removed_state.has_value());
        CHECK_FALSE(removed_state->IsAbominable());
        CHECK(removed_state->Id() == ToStates::TT);
        CHECK_FALSE(from.fsm.GetTransition(FromStates::FM, FromEvents::FR)->IsDangling());

        CHECK_NOTHROW(from.fsm.InsertEvent(FromEvents::FR));

        CHECK_THAT(from.states, Equals(std::vector<FromStates>{}));
        CHECK_THAT(to.states, Equals(std::vector<ToStates>{ToStates::TT}));
    }

    SECTION("Delete dangling transition") {
        CHECK(from.fsm.RemoveDanglingTransitions() == 1);
        CHECK(to.fsm.RemoveDanglingTransitions() == 1);
    }

    SECTION("Add remote transition before state") {
        from.fsm.AddRemoteTransition(FromStates::FM, FromEvents::FR, to.fsm, ToStates::TU, ToEvents::TR);
    }
}