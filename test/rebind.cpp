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
                .AddTransition(FromStates::FM, FromEvents::FE, FromStates::FT)
                .AddState(Main(fsm, FromStates::FM))
                .AddState(Main(fsm, FromStates::FT));

            fsm.SetCurrentState(FromStates::FM);
        }

        void ReaddTarget() {
            fsm.AddState(Main(fsm, FromStates::FT));
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
                .AddTransition(ToStates::TM, ToEvents::TE, ToStates::TT)
                .AddState(Main(fsm, ToStates::TM))
                .AddState(Main(fsm, ToStates::TT));

            fsm.SetCurrentState(ToStates::TM);
        }

        void ReaddTarget() {
            fsm.AddState(Main(fsm, ToStates::TT));
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

TEST_CASE("Dangling local transition", "[exceptions][advanced][rebinding]") {
    From from{};
    from.throw_on_resume = true;

    using Catch::Matchers::Equals;
    CHECK_THROWS_WITH(from.fsm.InsertEvent(FromEvents::FE), Equals("Throw!"));

    REQUIRE(from.fsm.FindState(FromStates::FT) != nullptr);
    CHECK(from.fsm.FindState(FromStates::FT)->IsAbominable());

    from.fsm.RemoveAbominableStates();
    
    SECTION("Readd state") {
        from.throw_on_resume = false;
        from.ReaddTarget();

        CHECK_NOTHROW(from.fsm.InsertEvent(FromEvents::FE));

        CHECK_THAT(from.states, Equals(std::vector{FromStates::FT}));
    }

    SECTION("Delete dangling transition") {
        CHECK(from.fsm.RemoveDanglingTransitions() == 1);
    }
}

TEST_CASE("Dangling remote transition", "[exceptions][advanced][remote][rebinding][removing]") {
    using Catch::Matchers::Equals;

    From from{};
    To to{};
    to.throw_on_resume = true;

    from.fsm.AddRemoteTransition(FromStates::FM, FromEvents::FR, to.fsm, ToStates::TT, ToEvents::TR);

    enum class RemoveStrategy { Exception, Remove };
    auto strat = GENERATE(RemoveStrategy::Exception, RemoveStrategy::Remove);

    switch (strat) {
    case RemoveStrategy::Exception:
        CHECK_THROWS_WITH(from.fsm.InsertEvent(FromEvents::FR), Equals("Throw!"));

        REQUIRE(to.fsm.FindState(ToStates::TT) != nullptr);
        CHECK(to.fsm.FindState(ToStates::TT)->IsAbominable());

        to.fsm.RemoveAbominableStates();
        break;
    case RemoveStrategy::Remove:
        to.fsm.RemoveState(ToStates::TT);
        break;
    }
    
    SECTION("Readd state and remote rebind") {
        to.throw_on_resume = false;
        to.ReaddTarget();
        from.fsm.RebindRemoteTransitions();

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