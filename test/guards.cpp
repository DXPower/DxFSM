#include <dxfsm/dxfsm.hpp>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

namespace {
    struct Guard {
        bool fail{};
    };

    using EventId = std::string_view;
    using StateId = std::string_view;
    using FSM = dxfsm::FSM<StateId, EventId>;

    FSM::State_t From(FSM& fsm, StateId) {
        auto e = co_await fsm.ReceiveInitialEvent();

        while (true) {
            e.Store("Reject");
            co_await fsm.EmitAndReceive(e);

            e.Store("Pass");
            co_await fsm.EmitAndReceive(e);
        }
    }
    FSM::State_t FromResets(FSM& fsm, StateId) {
        auto e = co_await fsm.ReceiveInitialEvent();

        while (true) {
            e.Store("Reject");
            bool should_reset = co_await fsm.EmitAndReceiveResettable(e);

            if (should_reset)
                throw std::runtime_error("Should not have reset!");

            e.Store("Pass");
            should_reset = co_await fsm.EmitAndReceiveResettable(e);

            if (!should_reset)
                throw std::runtime_error("Should have reset!");

            co_await fsm.EmitAndReceive(e);
        }
    }

    FSM::State_t To(FSM& fsm, StateId) {
        while (true) {
            co_await fsm.IgnoreEvent();
        }
    }

    std::unique_ptr<FSM> MakeFsm(bool use_resets = false) {
        auto fsm = std::make_unique<FSM>();

        auto from = (use_resets ? FromResets : From)(*fsm, "From");
        auto to = To(*fsm, "To");

        fsm->AddTransition(from.Id(), "Pass", to.Id());
        fsm->AddTransition(from.Id(), "Reject", to.Id());
        fsm->GetTransition(from.Id(), "Reject")->Guard([](const FSM::Event_t& e) {
            return e.GetId() != "Reject";
        });

        fsm->SetCurrentState("From");
        return fsm;
    }
}

TEST_CASE("Guard conditions", "[guards][basic][resets]") {
    auto fsm = MakeFsm(GENERATE(false, true));

    SECTION("Internal events") {
        fsm->InsertEvent("None");
        REQUIRE(fsm->GetCurrentState()->Id() == "From");

        fsm->InsertEvent("None");
        CHECK(fsm->GetCurrentState()->Id() == "To");
    }

    SECTION("External events") {
        fsm->InsertEvent("Reject");
        REQUIRE(fsm->GetCurrentState()->Id() == "From");

        fsm->InsertEvent("Pass");
        REQUIRE(fsm->GetCurrentState()->Id() == "To");
    }
}