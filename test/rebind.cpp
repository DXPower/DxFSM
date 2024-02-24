#include <dxfsm/dxfsm.hpp>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_vector.hpp>

namespace {
    enum class FromStates {
        FM, FT
    };

    enum class ToStates {
        TA
    };

    enum class FromEvents {
        FE
    };

    enum class ToEvents {
        TE
    };

    struct From {
        using FSM = dxfsm::FSM<FromStates, FromEvents>;
        using State = FSM::State_t;
        using Event = FSM::Event_t;

        FSM fsm{};
        std::vector<FromStates> states{};

        From() {
            fsm
                .Name("FromFSM")
                .AddState(Main());
        }

        State Main(FSM& fsm, FromStates id) {
            while (true) {
                co_await fsm.IgnoreEvent();
                states.push_back(id);
            }
        }
    };
}