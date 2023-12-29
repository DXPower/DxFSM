#include <dxfsm/dxfsm.hpp>

#include <format>
#include <iostream>

using dxfsm::Event;

namespace {
    struct TestData {
        int i = 0xDEADBEEF;
        std::string s = "Hello, world!";

        TestData(int i, std::string s) : i(i), s(s) { }

        bool operator==(const TestData& other) const = default;
    };

    std::ostream& operator<<(std::ostream& out, const TestData& t) {
        out << std::format("TestData|i = {:X}, s = \"{}\"|", t.i, t.s);
        return out;
    }

    TestData MakeData() { return TestData{0x100DFACE, "Applesauce"}; };


    enum class EventId {
        EA = 0x123,
        EB = 0x456,
        EC = 0x789
    };

    using Event_t = Event<EventId>;
}

#include <catch2/catch_test_macros.hpp>

using enum EventId;

TEST_CASE("Event Construction", "[events]") {
    SECTION("Default") {
        Event_t e;

        CHECK(e.Empty());
        CHECK_FALSE(e.HasData());
        CHECK(e.GetMaybe<TestData>() == nullptr);
        CHECK(e.Capacity() == 0);
        CHECK(e != EA);
        CHECK(e != EB);
        CHECK(e != EC);

        CHECK_THROWS(e.GetId());
        CHECK_THROWS(e.Get<TestData>());
    }

    SECTION("With id") {
        Event_t e(EB);

        CHECK_FALSE(e.Empty());
        CHECK_FALSE(e.HasData());
        CHECK(e.GetMaybe<TestData>() == nullptr);
        CHECK(e.Capacity() == 0);
        CHECK(e != EA);
        CHECK(e == EB);
        CHECK(e != EC);
        CHECK(e.GetId() == EB);

        CHECK_THROWS(e.Get<TestData>());
    }

    SECTION("With id and object") {
        Event_t e(EC, MakeData());

        CHECK_FALSE(e.Empty());
        CHECK(e.HasData());
        CHECK(e.Get<TestData>() == MakeData());
        REQUIRE(e.GetMaybe<TestData>() != nullptr);
        CHECK(*e.GetMaybe<TestData>() == MakeData());
        CHECK(e.GetMaybe<int>() == nullptr);
        CHECK(e.GetUnchecked<TestData>() == MakeData());
        CHECK(e.Capacity() == sizeof(TestData));
        CHECK(e != EA);
        CHECK(e != EB);
        CHECK(e == EC);
        CHECK(e.GetId() == EC);

        CHECK_THROWS(e.Get<int>());
    }

    SECTION("With id and emplacement args") {
        Event_t e(EA, std::in_place_type<TestData>, 0x100DFACE, "Applesauce");

        CHECK_FALSE(e.Empty());
        CHECK(e.HasData());
        CHECK(e.Get<TestData>() == MakeData());
        REQUIRE(e.GetMaybe<TestData>() != nullptr);
        CHECK(*e.GetMaybe<TestData>() == MakeData());
        CHECK(e.GetMaybe<int>() == nullptr);
        CHECK(e.GetUnchecked<TestData>() == MakeData());
        CHECK(e.Capacity() == sizeof(TestData));
        CHECK(e == EA);
        CHECK(e != EB);
        CHECK(e != EC);
        CHECK(e.GetId() == EA);

        CHECK_THROWS(e.Get<int>());
    }
}

namespace {
    struct TestDataTicker : TestData {
        int* destroy_count{};

        TestDataTicker(int i, std::string s, int* destroy_count)
            : TestData(i, std::move(s)),
              destroy_count(destroy_count)
        { }

        ~TestDataTicker() {
            (*destroy_count)++;
        }
    };
}

TEST_CASE("Event storage", "[events]") {
    int destroy_counter{};

    auto e_ptr = std::make_unique<Event_t>(
        EA,
        std::in_place_type<TestDataTicker>,
        0x100DFACE,
        "Applesauce",
        &destroy_counter
    );
    Event_t& e = *e_ptr;

    SECTION("Storage destroys prior data") {
        SECTION("No data") {
            SECTION("Same id") {
                e.Store(EA);
                CHECK(e == EA);
            }

            SECTION("Different id") {
                e.Store(EB);
                CHECK(e == EB);
            }

            CHECK_FALSE(e.Empty());
            CHECK_FALSE(e.HasData());
            CHECK(e.Capacity() == sizeof(TestDataTicker)); // Storage was not deallocated
            CHECK(destroy_counter == 1); // Prior data was destroyed
        }

        SECTION("With data") {
            int extra_destroy_count{};

            SECTION("Same type") {
                SECTION("Same id") {
                    SECTION("Store()") {
                        e.Store(EA, TestDataTicker(0x12345678, "Baguette", &destroy_counter));
                        extra_destroy_count++; // Destruction of TestDataTicker temporary above
                    }

                    SECTION("Emplace()") {
                        e.Emplace<TestDataTicker>(EA, 0x12345678, "Baguette", &destroy_counter);
                    }

                    CHECK(e == EA);
                }

                SECTION("Different id") {
                    SECTION("Store()") {
                        e.Store(EB, TestDataTicker(0x12345678, "Baguette", &destroy_counter));
                        extra_destroy_count++; // Destruction of TestDataTicker temporary above
                    }

                    SECTION("Emplace()") {
                        e.Emplace<TestDataTicker>(EB, 0x12345678, "Baguette", &destroy_counter);
                    }

                    CHECK(e == EB);
                }

                CHECK(e.Get<TestDataTicker>() == TestData(0x12345678, "Baguette"));
                CHECK(e.Capacity() == sizeof(TestDataTicker));
            }

            SECTION("Different type") {
                SECTION("Smaller type") {
                    e.Store(EA, 0x1337ABCD);

                    CHECK(e == EA);
                    CHECK(e.Get<int>() == 0x1337ABCD);
                    CHECK(e.Capacity() == sizeof(TestDataTicker));
                }

                SECTION("Bigger type") {
                    using Big = std::tuple<std::string, std::string>;
                    static_assert(sizeof(Big) > sizeof(TestDataTicker));

                    e.Emplace<Big>(EC, "Santa Claus", "C++ Coroutines rock!");

                    CHECK(e == EC);
                    CHECK(e.Get<Big>() == Big{"Santa Claus", "C++ Coroutines rock!"});
                    CHECK(e.Capacity() == sizeof(Big));
                }
            }

            CHECK((destroy_counter - extra_destroy_count) == 1);
        }
    }

    SECTION("Destroying stored data") {
        SECTION("Member functions") {
            SECTION("Clear") {
                auto original_capacity = e.Capacity();
                e.Clear();
                CHECK(e.Capacity() == original_capacity);
            }

            SECTION("ReleaseStorage") {
                e.ReleaseStorage();
                CHECK(e.Capacity() == 0);
            }

            CHECK(e.Empty());
            CHECK_FALSE(e.HasData());
            CHECK_THROWS(e.GetId());
            CHECK_THROWS(e.Get<TestDataTicker>());
        }

        SECTION("Destructor") {
            e_ptr.reset();
        }

        CHECK(destroy_counter == 1);
    }
}