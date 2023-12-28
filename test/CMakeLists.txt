find_package(Catch2 REQUIRED)

add_executable(dxfsm_tests)

target_sources(dxfsm_tests PRIVATE
    collatz.cpp
)

target_link_libraries(dxfsm_tests PRIVATE dxfsm Catch2::Catch2WithMain)

include(CTest)
include(Catch)
catch_discover_tests(dxfsm_tests
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)