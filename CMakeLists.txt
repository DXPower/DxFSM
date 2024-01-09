cmake_minimum_required(VERSION 3.20)

project(dxfsm
	VERSION 1.0
	DESCRIPTION "dxfsm"
	LANGUAGES CXX
)

add_library(dxfsm INTERFACE)
target_include_directories(dxfsm INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)

enable_testing()
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/test)

find_package(Doxygen)
doxygen_add_docs(dxfsm_docs ${CMAKE_CURRENT_SOURCE_DIR}/include/dxfsm/dxfsm.hpp)

# # Mark as system include to disable warnings
# set_target_properties(dxfsm PROPERTIES 
#     INTERFACE_SYSTEM_INCLUDE_DIRECTORIES $<TARGET_PROPERTY:dxfsm,INTERFACE_INCLUDE_DIRECTORIES>
# )