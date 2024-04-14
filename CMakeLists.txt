cmake_minimum_required(VERSION 3.20)

project(dxfsm
	VERSION 1.0
	DESCRIPTION "dxfsm"
	LANGUAGES CXX
)

add_library(dxfsm INTERFACE)
target_include_directories(dxfsm INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/examples)

enable_testing()
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/test)
