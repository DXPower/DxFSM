cmake_minimum_required(VERSION 3.20)

project(dxfsm
	VERSION 1.0
	DESCRIPTION "dxfsm"
	LANGUAGES CXX
)

add_library(dxfsm INTERFACE)
target_include_directories(dxfsm INTERFACE 
	$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
	$<INSTALL_INTERFACE:include>
)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/examples)

enable_testing()
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/test)

include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${PROJECT_BINARY_DIR}/dxfsmConfigVersion.cmake"
    VERSION 1.0
    COMPATIBILITY AnyNewerVersion
)

install(TARGETS dxfsm
    EXPORT dxfsmTargets
    PUBLIC_HEADER DESTINATION include COMPONENT Development
)

include(CMakePackageConfigHelpers)
configure_package_config_file(
    "${PROJECT_SOURCE_DIR}/dxfsmConfig.cmake.in"
    "${PROJECT_BINARY_DIR}/dxfsmConfig.cmake"
    INSTALL_DESTINATION lib/cmake/dxfsm
)

install(EXPORT dxfsmTargets DESTINATION lib/cmake/dxfsm)
install(FILES "${PROJECT_BINARY_DIR}/dxfsmConfigVersion.cmake"
              "${PROJECT_BINARY_DIR}/dxfsmConfig.cmake"
        DESTINATION lib/cmake/dxfsm)

install(DIRECTORY ${PROJECT_SOURCE_DIR}/include/ DESTINATION include)
