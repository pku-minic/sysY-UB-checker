cmake_minimum_required(VERSION 3.10.0)
project(koopa-UBcheck VERSION 0.1.0)
include(GNUInstallDirs)

#set(CMAKE_INSTALL_RPATH $ORIGIN/../lib)
#add_library(checklib lib.cpp)
add_executable(${PROJECT_NAME} main.cpp)
find_package(Clang REQUIRED CONFIG)
#target_link_libraries(${PROJECT_NAME} PRIVATE clang-cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE clangTooling)
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
if(NOT LLVM_ENABLE_RTTI)
  target_compile_options(${PROJECT_NAME} PRIVATE -fno-rtti)
endif()
#[[
target_include_directories(checklib PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
    )
]]
# from https://cmake.org/cmake/help/latest/command/target_include_directories.html
# INSTALL_INTERFACE expression are interpreted as relative to the installation prefix
target_include_directories(${PROJECT_NAME} PRIVATE
    #$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    #$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
    ${CLANG_INCLUDE_DIRS}
    )    

install(TARGETS ${PROJECT_NAME}
    # EXPORT checklib-targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    # INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

#[=[
install(EXPORT checklib-targets
    FILE checklib-targets.cmake
    NAMESPACE checklib::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/
)
]=]