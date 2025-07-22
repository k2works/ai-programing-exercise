cmake_minimum_required(VERSION 3.15)
project(FizzBuzzCpp VERSION 1.0.0 LANGUAGES CXX)

# C++17 standard required
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wpedantic")

# Coverage flags
option(ENABLE_COVERAGE "Enable coverage reporting" OFF)
if(ENABLE_COVERAGE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --coverage -fprofile-arcs -ftest-coverage")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} --coverage")
endif()

# Include directories
include_directories(src)

# Library for FizzBuzz logic
add_library(fizzbuzz_lib
    fizz_buzz.cpp
    src/domain/type/fizz_buzz_type.cpp
    src/domain/model/fizz_buzz_value.cpp
    src/domain/model/fizz_buzz_list.cpp
    src/application/fizz_buzz_command.cpp
)

# Main executable
add_executable(fizzbuzz
    main.cpp
)
target_link_libraries(fizzbuzz fizzbuzz_lib)

# Test configuration (using Google Test)
enable_testing()
find_package(GTest)
if(GTest_FOUND)
    add_executable(fizzbuzz_test
        test/learning_test.cpp
    )
    target_link_libraries(fizzbuzz_test fizzbuzz_lib GTest::GTest GTest::Main)
    add_test(NAME FizzBuzzTests COMMAND fizzbuzz_test)
endif()