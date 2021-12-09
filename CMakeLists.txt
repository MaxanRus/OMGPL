cmake_minimum_required(VERSION 3.10)

project("OMGL")

# set(CMAKE_CXX_FLAGS "-O3")
# set(CMAKE_CXX_FLAGS "-O0 --coverage -ftest-coverage -fprofile-arcs")

find_package(GTest REQUIRED)
find_package(Threads REQUIRED)

include_directories(
  "include"
  ${GTEST_INCLUDE_DIRS}
  )

set(CMAKE_CXX_STANDARD 17)

set(SOURCE_FILES
  src/preprocessor.cpp
  src/lexer.cpp
  src/syntax_tree.cpp
  )

set(TEST_FILES
  )

add_executable(OMGL src/main.cpp ${SOURCE_FILES})

# add_executable(Test tests/MainTest.cpp ${TEST_FILES} ${SOURCE_FILES})

# target_link_libraries(Test ${GTEST_LIBRARIES} Threads::Threads)
