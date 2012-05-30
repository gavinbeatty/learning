
project(bacongraph)

cmake_minimum_required(VERSION 2.4.0)

set(bacongraph_HDRS src)
set(bacongraph_LIBS )

include_directories( ${CMAKE_CURRENT_BINARY_DIR} ${bacongraph_HDRS} )

set(bacongraph_SRCS
src/main.cpp
)

add_executable(bacongraph ${bacongraph_SRCS} )
# target_link_libraries(bacongraph ${bacongraph_LIBS} )

enable_testing()

add_test(small bacongraph "${CMAKE_SOURCE_DIR}/input/small.txt")
add_test(medium bacongraph "${CMAKE_SOURCE_DIR}/input/medium.txt")
add_test(large bacongraph "${CMAKE_SOURCE_DIR}/input/large.txt")

