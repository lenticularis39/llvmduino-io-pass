cmake_minimum_required(VERSION 3.11)

include_directories(BEFORE "/home/tglozar/dev/llvmduino/llvm-install/usr/local/include")

add_library(OptimizeIOPass MODULE
    OptimizeIOPass.cpp
)

set_target_properties(OptimizeIOPass PROPERTIES
    COMPILE_FLAGS "-fno-rtti"
)


