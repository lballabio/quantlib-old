# configure clang, with leak sanitizer (no slow down implied)
./configure --with-boost-include=/home/peter/boost_1_55_0 --with-boost-lib=/home/peter/boost_1_55_0/stage/lib CXX=clang++ CXXFLAGS="-m64 -O3 -g -Wall -std=c++11 -fsanitize=leak -Qunused-arguments" LIBS="-lntl -lgmp -lm -lboost_timer -lboost_chrono -lboost_system"
