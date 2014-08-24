# configure clang, with memory sanitizer
./configure --with-boost-include=/home/peter/boost_1_55_0 --with-boost-lib=/home/peter/boost_1_55_0/stage/lib CXX="clang++" CXXFLAGS="-O2 -g -Wall -std=c++11 -fsanitize=memory -Qunused-arguments" LIBS="-lntl -lgmp -lm -lboost_timer -lboost_chrono -lboost_system"
