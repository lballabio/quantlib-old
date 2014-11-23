# configure clang, with analyzer
./configure --with-boost-include=/home/peter/boost_1_57_0 --with-boost-lib=/home/peter/boost_1_57_0/stage/lib CXX=clang++ CXXFLAGS="--analyze -g -Wall -std=c++11 -Qunused-arguments" LIBS="-lntl -lgmp -lm -lboost_timer -lboost_chrono -lboost_system"
