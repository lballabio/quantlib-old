# configure icc (with open MP enabled)
./configure --with-boost-include=/home/peter/boost_1_57_0 --with-boost-lib=/home/peter/boost_1_57_0/stage/lib CXX="icpc" CXXFLAGS="-g -m64 -O3 -qopenmp -diag-disable=858,3175" LIBS="-lntl -lgmp -lm -lboost_timer -lboost_chrono -lboost_system"
