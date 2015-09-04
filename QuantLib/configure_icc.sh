# configure icc (with open MP enabled)
./configure --with-boost-include=/home/peter/boost_1_57_0 --with-boost-lib=/home/peter/boost_1_57_0/stage/lib CXX="icpc" CXXFLAGS="-O3 -openmp -diag-disable=858" LIBS="-lntl -lgmp -lm -lboost_timer -lboost_chrono -lboost_system"
