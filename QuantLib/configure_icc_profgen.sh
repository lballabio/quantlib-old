# configure icc (no openmp, generate profile information for code coverage)
./configure --with-boost-include=/home/peter/boost_1_57_0 --with-boost-lib=/home/peter/boost_1_57_0/stage/lib CXX="icpc" CXXFLAGS="-prof-gen=srcpos -prof-dir=/home/peter/quantlib/QuantLib -g -m64 -O0 -diag-disable=858,3175" LIBS="-lntl -lgmp -lm -lboost_timer -lboost_chrono -lboost_system"
