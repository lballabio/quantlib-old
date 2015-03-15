# configure gcc, no open mp support
./configure --with-boost-include=/home/peter/boost_1_57_0 --with-boost-lib=/home/peter/boost_1_57_0/stage/lib --enable-error-lines CXXFLAGS="-m64 -O3 -g -Wall -Wno-unused-local-typedefs -Wno-unknown-pragmas -std=c++11" LIBS="-lntl -lgmp -lm -lboost_timer -lboost_chrono -lboost_system"
