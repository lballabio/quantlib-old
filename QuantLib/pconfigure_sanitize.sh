# use ASAN_OPTIONS="detect_leaks=1" to activate memory leak detection
./configure --with-boost-include=/home/peter/boost_1_55_0 --with-boost-lib=/home/peter/boost_1_55_0/stage/lib CC="clang" CXX="clang++" CXXFLAGS="-O2 -g -Wall -std=c++11 -fsanitize=address,undefined,integer"
