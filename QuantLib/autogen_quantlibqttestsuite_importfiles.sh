#!/bin/sh

echo "Print cpp/hpp/c/h file list for QuantLibQt-test.pro file..."

cd ~/Documents/QuantLib/quantlib
[ -f "qltestcpps.txt" ] && rm qltestcpps.txt
[ -f "qltesthpps.txt" ] && rm qltesthpps.txt

cd ~/Documents/QuantLib/quantlib/QuantLib/test-suite

find . -name '*.cpp' >> ../../cpp.txt
find . -name '*.hpp' >> ../../hpp.txt
cd ../../
sort cpp.txt > ccpps.txt
sort hpp.txt > hhpps.txt
#sed -e "s@^./@@g" ccpps.txt >> qltestcpps.txt
#sed -e "s@^./@@g" hhpps.txt >> qltesthpps.txt
sed -e "s@^./@test-suite/@g" -e "s@.cpp@.cpp \\\@g" ccpps.txt >> qltestcpps.txt
sed -e "s@^./@test-suite/@g" -e "s@.hpp@.hpp \\\@g" hhpps.txt >> qltesthpps.txt
#sed -e "s/¥.cpp$/.cpp \\/g" ccpps.txt >> qltestcpps.txt
#sed -e "s/¥.cpp$/.cpp \\/g" hhpps.txt >> qltesthpps.txt
#cat qltestcpps.txt >> c1.txt
#cat qltesthpps.txt >> h1.txt
rm cpp.txt
rm hpp.txt
rm ccpps.txt
rm hhpps.txt
#rm c1.txt
#rm h1.txt

echo "c/cpp/h/hpp test-suit file full name listup completed."
