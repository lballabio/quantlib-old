#!/bin/sh

echo "Print cpp/hpp/c/h file list for QuantLibQt.pro file..."

cd ~/Documents/QuantLib/quantlib
[ -f "qlccpps.txt" ] && rm qlccpps.txt
[ -f "qlhhpps.txt" ] && rm qlhhpps.txt


cd ~/Documents/QuantLib/quantlib/QuantLib/ql
find . -name '*.c' >> ../../c.txt
find . -name '*.h' >> ../../h.txt
find . -name '*.cpp' >> ../../cpp.txt
find . -name '*.hpp' >> ../../hpp.txt
cd ../../
sort cpp.txt > cpps.txt
sort hpp.txt > hpps.txt
cat c.txt cpps.txt > ccpps.txt
cat h.txt hpps.txt > hhpps.txt
sed -e "s@^.@ql@g" -e "s@.cpp@.cpp \\\@g" ccpps.txt >> qlccpps.txt
sed -e "s@^.@ql@g" -e "s@.hpp@.hpp \\\@g" hhpps.txt >> qlhhpps_.txt
sed '/all.hpp/d' qlhhpps_.txt > qlhhpps.txt
rm cpp.txt
rm hpp.txt
rm cpps.txt
rm hpps.txt
rm c.txt
rm h.txt
rm ccpps.txt
rm hhpps.txt
rm qlhhpps_.txt

echo "c/cpp/h/hpp file full name listup completed."
