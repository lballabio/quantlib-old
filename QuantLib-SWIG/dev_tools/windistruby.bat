
if exist QuantLib-Ruby-%1.zip del QuantLib-Ruby-%1.zip
cd QuantLib-Ruby-%1
rem ruby setup.rb sdist
rem copy dist\QuantLib-Ruby-%1.zip ..\
rem cd ..
rem rmdir /S /Q QuantLib-Ruby-%1
rem 7z x QuantLib-Ruby-%1.zip
rem cd QuantLib-Ruby-%1
ruby setup.rb test
ruby setup.rb install
cd ..
