
if exist QuantLib-Python-%1.zip del QuantLib-Python-%1.zip
cd QuantLib-Python-%1
python setup.py sdist
copy dist\QuantLib-Python-%1.zip ..\
cd ..
rmdir /S /Q QuantLib-Python-%1
7z x QuantLib-Python-%1.zip
cd QuantLib-Python-%1
python setup.py test
python setup.py bdist_wininst
copy dist\QuantLib-Python-%1.win32-py*.exe ..\
python setup.py install
cd ..
