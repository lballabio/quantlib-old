
if exist QuantLib-MzScheme-%1.zip del QuantLib-MzScheme-%1.zip
cd QuantLib-MzScheme-%1
mzscheme -r setup.scm sdist
copy QuantLib-MzScheme-%1.zip ..\
cd ..
rmdir /S /Q QuantLib-MzScheme-%1
7z x QuantLib-MzScheme-%1.zip
cd QuantLib-MzScheme-%1
mzscheme -r setup.scm build
mzscheme -r setup.scm test
mzscheme -r setup.scm install
cd ..
