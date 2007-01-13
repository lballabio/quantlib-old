@echo off

if %1/==/ goto usage
if not %1 == debug if not %1 == release if not %1 == unicode_d if not %1 == unicode_r goto usage

cd ..\..\tests

echo ********** Minimum **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite MinimumTestCase

echo ********** Logger **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite LoggerTestCase

echo ********** PatternLayout **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite PatternLayoutTest

echo ********** HierarchyThreshold **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite HierarchyThresholdTestCase

echo ********** CustomLogger **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite XLoggerTestCase

echo ********** DefaultInit **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite TestCase1
copy input\xml\defaultInit.xml log4j.xml
..\msvc\bin\%1\testsuite TestCase2
del log4j.xml
copy input\defaultInit3.properties log4j.properties
..\msvc\bin\%1\testsuite TestCase3
del log4j.properties
copy input\defaultInit3.properties log4j.properties
copy input\xml\defaultInit.xml log4j.xml
..\msvc\bin\%1\testsuite TestCase4

echo ********** AsyncAppender **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite AsyncAppenderTestCase

echo ********** BoundedFIFO **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite BoundedFIFOTestCase

echo ********** CyclicBuffer **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite CyclicBufferTestCase

echo ********** DRFA **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite DRFATestCase

echo ********** OptionConverter **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite OptionConverterTestCase

echo ********** CustomLevel **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite CustomLevelTestCase

echo ********** DOM **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite DOMTestCase

echo ********** SocketServer **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite SocketServerTestCase

echo ********** L7d **********
call ..\msvc\tests\clean.bat
..\msvc\bin\%1\testsuite L7dTestCase

cd ..\msvc\tests

goto done

:usage
echo Usage : runtests.bat TARGET 
echo where TARGET can be fixed to debug, release, unicode_d or unicode_r.

:done
