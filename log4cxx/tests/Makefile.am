SUBDIRS = input src witness
EXTRA_DIST = log4j.dtd L7D_en_US.properties L7D_fr_CH.properties L7D_fr.properties

if TESTS

prepare:
	@if test ! -d "output"; then \
		mkdir output; \
	fi;
	@rm -rf log4j.xml
	@rm -rf log4j.properties
	
build: check-recursive prepare

cleanOutputDir:
	@rm -f output/*
	
check: regression longUnit

regression: Minimum Logger DOM CustomLevel \
	CustomLogger PatternLayout \
	HierarchyThreshold DefaultInit SocketServer \
	XMLLayout AsyncAppender ErrorHandler \
	OptionConverter BoundedFIFO \
	CyclicBuffer DRFA LevelMatchFilter \
	LevelRangeFilter PatternParser L7d

longUnit:

Minimum: build cleanOutputDir
	src/testsuite MinimumTestCase
	
Logger: build cleanOutputDir
	src/testsuite LoggerTestCase
	
PatternLayout: build cleanOutputDir
	src/testsuite PatternLayoutTest

HierarchyThreshold: build cleanOutputDir
	src/testsuite HierarchyThresholdTestCase
	
DOM: build cleanOutputDir
	src/testsuite DOMTestCase

CustomLevel: build cleanOutputDir
	src/testsuite CustomLevelTestCase

CustomLogger: build cleanOutputDir
	src/testsuite XLoggerTestCase
	
DefaultInit: build cleanOutputDir
	src/testsuite TestCase1
	@cp input/xml/defaultInit.xml log4j.xml
	src/testsuite TestCase2
	@rm -f log4j.xml
	@cp input/defaultInit3.properties log4j.properties
	src/testsuite TestCase3
	@rm -f log4j.properties
	@cp input/defaultInit3.properties log4j.properties
	@cp input/xml/defaultInit.xml log4j.xml
	src/testsuite TestCase4
	@rm -f log4j.xml
	@rm -f log4j.properties

SocketServer: build cleanOutputDir
	src/testsuite SocketServerTestCase

XMLLayout: build cleanOutputDir
	src/testsuite XMLLayoutTestCase

AsyncAppender: build cleanOutputDir
	src/testsuite AsyncAppenderTestCase

LevelMatchFilter: build cleanOutputDir
	src/testsuite LevelMatchFilterTestCase

LevelRangeFilter: build cleanOutputDir
	src/testsuite LevelRangeFilterTestCase

ErrorHandler: build cleanOutputDir
	src/testsuite ErrorHandlerTestCase

OptionConverter: build cleanOutputDir
	src/testsuite OptionConverterTestCase

BoundedFIFO: build cleanOutputDir
	src/testsuite BoundedFIFOTestCase

CyclicBuffer: build cleanOutputDir
	src/testsuite CyclicBufferTestCase
	
DRFA: build cleanOutputDir
	src/testsuite DRFATestCase

PatternParser: build cleanOutputDir

L7d: build cleanOutputDir
	src/testsuite L7dTestCase

endif
