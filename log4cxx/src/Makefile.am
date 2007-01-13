lib_LTLIBRARIES = liblog4cxx.la

INCLUDES = -I$(top_srcdir)/include

liblog4cxx_la_SOURCES = \
	appenderattachableimpl.cpp \
	appenderskeleton.cpp \
	asyncappender.cpp \
	basicconfigurator.cpp \
	boundedfifo.cpp \
	class.cpp \
	condition.cpp \
	configurator.cpp \
	consoleappender.cpp \
	criticalsection.cpp \
	cyclicbuffer.cpp \
	dailyrollingfileappender.cpp \
	datagrampacket.cpp \
	datagramsocket.cpp \
	datelayout.cpp \
	dateformat.cpp \
	defaultcategoryfactory.cpp \
	domconfigurator.cpp \
	event.cpp \
	fallbackerrorhandler.cpp \
	fileappender.cpp \
	filewatchdog.cpp \
	formattinginfo.cpp \
	gnomexml.cpp \
	hierarchy.cpp \
	htmllayout.cpp \
	inetaddress.cpp \
	layout.cpp\
	level.cpp \
	levelmatchfilter.cpp \
	levelrangefilter.cpp \
	loader.cpp\
	locale.cpp\
	logger.cpp \
	loggingevent.cpp \
	loglog.cpp \
	logmanager.cpp \
	msxml.cpp \
	mutex.cpp \
	ndc.cpp \
	mdc.cpp \
	nteventlogappender.cpp \
	objectimpl.cpp \
	odbcappender.cpp \
	onlyonceerrorhandler.cpp \
	optionconverter.cpp \
	patternconverter.cpp \
	patternlayout.cpp \
	patternparser.cpp \
	properties.cpp \
	propertyconfigurator.cpp \
	propertyresourcebundle.cpp \
	propertysetter.cpp \
	resourcebundle.cpp \
	rollingfileappender.cpp \
	rootcategory.cpp \
	serversocket.cpp \
	semaphore.cpp \
	simplelayout.cpp \
	smtpappender.cpp \
	socket.cpp \
	socketappender.cpp \
	sockethubappender.cpp \
	socketimpl.cpp \
	socketinputstream.cpp \
	socketnode.cpp \
	socketoutputstream.cpp \
	stringhelper.cpp \
	stringmatchfilter.cpp \
	stringtokenizer.cpp \
	syslogappender.cpp \
	syslogwriter.cpp \
	system.cpp \
	telnetappender.cpp \
	timezone.cpp \
	transform.cpp \
	thread.cpp \
	threadspecificdata.cpp \
	ttcclayout.cpp \
	writerappender.cpp \
	xmllayout.cpp\
	xmlsocketappender.cpp

AM_CPPFLAGS = @CPPFLAGS_XML@ @CPPFLAGS_ODBC@
liblog4cxx_la_LDFLAGS = -version-info @LT_VERSION@ @LIBS_XML@ @LIBS_ODBC@

bin_PROGRAMS = simplesocketserver
simplesocketserver_SOURCES = simplesocketserver.cpp
simplesocketserver_LDADD = $(top_builddir)/src/liblog4cxx.la



