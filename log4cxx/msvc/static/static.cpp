#include <log4cxx/config.h>
#include <log4cxx/helpers/boundedfifo.h>
#include <log4cxx/asyncappender.h>
#include <log4cxx/consoleappender.h>
#include <log4cxx/dailyrollingfileappender.h>
#include <log4cxx/htmllayout.h>
#include <log4cxx/level.h>
#include <log4cxx/patternlayout.h>
#include <log4cxx/propertyconfigurator.h>
#include <log4cxx/rollingfileappender.h>
#include <log4cxx/simplelayout.h>
#include <log4cxx/ttcclayout.h>
#include <log4cxx/db/odbcappender.h>
#include <log4cxx/helpers/appenderattachableimpl.h>
#include <log4cxx/helpers/onlyonceerrorhandler.h>
#include <log4cxx/net/smtpappender.h>
#include <log4cxx/net/socketappender.h>
#include <log4cxx/net/sockethubappender.h>
#include <log4cxx/helpers/datagramsocket.h>
#include <log4cxx/net/syslogappender.h>
#include <log4cxx/net/telnetappender.h>
#include <log4cxx/net/xmlsocketappender.h>
#include <log4cxx/nt/nteventlogappender.h>
#include <log4cxx/spi/loggingevent.h>
#include <log4cxx/varia/denyallfilter.h>
#include <log4cxx/varia/fallbackerrorhandler.h>
#include <log4cxx/varia/levelmatchfilter.h>
#include <log4cxx/varia/levelrangefilter.h>
#include <log4cxx/varia/stringmatchfilter.h>
#include <log4cxx/xml/domconfigurator.h>
#include <log4cxx/xml/xmllayout.h>

using namespace log4cxx;
using namespace log4cxx::db;
using namespace log4cxx::helpers;
using namespace log4cxx::net;
using namespace log4cxx::nt;
using namespace log4cxx::spi;
using namespace log4cxx::varia;
using namespace log4cxx::xml;

/** Special function used to force symbol references for dynamic classes when
comiling a static library with msvc.

<b>This function must not be called directly !</b>
*/
void ForceSymbolReferences()
{
	AsyncAppender();
	ConsoleAppender();
	DailyRollingFileAppender();
	FileAppender();
	HTMLLayout();
	PatternLayout();
	PropertyConfigurator();
	RollingFileAppender();
	SimpleLayout();
	TTCCLayout();
#ifdef HAVE_ODBC
	ODBCAppender();
#endif
	AppenderAttachableImpl();
	OnlyOnceErrorHandler();
#ifdef HAVE_SMTP
	SMTPAppender();
	DefaultEvaluator();
#endif
	SocketAppender();
	SocketHubAppender();
	SyslogAppender();
	TelnetAppender();
	XMLSocketAppender();
	NTEventLogAppender();
	LoggingEvent();
	DenyAllFilter();
	FallbackErrorHandler();
	LevelMatchFilter();
	LevelRangeFilter();
	StringMatchFilter();
#ifdef HAVE_XML
	DOMConfigurator();
#endif
	XMLLayout();
}
