#include "qladdincpp.hpp"

void QL_LOGFILE(
	const string &logFileName) {
	setLogFile(logFileName);
}

void QL_LOGMESSAGE(
	const string &msg) {
	logMessage(msg);
}
