#include "qladdincpp.hpp"
using namespace ObjHandler;

void QL_LOGFILE(
	const string &logFileName) {
	setLogFile(logFileName);
}

void QL_LOGMESSAGE(
	const string &msg) {
	logMessage(msg);
}
