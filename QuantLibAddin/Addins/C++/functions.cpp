#include "qladdincpp.hpp"
using namespace ObjHandler;

void QL_LOGFILE(
	const std::string &logFileName) {
	setLogFile(logFileName);
}

void QL_LOGMESSAGE(
	const std::string &msg) {
	logMessage(msg);
}
