#ifndef utilities_h
#define utilities_h

#include "object.hpp"

string toUpper(const string &s);
string AnyToString(const any_ptr &a);
int setLogFile(const string &newLogFileName);
void logMessage(const string &msg);

#endif
