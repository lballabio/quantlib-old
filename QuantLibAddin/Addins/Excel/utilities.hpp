#include <windows.h>
#include "xlcall.h"
#include <string>
#include <QuantLibAddin/qladdindefines.hpp>

void setXLOPERString(XLOPER &xStr, const char *s);
void anyToXLOPER(const any_ptr &any, XLOPER &xOp);
string getCaller();
void setValues(LPXLOPER xArray, obj_ptr object, const std::string &handle);
