#include <windows.h>
#include "xlcall.h"
#include <string>
#include <QuantLibAddin/qladdindefines.hpp>

void setXLOPERString(XLOPER &xStr,
                     const char *s);
void anyToXLOPER(const ObjHandler::any_ptr &any,
                 XLOPER &xOp);
std::string getCaller();
void setValues(LPXLOPER xArray,
               ObjHandler::obj_ptr object,
               const std::string &handle);
