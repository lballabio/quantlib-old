#include <windows.h>
#include "xlcall.h"
#include "framewrk.hpp"

#define NUM_FUNCS 5
#define NUM_ATTS 6

static LPSTR func[NUM_FUNCS][NUM_ATTS] = {
	// utils
	{" QL_QUERY",			" RC",		" QL_QUERY",			" ", " 1", " QuantLib"},
	{" QL_LOGFILE",			" RC",		" QL_LOGFILE",			" ", " 1", " QuantLib"},
	// options
	{" QL_BLACKSCHOLES",	" REEEENN#"," QL_BLACKSCHOLES",		" ", " 1", " QuantLib"},
	{" QL_OPTION",			" RCCENNN#"," QL_OPTION",			" ", " 1", " QuantLib"},
	{" QL_OPTION_SETENGINE"," RCCN",	" QL_OPTION_SETENGINE",	" ", " 1", " QuantLib"},
};

int xlAutoOpen() {
	static XLOPER xDll;
	Excel(xlGetName, &xDll, 0);
	for (int i=0; i<NUM_FUNCS; i++)
		Excel(xlfRegister, 0, NUM_ATTS + 1, &xDll,
			TempStr(func[i][0]),
			TempStr(func[i][1]),
			TempStr(func[i][2]),
			TempStr(func[i][3]),
			TempStr(func[i][4]),
			TempStr(func[i][5]));
	Excel(xlFree, 0, 1, &xDll);
	return 1;
}
