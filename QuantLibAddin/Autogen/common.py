'global constants'

# XML

FUNCGROUPS = 'funcgroups'
FUNC = 'function'
FUNCLIST = 'functionlist'
NAME = 'name'
CODENAME = 'codename'
DESC = 'description'
HANDLE = 'handle'
HDRONLY = 'headeronly'
PARAMS = 'parameters'
PARAM = 'param'
TYPE = 'type'
DESC = 'desc'
NUMFUNC = 'numfunc'

# Excel

XL_ROOT = '../Addins/Excel/'
XL_FUNC = 'funcdef.hpp'
EXPORTFILE = 'qladdin.def'
EXPORTHEADER = 'LIBRARY QUANTLIBADDIN\n\nEXPORTS\n\txlAutoOpen\n\txlAutoFree\n\n'

# C

C_ROOT = '../Addins/C/'
C_INCLUDES = '#include <QuantLibAddin/qladdin.hpp>\n\
extern "C" {\n\
#include <Addins/C/varies.h>\n\
#include <Addins/C/defines.h>\n\
#include <Addins/C/%s.h>\n\
}\n\
#include <Addins/C/varies.hpp>\n\n\
using namespace ObjHandler;\n\
using namespace QuantLibAddin;\n\n'
C_BODY = '\
\ttry {\n\
\t\tProperties properties = %s(\n\
%s%s);\n\
\t\tpropertiesToVaries(properties, result);\n\
\t\treturn SUCCESS;\n\
\t} catch (const std::exception &e) {\n\
\t\tQL_LOGMESSAGE("%s_C Error: " + std::string(e.what()));\n\
\t\tresult = 0;\n\
\t\treturn FAIL;\n\
\t}\n\
}\n\n'

# Utils

CR_FILENAME = 'copyright.txt'
CR_BUFFER = ''
HEADER = '%s this file generated automatically by %s on %s\n\
%s editing this file manually is not recommended\n\n'

