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
RETVAL = 'returnval'
PROPVEC = 'propertyvector'
XMLSUFFIX = r'(.*).xml\Z'

# General

CR_FILENAME = 'copyright.txt'
CR_BUFFER = ''
HEADER = '%s this file generated automatically by %s on %s\n\
%s editing this file manually is not recommended\n\n'
ADDIN_ROOT = '../Addins/'

# C

C_ROOT = ADDIN_ROOT + 'C/'
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

# Calc

CALC_ROOT = ADDIN_ROOT + 'Calc/'
CALC_MAPFILE = 'funcdef.cpp'
CALC_MAPHEADER='#ifdef WIN32\n\
#pragma warning(disable: 4786)\n\
#pragma warning(disable: 4503)\n\
#endif\n\n\
#include <Addins/Calc/qladdin.hpp>\n\n\
QLAddin::QLAddin() throw () : m_refcount( 0 ) {\n'
CALC_MAPLINE='\tfuncMap[ STRFROMANSI( "%s" ) ]\n\
\t\t=  STRFROMANSI( "%s" );\n'
CALC_AUTOHDR = 'autogen.hpp'
CALC_INCLUDES = '#include <QuantLibAddin/qladdin.hpp>\n\
#include <Addins/Calc/qladdin.hpp>\n\
#include <Addins/Calc/utilities.hpp>\n\n\
using namespace ObjHandler;\n\
using namespace QuantLibAddin;\n\n'
CALC_BODY = '\ttry {\n\
\t\tProperties properties = %s(%s\n\
%s);\n\
\t\treturn getArray(properties, handle);\n\
\t} catch (const std::exception &e) {\n\
\t\tQL_LOGMESSAGE(std::string("ERROR: %s: ") + e.what());\n\
\t\tTHROW_RTE;\n\
\t}\n\
}\n\n'
CALC_IDL = 'QuantLibAddin.idl'
CALC_IDL_HEAD = '#include <com/sun/star/uno/XInterface.idl>\n\
#include <com/sun/star/beans/XPropertySet.idl>\n\n\
module com {\n\
  module sun {\n\
    module star {\n\
      module sheet {\n\
        module addin {\n\
          interface XQL: com::sun::star::uno::XInterface {\n\n'
CALC_IDL_FOOT = '          };\n\
        };\n\
      };\n\
    };\n\
  };\n\
};\n\n'
CALC_IDL_FUNC = '\t\t\t\t%s %s(\n\
%s%s)\n\
\t\t\t\t\traises( com::sun::star::lang::IllegalArgumentException );\n\n'

# Excel

XL_ROOT = ADDIN_ROOT + 'Excel/'
XL_FUNC = 'funcdef.hpp'
EXPORTFILE = 'qladdin.def'
EXPORTHEADER = 'LIBRARY QUANTLIBADDIN\n\nEXPORTS\n\txlAutoOpen\n\txlAutoFree\n\n'
XL_INCLUDE = '#include <QuantLibAddin/qladdin.hpp>\n\
#include <Addins/Excel/utilities.hpp>\n\n\
using namespace ObjHandler;\n\
using namespace QuantLibAddin;\n\n'
XL_SOURCE = 'LPXLOPER %s(\n\
%s) {\n\
\ttry {\n\
%s\t\tProperties properties = %s(\n%s\
%s);\n\
\t\tstatic XLOPER xRet;\n\
\t\tsetValues(&xRet, properties, handle);\n\
\t\treturn &xRet;\n\
\t} catch (const exception &e) {\n\
\t\tQL_LOGMESSAGE(std::string("ERROR: %s: ") + e.what());\n\
\t\treturn 0;\n\
\t}\n\
}\n\n'
FUNCDEC='\t{\t%-30s// function name (code)\n\
\t\t%-30s// parameter types\n\
\t\t%-30s// function name (Excel)\n\
\t\t// parameter names:\n\
\t\t%s\n\
\t\t" 1",                         // macro type\n\
\t\t" QuantLib",                  // category\n\
\t\t" ",                          // shortcut text\n\
\t\t" ",                          // help topic\n\
\t\t// function help:\n\
\t\t%s\n'
# maximum number of parameters to be passed to an Excel function -
# if you change this number you must also change the call to xlfRegister
# in QuantLibAddin\Addins\Excel\qladdin.cpp
XLMAXPARAM=7
