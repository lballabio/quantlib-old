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
    try {\n\
        Properties properties = %s(\n\
%s%s);\n\
        propertiesToVaries(properties, result);\n\
        return SUCCESS;\n\
    } catch (const std::exception &e) {\n\
        QL_LOGMESSAGE("%s_C Error: " + std::string(e.what()));\n\
        result = 0;\n\
        return FAIL;\n\
    }\n\
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
CALC_MAPLINE='    funcMap[ STRFROMANSI( "%s" ) ]\n\
        =  STRFROMANSI( "%s" );\n'
CALC_AUTOHDR = 'autogen.hpp'
CALC_INCLUDES = '#include <QuantLibAddin/qladdin.hpp>\n\
#include <Addins/Calc/qladdin.hpp>\n\
#include <Addins/Calc/utilities.hpp>\n\n\
using namespace ObjHandler;\n\
using namespace QuantLibAddin;\n\n'
CALC_BODY = '    try {\n\
        Properties properties = %s(%s\n\
%s);\n\
        return getArray(properties, handle);\n\
    } catch (const std::exception &e) {\n\
        QL_LOGMESSAGE(std::string("ERROR: %s: ") + e.what());\n\
        THROW_RTE;\n\
    }\n\
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
CALC_IDL_FUNC = '                %s %s(\n\
%s%s)\n\
                    raises( com::sun::star::lang::IllegalArgumentException );\n\n'

# Excel

XL_ROOT = ADDIN_ROOT + 'Excel/'
XL_FUNC = 'funcdef.hpp'
EXPORTFILE = 'qladdin.def'
EXPORTHEADER = 'LIBRARY QUANTLIBADDIN\n\nEXPORTS\n    xlAutoOpen\n    xlAutoFree\n\n'
XL_INCLUDE = '#include <QuantLibAddin/qladdin.hpp>\n\
#include <Addins/Excel/utilities.hpp>\n\n\
using namespace ObjHandler;\n\
using namespace QuantLibAddin;\n\n'
XL_SOURCE = 'LPXLOPER %s(\n\
%s) {\n\
    try {\n\
%s        Properties properties = %s(\n%s\
%s);\n\
        static XLOPER xRet;\n\
        setValues(&xRet, properties, handle);\n\
        return &xRet;\n\
    } catch (const exception &e) {\n\
        QL_LOGMESSAGE(std::string("ERROR: %s: ") + e.what());\n\
        return 0;\n\
    }\n\
}\n\n'
FUNCDEC='    {   %-30s// function name (code)\n\
        %-30s// parameter types\n\
        %-30s// function name (Excel)\n\
        // parameter names:\n\
        %s\n\
        " 1",                         // macro type\n\
        " QuantLib",                  // category\n\
        " ",                          // shortcut text\n\
        " ",                          // help topic\n\
        // function help:\n\
        %s\n'
# maximum number of parameters to be passed to an Excel function -
# if you change this number you must also change the call to xlfRegister
# in QuantLibAddin\Addins\Excel\qladdin.cpp
XLMAXPARAM=7
