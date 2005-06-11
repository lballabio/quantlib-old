'global constants'

# XML

ANY         = 'any'
BOOL        = 'bool'
CLASS       = 'class'
CODENAME    = 'codename'
CTOR        = 'constructor'
DESC        = 'desc'
DISPLAYNAME = 'displayname'
DOUBLE      = 'double'
FUNC        = 'function'
FUNCGROUPS  = 'funcgroups'
FUNCLIST    = 'functionlist'
HANDLE      = 'handle'
HDRONLY     = 'headeronly'
LONG        = 'long'
MATRIX      = 'matrix'
NAME        = 'name'
NUMFUNC     = 'numfunc'
PARAM       = 'param'
PARAMS      = 'parameters'
PROPERTY    = 'property'
QLFUNC      = 'qlfunction'
RETVAL      = 'returnval'
SCALAR      = 'scalar'
STRING      = 'string'
TENSOR      = 'tensorrank'
TYPE        = 'type'
VECTOR      = 'vector'

# General

ARGLINE         = 8 * ' ' + 'ArgumentStack args;\n'
ADDIN_ROOT      = '../Addins/'
MAKE_FUNCTION   = 'OH_MAKE_OBJECT'
TEMPFILE        = '.new'
MAKE_ARGS       = '\n\
            QuantLibAddin::%s,\n\
            %s,\n\
            args'
FUNC_BODY       = '\
        boost::shared_ptr<QuantLibAddin::%s> objectPointer =\n\
            OH_GET_OBJECT(QuantLibAddin::%s, %s);\n\
        if (!objectPointer)\n\
            QL_FAIL("%s: error retrieving object " + %s);\n'

