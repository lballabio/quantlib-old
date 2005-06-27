"""
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

'global constants'

# XML

ANY         = 'any'
ATTS        = 'attributes'
BOOL        = 'bool'
CALL_TYPE   = 'call_type'
CLASS       = 'class'
CODENAME    = 'codename'
CTOR        = 'constructor'
DEFS        = 'defs'
DESC        = 'desc'
DISPLAYNAME = 'displayname'
DOUBLE      = 'double'
ENUMDEFS    = 'enumdefs'
ENUM        = 'enum'
ENUMS       = 'enums'
FUNCS       = 'functions'
HANDLE      = 'handle'
HDRONLY     = 'headeronly'
IGNORE      = 'ignore'
LONG        = 'long'
MATRIX      = 'matrix'
NAME        = 'name'
PARAMS      = 'parameters'
PROPERTY    = 'property'
QLFUNC      = 'qlfunction'
RETVAL      = 'returnval'
SCALAR      = 'scalar'
STRING      = 'string'
TENSOR      = 'tensorrank'
TRUE        = 'true'
TYPE        = 'type'
TYPE_CNV    = 'ql_type'
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

