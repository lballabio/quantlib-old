
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
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

"""Generate source code for Excel addin."""

from gensrc.Addins import addin
from gensrc.Serialization import serializable
from gensrc.Utilities import outputfile
from gensrc.Functions import function
from gensrc.Serialization import xmlreader
from gensrc.Utilities import buffer
from gensrc.Utilities import common
from gensrc.Utilities import log
from gensrc.Categories import category
from gensrc.Configuration import environment

import sys
import re
import string

# constants

MAXLEN = 255                        # max length of excel string
MAXPARAM = 30                       # max #/params to an Excel function
NUMDESC = 10                        # #/params to register a function
MAXUSERPARAM = MAXPARAM - NUMDESC   # #/slots remaining for param descriptions
MAXLENERR = 'string length of %d exceeds Excel maximum of %d:\n\n%s'
MAXPARAMERR = 'number of parameters to function "%s" exceeds Excel max of %d'
REGLINE = '            TempStrNoSize("\\x%02X""%s")%s'
UNREGISTER = '''
        Excel4(xlfRegisterId, &xlRegID, 2, &xDll,
            TempStrNoSize("\\x%02X""%s"));
        Excel4(xlfUnregister, 0, 1, &xlRegID);\n'''
# Normally 255 is the max length of a string passed to excel.
# For some reason we find that the Excel Function Wizard malfunctions
# if the length of the comma-delimited list of param names exceeds 232 bytes.
MAX_LEN_PARAMLIST = 232
PARAM_ERROR = '''
 
***********************************************************************
 
Error processing function %(funcname)s -
The comma-delimited list of parameter names is invalid:
 
%(paramNames)s
 
This string has a length of %(len)d which exceeds the max of %(max)d
allowed by the Excel function wizard.
Please shorten the names of the parameters.
 
***********************************************************************
 '''

LOOP_INCLUDES = '''\
#include <%s/loop_%s.hpp>
#include <ohxl/loop.hpp>\n'''

# some constant values for processing cell range names
CELL_MAX_COL_ID = "XFD"         # the last column in Excel 2007
CELL_MAX_COL_NUM = 16384        # value XFD expressed as base 26 number
CELL_MAX_ROW_NUM = 1048576      # the last row in Excel 2007
CELL_NAME_REGEX = re.compile(r'(?P<colLabel>[A-Z]+)(?P<rowLabel>\d+)', re.I)
# we need the 1-based index of letters so prefix the alphabet with a dummy character
COL_TO_NUM = "_" + string.lowercase
CELL_NAME_ERROR = '''
 
***********************************************************************
 
Error processing function %(funcname)s -
The function name "%(funcname)s" is invalid, because
it conflicts with an Excel cell range name.
 
Excel 2007 cells are named from A1 to %(maxcol)s%(maxrow)s
and these identifiers may not be used as function names.
 
***********************************************************************
 '''

class AddinExcel(addin.Addin):
    """Generate source code for Excel addin."""
    voSupported = True
    convertPermanentFlag = '''
        bool permanentCpp =
            ObjHandler::operToScalar<bool>(*permanent, false, "permanent");'''

    def postSerialize(self):
        """Perform post serialization initialization."""
        super(AddinExcel, self).postSerialize()
        xmlConfig = xmlreader.XmlReader('config/excel')
        xmlConfig.serializeBoolean(self, 'exportSymbols')
        if self.exportSymbols:
            xmlConfig.serializeObject(self, buffer.Buffer)

    def generate(self, categoryList, enumerationList):
        """Generate source code for Excel addin."""

        self.categoryList_ = categoryList
        self.enumerationList_ = enumerationList

        log.Log.getInstance().logMessage(' begin generating %s...' % self.name)
        self.generateAddin()
        self.generateFunctions()
        self.generateFunctionCount()
        if self.exportSymbols: self.generateExportSymbols()
        log.Log.getInstance().logMessage(' done generating %s.' % self.name)

    def generateFunctions(self):
        """Generate source code for all functions in all categories."""
        for cat in self.categoryList_.categories(self.name):
            categoryIncludes = cat.includeList()
            if cat.containsLoopFunction:
                categoryIncludes += LOOP_INCLUDES % (
                    environment.config().loopRootDirectory,
                    cat.name)
            buf = self.bufferIncludes.text % { 
                'categoryIncludes' : categoryIncludes }
            for func in cat.getFunctions(self.name): 
                buf += self.generateFunction(func)
            fileName = '%sFunctions/%s.cpp' % (
                environment.config().excelFullPath, cat.name)
            outputfile.OutputFile(self, fileName, cat.copyright, buf)

    def indexOfCol(self, str):
        """convert an Excel column ID to an int"""
        return reduce( lambda x, y: 26 * x + y, map(COL_TO_NUM.index, str.lower()))

    def cellNameConflict(self, funcName):
        m = CELL_NAME_REGEX.match(funcName.upper())
        return m \
        and self.indexOfCol(m.group('colLabel')) <= CELL_MAX_COL_NUM \
        and int(m.group('rowLabel')) <= CELL_MAX_ROW_NUM

    def generateFunction(self, func):
        """Generate source code for a given function."""
        if func.ParameterList.ParameterCount > MAXPARAM:
            sys.exit(MAXPARAMERR % (func.name, MAXPARAM))
        if self.cellNameConflict(func.name):
            sys.exit(CELL_NAME_ERROR % {
                'funcname' : func.name,
                'maxcol' : CELL_MAX_COL_ID,
                'maxrow' : CELL_MAX_ROW_NUM })
        return self.bufferFunction.text % {
            'cppConversions' : func.ParameterList.generate(self.cppConversions),
            'enumConversions' : func.ParameterList.generate(self.enumConversions),
            'functionBody' : func.generateBody(self),
            'functionDeclaration' : func.ParameterList.generate(self.functionDeclaration),
            'functionName' : func.name,
            'functionReturnType' : self.functionReturnType.apply(func.returnValue),
            'libConversions' : func.ParameterList.generate(self.libraryConversions),
            'objectConversions' : func.ParameterList.generate(self.objectConversions),
            'refConversions' : func.ParameterList.generate(self.referenceConversions),
            'resetCaller' : func.resetCaller,
            'returnConversion' : self.returnConversion.apply(func.returnValue),
            'validatePermanent' : func.validatePermanent,
            'xlTrigger' : func.xlTrigger,
            'xlWizardRecalc' : func.xlWizardCheck }

    def checkLen(self, str):
        "Ensure that the length of the string doesn't exceed Excel's limit."
        strLen = len(str)
        if strLen >= MAXLEN:
            sys.exit(MAXLENERR % (strLen, MAXLEN, str))
        else:
            return strLen

    def generateRegisterFunction(self, func, categoryName, register = True):
        """Generate code to register/unregister given function."""

        paramStr = self.xlRegisterReturn.apply(func.returnValue) \
            + func.ParameterList.generate(self.xlRegisterParam)
        if func.xlMacro():
            paramStr += '#'
        paramNames = func.ParameterList.generate(self.parameterList)
        if len(paramNames) > MAX_LEN_PARAMLIST:
            sys.exit(PARAM_ERROR % {
                'funcname' : func.name,
                'paramNames' : paramNames,
                'max' : MAX_LEN_PARAMLIST,
                'len' : len(paramNames) })

        # Configure call to xlfRegister.  We will pass in NUMDESC params to
        # register the function, plus one additional param to describe each
        # param in the function being registered.  If we exceed the limit of
        # MAXPARAM values accepted by xlfRegister we omit descriptions as necessary.
        numUserParams = min(func.ParameterList.ParameterCount, MAXUSERPARAM)
        numRegisterParams = numUserParams + NUMDESC

        # A bug in the Excel Function Wizard causes the last string to be corrupted.
        # The workaround is to pad the last string with two spaces:
        # - If the function has parameters then the last parameter will get padded
        #   in rule.py according to RuleGroup attribute "padLastParamName='True'"
        # - If the function has no parameters then the function description will be the
        #   last parameter and we pad it here.
        if numUserParams:
            funcDesc = func.description
            delim = ','
        else:
            funcDesc = func.description + '  '
            delim = ''

        if register:
            unregister = ''
        else:
            unregister = UNREGISTER % (len(func.name), func.name)

        return self.bufferRegisterFunction.text % {
            'category' : categoryName,
            'categoryLen' : self.checkLen(categoryName),
            'delim' : delim,
            'funcDesc' : funcDesc,
            'funcDescLen' : self.checkLen(funcDesc),
            'functionName' : func.name,
            'functionNameLen' : self.checkLen(func.name),
            'functionType' : register,
            'numParams' : numRegisterParams,
            'parameterList' : func.ParameterList.generate(self.registerParameters),
            'paramNames' : paramNames,
            'paramNamesLen' : self.checkLen(paramNames),
            'paramStr' : paramStr,
            'paramStrLen' : self.checkLen(paramStr),
            'unregister' : unregister }

    def outputRegisterFile(self, registerCode, unregisterCode, categoryName):
        registerBuffer = self.bufferRegisterFile.text % {
            'categoryName' : categoryName.capitalize(),
            'registerCode' : registerCode,
            'unregisterCode' : unregisterCode }
        registerFile = "%sRegister/register_%s.cpp" % (
            environment.config().excelFullPath, categoryName)
        outputfile.OutputFile(self, registerFile, self.copyright, registerBuffer)

    def generateRegisterFunctions(self, cat):
        registerCode = ''
        unregisterCode = ''
        for func in cat.getFunctions(self.name, function.MANUAL): 
            self.functionCount += 1
            registerCode += self.generateRegisterFunction(func, 
                cat.xlFunctionWizardCategory)
            unregisterCode += self.generateRegisterFunction(func, 
                cat.xlFunctionWizardCategory, False)
        self.outputRegisterFile(registerCode, unregisterCode, cat.name)

    def generateAddin(self):
        """Generate source code to register functions."""
        registerCalls = ''
        unregisterCalls = ''
        registerDeclarations = ''
        unregisterDeclarations = ''
        self.functionCount = 0
        for cat in self.categoryList_.categories(self.name, function.MANUAL):
            categoryName = cat.name.capitalize()
            registerCalls += 8 * ' ' + 'register' + categoryName + '(xDll);\n'
            unregisterCalls += 8 * ' ' + 'unregister' + categoryName + '(xDll);\n'
            registerDeclarations += 'extern void register' + categoryName + '(const XLOPER&);\n'
            unregisterDeclarations += 'extern void unregister' + categoryName + '(const XLOPER&);\n'
            self.generateRegisterFunctions(cat)

        registerCallBuffer = self.bufferRegisterCall.text % {
            'prefix' : environment.config().prefix.capitalize(),
            'registerCalls' : registerCalls,
            'unregisterCalls' : unregisterCalls,
            'registerDeclarations' : registerDeclarations,
            'unregisterDeclarations' : unregisterDeclarations }
        registerCall = environment.config().excelFullPath + 'Register/register_all.cpp'
        outputfile.OutputFile(self, registerCall, self.copyright, registerCallBuffer)

    def generateExportSymbols(self):
        """Generate directives that cause exported symbols to be available to
        clients of this Addin."""
        exportSymbols = ''
        for cat in self.categoryList_.categories(self.name, function.MANUAL):
            for func in cat.getFunctions(self.name, function.MANUAL): 
                exportSymbols += '#pragma comment (linker, "/export:_%s")\n' % func.name
        buf = self.exportStub.text % exportSymbols
        fileName = environment.config().excelFullPath + 'Functions/export.hpp'
        outputfile.OutputFile(self, fileName, self.copyright, buf)

    def generateFunctionCount(self):
        """Generate a header indicating the number of functions in this addin."""
        buf = self.bufferNumFunc.text % self.functionCount
        fileName = environment.config().excelFullPath + 'Functions/functioncount.hpp'
        outputfile.OutputFile(self, fileName, self.copyright, buf)

