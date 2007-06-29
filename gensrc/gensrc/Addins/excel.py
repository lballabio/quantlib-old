
"""
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

"""Generate source code for Excel addin."""

from gensrc.Addins import addin
from gensrc.Addins import excelexceptions
from gensrc.Addins import serialization
from gensrc.Serialization import serializable
from gensrc.Utilities import outputfile
from gensrc.Functions import supportedplatform
from gensrc.Serialization import xmlreader
from gensrc.Utilities import buffer
from gensrc.Utilities import common
from gensrc.Utilities import log
from gensrc.Categories import category
from gensrc.Configuration import environment

import re
import string

# constants

MAXLEN = 255                        # max length of excel string
MAXPARAM = 30                       # max #/params to an Excel function
NUMDESC = 10                        # #/params to register a function
MAXUSERPARAM = MAXPARAM - NUMDESC   # #/slots remaining for param descriptions
REGLINE = '            TempStrNoSize("\\x%02X""%s")%s'
UNREGISTER = '''
        Excel4(xlfRegisterId, &xlRegID, 2, &xDll,
            TempStrNoSize("\\x%02X""%s"));
        Excel4(xlfUnregister, 0, 1, &xlRegID);\n'''
# Normally 255 is the max length of a string passed to excel.
# For some reason we find that the Excel Function Wizard malfunctions
# if the length of the comma-delimited list of param names exceeds 232 bytes.
MAX_LEN_PARAMLIST = 232

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

class ExcelAddin(addin.Addin):
    """Generate source code for Excel addin."""

    #############################################
    # class variables
    #############################################

    voSupported_ = True
    convertPermanentFlag_ = '''
        if (permanentCpp)
            objectPointer->setPermanent();'''

    #############################################
    # public interface
    #############################################

    def generate(self, categoryList, enumerationList):
        """Generate source code for Excel addin."""

        self.categoryList_ = categoryList
        self.enumerationList_ = enumerationList

        log.Log.instance().logMessage(' begin generating %s...' % self.name_)
        self.generateAddin()
        self.generateFunctions()
        self.generateFunctionCount()
        if environment.config().usingSerialization():
            self.generateSerialization()
        if self.exportSymbols_: self.generateExportSymbols()
        log.Log.instance().logMessage(' done generating %s.' % self.name_)

    def generateFunctions(self):
        """Generate source code for all functions in all categories."""
        for cat in self.categoryList_.categories(self.name_):
            categoryIncludes = cat.includeList(LOOP_INCLUDES)
            buf = self.bufferIncludes_.text() % {
                'categoryIncludes' : categoryIncludes }
            for func in cat.functions(self.name_):
                buf += self.generateFunction(func)
            fileName = '%sFunctions/%s.cpp' % (
                self.rootPath_, cat.name())
            outputfile.OutputFile(self, fileName, cat.copyright(), buf)

    def indexOfCol(self, str):
        """convert an Excel column ID to an int"""
        return reduce(lambda x, y: 26 * x + y, map(COL_TO_NUM.index, str.lower()))

    def cellNameConflict(self, funcName):
        m = CELL_NAME_REGEX.match(funcName.upper())
        return m \
        and self.indexOfCol(m.group('colLabel')) <= CELL_MAX_COL_NUM \
        and int(m.group('rowLabel')) <= CELL_MAX_ROW_NUM

    def generateFunction(self, func):
        """Generate source code for a given function."""
        if func.parameterList().parameterCount() > MAXPARAM:
            raise excelexceptions.ExcelParameterCountException(
                func.name(), func.parameterList().parameterCount(), MAXPARAM)
        if self.cellNameConflict(func.name()):
            raise excelexceptions.ExcelCellNameException(
                func.name(), CELL_MAX_COL_ID, CELL_MAX_ROW_NUM)

        return self.bufferFunction_.text() % {
            'cppConversions' : func.parameterList().generate(self.cppConversions_),
            'enumConversions' : func.parameterList().generate(self.enumConversions_),
            'functionBody' : func.generateBody(self),
            'functionDeclaration' : func.parameterList().generate(self.functionDeclaration_),
            'functionName' : func.name(),
            'functionReturnType' : self.functionReturnType_.apply(func.returnValue()),
            'libConversions' : func.parameterList().generate(self.libraryConversions_),
            'objectConversions' : func.parameterList().generate(self.objectConversions_),
            'refConversions' : func.parameterList().generate(self.referenceConversions_),
            'returnConversion' : self.returnConversion_.apply(func.returnValue()),
            'validatePermanent' : func.validatePermanent(),
            'xlTrigger' : func.xlTrigger() }

    def checkLen(self, str):
        """Calculate the length of the string, ensure that this value doesn't exceed
        Excel's limit, and return the value."""
        strLen = len(str)
        if strLen >= MAXLEN:
            raise excelexceptions.ExcelStringLengthException(str, strLen, MAXLEN)
        return strLen

    def generateRegisterFunction(self, func, categoryName, register = True):
        """Generate code to register/unregister given function."""

        paramStr = self.xlRegisterReturn_.apply(func.returnValue()) \
            + func.parameterList().generate(self.xlRegisterParam_)
        if func.xlMacro():
            paramStr += '#'
        paramNames = func.parameterList().generate(self.parameterList_)
        if len(paramNames) > MAX_LEN_PARAMLIST:
            raise excelexceptions.ExcelParameterLengthException(func.name(), paramNames, MAX_LEN_PARAMLIST)

        # Configure call to xlfRegister.  We will pass in NUMDESC params to
        # register the function, plus one additional param to describe each
        # param in the function being registered.  If we exceed the limit of
        # MAXPARAM values accepted by xlfRegister we omit descriptions as necessary.
        numUserParams = min(func.parameterList().parameterCount(), MAXUSERPARAM)
        numRegisterParams = numUserParams + NUMDESC

        # A bug in the Excel Function Wizard causes the last string to be corrupted.
        # The workaround is to pad the last string with two spaces:
        # - If the function has parameters then the last parameter will get padded
        #   in rule.py according to RuleGroup attribute "padLastParamName='True'"
        # - If the function has no parameters then the function description will be the
        #   last parameter and we pad it here.
        if numUserParams:
            funcDesc = func.description()
            delim = ','
        else:
            funcDesc = func.description() + '  '
            delim = ''

        if register:
            functionType = func.visible()
            unregister = ''
        else:
            functionType = 0
            unregister = UNREGISTER % (len(func.name()), func.name())

        return self.bufferRegisterFunction_.text() % {
            'category' : categoryName,
            'categoryLen' : self.checkLen(categoryName),
            'delim' : delim,
            'funcDesc' : funcDesc,
            'funcDescLen' : self.checkLen(funcDesc),
            'functionName' : func.name(),
            'functionNameLen' : self.checkLen(func.name()),
            'functionType' : functionType,
            'numParams' : numRegisterParams,
            'parameterList' : func.parameterList().generate(self.registerParameters_),
            'paramNames' : paramNames,
            'paramNamesLen' : self.checkLen(paramNames),
            'paramStr' : paramStr,
            'paramStrLen' : self.checkLen(paramStr),
            'unregister' : unregister }

    def outputRegisterFile(self, registerCode, unregisterCode, categoryName):
        registerBuffer = self.bufferRegisterFile_.text() % {
            'categoryName' : categoryName.capitalize(),
            'registerCode' : registerCode,
            'unregisterCode' : unregisterCode }
        registerFile = "%sRegister/register_%s.cpp" % (
            self.rootPath_, categoryName)
        outputfile.OutputFile(self, registerFile, self.copyright_, registerBuffer)

    def generateRegisterFunctions(self, cat):
        registerCode = ''
        unregisterCode = ''
        for func in cat.functions(self.name_, supportedplatform.MANUAL):
            self.functionCount_ += 1
            registerCode += self.generateRegisterFunction(func,
                cat.xlFunctionWizardCategory())
            unregisterCode += self.generateRegisterFunction(func,
                cat.xlFunctionWizardCategory(), False)
        self.outputRegisterFile(registerCode, unregisterCode, cat.name())

    def generateAddin(self):
        """Generate source code to register functions."""
        registerCalls = ''
        unregisterCalls = ''
        registerDeclarations = ''
        unregisterDeclarations = ''
        self.functionCount_ = 0
        for cat in self.categoryList_.categories(self.name_, supportedplatform.MANUAL):
            categoryName = cat.name().capitalize()
            registerCalls += 8 * ' ' + 'register' + categoryName + '(xDll);\n'
            unregisterCalls += 8 * ' ' + 'unregister' + categoryName + '(xDll);\n'
            registerDeclarations += 'extern void register' + categoryName + '(const XLOPER&);\n'
            unregisterDeclarations += 'extern void unregister' + categoryName + '(const XLOPER&);\n'
            self.generateRegisterFunctions(cat)

        registerCallBuffer = self.bufferRegisterCall_.text() % {
            'prefix' : environment.config().prefix().capitalize(),
            'registerCalls' : registerCalls,
            'unregisterCalls' : unregisterCalls,
            'registerDeclarations' : registerDeclarations,
            'unregisterDeclarations' : unregisterDeclarations }
        registerCall = self.rootPath_ + 'Register/register_all.cpp'
        outputfile.OutputFile(self, registerCall, self.copyright_, registerCallBuffer)

    def generateExportSymbols(self):
        """Generate directives that cause exported symbols to be available to
        clients of this Addin."""
        exportSymbols = ''
        for cat in self.categoryList_.categories(self.name_, supportedplatform.MANUAL):
            for func in cat.functions(self.name_, supportedplatform.MANUAL):
                exportSymbols += '#pragma comment (linker, "/export:_%s")\n' % func.name()
        buf = self.exportStub_.text() % exportSymbols
        fileName = self.rootPath_ + 'Functions/export.hpp'
        outputfile.OutputFile(self, fileName, self.copyright_, buf)

    def generateFunctionCount(self):
        """Generate a header indicating the number of functions in this addin."""
        buf = self.bufferNumFunc_.text() % self.functionCount_
        fileName = self.rootPath_ + 'Functions/functioncount.hpp'
        outputfile.OutputFile(self, fileName, self.copyright_, buf)

    def generateSerialization(self):
        """Generate source code for all functions in all categories.

        FIXME:  This function generates code which must belong to the Addin, but the code
        is platform independent.  This function will be moved into serialization.py and
        each addin will call it."""

        allIncludes = ''
        bufferRegister = ''
        for cat in self.categoryList_.categories('*'):

            if not cat.generateVOs(): continue

            bufferCpp = ''
            allIncludes += serialization.Serialization.REGISTER_INCLUDE % {
                'categoryName' : cat.name(),
                'addinDirectory' : environment.config().prefixExcel() }

            bufferRegister += serialization.Serialization.REGISTER_TYPE % {
                'categoryDisplayName' : cat.displayName(),
                'categoryName' : cat.name() }

            bufferHpp = self.bufferSerializeDeclaration_.text() % {
                'addinDirectory' : environment.config().prefixExcel(),
                'categoryName' : cat.name(),
                'namespaceAddin' : self.namespaceAddin_ }
            headerFile = '%sSerialization/serialization_%s.hpp' % ( self.rootPath_, cat.name() )
            outputfile.OutputFile(self, headerFile, self.copyright_, bufferHpp)

            for func in cat.functions('*'):
                if not func.generateVOs(): continue

                bufferCpp += serialization.Serialization.REGISTER_CALL % {
                    'functionName' : func.name(),
                    'namespaceObjects' : environment.config().namespaceObjects() }

            bufferBody = self.bufferSerializeBody_.text() % {
                'addinDirectory' : environment.config().prefixExcel(),
                'bufferCpp' : bufferCpp,
                'categoryName' : cat.name(),
	'libRootDirectory' : environment.config().libRootDirectory(),
                'namespaceAddin' : self.namespaceAddin_ }
            cppFile = '%sSerialization/serialization_%s.cpp' % ( self.rootPath_, cat.name() )
            outputfile.OutputFile(self, cppFile, self.copyright_, bufferBody)
        allBuffer =  self.bufferSerializeAll_.text() % {
            'allIncludes' : allIncludes,
            'addinDirectory' : environment.config().prefixExcel() }
        allFilename = self.rootPath_ + 'Serialization/serialization_all.hpp'
        outputfile.OutputFile(self, allFilename, self.copyright_, allBuffer)

        if self.serializationBase_:
            callBaseIn = "%s::register_in(ia);" % self.serializationBase_
            callBaseOut = "%s::register_out(oa);" % self.serializationBase_
        else:
            callBaseIn =''
            callBaseOut =''

        bufferFactory = self.bufferSerializeRegister_.text() % {
            'addinDirectory' : environment.config().prefixExcel(),
            'bufferRegister' : bufferRegister,
            'libRootDirectory' : environment.config().libRootDirectory(),
            'namespaceAddin' : self.namespaceAddin_,
            'callBaseIn': callBaseIn,
            'callBaseOut': callBaseOut}
        factoryFile = self.rootPath_ + 'Serialization/serializationfactory.cpp'
        outputfile.OutputFile(self, factoryFile, self.copyright_, bufferFactory)

    def printDebug(self):
        self.xlRegisterParam_.printDebug()

    def functionDeclaration(self):
        return self.functionDeclaration_

    def functionReturnType(self):
        return self.functionReturnType_

    def xlRegisterParam(self):
        return self.xlRegisterParam_

    def xlRegisterReturn(self):
        return self.xlRegisterReturn_

    def parameterList(self):
        return self.parameterList_

    def registerParameters(self):
        return self.registerParameters_

    def cppConversions(self):
        return self.cppConversions_

    def libraryConversions(self):
        return self.libraryConversions_

    def enumConversions(self):
        return self.enumConversions_

    def objectConversions(self):
        return self.objectConversions_

    def referenceConversions(self):
        return self.referenceConversions_

    def returnConversion(self):
        return self.returnConversion_

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """load/unload class state to/from serializer object."""
        super(ExcelAddin, self).serialize(serializer)
        serializer.serializeBoolean(self, 'exportSymbols')
        serializer.serializeProperty(self, 'serializationBase')

