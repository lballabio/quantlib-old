
"""
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005, 2006, 2007, 2008 Eric Ehlers

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

from gensrc.configuration import environment
from gensrc.addins import addin
from gensrc.addins import excelexceptions
from gensrc.functions import supportedplatform
from gensrc.categories import category
from gensrc.serialization import serializable
from gensrc.serialization import xmlreader
from gensrc.utilities import outputfile
from gensrc.utilities import buffer
from gensrc.utilities import common
from gensrc.utilities import log

import re
import string

# Constants

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
#include <%s/loop/loop_%s.hpp>
#include <ohxl/loop.hpp>\n'''

# Some constant values for processing cell range names
CELL_MAX_COL_ID = "XFD"         # the last column in Excel 2007
CELL_MAX_COL_NUM = 16384        # value XFD expressed as base 26 number
CELL_MAX_ROW_NUM = 1048576      # the last row in Excel 2007
CELL_NAME_REGEX = re.compile(r'(?P<colLabel>[A-Z]+)(?P<rowLabel>\d+)', re.I)
# We need the 1-based index of letters so prefix the alphabet with a dummy character
COL_TO_NUM = "_" + string.lowercase

class ExcelAddin(addin.Addin):
    """Generate source code for Excel addin."""

    #############################################
    # class variables
    #############################################

    XL_WIZARD_CHECK = '''
        if (functionCall->calledByFunctionWizard())
            return 0;\n'''
            
    ID_STRIP = '''
        // Strip the Excel cell update counter suffix from Object IDs
        
        std::string ObjectIdStrip = ObjectHandler::CallingRange::getStub(ObjectId);%s\n'''
        
    VALIDATE_TRIGGER = '''
        ObjectHandler::validateRange(Trigger, "Trigger");'''

    objectIdSuffix_ = 'Strip'
    repositoryClass_ = 'RepositoryXL'
    overwriteVariable_ = '*Overwrite'

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
        if self.exportSymbols_: self.generateExportSymbols()
        log.Log.instance().logMessage(' done generating %s.' % self.name_)

    def generateFunctions(self):
        """Generate source code for all functions in all categories."""
        for cat in self.categoryList_.categories(self.name_, self.coreCategories_, self.addinCategories_):
            categoryIncludes = cat.includeList(LOOP_INCLUDES)
            buf = self.bufferIncludes_.text() % {
                'categoryIncludes' : categoryIncludes }
            for func in cat.functions(self.name_):
                buf += self.generateFunction(func)
            fileName = '%sfunctions/%s.cpp' % (
                self.rootPath_, cat.name())
            outputfile.OutputFile(self, fileName, cat.copyright(), buf)

    def indexOfCol(self, str):
        """Convert an Excel column ID to an int"""
        return reduce(lambda x, y: 26 * x + y, map(COL_TO_NUM.index, str.lower()))

    def cellNameConflict(self, funcName):
        """Return a boolean indicating whether the given function name
        conflicts with a cell range name in Excel."""
        m = CELL_NAME_REGEX.match(funcName.upper())
        return m \
        and self.indexOfCol(m.group('colLabel')) <= CELL_MAX_COL_NUM \
        and int(m.group('rowLabel')) <= CELL_MAX_ROW_NUM

    def xlWizardCheck(self, func):
        """Test whether the function should check for the Function Wizard
        and if so return the relevant code snippet."""
        if func.calcInWizard():
            return ''
        else:
            return ExcelAddin.XL_WIZARD_CHECK

    def generateFunction(self, func):
        """Generate source code for a given function."""
        if func.parameterList().parameterCount() > MAXPARAM:
            raise excelexceptions.ExcelParameterCountException(
                func.name(), func.parameterList().parameterCount(), MAXPARAM)
        if self.cellNameConflict(func.name()):
            raise excelexceptions.ExcelCellNameException(
                func.name(), CELL_MAX_COL_ID, CELL_MAX_ROW_NUM)

        if func.dependencyTrigger():
            xlTrigger = ExcelAddin.VALIDATE_TRIGGER
        else:
            xlTrigger = ''

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
            'xlTrigger' : xlTrigger,
            'xlWizardCheck' : self.xlWizardCheck(func) }

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
            raise excelexceptions.ExcelParameterLengthException(
                func.name(), paramNames, MAX_LEN_PARAMLIST)

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
        # - If the function has no parameters then the function description will be
        #   the last parameter and we pad it here.
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

        # Confirm that parameter descriptions don't exceed max Excel string length.
        for param in func.parameterList().parameters():
            self.checkLen(param.description())

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
        """Write to disk the buffer for registering functions."""
        registerBuffer = self.bufferRegisterFile_.text() % {
            'categoryName' : categoryName.capitalize(),
            'registerCode' : registerCode,
            'unregisterCode' : unregisterCode }
        registerFile = "%sregister/register_%s.cpp" % (
            self.rootPath_, categoryName)
        outputfile.OutputFile(self, registerFile, self.copyright_, registerBuffer)

    def generateRegisterFunctions(self, cat):
        """Generate the code for registering addin functions with Excel."""
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
        for cat in self.categoryList_.categories(self.name_, self.coreCategories_, self.addinCategories_, supportedplatform.MANUAL):
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
        registerCall = self.rootPath_ + 'register/register_all.cpp'
        outputfile.OutputFile(self, registerCall, self.copyright_, registerCallBuffer)

    def generateExportSymbols(self):
        """Generate directives that cause exported symbols to be available to
        clients of this Addin."""
        exportSymbols = ''
        for cat in self.categoryList_.categories(self.name_, self.coreCategories_, self.addinCategories_, supportedplatform.MANUAL):
            for func in cat.functions(self.name_, supportedplatform.MANUAL):
                exportSymbols += '#pragma comment (linker, "/export:_%s")\n' % func.name()
        buf = self.exportStub_.text() % exportSymbols
        fileName = self.rootPath_ + 'functions/export.hpp'
        outputfile.OutputFile(self, fileName, self.copyright_, buf)

    def generateFunctionCount(self):
        """Generate a header indicating the number of functions in this addin."""
        buf = self.bufferNumFunc_.text() % self.functionCount_
        fileName = self.rootPath_ + 'functions/functioncount.hpp'
        outputfile.OutputFile(self, fileName, self.copyright_, buf)

    def printDebug(self):
        """Write debug info to stdout."""
        self.xlRegisterParam_.printDebug()

    def functionDeclaration(self):
        """Return the RuleGroup object named functionDeclaration which was loaded
        from the XML rule metadata for this addin."""
        return self.functionDeclaration_

    def functionReturnType(self):
        """Return the RuleGroup object named functionReturnType which was loaded
        from the XML rule metadata for this addin."""
        return self.functionReturnType_

    def xlRegisterParam(self):
        """Return the RuleGroup object named xlRegisterParam which was loaded
        from the XML rule metadata for this addin."""
        return self.xlRegisterParam_

    def xlRegisterReturn(self):
        """Return the RuleGroup object named xlRegisterReturn which was loaded
        from the XML rule metadata for this addin."""
        return self.xlRegisterReturn_

    def parameterList(self):
        """Return the RuleGroup object named parameterList which was loaded
        from the XML rule metadata for this addin."""
        return self.parameterList_

    def registerParameters(self):
        """Return the RuleGroup object named registerParameters which was loaded
        from the XML rule metadata for this addin."""
        return self.registerParameters_

    def cppConversions(self):
        """Return the RuleGroup object named cppConversions which was loaded
        from the XML rule metadata for this addin."""
        return self.cppConversions_

    def libraryConversions(self):
        """Return the RuleGroup object named libraryConversions which was loaded
        from the XML rule metadata for this addin."""
        return self.libraryConversions_

    def enumConversions(self):
        """Return the RuleGroup object named enumConversions which was loaded
        from the XML rule metadata for this addin."""
        return self.enumConversions_

    def objectConversions(self):
        """Return the RuleGroup object named objectConversions which was loaded
        from the XML rule metadata for this addin."""
        return self.objectConversions_

    def referenceConversions(self):
        """Return the RuleGroup object named referenceConversions which was loaded
        from the XML rule metadata for this addin."""
        return self.referenceConversions_

    def returnConversion(self):
        """Return the RuleGroup object named returnConversion which was loaded
        from the XML rule metadata for this addin."""
        return self.returnConversion_

    def loopName(self, param):
        """Return the name of the given parameter as required for loop code - in
        this case no conversion is performed."""
        return param.name()

    def idStrip(self, parameterList):
        """Return a snippet of code comprising the list of parameters,
        where the Excel cell update counter suffix has been removed
        from object IDs."""
        return ExcelAddin.ID_STRIP % parameterList.generate(self.idStrip_)

    #############################################
    # serializer interface
    #############################################

    def serialize(self, serializer):
        """Load/unload class state to/from serializer object."""
        super(ExcelAddin, self).serialize(serializer)
        serializer.serializeBoolean(self, 'exportSymbols')

