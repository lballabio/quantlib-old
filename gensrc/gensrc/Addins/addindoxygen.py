
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

"""generate doxygen documentation files."""

from gensrc.Addins import addin
from gensrc.Functions import function
from gensrc.Utilities import outputfile
from gensrc.Utilities import common
from gensrc.Utilities import log
from gensrc.Categories import category
from gensrc.Configuration import environment

class AddinDoxygen(addin.Addin):
    """Generate doxygen documentation files."""

    # constants

    LINE_ENUM = '    <tr><td>%s</td><td>%s</td></tr>\n'
    LINE_REF_CLASS =     '        \\ref enum_class_%d\\n\n'
    LINE_REF_TYPE =      '        \\ref enum_type_%d\\n\n'
    LINE_SECTION_CLASS = '    \\section enum_class_%d %s\n'
    LINE_SECTION_TYPE =  '    \\section enum_type_%d %s\n'
    LINE_TABLE = """    <table>\n
    <tr><td><b>String</b></td><td><b>%s</b></td></tr>\n"""

    def generate(self, categoryList, enumerationList):
        """Generate doxygen documentation files."""

        self.categoryList_ = categoryList
        self.enumerationList_ = enumerationList

        log.Log.getInstance().logMessage(' begin generating Doxygen ...')
        self.generateDocs()
        if environment.config().usingEnumerations:
            self.generateEnums()
        self.generateCategoryDoc()
        log.Log.getInstance().logMessage(' done generating Doxygen.')

    def generateEnum(self, enumeration, i, line1, line2):
        ret = line1 % (i, enumeration.type)
        ret += line2
        for enumDef in enumeration.getEnumerationDefinitions():
            ret += AddinDoxygen.LINE_ENUM % (enumDef.string, enumDef.libraryClass)
        ret += '    </table>\n\n'
        return ret

    def generateEnums(self):
        """Generate documentation for enumerations."""
        bufClassLinks = ''
        for i in xrange(len(self.enumerationList_.EnumClass)):
            bufClassLinks += AddinDoxygen.LINE_REF_CLASS % i
        bufTypeLinks = ''
        for i in xrange(len(self.enumerationList_.EnumType)):
            bufTypeLinks += AddinDoxygen.LINE_REF_TYPE % i
        bufClassDocs = ''
        i = 0
        for enumeration in self.enumerationList_.enumeratedClasses():
            bufClassDocs += self.generateEnum(enumeration, i, 
                AddinDoxygen.LINE_SECTION_CLASS, AddinDoxygen.LINE_TABLE % 'Class')
            i += 1
        bufTypeDocs = ''
        i = 0
        for enumeration in self.enumerationList_.enumeratedTypes():
            bufTypeDocs += self.generateEnum(enumeration, i, 
                AddinDoxygen.LINE_SECTION_TYPE, AddinDoxygen.LINE_TABLE % 'Type')
            i += 1
        buf = self.bufferEnumerations.text % {
            'classLinks' : bufClassLinks,
            'typeLinks' : bufTypeLinks,
            'classDocs' : bufClassDocs,
            'typeDocs' : bufTypeDocs }
        fileName = self.rootDirectory + 'enums.docs'
        outputfile.OutputFile(self, fileName, 
            self.enumerationList_.enumTypeCopyright, buf)

    def generateFunctionDoc(self, func):
        """Generate documentation for given function."""
        bufParam = ''
        for param in func.ParameterList.Parameters:
            bufParam += '\\param %s %s\n' % (param.name, param.description)
        return self.bufferFunction.text % {
            'functionName' : func.name,
            'retCode' : self.functionReturn.apply(func.returnValue),
            'functionDoc' : func.ParameterList.generate(self.functionDocs),
            'functionLongDesc' : func.longDescription,
            'paramDoc' : bufParam }

    def generateCategoryDoc(self):
        """Generate page listing function categories."""
        # ensure list of links is sorted alphabetically by display name
        dispNmToCatNm = {}
        displayNames = []
        for cat in self.categoryList_.categories('*'):
            dispNmToCatNm[cat.displayName] = cat.name
            displayNames.append(cat.displayName)
        displayNames.sort()
        bufCat = ''
        for displayKey in displayNames:
            bufCat += '    \\ref %s\\n\n' % dispNmToCatNm[displayKey]
        buf = self.bufferCategories.text % { 'categories' : bufCat }
        fileName = self.rootDirectory + 'categories.docs'
        outputfile.OutputFile(self, fileName, self.copyright, buf)

    def generateFunctionList(self, allFuncs):
        """Generate alphabetical list of links to all functions."""
        allFuncs.sort()
        bufList = ''
        for func in allFuncs:
            bufList += '\\ref %s ()\\n\n' % func
        buf = self.bufferHeader.text % {
            'count' : len(allFuncs),
            'list' : bufList }
        fileName = self.rootDirectory + 'all.docs'
        outputfile.OutputFile(self, fileName, self.copyright, buf)

    def generateDocs(self):
        """Generate doxygen documentation files."""
        allFuncs = []
        for cat in self.categoryList_.categories('*'):
            bufLink = ''
            bufDoc = ''
            for func in cat.getFunctions('*'): 
                bufLink += '\\ref %s ()\\n\n' % func.name
                bufDoc += self.generateFunctionDoc(func)
                allFuncs.append(func.name)
            buf = self.bufferFile.text % {
                'categoryDescription' : cat.description,
                'categoryDisplayName' : cat.displayName,
                'categoryName' : cat.name,
                'documentation' : bufDoc,
                'links' : bufLink }
            fileName = self.rootDirectory + cat.name + '.docs'
            outputfile.OutputFile(self, fileName, cat.copyright, buf)
        self.generateFunctionList(allFuncs)

