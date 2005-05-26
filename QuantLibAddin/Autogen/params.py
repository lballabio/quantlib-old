'parameters'

# this module defines a hierarchy of classes
# for reformatting a list of the parameters to a QuantLibAddin function
# into the corresponding Addin source code

import common

# constants

TYPE  = 'type'
DEREF = 'deref'

# base class to handle tasks common to all formatting requests
# e.g. indentation, line feeds

class ParameterList(object):
    'generate Addin source code corresponding list of function parameters'

    def __init__(self,
            indent,             # #/tabstops to indent 
            prefix,             # text to prefix to each parameter
            skipFirst,          # ignore 1st param (e.g. if it's an object handle)
            delimiter,          # delimiter between arguments in the list
            delimitLast):       # whether or not to append delimiter to last arg
        'initialize the object'
        self.indent = indent * 4 * ' '
        self.prefix = prefix
        self.skipFirst = skipFirst
        self.delimiter = delimiter
        self.delimitLast = delimitLast
        self.outList = []       # list of output items
        self.item = ''          # current item in list
        self.param = ''         # current function parameter
        self.idx = 0            # index of current item

    def generateCode(self, paramList):
        'generate a list of output strings given param list & initial state'
        self.outList = []
        firstItem = True
        self.idx = 0
        for self.param in paramList:
            if firstItem:
                firstItem = False
                if self.skipFirst:
                    continue
            self.item = ''
            self.process()      # defined in derived class
            self.outList.append(self.item)
            self.idx += 1
        return self.generateOutput()

    def generateOutput(self):
        'concatenate list of output items into a string'
        ret = ''
        i = 0
        for item in self.outList:
            i += 1
            ret += self.indent + self.prefix + item
            if i < len(self.outList) or self.delimitLast:
                ret += self.delimiter
        return ret

# class to format parameters into a function declaration e.g.
#   f(char *param0,
#     vector < long > param1,
#     ...) {}

class ParameterDeclare(ParameterList):
    def __init__(self,
            # base class parameters
            indent = 0,
            prefix = '',
            skipFirst = False,
            delimiter = ',\n',
            delimitLast = False,
            arrayCount = False,     # calculate array sizes (for C code)
            derefString = '',       # character to dereference strings e.g. *
            derefTensor = '',       # character to dereference vectors/matrices
            derefTensorString = '', # character to dereference string vectors/matrices
            derefOther = '',        # character to dereference other datatypes
            replaceString = '',     # text to overwrite string datatype e.g. 'char *'
            replaceLong = '',       # text to overwrite long datatype
            replaceTensor = '',     # text to overwrite vector/matrix datatype
            replaceTensorStr = '',  # text to overwrite string vector/matrix datatype
            formatString = '',      # text to reformat datatype for string params
            formatVector = '',      # text to reformat datatype for vector params
            formatMatrix = ''):     # text to reformat datatype for matrix params
        super(ParameterDeclare, self).__init__(
            indent,
            prefix,
            skipFirst,
            delimiter,
            delimitLast)
        self.arrayCount = arrayCount
        self.derefString = derefString
        self.derefTensor = derefTensor
        self.derefTensorString = derefTensorString
        self.derefOther = derefOther
        self.replaceString = replaceString
        self.replaceLong = replaceLong
        self.replaceTensor = replaceTensor
        self.replaceTensorStr = replaceTensorStr
        self.formatString = formatString
        self.formatVector = formatVector
        self.formatMatrix = formatMatrix

    def process(self):
        'generate source code given the input parameters'
        self.initializeLine()
        self.replaceType()
        if self.arrayCount:
            self.formatCount()
        else:
            self.formatType()
        self.item = self.line[TYPE] + ' ' + self.line[DEREF] + self.param[common.NAME]

    def initializeLine(self):
        'initialize line of output corresponding to current parameter'
        self.line = {}
        self.line[TYPE] = self.param[common.TYPE]
        self.line[DEREF] = ''
        # derive a value for the dereference character
        if self.param[common.TENSOR] == common.SCALAR:
            if self.derefString and self.line[TYPE] == common.STRING:
                self.line[DEREF] = self.derefString
            elif self.derefOther:
                self.line[DEREF] = self.derefOther
        else:
            if self.derefTensorString and self.line[TYPE] == common.STRING:
                self.line[DEREF] = self.derefTensorString
            elif self.derefTensor:
                self.line[DEREF] = self.derefTensor

    def replaceType(self):
        'overwrite datatype of parameter if indicated by class state variables'
        if self.replaceTensorStr \
                and self.param[common.TENSOR] != common.SCALAR \
                and self.param[common.TYPE] == common.STRING:
            self.line[TYPE] = self.replaceTensorStr
        elif self.replaceTensor and self.param[common.TENSOR] != common.SCALAR:
            self.line[TYPE] = self.replaceTensor
        elif self.replaceString and self.param[common.TYPE] == common.STRING:
            self.line[TYPE] = self.replaceString
        elif self.replaceLong and self.param[common.TYPE] == common.LONG:
            self.line[TYPE] = self.replaceLong

    def formatType(self):
        'reformat datatype of parameter if indicated'
        if self.formatString and self.param[common.TYPE] == common.STRING:
            self.line[TYPE] = self.formatString % self.line[TYPE]
        elif self.formatVector and self.param[common.TENSOR] == common.VECTOR:
            self.line[TYPE] = self.formatVector % self.line[TYPE]
        elif self.formatMatrix and self.param[common.TENSOR] == common.MATRIX:
            self.line[TYPE] = self.formatMatrix % self.line[TYPE]

    def formatCount(self):
        'insert additional variables for vector/matrix dimensions'
        if self.param[common.TENSOR] == common.VECTOR:
            numRows = 'long ' + self.param[common.NAME] + 'Size'
            self.outList.insert(self.idx, numRows)
            self.idx += 1
            self.line[DEREF] += '*'
        elif self.param[common.TENSOR] == common.MATRIX:
            numRows = 'long ' + self.param[common.NAME] + 'Rows'
            self.outList.insert(self.idx, numRows)
            self.idx += 1
            numCols = 'long ' + self.param[common.NAME] + 'Cols'
            self.outList.insert(self.idx, numCols)
            self.idx += 1
            self.line[DEREF] += '**'

# class to format parameters for passing to another function e.g.
#   f(param0, std::string(param1), ...)

class ParameterPass(ParameterList):

    def __init__(self,
            # base class parameters
            indent = 0,
            prefix = '',
            skipFirst = False,
            delimiter = ',\n',
            delimitLast = False,
            derefOther = '',        # dereference for non-string datatypes
            convertString = '',     # text to convert string datatype
            wrapFormat = '',        # text to reformat entire parameter line
            appendTensor = False):  # append tensor rank (Vector/Matrix) to variable names
        super(ParameterPass, self).__init__(
            indent,
            prefix,
            skipFirst,
            delimiter,
            delimitLast)
        self.derefOther = derefOther
        self.convertString = convertString
        self.wrapFormat = wrapFormat
        self.appendTensor = appendTensor

    def process(self):
        'generate source code given the input parameters'
        self.convertValue()
        self.wrapArgument()

    def convertValue(self):
        'apply any conversion strings to parameter name'
        if self.param[common.TYPE] == common.STRING:
            name = ''
        else:
            name = self.derefOther
        name += self.param[common.NAME]
        if self.appendTensor:
            if self.param[common.TENSOR] == common.VECTOR:
                name += 'Vector'
            elif self.param[common.TENSOR] == common.MATRIX:
                name += 'Matrix'
        if self.convertString and self.param[common.TYPE] == common.STRING \
                and self.param[common.TENSOR] == common.SCALAR:
            self.item = self.convertString % name
        else:
            self.item = self.item + name

    def wrapArgument(self):
        'wrap entire parameter definition in a conversion string if provided'
        if self.wrapFormat:
            self.item = self.wrapFormat % self.item

