'output doxygen documentation files'

import common
import utils

# constants

ROOT = '../Docs/pages/'

def generateFuncDoc(fileFunc, function):
    'generate documentation for given function'
    paramList = utils.generateParamList(function[common.PARAMS], 2, True,
        convertVec = 'vector < %s >',
        convertMat = 'vector < vector < %s > >')
    fileFunc.write('\\anchor %s\n' % function[common.NAME])
    fileFunc.write('\\code\n')
    fileFunc.write('%s(' % function[common.NAME])
    if paramList != '':
        fileFunc.write('\n')
        fileFunc.write(paramList)
    fileFunc.write(')\n')
    fileFunc.write('\\endcode\n')
    fileFunc.write(function[common.DESC])
    for param in function[common.PARAMS]:
        fileFunc.write('\\param %s %s\n' % (param[common.NAME], param[common.DESC]))
    fileFunc.write('\\return %s\n\n' % function[common.RETVAL][common.DESC])

def generateDocs(functionGroups):
    'generate doxygen documentation files'
    for groupName in functionGroups.keys():
        functionGroup = functionGroups[groupName]
        fileName = ROOT + groupName + '.docs' + common.TEMPFILE
        fileDoc = file(fileName, 'w')
        utils.printHeader(fileDoc)
        fileDoc.write('/*! \page %s %s\n' % (groupName, groupName))
        fileDoc.write('\\section overview Overview\n')
        fileDoc.write('%s\n' % functionGroup[common.DESC])
        fileDoc.write('\\section functions Function List\n')
        for function in functionGroup[common.FUNCLIST]:
            fileDoc.write('\\ref %s ()\\n\n' % function[common.NAME])
        fileDoc.write('\\section documentation Function Documentation\n')
        for function in functionGroup[common.FUNCLIST]:
            generateFuncDoc(fileDoc, function)
        fileDoc.write('*/\n')
        fileDoc.close()
        utils.updateIfChanged(fileName)

def generate(functionDefs):
    'generate doxygen documentation files'
    utils.logMessage('  begin generating Doxygen ...')
    functionGroups = functionDefs[common.FUNCGROUPS]
    generateDocs(functionGroups)
    utils.logMessage('  done generating Doxygen.')

