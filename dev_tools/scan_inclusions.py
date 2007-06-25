import os, sys, re, string

import xml.dom.minidom
import xml.dom.ext


QL_ROOT = "C:/Projects/QuantLibSVN/trunk/"
VC8 = "C:/Program Files/Microsoft Visual Studio 8/"
BOOST = "C:/Boost/boost_1_33_1/"

QL = QL_ROOT +"QuantLib/"
QL_ADDIN = QL_ROOT + "QuantLibAddin/"
OBJECT_HANDLER = QL_ROOT + "ObjectHandler/"
QL_XL = QL_ROOT + "QuantLibXL/"

STD = VC8 + "VC/include/"
SDK = VC8 + "VC/PlatformSDK/Include"


INCLUDE_PATH = [QL, QL_ADDIN, OBJECT_HANDLER, QL_XL, BOOST, STD, SDK]
PREFIX_PATH = ["ql", "qlo", "oh", "boost", "qlxl", "ohxl", "xlsdk"]

class MyError(Exception):
     def __init__(self, value):
        self.value = value
     def __str__(self):
        return repr(self.value)

def searchAndParseHeaderFile(fileName):
    for includePath in INCLUDE_PATH:
        filePath = includePath + fileName[0].lower() + fileName[1:]
        if os.path.isfile(filePath):
            return parseHeaderFile(filePath)
        filePath = includePath + fileName[0].upper() + fileName[1:]
        if os.path.isfile(filePath):
            return parseHeaderFile(filePath)
    raise MyError("searchAndParseHeaderFile: " + fileName + " not found")

 
def getFilePrefix(include):
    for prefix in PREFIX_PATH:
        if re.match(prefix + '/.*',include):
            return prefix
    return "std"
        
def parseHeaderFile(filePath):
    includes = []
    nbLines = 0
    f=open(filePath)
    for line in f:
        nbLines +=1
        if not re.match("//", line):
            includesLines = re.findall('^#include.*<.*>', line)
            if includesLines:
                includeName = re.findall('<.*>', includesLines[0])[0][1:-1]
                includes.append(includeName)
    f.close()                    
    return includes, nbLines


def walkThroughIncludesFiles(fileName, files, filesCounters, node, document):
    new = document.createElement('header')
    node.appendChild(new)
    parsingResults = searchAndParseHeaderFile(fileName)
    includes = parsingResults[0]
    attribute = "%i" % parsingResults[1]
    new.setAttribute('nbLines', attribute)
    nbLines = parsingResults[1]

    for include in includes:
        #if the son is not recorded yet we explore it
        include = "%s" % include
        if not files.count(include) > 0:
            files.append(include)
            try:
                prefix = getFilePrefix(include)
                filesCounters[prefix][0] +=1
                result = walkThroughIncludesFiles(include, files, filesCounters, new, document)
                nbLines += result[0]
                filesCounters[prefix][1] += result[1]
            except MyError, e:
				print e.value, " in : " + fileName
    attribute = "%i" % nbLines
    new.setAttribute('total', attribute)
    new.setAttribute('name', fileName)
    return int(nbLines), parsingResults[1]
    
def trackDependencies(fileName):
    document = xml.dom.minidom.Document()
    filesCounters = {}
    filesCounters["boost"] = [0,0]
    filesCounters["ql"] = [0,0]
    filesCounters["qlo"] = [0,0]
    filesCounters["qlxl"] = [0,0]
    filesCounters["oh"] = [0,0]
    filesCounters["ohxl"] = [0,0]
    filesCounters["xlsdk"] = [0,0]
    filesCounters["std"] = [0,0]
    files = []
    files.append(fileName)
    nbLines = walkThroughIncludesFiles(fileName, files, filesCounters, document, document)
    return filesCounters, document, nbLines, files

if __name__ == '__main__':
	if len(sys.argv) != 2:
		print 'Give the relative path of the file you want to scan (wrt to the included folders)'
		sys.exit()
	args = sys.argv[1:]
	fileName = args[0]

	result = trackDependencies(fileName)
	nbLinesParsed = result[2][0]
	print "number of files parsed ", len(result[3])
	print "number of lines parsed ", nbLinesParsed
	namespaces = result[0]
	for namespace in namespaces:
		print namespace, ":\tnb Files ", namespaces[namespace][0]
		print "\tnb lines ", namespaces[namespace][1]
		print "\t%(nbLines)02d" % {'nbLines': float(namespaces[namespace][1])/nbLinesParsed * 100}, "%"
		
	outputName = fileName.replace("/", "-") + ".xml"
	output = "./" + outputName
	f=open(output, 'w')
	xml.dom.ext.PrettyPrint(result[1], f)
	f.close()
	print "result saved in ", outputName
    