'utilities'

import common
import time
import sys
import os

def generateParamList(params, prefix = '', suffix = ''):
	paramList = ''
	i = 0
	for param in params:
		paramList += prefix + param[common.NAME]
		i += 1
		if i < len(params):
			paramList += ',' + suffix
	return paramList

def printTimeStamp(fileBuf, commentChar):
	fileBuf.write(common.HEADER	\
		% (commentChar, os.path.basename(sys.argv[0]), time.asctime(), commentChar))
	
def printHeader(fileBuf):
	fileBuf.write(common.CR_BUFFER)
	printTimeStamp(fileBuf, '//')

def loadCopyright():
	copyFile = open(common.CR_FILENAME)
	common.CR_BUFFER = copyFile.read()
	copyFile.close()

def init():
	loadCopyright()
