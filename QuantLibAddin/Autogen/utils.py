'utilities'

import common
import time
import sys
import os

def timeStamp():
	moduleName = os.path.basename(sys.argv[0])
	s = '// this file generated automatically by ' + moduleName + \
	' on ' + time.asctime() + '\n' + \
	'// editing this file manually is not recommended\n\n'
	return s

def loadCopyright():
	copyFile = open(common.CR_FILENAME)
	common.CR_BUFFER = copyFile.read()
	copyFile.close()

def init():
	loadCopyright()
