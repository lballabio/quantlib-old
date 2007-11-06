# Script to update the XLLs delivered by the launcher.  Invoked via one of two wrappers:
# updateLocal.py   - to get the files from your local copy of subversion
# updateNetwork.py - to get the files from the network

import sys
import os
import shutil
import glob

COPY_MESSAGE = 'copy from:\n%s\ncopy to:\n%s\n'

# Current working dir with standardized delimiters
def getcwd():
    return os.getcwd().replace('\\', '/')

def deleteGlob(path):
    for file in glob.glob(path):
        file = file.replace('\\', '/')
        print "deleting file\n" + file
        os.unlink(file)

def update(list):

    print "\nDeleting old copies of XLLs...\n"

    cwd = getcwd()

    for sourceDir, targetDir, xllName in list:
        deleteGlob(cwd + "/" + targetDir + "/*.xll")

    print "\nDone.\n"

    print "Copying XLLs...\n"

    for (sourceDir, targetDir, xllName) in list:
        sourceFile = sourceDir + "/" + xllName
        if not os.path.exists(sourceFile):
            raw_input("Error - nonexistent file:\n" + sourceFile)
            sys.exit()
        targetFile = cwd + "/" + targetDir + "/" + xllName
        print COPY_MESSAGE % (sourceFile, targetFile)
        shutil.copy(sourceFile, targetFile)

    raw_input('Done, press any key to exit.\n')

