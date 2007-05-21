import os
import shutil
import glob

SOURCE_TARGET_DIRS = (
    ( "X:/Apps/Appsscript/CabotoXL/01 Production/xll", "Addins/01 Production" ),
    ( "X:/Apps/Appsscript/CabotoXL/02 Pre-Production/xll", "Addins/02 Pre-Production" ),
    ( "X:/Apps/Appsscript/CabotoXL/03 Testing/xll", "Addins/03 Testing" ),
)

#SOURCE_TARGET_DIRS = (
#    ( "C:/erik/projects/trunk/QuantLibXL/xll", "Addins/01 Production" ),
#    ( "C:/erik/projects/trunk/QuantLibXL/xll", "Addins/02 Pre-Production" ),
#    ( "C:/erik/projects/trunk/QuantLibXL/xll", "Addins/03 Testing" ),
#)

DELETE_PATTERN = "QuantLibXL-*.xll"
COPY_PATTERN = "QuantLibXL-vc80-mt-s-*.xll"
#COPY_PATTERN = "QuantLibXL-*.xll"

def deleteGlob(path):
    for file in glob.glob(path):
        print "deleting file\n" + file
        os.unlink(file)

def copyGlob(sourcePattern, targetPath):
    for sourceFile in glob.glob(sourcePattern):
        print "copy from =\n" + sourceFile
        print "copy to =\n" + targetPath
        shutil.copy(sourceFile, targetPath)

for (sourceDir, targetDir) in SOURCE_TARGET_DIRS:
    targetDir = os.getcwd() + "/" + targetDir
    deleteGlob(targetDir + "/" + DELETE_PATTERN)
    copyGlob(sourceDir + "/" + COPY_PATTERN, targetDir)

raw_input('copy complete. press any key to exit.')

