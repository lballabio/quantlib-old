import os
import shutil
import glob

SOURCE_TARGET_DIRS = (
    ( "C:/Projects/QuantLibSVN/DevCycle/Rev10747/QuantLibXL/xll", "Addins/01 Production" ),
    ( "C:/Projects/QuantLibSVN/DevCycle/Rev11190/QuantLibXL/xll", "Addins/02 Pre-Production" ),
    ( "C:/Projects/QuantLibSVN/DevCycle/Rev11705/QuantLibXL/xll", "Addins/03 Testing" ),
)

#SOURCE_TARGET_DIRS = (
#    ( "C:/katiuscia/Projects/QuantLibSVN/DevCycle/Rev10747/QuantLibXL/xll", "Addins/01 Production" ),
#    ( "C:/katiuscia/Projects/QuantLibSVN/DevCycle/Rev11190/QuantLibXL/xll", "Addins/02 Pre-Production" ),
#    ( "C:/katiuscia/Projects/QuantLibSVN/DevCycle/Rev11705/QuantLibXL/xll", "Addins/03 Testing" ),
#)

DELETE_PATTERN = "*.xll"
COPY_PATTERN = "*.xll"

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
