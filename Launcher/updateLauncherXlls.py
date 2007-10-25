import sys
import os
import shutil
import glob

# Current working dir
cwd = os.getcwd().replace('\\', '/')
# Our subpath from svn trunk - must be updated if this script is moved
relativePath = 'Launcher'
# The path to the trunk directory
trunkPath = cwd[0:len(cwd)-len(relativePath)]

# Use these values to update from production
SOURCE_TARGET_LIST = (
    ( "X:/Apps/Appsscript/CabotoXL/Rev12288/xll", "Addins/01 Production", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev12288/xll", "Addins/01 Production", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev12288/xll", "Addins/01 Production", "saohxll-vc80-mt-0_1_9.xll" ),

    ( "X:/Apps/Appsscript/CabotoXL/Rev12939/xll", "Addins/02 Pre-Production", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev12939/xll", "Addins/02 Pre-Production", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev12939/xll", "Addins/02 Pre-Production", "saohxll-vc80-mt-0_1_9.xll" ),

    ( "X:/Apps/Appsscript/CabotoXL/Rev13059/xll", "Addins/03 Testing", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev13059/xll", "Addins/03 Testing", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev13059/xll", "Addins/03 Testing", "saohxll-vc80-mt-0_1_9.xll" ),
)

# Use these values to update from subversion
#SOURCE_TARGET_LIST = (
#    ( trunkPath + "QuantLibXL/xll",                  "Addins/01 Production", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
#    ( trunkPath + "ObjectHandler/xll",               "Addins/01 Production", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
#    ( trunkPath + "SensitivityAnalysis/saohxll/xll", "Addins/01 Production", "saohxll-vc80-mt-0_1_9.xll" ),
#    ( trunkPath + "QuantLibXL/xll",                  "Addins/02 Pre-Production", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
#    ( trunkPath + "ObjectHandler/xll",               "Addins/02 Pre-Production", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
#    ( trunkPath + "SensitivityAnalysis/saohxll/xll", "Addins/02 Pre-Production", "saohxll-vc80-mt-0_1_9.xll" ),
#    ( trunkPath + "QuantLibXL/xll",                  "Addins/03 Testing", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
#    ( trunkPath + "ObjectHandler/xll",               "Addins/03 Testing", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
#    ( trunkPath + "SensitivityAnalysis/saohxll/xll", "Addins/03 Testing", "saohxll-vc80-mt-0_1_9.xll" ),
#)

COPY_MESSAGE = 'copy from:\n%s\ncopy to:\n%s\n'

def deleteGlob(path):
    for file in glob.glob(path):
        file = file.replace('\\', '/')
        print "deleting file\n" + file
        os.unlink(file)

print "\nDeleting old copies of XLLs...\n"

for sourceDir, targetDir, xllName in SOURCE_TARGET_LIST:
    deleteGlob(cwd + "/" + targetDir + "/*.xll")

print "\nDone.\n"

print "Copying XLLs...\n"

for (sourceDir, targetDir, xllName) in SOURCE_TARGET_LIST:
    sourceFile = sourceDir + "/" + xllName
    if not os.path.exists(sourceFile):
        raw_input("Error - nonexistent file:\n" + sourceFile)
        sys.exit()
    targetFile = cwd + "/" + targetDir + "/" + xllName
    print COPY_MESSAGE % (sourceFile, targetFile)
    shutil.copy(sourceFile, targetFile)

raw_input('Done, press any key to exit.\n')

