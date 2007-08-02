import sys
import os
import re
import shutil
import glob

SOURCE_TARGET_LIST = (
    ( "X:/Apps/Appsscript/CabotoXL/Rev11705/xll", "Addins/01 Production", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev11705/xll", "Addins/01 Production", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev11705/xll", "Addins/01 Production", "saohxll-vc80-mt-0_1_9.xll" ),

    ( "X:/Apps/Appsscript/CabotoXL/Rev11940/xll", "Addins/02 Pre-Production", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev11940/xll", "Addins/02 Pre-Production", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev11940/xll", "Addins/02 Pre-Production", "saohxll-vc80-mt-0_1_9.xll" ),

    ( "X:/Apps/Appsscript/CabotoXL/Rev11940/xll", "Addins/03 Testing", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev11940/xll", "Addins/03 Testing", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
    ( "X:/Apps/Appsscript/CabotoXL/Rev11940/xll", "Addins/03 Testing", "saohxll-vc80-mt-0_1_9.xll" ),
)

#SOURCE_TARGET_LIST = (
#    ( "C:/erik/projects/trunk/QuantLibXL/xll",                  "Addins/01 Production", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
#    ( "C:/erik/projects/trunk/ObjectHandler/xll",               "Addins/01 Production", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
#    ( "C:/erik/projects/trunk/SensitivityAnalysis/saohxll/xll", "Addins/01 Production", "saohxll-vc80-mt-0_1_9.xll" ),
#    ( "C:/erik/projects/trunk/QuantLibXL/xll",                  "Addins/02 Pre-Production", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
#    ( "C:/erik/projects/trunk/ObjectHandler/xll",               "Addins/02 Pre-Production", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
#    ( "C:/erik/projects/trunk/SensitivityAnalysis/saohxll/xll", "Addins/02 Pre-Production", "saohxll-vc80-mt-0_1_9.xll" ),
#    ( "C:/erik/projects/trunk/QuantLibXL/xll",                  "Addins/03 Testing", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
#    ( "C:/erik/projects/trunk/ObjectHandler/xll",               "Addins/03 Testing", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
#    ( "C:/erik/projects/trunk/SensitivityAnalysis/saohxll/xll", "Addins/03 Testing", "saohxll-vc80-mt-0_1_9.xll" ),
#)

COPY_MESSAGE = 'copy from:\n%s\ncopy to:\n%s\n'

def deleteGlob(path):
    for file in glob.glob(path):
        print "deleting file\n" + file
        os.unlink(file)

cwd = re.sub(r'\\', '/', os.getcwd())

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

