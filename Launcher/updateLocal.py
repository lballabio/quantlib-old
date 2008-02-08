# Script to update the XLLs delivered by the Launcher
# from files in the local copy of the subversion repository

import update

# Our subpath from svn trunk - must be updated if this script is moved
RELATIVE_PATH = 'Launcher'

# THe current directory, e.g. C:/projects/trunk/Launcher
cwd = update.getcwd()

# The absolute path to the svn trunk, e.g. C:/projects/trunk/
trunkPath = cwd[0:len(cwd)-len(RELATIVE_PATH)]

SOURCE_TARGET_LIST = (
    ( trunkPath + "QuantLibXL/xll",                  "Addins/01 Production", "QuantLibXLDynamic-vc80-mt-0_9_5.xll" ),
    ( trunkPath + "ObjectHandler/xll",               "Addins/01 Production", "ObjectHandler-xll-vc80-mt-0_9_5.xll" ),
    ( trunkPath + "SensitivityAnalysis/xll", "Addins/01 Production", "saohxll-vc80-mt-0_9_5.xll" ),
    ( trunkPath + "SensitivityAnalysis/xll", "Addins/01 Production", "PDGLib_candidate.xll" ),

    ( trunkPath + "QuantLibXL/xll",                  "Addins/02 Pre-Production", "QuantLibXLDynamic-vc80-mt-0_9_5.xll" ),
    ( trunkPath + "ObjectHandler/xll",               "Addins/02 Pre-Production", "ObjectHandler-xll-vc80-mt-0_9_5.xll" ),
    ( trunkPath + "SensitivityAnalysis/xll", "Addins/02 Pre-Production", "saohxll-vc80-mt-0_9_5.xll" ),
    ( trunkPath + "SensitivityAnalysis/xll", "Addins/02 Pre-Production", "PDGLib_candidate.xll" ),

    ( trunkPath + "QuantLibXL/xll",                  "Addins/03 Testing", "QuantLibXLDynamic-vc80-mt-0_9_5.xll" ),
    ( trunkPath + "ObjectHandler/xll",               "Addins/03 Testing", "ObjectHandler-xll-vc80-mt-0_9_5.xll" ),
    ( trunkPath + "SensitivityAnalysis/xll", "Addins/03 Testing", "saohxll-vc80-mt-0_9_5.xll" ),
    ( trunkPath + "SensitivityAnalysis/xll", "Addins/03 Testing", "PDGLib_candidate.xll" ),
)

update.update(SOURCE_TARGET_LIST)

