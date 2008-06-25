# Script to update the XLLs delivered by the Launcher
# from files on the network

import update

SOURCE_TARGET_LIST = (
#    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/R000900-branch/xll", "Addins/01 Production", "QuantLibXLDynamic-vc80-mt-0_9_0.xll" ),
#    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/R000900-branch/xll", "Addins/01 Production", "ObjectHandler-xll-vc80-mt-0_9_0.xll" ),
#    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/R000900-branch/xll", "Addins/01 Production", "saohxll-vc80-mt-0_1_9.xll" ),
#    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/R000900-branch/xll", "Addins/01 Production", "PDGLib_candidate.xll" ),

    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/Rev14441/xll", "Addins/01 Production", "QuantLibXLDynamic-vc80-mt-0_9_5.xll" ),
    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/Rev14441/xll", "Addins/01 Production", "ObjectHandler-xll-vc80-mt-0_9_5.xll" ),
    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/Rev14441/xll", "Addins/01 Production", "PDGLib.xll" ),

    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/Rev14991/xll", "Addins/02 Pre-Production", "QuantLibXLDynamic-vc90-mt-0_9_5.xll" ),
    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/Rev14991/xll", "Addins/02 Pre-Production", "ObjectHandler-xll-vc90-mt-0_9_5.xll" ),
    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/Rev14991/xll", "Addins/02 Pre-Production", "PDGLib.xll" ),

    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/Rev15125/xll", "Addins/03 Testing", "QuantLibXLDynamic-vc80-mt-0_9_5.xll" ),
    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/Rev15125/xll", "Addins/03 Testing", "ObjectHandler-xll-vc80-mt-0_9_5.xll" ),
    ( "\\\\srv0001.caboto.it/risorse/Apps/Appsscript/CabotoXL/Rev15125/xll", "Addins/03 Testing", "PDGLib.xll" ),

)

update.update(SOURCE_TARGET_LIST)
