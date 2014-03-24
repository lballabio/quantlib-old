
import shutil
import re

OLD_VER = "vc10"
NEW_VER = "vc11"
ROOT_DIR = "C:/Users/erik/Documents/repos/quantlib.R01040x-branch/"

FILES = (

    "log4cxx/msvc/log4cxx_vcxx.sln",
    "log4cxx/msvc/apr_vcxx.vcxproj.filters",
    "log4cxx/msvc/apr_vcxx.vcxproj",
    "log4cxx/msvc/aprutil_vcxx.vcxproj.filters",
    "log4cxx/msvc/aprutil_vcxx.vcxproj",
    "log4cxx/msvc/log4cxx_vcxx.vcxproj.filters",
    "log4cxx/msvc/log4cxx_vcxx.vcxproj",

    "ObjectHandler/ObjectHandler_vcxx.sln",
    "ObjectHandler/Docs/docs_vcxx.vcxproj.filters",
    "ObjectHandler/Docs/docs_vcxx.vcxproj",
    #"ObjectHandler/Examples/C++/ExampleCpp_vcxx.vcxproj.filters",
    "ObjectHandler/Examples/C++/ExampleCpp_vcxx.vcxproj",
    "ObjectHandler/Examples/ExampleObjects/ExampleObjects_vcxx.vcxproj.filters",
    "ObjectHandler/Examples/ExampleObjects/ExampleObjects_vcxx.vcxproj",
    #"ObjectHandler/Examples/xl/ExampleXllDynamic1_vcxx.vcxproj.filters",
    "ObjectHandler/Examples/xl/ExampleXllDynamic1_vcxx.vcxproj",
    #"ObjectHandler/Examples/xl/ExampleXllDynamic2_vcxx.vcxproj.filters",
    "ObjectHandler/Examples/xl/ExampleXllDynamic2_vcxx.vcxproj",
    #"ObjectHandler/Examples/xl/ExampleXllStatic_vcxx.vcxproj.filters",
    "ObjectHandler/Examples/xl/ExampleXllStatic_vcxx.vcxproj",
    "ObjectHandler/gensrc/ohgensrc_vcxx.vcxproj.filters",
    "ObjectHandler/gensrc/ohgensrc_vcxx.vcxproj",
    "ObjectHandler/ohlib_vcxx.vcxproj.filters",
    "ObjectHandler/ohlib_vcxx.vcxproj",
    "ObjectHandler/ohxl/ohxll/ohxll_vcxx.vcxproj.filters",
    "ObjectHandler/ohxl/ohxll/ohxll_vcxx.vcxproj",
    "ObjectHandler/ohxl/ohxllib/ohxllib_vcxx.vcxproj.filters",
    "ObjectHandler/ohxl/ohxllib/ohxllib_vcxx.vcxproj",
    #"ObjectHandler/xlsdk/xlsdk_vcxx.vcxproj.filters",
    "ObjectHandler/xlsdk/xlsdk_vcxx.vcxproj",

    "QuantLibAddin/QuantLibAddin_vcxx.sln",
    "QuantLibAddin/Addins/Cpp/AddinCpp_vcxx.vcxproj.filters",
    "QuantLibAddin/Addins/Cpp/AddinCpp_vcxx.vcxproj",
    #"QuantLibAddin/Clients/Cpp/ClientCppDemo_vcxx.vcxproj.filters",
    "QuantLibAddin/Clients/Cpp/ClientCppDemo_vcxx.vcxproj",
    #"QuantLibAddin/Clients/CppInstrumentIn/CppInstrumentIn_vcxx.vcxproj.filters",
    "QuantLibAddin/Clients/CppInstrumentIn/CppInstrumentIn_vcxx.vcxproj",
    #"QuantLibAddin/Clients/CppSwapOut/ClientCppSwapOut_vcxx.vcxproj.filters",
    "QuantLibAddin/Clients/CppSwapOut/ClientCppSwapOut_vcxx.vcxproj",
    "QuantLibAddin/Docs/docs-QuantLibAddin_vcxx.vcxproj.filters",
    "QuantLibAddin/Docs/docs-QuantLibAddin_vcxx.vcxproj",
    "QuantLibAddin/gensrc/qlgensrc_vcxx.vcxproj.filters",
    "QuantLibAddin/gensrc/qlgensrc_vcxx.vcxproj",
    "QuantLibAddin/QuantLibObjects_vcxx.vcxproj.filters",
    "QuantLibAddin/QuantLibObjects_vcxx.vcxproj",

    "QuantLibXL/QuantLibAllDynamic_vcxx.sln",
    "QuantLibXL/QuantLibXL_basic_vcxx.sln",
    "QuantLibXL/QuantLibXL_full_vcxx.sln",
    "QuantLibXL/Docs/docs-QuantLibXL_vcxx.vcxproj.filters",
    "QuantLibXL/Docs/docs-QuantLibXL_vcxx.vcxproj",
    "QuantLibXL/qlxl/QuantLibXLDynamic_vcxx.vcxproj.filters",
    "QuantLibXL/qlxl/QuantLibXLDynamic_vcxx.vcxproj",
    "QuantLibXL/qlxl/QuantLibXLStatic_vcxx.vcxproj.filters",
    "QuantLibXL/qlxl/QuantLibXLStatic_vcxx.vcxproj",
)

for fileName in FILES:
    fileNameOld = re.sub("vcxx", OLD_VER, fileName)
    fileNameNew = re.sub("vcxx", NEW_VER, fileName)
    filePathOld = ROOT_DIR + fileNameOld
    filePathNew = ROOT_DIR + fileNameNew
    print filePathNew
    shutil.copy(filePathOld, filePathNew)
    if ".vcxproj.filters" == fileName[-16:]:
        continue
    f = open(filePathNew, 'r+')
    buf = f.read()
    buf = re.sub(OLD_VER, NEW_VER, buf)
    f.seek(0)
    f.write(buf)
    f.close()

