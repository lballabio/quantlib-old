
##########################################################################
# code for Excel
##########################################################################

code11 = '''\
        std::string %(name)sCpp =
            ObjHandler::callOperToScalar<std::string>(*%(name)s, "%(name)s", %(defaultValue)s);\n'''

code12 = '''\
        %(nativeType)s %(name)sCpp =
            ObjHandler::callOperToScalar<%(nativeType)s>(*%(name)s, "%(name)s", %(defaultValue)s);\n'''

code12b = '''\
        boost::any %(name)sCpp =
            ObjHandler::callOperToScalar<boost::any>(*%(name)s, "%(name)s", boost::any());\n'''

code13 = '''\
        std::vector<%(nativeType)s> %(name)sCpp =
            ObjHandler::operToVector<%(nativeType)s>(*%(name)s, "%(name)s");\n'''

code13b = '''\
        std::vector<boost::any> %(name)sCpp =
            ObjHandler::operToVector<boost::any>(*%(name)s, "%(name)s");\n'''

code14 = '''\
        std::vector<long> %(name)sCpp =
            ObjHandler::operToVector<long>(*%(name)s, "%(name)s");\n'''

code15 = '''\
        std::vector<double> %(name)sCpp =
            ObjHandler::operToVector<double>(*%(name)s, "%(name)s");\n'''

code16 = '''\
        std::vector<std::string> %(name)sCpp =
            ObjHandler::operToVector<std::string>(*%(name)s, "%(name)s");\n'''

code17 = '''\
        std::vector<boost::any> %(name)sCpp =
            ObjHandler::operToVector<boost::any>(*%(name)s, "%(name)s");\n'''

code18 = '''\
        std::vector<std::vector<%(type)s> > %(name)sCpp =
            ObjHandler::operToMatrix<%(type)s>(*%(name)s);\n'''

code19 = '''\
        std::vector<std::vector<long> > %(name)sCpp =
            ObjHandler::fpToMatrix<long>(*%(name)s);\n'''

code20 = '''\
        std::vector<std::vector<double> > %(name)sCpp =
            ObjHandler::fpToMatrix<double>(*%(name)s);\n'''

code21a = '''\
        std::vector<std::vector<std::string> > %(name)sCpp =
            ObjHandler::operToMatrix<std::string>(*%(name)s);\n'''

code21b = '''\
        std::vector<std::vector<boost::any> > %(name)sCpp =
            ObjHandler::operToMatrix<boost::any>(*%(name)s);\n'''

code22 = '''\
        %(type)s %(name)sLib;
        ObjHandler::cppToLibrary(%(name)s, %(name)sLib);\n'''

code23 = '''\
        %(type)s %(name)sLib;
        ObjHandler::cppToLibrary(*%(name)s, %(name)sLib);\n'''

code24 = '''\
        QuantLib::Date %(name)sLib =
            ObjHandler::callOperToScalar<QuantLib::Date>(*%(name)s, "%(name)s");\n'''

code25 = '''\
        %(type)s %(name)sLib =
            ObjHandler::callOperToScalar<%(type)s>(%(name)s, "%(name)s", %(defaultValue)s);\n'''

code26 = '''\
        %(type)s %(name)sLib =
            ObjHandler::callOperToScalar<%(type)s>(*%(name)s, "%(name)s", %(defaultValue)s);\n'''

code27 = '''\
        QuantLib::Date %(name)sLib =
            ObjHandler::callOperToScalar<QuantLib::Date>(*%(name)s, "%(name)s", %(defaultValue)s);\n'''

code28 = '''\
        %(type)s %(name)sLib =
            ObjHandler::operToVector(*%(name)s, "%(name)s");\n'''

code29 = '''\
        %(type)s %(name)sLib =
            ObjHandler::operToMatrix(*%(name)s);\n'''

code30 = '''\
        std::vector<QuantLib::Date> %(name)sLib =
            ObjHandler::operToVector<QuantLib::Date>(*%(name)s, "%(name)s");\n'''

code31 = '''\
        std::vector<%(type)s> %(name)sLib =
            ObjHandler::operToVectorLibrary<%(type)s>(
            *%(name)s);\n'''

code32 = '''\
        std::vector<%(type)s> %(name)sLib =
            ObjHandler::operToVector<%(type)s>(
            *%(name)s, "%(name)s");\n'''

code33 = '''\
        %(type)s %(name)sEnum =
            ObjHandler::operToScalarEnum<%(type)s,
                %(namespaceObjects)s::Create<%(type)s> >(
                    *%(name)s, "%(name)s", %(defaultValue)s);\n'''

code34 = '''\
        %(type)s %(name)sEnum =
            %(namespaceObjects)s::Create<%(type)s>()(%(name)s);\n'''

code35 = '''\
        std::vector<%(type)s> %(name)sEnum =
            ObjHandler::operToVectorEnum<%(type)s,
            %(namespaceObjects)s::Create<%(type)s> >(*%(name)s);\n'''

code36a = '''\
        OH_GET_OBJECT(%(name)sObj, %(name)s, %(type)s)\n'''

code36b = '''\
        OH_GET_OBJECT(%(name)sObj, %(name)s, QuantLibAddin::RelinkableHandle<%(type)s>)\n'''

code37 = '''\
        std::vector<boost::shared_ptr<%(namespaceObjects)s::%(classname)s> > %(name)sObj =
            getObjectVector<%(namespaceObjects)s::%(classname)s>(%(name)sCpp);\n'''

code38 = '''\
        OH_GET_REFERENCE(%(name)sLibObj, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code39 = '''\
        OH_GET_REFERENCE_DEFAULT(%(name)sLibObj, %(name)sCpp,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code40 = '''\
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            ObjHandler::CoerceIndex<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)s);\n'''

code41 = '''\
        std::vector<boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> > %(name)sLibObj =
            ObjHandler::CoerceIndexVector<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>(
                    %(name)sCpp);\n'''

code42 = '''\
        std::vector<boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> > %(name)sLibObj =
            ObjHandler::operToObjectVector<%(namespaceLibrary)s::%(classname)s, %(namespaceObjects)s::%(classname)s>(
            *%(name)s);\n'''

code43 = '''\
        std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > %(name)sLibObj =
            ObjHandler::operToVector<QuantLib::RelinkableHandle<QuantLib::Quote> >(*%(name)s, "%(name)s");\n'''

code44 = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjHandler::Object)
        %(namespaceLibrary)s::RelinkableHandle<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            ObjHandler::CoerceHandle<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

code45a = '''\
        QuantLib::RelinkableHandle<QuantLib::Quote> %(name)sLibObj =
            ObjHandler::callOperToScalar<QuantLib::RelinkableHandle<QuantLib::Quote> >(*%(name)s, "%(name)s");\n'''

code45b = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            ObjHandler::CoerceObject<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

code46 = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            ObjHandler::CoerceTermStructure()(%(name)sTemp);\n'''

code46b = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            ObjHandler::CoerceCurve<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s,
                QuantLib::SwaptionVolatilityStructure>()(
                    %(name)sTemp);\n'''

code47 = '''\
        boost::shared_ptr<QuantLib::Quote> %(name)sLibObj =
            ObjHandler::callOperToScalar<boost::shared_ptr<QuantLib::Quote> >(*%(name)s, "%(name)s");\n'''

code48 = '''\
        OH_GET_UNDERLYING(%(name)sLibObj, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code49 = '''\
        OH_GET_UNDERLYING_NONCONST(%(name)sLibObj, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code50 = '''\
        std::vector<std::vector<%(namespaceLibrary)s::RelinkableHandle<%(namespaceLibrary)s::%(classname)s> > > %(name)sLibObj =
            ObjHandler::operToMatrixHandle<%(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s>(*%(name)s);\n'''

code51 = '''\
std::vector<std::vector<std::string> > returnValue = '''

code52 = '''\
std::vector<std::vector<%(type)s> > returnValue = '''

code53 = '''\
std::vector<std::vector<boost::any> > returnValue = '''

code54 = '''\
            %(name)sLib.begin(),
            %(name)sLib.end()'''

code55 = '''\
            %(name)sCpp.begin(),
            %(name)sCpp.end()'''

code56a = '''\
        std::string str = ObjHandler::libraryToScalar(returnValue);
        static char ret[XL_MAX_STR_LEN];
        ObjHandler::stringToChar(str, ret);
        return ret;'''
 
code56b = '''\
        static %(nativeType)s returnValueXL;
        returnValueXL = ObjHandler::libraryToScalar(returnValue);
        return &returnValueXL;'''

code57 = '''\
        std::ostringstream os;
        os << returnValue;
        static char ret[XL_MAX_STR_LEN];
        ObjHandler::stringToChar(os.str(), ret);
        return ret;'''

code58 = '''\
        static char ret[XL_MAX_STR_LEN];
        ObjHandler::stringToChar(returnValue, ret);
        return ret;'''

code59 = '''\
        static OPER xRet;
        ObjHandler::%(tensorRank)sToOper(returnValue, xRet);
        return &xRet;'''

code60 = '''\
        return &returnValue;'''

code61 = '''\
        static OPER xRet;
        ObjHandler::%(tensorRank)sToOper(returnValue, xRet);
        return &xRet;'''

code62 = '''\
        std::vector<%(nativeType)s> returnValVec = ObjHandler::libraryToVector(returnValue);
        static OPER xRet;
        ObjHandler::vectorToOper(returnValVec, xRet);
        return &xRet;'''

code63 = '''\
        std::vector<std::string> returnValVec = ObjHandler::libraryToVector(returnValue);
        static OPER xRet;
        ObjHandler::vectorToOper(returnValVec, xRet);
        return &xRet;'''

code64 = '''\
        static OPER xRet;
        ObjHandler::%(tensorRank)sToOper(returnValue, xRet);
        return &xRet;'''

code65 = '''\
        static OPER xRet;
        ObjHandler::%(tensorRank)sToOper(returnValue, xRet);
        return &xRet;'''

##########################################################################
# code for Calc
##########################################################################

code70 = '''\
        std::string %(name)sCpp = ouStringToStlString(%(name)s);\n'''

code71 = '''\
        std::string %(name)sCpp;
        calcToScalar(%(name)sCpp, %(name)s);\n'''

code72 = '''\
        %(type)s %(name)sCpp;
        calcToScalar(%(name)sCpp, %(name)s);\n'''

code73 = '''\
        std::vector<std::string> %(name)sCpp;
        calcToVector(%(name)sCpp, %(name)s);\n'''

code74 = '''\
        std::vector<%(type)s> %(name)sCpp;
        calcToVector(%(name)sCpp, %(name)s);\n'''

code75 = '''\
        std::vector<std::vector<%(type)s> > %(name)sCpp;
        calcToMatrix(%(name)sCpp, %(name)s);\n'''

code76 = '''\
        std::vector<%(superType)s> %(name)sLib;
        calcToVector(%(name)sLib, %(name)s);\n'''

code77 = '''\
        %(superType)s %(name)sLib;
        calcToVector(%(name)sLib, %(name)s);\n'''

code78 = '''\
        std::vector<std::string> %(name)sLib;
        calcToVector(%(name)sLib, %(name)s);\n'''

code79 = '''\
        std::vector<%(type)s> %(name)sLib;
        calcToVector(%(name)sLib, %(name)s);\n'''

code80 = '''\
        OH_GET_REFERENCE(%(name)sLibObj, %(name)sCpp,
            %(namespaceObjects)s::%(superType)s, %(namespaceLibrary)s::%(superType)s)\n'''

code81 = '''\
        OH_GET_REFERENCE(%(name)sLibObj, %(name)sCpp,
            %(namespaceObjects)s::%(superType)s, %(namespaceLibrary)s::%(superType)s)\n'''

code82 = '''\
        %(superType)s %(name)sEnum =
            %(namespaceObjects)s::Create<%(superType)s>()(%(name)sCpp);\n'''

code83 = '''\
        sal_Int32 returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code84 = '''\
        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code85 = '''\
        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code86 = '''\
        %(type)s returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code87 = '''\
        %(type)s returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code88 = '''\
        SEQSEQ(sal_Int32) returnValueCalc;
        %(tensorRank)sToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code89 = '''\
        SEQSEQ(STRING) returnValueCalc;
        %(tensorRank)sToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code90 = '''\
        SEQSEQ(ANY) returnValueCalc;
        %(tensorRank)sToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code91 = '''\
        SEQSEQ(%(type)s) returnValueCalc;
        %(tensorRank)sToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code92 = '''\
        SEQSEQ(%(type)s) returnValueCalc;
        %(tensorRank)sToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code93 = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)sCpp, ObjHandler::Object)
        %(namespaceLibrary)s::Handle<%(namespaceLibrary)s::%(superType)s> %(name)sLibObj =
            ObjHandler::CoerceHandle<
                %(namespaceObjects)s::%(superType)s,
                %(namespaceLibrary)s::%(superType)s>()(
                    %(name)sTemp);\n'''

code94 = '''\
        %(superType)s %(name)sLib = calcToQlMatrix(%(name)s);\n'''

code95 = '''\
        OH_GET_OBJECT(%(name)sObj, %(name)sCpp, %(namespaceObjects)s::%(object)s)\n'''

code96 = '''\
        %(superType)s %(name)sLib;
        calcToScalar(%(name)sLib, %(name)s);\n'''

##########################################################################
# code for ValueObjects
##########################################################################

code66 = '''\
if(name == "%(name)s") return %(name)s_;
        else '''

##########################################################################
# code common to mutiple addins
##########################################################################

wrap1 = '''
        // convert input datatypes to C++ datatypes

%s'''

wrap2 = '''
        // convert input datatypes to QuantLib datatypes

%s'''

wrap3 = '''
        // convert input datatypes to QuantLib enumerated datatypes

%s'''

wrap4 = '''
        // convert input datatypes to Object references

%s'''

wrap5 = '''
        // convert object IDs into library objects

%s'''

wrap6 = '''
        // perform data conversion for return value of function

%s'''

