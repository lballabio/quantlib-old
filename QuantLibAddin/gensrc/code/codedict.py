
##########################################################################
# code for Excel
##########################################################################

code11 = '''\
        std::string %(name)sCpp =
            ObjectHandler::callOperToScalar<std::string>(*%(name)s, "%(name)s", %(defaultValue)s);\n'''

code12 = '''\
        %(nativeType)s %(name)sCpp =
            ObjectHandler::callOperToScalar<%(nativeType)s>(*%(name)s, "%(name)s", %(defaultValue)s);\n'''

code12b = '''\
        boost::any %(name)sCpp =
            ObjectHandler::callOperToScalar<boost::any>(*%(name)s, "%(name)s", boost::any());\n'''

code13 = '''\
        std::vector<%(nativeType)s> %(name)sCpp =
            ObjectHandler::operToVector<%(nativeType)s>(*%(name)s, "%(name)s");\n'''

code13b = '''\
        std::vector<boost::any> %(name)sCpp =
            ObjectHandler::operToVector<boost::any>(*%(name)s, "%(name)s");\n'''

code14 = '''\
        std::vector<long> %(name)sCpp =
            ObjectHandler::operToVector<long>(*%(name)s, "%(name)s");\n'''

code15 = '''\
        std::vector<double> %(name)sCpp =
            ObjectHandler::operToVector<double>(*%(name)s, "%(name)s");\n'''

code16 = '''\
        std::vector<std::string> %(name)sCpp =
            ObjectHandler::operToVector<std::string>(*%(name)s, "%(name)s");\n'''

code17 = '''\
        std::vector<boost::any> %(name)sCpp =
            ObjectHandler::operToVector<boost::any>(*%(name)s, "%(name)s");\n'''

code18 = '''\
        std::vector<std::vector<%(type)s> > %(name)sCpp =
            ObjectHandler::operToMatrix<%(type)s>(*%(name)s);\n'''

code19 = '''\
        std::vector<std::vector<long> > %(name)sCpp =
            ObjectHandler::fpToMatrix<long>(*%(name)s);\n'''

code20 = '''\
        std::vector<std::vector<double> > %(name)sCpp =
            ObjectHandler::fpToMatrix<double>(*%(name)s);\n'''

code21a = '''\
        std::vector<std::vector<std::string> > %(name)sCpp =
            ObjectHandler::operToMatrix<std::string>(*%(name)s);\n'''

code21b = '''\
        std::vector<std::vector<boost::any> > %(name)sCpp =
            ObjectHandler::operToMatrix<boost::any>(*%(name)s);\n'''

code22 = '''\
        %(type)s %(name)sLib;
        ObjectHandler::cppToLibrary(%(name)s, %(name)sLib);\n'''

code23 = '''\
        %(type)s %(name)sLib;
        ObjectHandler::cppToLibrary(*%(name)s, %(name)sLib);\n'''

code24 = '''\
        QuantLib::Date %(name)sLib =
            ObjectHandler::callOperToScalar<QuantLib::Date>(*%(name)s, "%(name)s");\n'''

code25 = '''\
        %(type)s %(name)sLib =
            ObjectHandler::callOperToScalar<%(type)s>(%(name)s, "%(name)s", %(defaultValue)s);\n'''

code26 = '''\
        %(type)s %(name)sLib =
            ObjectHandler::callOperToScalar<%(type)s>(*%(name)s, "%(name)s", %(defaultValue)s);\n'''

code27 = '''\
        QuantLib::Date %(name)sLib =
            ObjectHandler::callOperToScalar<QuantLib::Date>(*%(name)s, "%(name)s", %(defaultValue)s);\n'''

code28 = '''\
        %(type)s %(name)sLib =
            ObjectHandler::operToVector(*%(name)s, "%(name)s");\n'''

code29 = '''\
        %(type)s %(name)sLib =
            ObjectHandler::operToMatrix(*%(name)s);\n'''

code30 = '''\
        std::vector<QuantLib::Date> %(name)sLib =
            ObjectHandler::operToVector<QuantLib::Date>(*%(name)s, "%(name)s");\n'''

code31 = '''\
        std::vector<%(type)s> %(name)sLib =
            ObjectHandler::operToVectorLibrary<%(type)s>(
            *%(name)s);\n'''

code32 = '''\
        std::vector<%(type)s> %(name)sLib =
            ObjectHandler::operToVector<%(type)s>(
            *%(name)s, "%(name)s");\n'''

code33 = '''\
        %(type)s %(name)sEnum =
            ObjectHandler::operToScalarEnum<%(type)s,
                %(namespaceObjects)s::Create<%(type)s> >(
                    *%(name)s, "%(name)s", %(defaultValue)s);\n'''

code34 = '''\
        %(type)s %(name)sEnum =
            %(namespaceObjects)s::Create<%(type)s>()(%(name)s);\n'''

code35 = '''\
        std::vector<%(type)s> %(name)sEnum =
            ObjectHandler::operToVectorEnum<%(type)s,
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
            ObjectHandler::CoerceIndex<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)s);\n'''

code41 = '''\
        std::vector<boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> > %(name)sLibObj =
            ObjectHandler::CoerceIndexVector<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>(
                    %(name)sCpp);\n'''

code42 = '''\
        std::vector<boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> > %(name)sLibObj =
            ObjectHandler::operToObjectVector<%(namespaceLibrary)s::%(classname)s, %(namespaceObjects)s::%(classname)s>(
            *%(name)s);\n'''

code43 = '''\
        std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > %(name)sLibObj =
            ObjectHandler::operToVector<QuantLib::RelinkableHandle<QuantLib::Quote> >(*%(name)s, "%(name)s");\n'''

code44 = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        %(namespaceLibrary)s::RelinkableHandle<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            ObjectHandler::CoerceHandle<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

code45a = '''\
        QuantLib::RelinkableHandle<QuantLib::Quote> %(name)sLibObj =
            ObjectHandler::callOperToScalar<QuantLib::RelinkableHandle<QuantLib::Quote> >(*%(name)s, "%(name)s");\n'''

code45b = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            ObjectHandler::CoerceObject<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

code46 = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            ObjectHandler::CoerceTermStructure()(%(name)sTemp);\n'''

code46b = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            ObjectHandler::CoerceCurve<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s,
                QuantLib::SwaptionVolatilityStructure>()(
                    %(name)sTemp);\n'''

code47 = '''\
        boost::shared_ptr<QuantLib::Quote> %(name)sLibObj =
            ObjectHandler::callOperToScalar<boost::shared_ptr<QuantLib::Quote> >(*%(name)s, "%(name)s");\n'''

code48 = '''\
        OH_GET_UNDERLYING(%(name)sLibObj, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code49 = '''\
        OH_GET_UNDERLYING_NONCONST(%(name)sLibObj, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code50 = '''\
        std::vector<std::vector<%(namespaceLibrary)s::RelinkableHandle<%(namespaceLibrary)s::%(classname)s> > > %(name)sLibObj =
            ObjectHandler::operToMatrixHandle<%(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s>(*%(name)s);\n'''

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
        std::string str = ObjectHandler::libraryToScalar(returnValue);
        static char ret[XL_MAX_STR_LEN];
        ObjectHandler::stringToChar(str, ret);
        return ret;'''
 
code56b = '''\
        static %(nativeType)s returnValueXL;
        returnValueXL = ObjectHandler::libraryToScalar(returnValue);
        return &returnValueXL;'''

code57 = '''\
        std::ostringstream os;
        os << returnValue;
        static char ret[XL_MAX_STR_LEN];
        ObjectHandler::stringToChar(os.str(), ret);
        return ret;'''

code58 = '''\
        static char ret[XL_MAX_STR_LEN];
        ObjectHandler::stringToChar(returnValue, ret);
        return ret;'''

code59 = '''\
        static OPER xRet;
        ObjectHandler::%(tensorRank)sToOper(returnValue, xRet);
        return &xRet;'''

code60 = '''\
        return &returnValue;'''

code61 = '''\
        static OPER xRet;
        ObjectHandler::%(tensorRank)sToOper(returnValue, xRet);
        return &xRet;'''

code62 = '''\
        std::vector<%(nativeType)s> returnValVec = ObjectHandler::libraryToVector(returnValue);
        static OPER xRet;
        ObjectHandler::vectorToOper(returnValVec, xRet);
        return &xRet;'''

code63 = '''\
        std::vector<std::string> returnValVec = ObjectHandler::libraryToVector(returnValue);
        static OPER xRet;
        ObjectHandler::vectorToOper(returnValVec, xRet);
        return &xRet;'''

code64 = '''\
        static OPER xRet;
        ObjectHandler::%(tensorRank)sToOper(returnValue, xRet);
        return &xRet;'''

code65 = '''\
        static OPER xRet;
        ObjectHandler::%(tensorRank)sToOper(returnValue, xRet);
        return &xRet;'''

##########################################################################
# code for Calc
##########################################################################

code70 = '''\
        std::string %(name)sCpp = ouStringToStlString(%(name)s);\n'''

code71 = '''\
        std::string %(name)sCpp;
        calcToScalar(%(name)sCpp, %(name)s);\n'''

code71b = '''\
        boost::any %(name)sCpp;
        calcToScalar(%(name)sCpp, %(name)s);\n'''

code72 = '''\
        %(nativeType)s %(name)sCpp;
        calcToScalar(%(name)sCpp, %(name)s);\n'''

code73 = '''\
        std::vector<std::string> %(name)sCpp;
        calcToVector(%(name)sCpp, %(name)s);\n'''

code73b = '''\
        std::vector<boost::any> %(name)sCpp;
        calcToVector(%(name)sCpp, %(name)s);\n'''

code74 = '''\
        std::vector<%(nativeType)s> %(name)sCpp;
        calcToVector(%(name)sCpp, %(name)s);\n'''

code75 = '''\
        std::vector<std::vector<%(nativeType)s> > %(name)sCpp;
        calcToMatrix(%(name)sCpp, %(name)s);\n'''

code76 = '''\
        std::vector<%(type)s> %(name)sLib;
        calcToVector(%(name)sLib, %(name)s);\n'''

code77 = '''\
        %(type)s %(name)sLib;
        calcToVector(%(name)sLib, %(name)s);\n'''

code78 = '''\
        std::vector<std::string> %(name)sLib;
        calcToVector(%(name)sLib, %(name)s);\n'''

code79 = '''\
        std::vector<%(type)s> %(name)sLib;
        calcToVector(%(name)sLib, %(name)s);\n'''

code80 = '''\
        OH_GET_REFERENCE(%(name)sLibObj, %(name)sCpp,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code81 = '''\
        OH_GET_REFERENCE(%(name)sLibObj, %(name)sCpp,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code82 = '''\
        %(type)s %(name)sEnum =
            %(namespaceObjects)s::Create<%(type)s>()(%(name)sCpp);\n'''

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
        %(nativeType)s returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code87 = '''\
        %(nativeType)s returnValueCalc;
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
        SEQSEQ(%(nativeType)s) returnValueCalc;
        %(tensorRank)sToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code92 = '''\
        SEQSEQ(%(nativeType)s) returnValueCalc;
        %(tensorRank)sToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code93 = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)sCpp, ObjectHandler::Object)
        %(namespaceLibrary)s::Handle<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            ObjectHandler::CoerceHandle<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

code94 = '''\
        %(type)s %(name)sLib = calcToQlMatrix(%(name)s);\n'''

code95 = '''\
        OH_GET_OBJECT(%(name)sObj, %(name)sCpp, %(namespaceObjects)s::%(object)s)\n'''

code96 = '''\
        %(type)s %(name)sLib;
        calcToScalar(%(name)sLib, %(name)s);\n'''

##########################################################################
# code for ValueObjects
##########################################################################

code66 = '''\
if(name == "%(name)s") return %(name)s_;
        else '''

##########################################################################
# code for C
##########################################################################

code101 = '''\
        %(type)s %(name)sLib;
        cToLib(%(name)sLib, %(name)s);\n'''

code102 = '''\
        OH_GET_REFERENCE(%(name)sLibObj, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code103 = '''\
        %(type)s %(name)sEnum =
            %(namespaceObjects)s::Create<%(type)s>()(%(name)s);\n'''

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

