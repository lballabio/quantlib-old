
##########################################################################
# code for Excel
##########################################################################

code11 = '''\
        std::string %(name)sCpp = ObjectHandler::convert2<std::string>(
            ObjectHandler::ConvertOper(*%(name)s), "%(name)s", %(defaultValue)s);\n'''

code12 = '''\
        %(nativeType)s %(name)sCpp = ObjectHandler::convert2<%(nativeType)s>(
            ObjectHandler::ConvertOper(*%(name)s), "%(name)s", %(defaultValue)s);\n'''

code12b = '''\
        ObjectHandler::property_t %(name)sCpp = ObjectHandler::convert2<ObjectHandler::property_t>(
            ObjectHandler::ConvertOper(*%(name)s));\n'''

code12c = '''\
        %(nativeType)s %(name)sCpp = ObjectHandler::convert2<%(nativeType)s>(
            ObjectHandler::ConvertOper(*%(name)s), "%(name)s", %(defaultValue)s, %(errorValue)s);\n'''

code13 = '''\
        std::vector<%(nativeType)s> %(name)sCpp =
            ObjectHandler::operToVector<%(nativeType)s>(*%(name)s, "%(name)s");\n'''

code14 = '''\
        std::vector<ObjectHandler::property_t> %(name)sCpp =
            ObjectHandler::operToVector<ObjectHandler::property_t>(*%(name)s, "%(name)s");\n'''

code15 = '''\
        std::vector<std::string> %(name)sCpp =
            ObjectHandler::operToVector<std::string>(*%(name)s, "%(name)s");\n'''

code18 = '''\
        std::vector<std::vector<%(type)s> > %(name)sCpp =
            ObjectHandler::operToMatrix<%(type)s>(*%(name)s, "%(name)s");\n'''

code19 = '''\
        std::vector<std::vector<long> > %(name)sCpp =
            ObjectHandler::fpToMatrix<long>(*%(name)s);\n'''

code20 = '''\
        std::vector<std::vector<double> > %(name)sCpp =
            ObjectHandler::fpToMatrix<double>(*%(name)s);\n'''

code21a = '''\
        std::vector<std::vector<std::string> > %(name)sCpp =
            ObjectHandler::operToMatrix<std::string>(*%(name)s, "%(name)s");\n'''

code21b = '''\
        std::vector<std::vector<ObjectHandler::property_t> > %(name)sCpp =
            ObjectHandler::operToMatrix<ObjectHandler::property_t>(*%(name)s, "%(name)s");\n'''

code22 = '''\
        %(type)s %(nameConverted)s;
        QuantLibAddin::cppToLibrary(%(name)s, %(nameConverted)s);\n'''

code23 = '''\
        %(type)s %(nameConverted)s;
        QuantLibAddin::cppToLibrary(*%(name)s, %(nameConverted)s);\n'''

code24 = '''\
        %(type)s %(nameConverted)s = ObjectHandler::convert2<%(type)s>(
            ObjectHandler::ConvertOper(*%(name)s), "%(name)s");\n'''

code25 = '''\
        %(type)s %(nameConverted)s = ObjectHandler::convert2<%(type)s>(
            ObjectHandler::ConvertOper(*%(name)s), "%(name)s", %(defaultValue)s);\n'''

code26 = '''\
        %(type)s %(nameConverted)s = ObjectHandler::convert2<%(type)s>(
            ObjectHandler::ConvertOper(*%(name)s), "%(name)s", %(defaultValue)s);\n'''

code27 = '''\
        %(type)s %(nameConverted)s = ObjectHandler::convert2<%(type)s>(
            ObjectHandler::ConvertOper(*%(name)s), "%(name)s", %(defaultValue)s, %(errorValue)s);\n'''

code27a = '''\
        %(type)s %(nameConverted)s = ObjectHandler::convert2<long>(
            ObjectHandler::ConvertOper(*%(name)s), "%(name)s", %(defaultValue)s);\n'''


code28 = '''\
        %(type)s %(nameConverted)s =
            QuantLibXL::operToQlArray(*%(name)s, "%(name)s");\n'''

code29 = '''\
        %(type)s %(nameConverted)s =
            QuantLibXL::operToQlMatrix(*%(name)s);\n'''

code31 = '''\
        std::vector<%(type)s> %(nameConverted)s =
            ObjectHandler::operToVector<%(type)s>(*%(name)s, "%(name)s");\n'''

code33 = '''\
        %(type)s %(nameConverted)s =
            ObjectHandler::Create<%(type)s>()(%(name)sCpp);\n'''

code34 = '''\
        %(type)s %(nameConverted)s =
            ObjectHandler::Create<%(type)s>()(%(name)s);\n'''

code35 = '''\
        std::vector<%(type)s> %(nameConverted)s =
            ObjectHandler::vectorStringToEnum<%(type)s>(%(name)sCpp, "%(name)s");\n'''

code35b = '''\
        std::vector<%(type)s> %(nameConverted)s =
            ObjectHandler::vectorStringToEnum<%(type)s>(%(name)s, "%(name)s");\n'''

code36 = '''\
        OH_GET_OBJECT(%(nameConverted)s, %(name)s, %(type)s)\n'''

code36b = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        boost::shared_ptr<%(type)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceQuoteObject<%(type)s>()(
                %(name)sTemp);\n'''

code36c = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        boost::shared_ptr<%(type)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceTermStructureObject<%(type)s>()(
                %(name)sTemp);\n'''

code37 = '''\
        std::vector<boost::shared_ptr<%(namespaceObjects)s::%(classname)s> > %(nameConverted)s =
            ObjectHandler::getObjectVector<%(namespaceObjects)s::%(classname)s>(%(name)sCpp);\n'''

code37b = '''\
        std::vector<boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> > %(nameConverted)s =
            ObjectHandler::getLibraryObjectVector<%(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s>(%(name)sCpp);\n'''

code37c = '''\
        std::vector<boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> > %(nameConverted)s =
            ObjectHandler::getLibraryObjectVector<%(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s>(%(name)s);\n'''

code38 = '''\
        OH_GET_REFERENCE(%(nameConverted)s, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code39 = '''\
        OH_GET_REFERENCE_DEFAULT(%(nameConverted)s, %(name)sCpp,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code42 = '''\
        std::vector<boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> > %(nameConverted)s =
            ObjectHandler::operToObjectVector<%(namespaceLibrary)s::%(classname)s, %(namespaceObjects)s::%(classname)s>(
            *%(name)s, "%(name)s");\n'''

code43 = '''\
        std::vector<QuantLib::Handle<QuantLib::Quote> > %(nameConverted)s =
            ObjectHandler::operToVector<QuantLib::Handle<QuantLib::Quote> >(*%(name)s, "%(name)s");\n'''

code44a = '''\
        OH_GET_OBJECT_DEFAULT(%(name)sCoerce, %(name)sCpp, ObjectHandler::Object)
        %(namespaceLibrary)s::Handle<%(namespaceLibrary)s::%(classname)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceHandle<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sCoerce, %(namespaceLibrary)s::Handle<%(namespaceLibrary)s::%(classname)s>());\n'''

code44b = '''\
        OH_GET_OBJECT(%(name)sCoerce, %(name)s, ObjectHandler::Object)
        %(namespaceLibrary)s::Handle<%(namespaceLibrary)s::%(classname)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceHandle<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sCoerce);\n'''

code45a = '''\
        QuantLib::Handle<QuantLib::Quote> %(nameConverted)s = 
            ObjectHandler::convert2<QuantLib::Handle<QuantLib::Quote> >(
                ObjectHandler::ConvertOper(*%(name)s), "%(name)s");\n'''

code45b = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceLibrarySame<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

code45c = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceQuote<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

code46 = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceTermStructure<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

code47 = '''\
        boost::shared_ptr<QuantLib::Quote> %(nameConverted)s =
            ObjectHandler::convert2<boost::shared_ptr<QuantLib::Quote> >(
                ObjectHandler::ConvertOper(*%(name)s), "%(name)s");\n'''

code48 = '''\
        OH_GET_UNDERLYING(%(nameConverted)s, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code48b = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(name)sTemp2 =
            %(namespaceObjects)s::CoerceLibrarySame<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);
        const %(namespaceLibrary)s::%(classname)s &%(nameConverted)s =
            *(%(name)sTemp2.get());\n'''

code49 = '''\
        OH_GET_UNDERLYING_NONCONST(%(nameConverted)s, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code50 = '''\
        std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > > %(nameConverted)s =
            ObjectHandler::operToMatrix<QuantLib::Handle<QuantLib::Quote> >(*%(name)s, "%(name)s");\n'''

code51 = '''\
std::vector<std::vector<std::string> > returnValue = '''

code52 = '''\
std::vector<std::vector<%(type)s> > returnValue = '''

code53 = '''\
std::vector<std::vector<ObjectHandler::property_t> > returnValue = '''

code54 = '''\
            %(nameConverted)s.begin(),
            %(nameConverted)s.end()'''

code55 = '''\
            %(name)sCpp.begin(),
            %(name)sCpp.end()'''

code56a = '''\
        std::string str = %(namespaceObjects)s::libraryToScalar(returnValue);
        static char ret[XL_MAX_STR_LEN];
        ObjectHandler::stringToChar(str, ret);
        return ret;'''
 
code56b = '''\
        static %(nativeType)s returnValueXL;
        returnValueXL = %(namespaceObjects)s::libraryToScalar(returnValue);
        return &returnValueXL;'''
 
code56c = '''\
        static long returnValueXL;
        returnValueXL = static_cast<long>(%(namespaceObjects)s::libraryToScalar(returnValue));
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
        static OPER xRet;
        ObjectHandler::scalarToOper2(returnValue, xRet);
        return &xRet;'''

code60b = '''\
        static OPER xRet;
        ObjectHandler::matrixToOper(returnValue, xRet);
        return &xRet;'''

code61 = '''\
        return &returnValue;'''

code62 = '''\
        std::vector<%(nativeType)s> returnValVec = %(namespaceObjects)s::libraryToVector(returnValue);
        static OPER xRet;
        ObjectHandler::vectorToOper(returnValVec, xRet);
        return &xRet;'''

code63 = '''\
        std::vector<std::string> returnValVec = %(namespaceObjects)s::libraryToVector(returnValue);
        static OPER xRet;
        ObjectHandler::vectorToOper(returnValVec, xRet);
        return &xRet;'''

code63b = '''\
        std::vector<%(nativeType)s> returnValVec =
            %(namespaceObjects)s::convertVector<%(type)s, %(nativeType)s>(returnValue);
        static OPER xRet;
        ObjectHandler::vectorToOper(returnValVec, xRet);
        return &xRet;'''

code64 = '''\
        static OPER xRet;
        ObjectHandler::%(tensorRank)sToOper(returnValue, xRet);
        return &xRet;'''

code65 = '''\
        std::string %(name)sStrip = ObjectHandler::CallingRange::getStub(%(name)s);'''

code65b = '''\
        std::string %(name)sStrip = ObjectHandler::CallingRange::getStub(%(name)sCpp);'''

##########################################################################
# code for C++
##########################################################################

code200 = '''\
        %(type)s %(nameConverted)s = ObjectHandler::convert2<%(type)s>(
            %(name)s, "%(name)s");\n'''

code201 = '''\
        %(type)s %(nameConverted)s = ObjectHandler::convert2<%(type)s>(
            %(name)s, "%(name)s", %(defaultValue)s);\n'''

code201a = '''\
        %(type)s %(nameConverted)s = ObjectHandler::convert2<long>(
            %(name)s, "%(name)s", %(defaultValue)s);\n'''

code203 = '''\
        /*%(type)s %(nameConverted)s =
            QuantLibXL::operToQlArray(%(name)s, "%(name)s");*/\n'''

code204 = '''\
        std::vector<%(type)s> %(nameConverted)s = 
            ObjectHandler::vector::convert2<%(type)s>(%(name)s, "%(name)s");\n'''

code205 = '''\
        std::vector<%(type)s> %(nameConverted)s =
            ObjectHandler::vector::convert2<%(type)s>(%(name)s, "%(name)s");\n'''

code206 = '''\
        %(type)s %(nameConverted)s =
            QuantLibAddin::vvToQlMatrix(%(name)s);\n'''

code207 = '''\
        std::string %(name)sCpp = ObjectHandler::convert2<std::string>(
            %(name)s, "%(name)s", %(defaultValue)s);\n'''

code208 = '''\
        %(nativeType)s %(name)sCpp = ObjectHandler::convert2<%(nativeType)s>(
            %(name)s, "%(name)s", %(defaultValue)s);\n'''

code209 = '''\
        std::vector<%(nativeType)s> returnValueLib = %(namespaceObjects)s::libraryToVector(returnValue);
        return returnValueLib;'''

code209b = '''\
        std::vector<%(nativeType)s> returnValueLib =
            %(namespaceObjects)s::convertVector<%(type)s, %(nativeType)s>(returnValue);
        return returnValueLib;'''

code209c = '''\
        std::vector<std::vector<%(nativeType)s> > returnValueLib =
            %(namespaceObjects)s::qlMatrixToVv(returnValue);
        return returnValueLib;'''

code210 = '''\
        %(nativeType)s returnValueLib = %(namespaceObjects)s::libraryToScalar(returnValue);
        return returnValueLib;'''

code210b = '''\
        long returnValueLib = static_cast<long>(%(namespaceObjects)s::libraryToScalar(returnValue));
        return returnValueLib;'''

code211 = '''\
        std::ostringstream os;
        os << returnValue;
        return os.str();'''

code213 = '''\
        %(type)s %(nameConverted)s = ObjectHandler::convert2<%(type)s>(
            %(name)s, "%(name)s", %(defaultValue)s);\n'''

code214 = '''\
        QuantLib::Handle<QuantLib::Quote> %(nameConverted)s = 
            ObjectHandler::convert2<QuantLib::Handle<QuantLib::Quote> >(
                %(name)s, "%(name)s");\n'''

code215 = '''\
        std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > > %(nameConverted)s =
            ObjectHandler::matrix::convert2<QuantLib::Handle<QuantLib::Quote> >(%(name)s, "%(name)s");\n'''

code218 = '''\
        std::vector<QuantLib::Handle<QuantLib::Quote> > %(nameConverted)s =
            ObjectHandler::vector::convert2<QuantLib::Handle<QuantLib::Quote> >(%(name)s, "%(name)s");\n'''

##########################################################################
# code for Calc
##########################################################################

codeCalc37b = '''\
        std::vector<boost::shared_ptr<QuantLib::BootstrapHelper<QuantLib::ZeroInflationTermStructure> > > %(nameConverted)s =
            ObjectHandler::getLibraryObjectVector<%(namespaceObjects)s::%(classname)s, QuantLib::BootstrapHelper<QuantLib::ZeroInflationTermStructure> >(%(name)sCpp);\n'''

code71a = '''\
        std::string %(name)sCpp = ouStringToStlString(%(name)s);\n'''

code71b = '''\
        std::string %(name)sCpp;
        calcToScalar(%(name)sCpp, %(name)s);\n'''

code71bdef = '''\
        std::string %(name)sCpp;
        if(%(name)s.hasValue()) 
            calcToScalar(%(name)sCpp, %(name)s);
        else
            %(name)sCpp = %(defaultValue)s;\n'''

code71c = '''\
        ObjectHandler::property_t %(name)sCpp;
        calcToScalar(%(name)sCpp, %(name)s);\n'''

code71cdef = '''\
        ObjectHandler::property_t %(name)sCpp;
        if(%(name)s.hasValue()) 
            calcToScalar(%(name)sCpp, %(name)s);
        else
            %(name)sCpp = %(defaultValue)s;\n'''

code72 = '''\
        %(nativeType)s %(name)sCpp;
        calcToScalar(%(name)sCpp, %(name)s);\n'''

code72def = '''\
        %(nativeType)s %(name)sCpp;
        if(%(name)s.hasValue()) 
            calcToScalar(%(name)sCpp, %(name)s);
        else
            %(name)sCpp = %(defaultValue)s;\n'''

code73 = '''\
        std::vector<std::string> %(name)sCpp;
        calcToVector(%(name)sCpp, %(name)s);\n'''

code73b = '''\
        std::vector<ObjectHandler::property_t> %(name)sCpp;
        calcToVector(%(name)sCpp, %(name)s);\n'''

code74 = '''\
        std::vector<%(nativeType)s> %(name)sCpp;
        calcToVector(%(name)sCpp, %(name)s);\n'''

code75 = '''\
        std::vector<std::vector<%(nativeType)s> > %(name)sCpp;
        calcToMatrix(%(name)sCpp, %(name)s);\n'''

code76 = '''\
        std::vector<%(type)s> %(nameConverted)s;
        calcToVector(%(nameConverted)s, %(name)s);\n'''

code77 = '''\
        %(type)s %(nameConverted)s;
        calcToVector(%(nameConverted)s, %(name)s);\n'''

code78 = '''\
        std::vector<std::string> %(nameConverted)s;
        calcToVector(%(nameConverted)s, %(name)s);\n'''

code79 = '''\
        std::vector<%(type)s> %(nameConverted)s;
        calcToVector(%(nameConverted)s, %(name)s);\n'''

code80 = '''\
        OH_GET_REFERENCE(%(nameConverted)s, %(name)sCpp,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code81 = '''\
        OH_GET_REFERENCE(%(nameConverted)s, %(name)sCpp,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code81Calc = '''\
        OH_GET_REFERENCE(%(nameConverted)s, %(name)sCpp,
            %(namespaceObjects)s::%(classname)sSRM, %(namespaceLibrary)s::%(classname)s)\n'''

code81Seri = '''\
        OH_GET_REFERENCE(%(nameConverted)s, %(name)s,
            %(namespaceObjects)s::%(classname)sSRM, %(namespaceLibrary)s::%(classname)s)\n'''

code82 = '''\
        %(type)s %(nameConverted)s =
            ObjectHandler::Create<%(type)s>()(%(name)sCpp);\n'''

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

code85a = '''\
        SEQSEQ(sal_Int32) returnValueCalc;
        vectorToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code85any = '''\
        SEQSEQ(ANY) returnValueCalc;
        vectorToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code85b = '''\
        SEQSEQ(ANY) returnValueCalc;
        vectorToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code86 = '''\
        %(nativeType)s returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code86a = '''\
        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;\n'''

code86any = '''\
        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;\n'''

code86anyvoid = '''\
        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        STRING s = STRFROMASCII( std::string("VOID").c_str() );    
        retAnyVector[0] = CSS::uno::makeAny( s );
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;\n'''

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

#code93 = '''\
#        OH_GET_OBJECT(%(name)sCoerce, %(name)sTemp, ObjectHandler::Object)
#        %(namespaceLibrary)s::Handle<%(namespaceLibrary)s::%(classname)s> %(nameConverted)s =
#            %(namespaceObjects)s::CoerceHandle<
#                %(namespaceObjects)s::%(classname)s,
#                %(namespaceLibrary)s::%(classname)s>()(
#                    %(name)sCoerce);\n'''
# RL: amended (Temp->Cpp)
code93 = '''\
        OH_GET_OBJECT(%(name)sCoerce, %(name)sCpp, ObjectHandler::Object)
        %(namespaceLibrary)s::Handle<%(namespaceLibrary)s::%(classname)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceHandle<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sCoerce);\n'''

code93a = '''\
        OH_GET_OBJECT_DEFAULT(%(name)sCoerce, %(name)sCpp, ObjectHandler::Object)
        %(namespaceLibrary)s::Handle<%(namespaceLibrary)s::%(classname)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceHandle<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sCoerce, %(namespaceLibrary)s::Handle<%(namespaceLibrary)s::%(classname)s>());\n'''

code94 = '''\
        %(type)s %(nameConverted)s = calcToQlMatrix(%(name)s);\n'''

#code95 = '''\
#        OH_GET_OBJECT(%(nameConverted)s, %(name)sCpp, %(namespaceObjects)s::%(object)s)\n'''

code95 = '''\
        OH_GET_OBJECT(%(nameConverted)s, %(name)sCpp, %(type)s)\n'''

code96 = '''\
        %(type)s %(nameConverted)s;
        calcToScalar(%(nameConverted)s, %(name)s);\n'''

code96defdate = '''\
        %(type)s %(nameConverted)s;
        if(!%(name)s.hasValue() and typeid(%(defaultValue)s)==typeid(QuantLib::Date())) 
            %(nameConverted)s = %(defaultValue)s;
        else
            calcToScalar(%(nameConverted)s, %(name)s);\n'''

code96defperiod = '''\
        %(type)s %(nameConverted)s;
        if(!%(name)s.hasValue() and typeid(%(defaultValue)s)==typeid("string")) 
            calcToScalar(%(nameConverted)s, %(defaultValue)s);
        else
            calcToScalar(%(nameConverted)s, %(name)s);\n'''

code96 = '''\
        %(type)s %(nameConverted)s;
        calcToScalar(%(nameConverted)s, %(name)s);\n'''

code100 = '''\
        OH_GET_OBJECT(%(nameConverted)s, %(name)s, %(type)s)\n'''

code100b = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)sCpp, ObjectHandler::Object)
        boost::shared_ptr<%(type)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceQuoteObject<%(type)s>()(
                %(name)sTemp);\n'''

code100c = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)sCpp, ObjectHandler::Object)
        boost::shared_ptr<%(type)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceTermStructureObject<%(type)s>()(
                %(name)sTemp);\n'''

code101 = '''\
        std::vector<boost::shared_ptr<%(namespaceObjects)s::%(classname)s> > %(nameConverted)s =
            ObjectHandler::getObjectVector<%(namespaceObjects)s::%(classname)s>(%(name)sCpp);\n'''

codeCalc45c = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)sCpp, ObjectHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceQuote<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

codeCalc46 = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)sCpp, ObjectHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceTermStructure<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

codeCalc48 = '''\
        OH_GET_UNDERLYING(%(nameConverted)s, %(name)sCpp,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

codeCalc49 = '''\
        OH_GET_UNDERLYING_NONCONST(%(nameConverted)s, %(name)sCpp,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

codeCalc53 = '''\
std::vector<std::vector<ObjectHandler::property_t> > returnValue = '''

codeCalc218 = '''\
        std::vector<QuantLib::Handle<QuantLib::Quote> > %(nameConverted)s =
            ObjectHandler::vector::convert2<QuantLib::Handle<QuantLib::Quote> >(%(name)sCpp, "%(name)s");\n'''

##########################################################################
# code for ValueObjects
##########################################################################

code66 = '''\
        else if(strcmp(nameUpper.c_str(), "%(nameUpper)s")==0)
            return %(name)s_;\n'''

code66a = '''\
        else if(strcmp(nameUpper.c_str(), "%(nameUpper)s")==0)
            return %(name)s_;\n'''

code67a = '''\
        else if(strcmp(nameUpper.c_str(), "%(nameUpper)s")==0)
            %(name)s_ = value;\n'''

code67b = '''\
        else if(strcmp(nameUpper.c_str(), "%(nameUpper)s")==0)
            %(name)s_ = ObjectHandler::convert2<std::string>(value);\n'''

code67c = '''\
        else if(strcmp(nameUpper.c_str(), "%(nameUpper)s")==0)
            %(name)s_ = ObjectHandler::convert2<%(nativeType)s>(value);\n'''

code67d = '''\
        else if(strcmp(nameUpper.c_str(), "%(nameUpper)s")==0)
            %(name)s_ = ObjectHandler::vector::convert2<ObjectHandler::property_t>(value, nameUpper);\n'''

code67e = '''\
        else if(strcmp(nameUpper.c_str(), "%(nameUpper)s")==0)
            %(name)s_ = ObjectHandler::vector::convert2<std::string>(value, nameUpper);\n'''

code67f = '''\
        else if(strcmp(nameUpper.c_str(), "%(nameUpper)s")==0)
            %(name)s_ = ObjectHandler::vector::convert2<%(nativeType)s>(value, nameUpper);\n'''

code67g = '''\
        else if(strcmp(nameUpper.c_str(), "%(nameUpper)s")==0)
            %(name)s_ = ObjectHandler::matrix::convert2<ObjectHandler::property_t>(value, nameUpper);\n'''

code67h = '''\
        else if(strcmp(nameUpper.c_str(), "%(nameUpper)s")==0)
            %(name)s_ = ObjectHandler::matrix::convert2<std::string>(value, nameUpper);\n'''

code67i = '''\
        else if(strcmp(nameUpper.c_str(), "%(nameUpper)s")==0)
            %(name)s_ = ObjectHandler::matrix::convert2<%(nativeType)s>(value, nameUpper);\n'''

code68 = '''\
            processVariant(%(name)s);'''

code69 = '''\
            processPrecedentID(%(name)s);'''

code70 = '''\
            for (std::vector<std::string>::const_iterator i = %(name)s.begin();
                    i != %(name)s.end(); ++i)
                processPrecedentID(*i);'''

code68b = '''\
        valueObject->processVariant(%(name)s);'''

code69b = '''\
        valueObject->processPrecedentID(%(name)s);'''

code70b = '''\
        for (std::vector<std::string>::const_iterator i = %(name)s.begin();
                i != %(name)s.end(); ++i)
            valueObject->processPrecedentID(*i);'''


##########################################################################
# code for Serialization
##########################################################################

code110 = '''\
        ObjectHandler::property_t %(name)s =
            valueObject->getProperty("%(name)s");\n'''

code111 = '''\
        std::string %(name)s =
            ObjectHandler::convert2<std::string>(valueObject->getProperty("%(name)s"));\n'''

code112 = '''\
        %(nativeType)s %(name)s =
            ObjectHandler::convert2<%(nativeType)s>(valueObject->getProperty("%(name)s"));\n'''

code113 = '''\
        std::vector<%(nativeType)s> %(name)s =
            ObjectHandler::vector::convert2<%(nativeType)s>(valueObject->getProperty("%(name)s"), "%(name)s");\n'''

#code114 = '''\
#        std::vector<ObjectHandler::property_t> %(name)s =
#            ObjectHandler::convert2<std::vector<ObjectHandler::property_t> >(valueObject->getProperty("%(name)s"), "%(name)s");\n'''
code114 = '''\
        std::vector<ObjectHandler::property_t> %(name)s =
            ObjectHandler::vector::convert2<ObjectHandler::property_t>(valueObject->getProperty("%(name)s"), "%(name)s");\n'''

code115 = '''\
        std::vector<std::string> %(name)s =
            ObjectHandler::vector::convert2<std::string>(valueObject->getProperty("%(name)s"), "%(name)s");\n'''

code116 = '''\
        std::vector<std::vector<std::string> > %(name)s =
            ObjectHandler::matrix::convert2<std::string>(valueObject->getProperty("%(name)s"), "%(name)s");\n'''

code117 = '''\
        std::vector<std::vector<%(nativeType)s> > %(name)s =
            ObjectHandler::matrix::convert2<%(nativeType)s>(valueObject->getProperty("%(name)s"), "%(name)s");\n'''

code118 = '''\
        std::vector<std::vector<ObjectHandler::property_t> > %(name)s =
            ObjectHandler::matrix::convert2<ObjectHandler::property_t>(valueObject->getProperty("%(name)s"), "%(name)s");\n'''

code121a = '''\
        %(type)s %(nameConverted)s = %(name)s;\n'''

code121 = '''\
        %(type)s %(nameConverted)s = ObjectHandler::convert2<%(type)s>(
            valueObject->getProperty("%(name)s"), "%(name)s");\n'''

code122 = '''\
        %(type)s %(nameConverted)s = ObjectHandler::convert2<%(type)s>(
            valueObject->getProperty("%(name)s"), "%(name)s", %(defaultValue)s);\n'''

code123 = '''\
        std::string %(name)sCpp = ObjectHandler::convert2<std::string>(
            %(name)s, "%(name)s", %(defaultValue)s);\n'''

code124 = '''\
        %(nativeType)s %(name)sCpp = ObjectHandler::convert2<%(nativeType)s>(
            %(name)s, "%(name)s", %(defaultValue)s);\n'''

code131 = '''\
        std::vector<%(type)s> %(nameConverted)s =
            ObjectHandler::vector::convert2<%(type)s>(%(name)s, "%(name)s");\n'''

code132 = '''\
        std::vector<%(type)s> %(nameConverted)s = 
            QuantLibAddin::convertVector<%(nativeType)s, %(type)s>(%(name)s);\n'''

code133 = '''\
        std::vector<%(type)s> %(nameConverted)s = 
            QuantLibAddin::convertVector<%(nativeType)s, %(type)s>(%(name)sCpp);\n'''

code135 = '''\
        QuantLib::Matrix %(nameConverted)s =
            QuantLibAddin::vvToQlMatrix(%(name)s);\n'''

code140 = '''\
        QuantLib::Handle<QuantLib::Quote> %(nameConverted)s = 
            ObjectHandler::convert2<QuantLib::Handle<QuantLib::Quote> >(
                %(name)s, "%(name)s");\n'''

code150 = '''\
        std::vector<QuantLib::Handle<QuantLib::Quote> > %(nameConverted)s =
            ObjectHandler::vector::convert2<QuantLib::Handle<QuantLib::Quote> >(%(name)s, "%(name)s");\n'''

code151 = '''\
        std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > > %(nameConverted)s =
            ObjectHandler::matrix::convert2<QuantLib::Handle<QuantLib::Quote> >(%(name)s, "%(name)s");\n'''

code152 = '''\
        boost::shared_ptr<QuantLib::Quote> %(nameConverted)s =
            ObjectHandler::convert2<boost::shared_ptr<QuantLib::Quote> >(
                %(name)s, "%(name)s");\n'''

code153 = '''\
        std::vector<boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> > %(nameConverted)s =
            ObjectHandler::ohVariantToObjectVector<%(namespaceLibrary)s::%(classname)s, %(namespaceObjects)s::%(classname)s>(
            %(name)s, "%(name)s");\n'''

code154 = '''\
        std::vector<QuantLib::Handle<%(namespaceLibrary)s::%(classname)s> > %(nameConverted)s =
            QuantLibAddin::coerceHandleVector<%(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s>(%(name)s);\n'''

code159 = '''\
        std::vector<boost::shared_ptr<ObjectHandler::Object> > %(nameConverted)s =
            ObjectHandler::getObjectVector<ObjectHandler::Object>(%(name)s);\n'''

code160 = '''\
        std::vector<boost::shared_ptr<%(namespaceObjects)s::%(classname)s> > %(nameConverted)s =
            ObjectHandler::getObjectVector<%(namespaceObjects)s::%(classname)s>(%(name)s);\n'''

code161 = '''\
        OH_GET_REFERENCE_DEFAULT(%(nameConverted)s, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code162 = '''\
        std::vector<%(type)s> %(nameConverted)s =
            ObjectHandler::vectorStringToEnum<%(type)s>(%(name)s, "%(name)s");\n'''

code163 = '''\
        OH_GET_OBJECT_DEFAULT(%(name)sCoerce, %(name)s, ObjectHandler::Object)
        %(namespaceLibrary)s::Handle<%(namespaceLibrary)s::%(classname)s> %(nameConverted)s =
            %(namespaceObjects)s::CoerceHandle<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sCoerce, %(namespaceLibrary)s::Handle<%(namespaceLibrary)s::%(classname)s>());\n'''

##########################################################################
# code for C
##########################################################################

code101 = '''\
        %(type)s %(nameConverted)s;
        cToLib(%(nameConverted)s, %(name)s);\n'''

code102 = '''\
        OH_GET_REFERENCE(%(nameConverted)s, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code103 = '''\
        %(type)s %(nameConverted)s =
            ObjectHandler::Create<%(type)s>()(%(name)s);\n'''

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
