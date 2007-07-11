
##########################################################################
# code for Excel
##########################################################################

code11 = '''\
        std::string %(name)sCpp = ObjectHandler::operToScalar<std::string>(
            *%(name)s, "%(name)s", %(defaultValue)s);\n'''

code12 = '''\
        %(nativeType)s %(name)sCpp = ObjectHandler::operToScalar<%(nativeType)s>(
            *%(name)s, "%(name)s", %(defaultValue)s);\n'''

code13 = '''\
        std::vector<%(nativeType)s> %(name)sCpp =
            ObjectHandler::operToVector<%(nativeType)s>(
                *%(name)s, "%(name)s");\n'''

code14 = '''\
        std::vector<long> %(name)sCpp =
            ObjectHandler::operToVector<long>(
                *%(name)s, "%(name)s");\n'''

code15 = '''\
        std::vector<double> %(name)sCpp =
            ObjectHandler::operToVector<double>(
                *%(name)s, "%(name)s");\n'''

code16 = '''\
        std::vector<std::string> %(name)sCpp =
            ObjectHandler::operToVector<std::string>(
                *%(name)s, "%(name)s");\n'''

code17 = '''\
        std::vector<boost::any> %(name)sCpp =
            ObjectHandler::operToVector<boost::any>(
                *%(name)s, "%(name)s");\n'''

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

code25 = '''\
        %(type)s %(name)sLib = ObjectHandler::operToScalar<%(type)s>(
            %(name)s, "%(name)s", %(defaultValue)s);\n'''

code26 = '''\
        %(type)s %(name)sLib = ObjectHandler::operToScalar<%(type)s>(
            *%(name)s, "%(name)s", %(defaultValue)s);\n'''

code31 = '''\
        std::vector<%(type)s> %(name)sLib =
            ObjectHandler::operToVectorLibrary<%(type)s>(
            *%(name)s);\n'''

code32 = '''\
        std::vector<%(type)s> %(name)sLib =
            ObjectHandler::operToVector<%(type)s>(
                *%(name)s, "%(name)s");\n'''

code34 = '''\
        %(type)s %(name)sEnum =
            %(namespaceObjects)s::Create<%(type)s>()(%(name)s);\n'''

code35 = '''\
        std::vector<%(type)s> %(name)sEnum =
            ObjectHandler::operToVectorEnum<%(type)s,
            %(namespaceObjects)s::Create<%(type)s> >(*%(name)s);\n'''

code36a = '''\
        OH_GET_OBJECT(%(name)sObj, %(name)s, %(type)s)\n'''

code37 = '''\
        std::vector<boost::shared_ptr<%(namespaceObjects)s::%(classname)s> > %(name)sObj =
            ObjectHandler::getObjectVector<%(namespaceObjects)s::%(classname)s>(%(name)sCpp);\n'''

code38 = '''\
        OH_GET_REFERENCE(%(name)sLibObj, %(name)s,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code39 = '''\
        OH_GET_REFERENCE_DEFAULT(%(name)sLibObj, %(name)sCpp,
            %(namespaceObjects)s::%(classname)s, %(namespaceLibrary)s::%(classname)s)\n'''

code42 = '''\
        std::vector<boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> > %(name)sLibObj =
            ObjectHandler::operToObjectVector<%(namespaceLibrary)s::%(classname)s, %(namespaceObjects)s::%(classname)s>(
            *%(name)s);\n'''

code44 = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        %(namespaceLibrary)s::RelinkableHandle<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            %(namespaceObjects)s::CoerceHandle<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

code45b = '''\
        OH_GET_OBJECT(%(name)sTemp, %(name)s, ObjectHandler::Object)
        boost::shared_ptr<%(namespaceLibrary)s::%(classname)s> %(name)sLibObj =
            %(namespaceObjects)s::CoerceToObject<
                %(namespaceObjects)s::%(classname)s,
                %(namespaceLibrary)s::%(classname)s>()(
                    %(name)sTemp);\n'''

code48 = '''\
        OH_GET_UNDERLYING(%(name)sLibObj, %(name)s,
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

code64 = '''\
        static OPER xRet;
        ObjectHandler::%(tensorRank)sToOper(returnValue, xRet);
        return &xRet;'''

code65 = '''\
        static OPER xRet;
        ObjectHandler::%(tensorRank)sToOper(returnValue, xRet);
        return &xRet;'''

##########################################################################
# code for ValueObjects
##########################################################################

code66 = '''\
        else if(name == "%(name)s") return %(name)s_;\n'''

code67a = '''\
        else if(name == "%(name)s") %(name)s_ = boost::any_cast<ObjectHandler::Variant>(value);\n'''

code67b = '''\
        else if(name == "%(name)s") %(name)s_ = boost::any_cast<std::string>(value);\n'''

code67c = '''\
        else if(name == "%(name)s") %(name)s_ = boost::any_cast<%(nativeType)s>(value);\n'''

code67d = '''\
        else if(name == "%(name)s") %(name)s_ = boost::any_cast<std::vector<ObjectHandler::Variant> >(value);\n'''

code67e = '''\
        else if(name == "%(name)s") %(name)s_ = boost::any_cast<std::vector<std::string> >(value);\n'''

code67f = '''\
        else if(name == "%(name)s") %(name)s_ = boost::any_cast<std::vector<%(nativeType)s> >(value);\n'''

code67g = '''\
        else if(name == "%(name)s") %(name)s_ = boost::any_cast<std::vector<std::vector<ObjectHandler::Variant> > >(value);\n'''

code67h = '''\
        else if(name == "%(name)s") %(name)s_ = boost::any_cast<std::vector<std::vector<std::string> > >(value);\n'''

code67i = '''\
        else if(name == "%(name)s") %(name)s_ = boost::any_cast<std::vector<std::vector<%(nativeType)s> > >(value);\n'''

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

