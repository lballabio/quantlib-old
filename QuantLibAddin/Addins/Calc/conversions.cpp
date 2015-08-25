
/*
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <oh/objecthandler.hpp>
#include <qldefs.hpp>
#include <sstream>
#include <conversions.hpp>
// QuantLib
#include <ql/quantlib.hpp>


std::string ouStringToStlString(const STRING& s1) {
    ::rtl::OString s2;
    if (s1.convertToString(&s2, RTL_TEXTENCODING_ASCII_US, 
        OUSTRING_TO_OSTRING_CVTFLAGS))
        return s2.getStr();
    else
        throw ObjectHandler::Exception("ouStringToStlString: unable to convert string");
}

ANY stlStringToCalcAny(const std::string &s) {
    STRING s2 = STRFROMASCII( s.c_str() );
    return CSS::uno::makeAny(s2);
}

/*
SEQSEQ(ANY) boostAnyToSeqSeq(const ObjectHandler::any_ptr &a) {
    if (a->type() == typeid(long)) {
        long l = boost::any_cast< long >(*a);
        sal_Int32 l2 = static_cast< sal_Int32 >(l);
        SEQSEQ( ANY ) ss(1);
        SEQ( ANY ) s(1);
        s[0] = CSS::uno::makeAny(l2);
        ss[0] = s;
        return ss;
    } else if (a->type() == typeid(double)) {
        double d = boost::any_cast< double >(*a);
        SEQSEQ( ANY ) ss(1);
        SEQ( ANY ) s(1);
        s[0] = CSS::uno::makeAny(d);
        ss[0] = s;
        return ss;
    } else if (a->type() == typeid(bool)) {
        bool b = boost::any_cast< bool >(*a);
        SEQSEQ( ANY ) ss(1);
        SEQ( ANY ) s(1);
        sal_Int32 b2 = static_cast< sal_Int32 >(b);
        s[0] = CSS::uno::makeAny(b2);
        ss[0] = s;
        return ss;
    } else if (a->type() == typeid(std::string)) {
        std::string str = boost::any_cast<std::string>(*a);
        SEQSEQ( ANY ) ss(1);
        SEQ( ANY ) s(1);
        s[0] = CSS::uno::makeAny(STRFROMASCII(str.c_str()));
        ss[0] = s;
        return ss;
    } else if (a->type() == typeid( boost::any )) {
        boost::any a2 = boost::any_cast< boost::any >(*a);
        SEQSEQ( ANY ) ss(1);
        SEQ( ANY ) s(1);
        scalarToCalc(s[0], a2);
        ss[0] = s;
        return ss;
    } else if (a->type() == typeid(std::vector< long >)) {
        std::vector< long > v= boost::any_cast< std::vector< long > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); ++i) {
            SEQ( ANY ) s(1);
            s[0] = CSS::uno::makeAny(v[i]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< double >)) {
        std::vector< double > v= boost::any_cast< std::vector< double > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); ++i) {
            SEQ( ANY ) s(1);
            s[0] = CSS::uno::makeAny(v[i]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< bool >)) {
        std::vector< bool > v= boost::any_cast< std::vector< bool > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); ++i) {
            SEQ( ANY ) s(1);
            sal_Int32 b = static_cast< sal_Int32 >(v[i]);
            s[0] = CSS::uno::makeAny(b);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector<std::string>)) {
        std::vector<std::string> v= boost::any_cast< std::vector<std::string> >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); ++i) {
            SEQ( ANY ) s(1);
            s[0] = CSS::uno::makeAny(STRFROMASCII(v[i].c_str()));
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< boost::any >)) {
        std::vector< boost::any > v= boost::any_cast< std::vector< boost::any > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); ++i) {
            SEQ( ANY ) s(1);
            scalarToCalc(s[0], v[i]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< long > >)) {
        std::vector< std::vector< long > > vv= boost::any_cast< std::vector< std::vector< long > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); ++i) {
            std::vector< long > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); ++j)
                s[j] = CSS::uno::makeAny(v[j]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< double > >)) {
        std::vector< std::vector< double > > vv= boost::any_cast< std::vector< std::vector< double > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); ++i) {
            std::vector< double > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); ++j)
                s[j] = CSS::uno::makeAny(v[j]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< bool > >)) {
        std::vector< std::vector< bool > > vv= boost::any_cast< std::vector< std::vector< bool > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); ++i) {
            std::vector< bool > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); ++j) {
                sal_Int32 b = static_cast< sal_Int32 >(v[j]);
                s[j] = CSS::uno::makeAny(b);
            }
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< std::string > >)) {
        std::vector< std::vector< std::string > > vv= boost::any_cast< std::vector< std::vector< std::string > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); ++i) {
            std::vector< std::string > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); ++j)
                s[j] = CSS::uno::makeAny(STRFROMASCII(v[j].c_str()));
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< boost::any > >)) {
        std::vector< std::vector< boost::any > > vv= boost::any_cast< std::vector< std::vector< boost::any > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); ++i) {
            std::vector< boost::any > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); ++j)
                scalarToCalc(s[j], v[j]);
            ss[i] = s;
        }
        return ss;
    } else
        throw ObjectHandler::Exception("boostAnyToSeqSeq: unable to interpret value");
}
*/

// conversions from native C++ datatypes to Calc datatypes

void scalarToCalc(sal_Int32 &ret, const bool &value) {
    ret = value;
}

void scalarToCalc(STRING &ret, const std::string &value) {
    ret = STRFROMANSI(value.c_str());
}

void scalarToCalc(ANY &ret, const boost::any &value) {
    if (value.type() == typeid(QuantLib::Date)) {
    	QuantLib::Date temp1 = boost::any_cast<QuantLib::Date>(value);
        sal_Int32 temp2 = static_cast< sal_Int32 >(temp1.serialNumber());
        ret = CSS::uno::makeAny(temp2);
    } else if (value.type() == typeid(bool)) {
        bool temp = boost::any_cast< bool >(value);
        sal_Int32 temp2 = static_cast< sal_Int32 >(temp*1);
        ret = CSS::uno::makeAny(temp2);
    } else if (value.type() == typeid(short int) ) {
        short int temp1 = boost::any_cast<short int>(value);
        sal_Int32 temp2 = static_cast< sal_Int32 >(temp1);
        ret = CSS::uno::makeAny(temp2);
    } else if (value.type() == typeid(int) ) {
        int temp1 = boost::any_cast<int>(value);
        sal_Int32 temp2 = static_cast< sal_Int32 >(temp1);
        ret = CSS::uno::makeAny(temp2);
    } else if (value.type() == typeid(long)) {
        long temp1 = boost::any_cast< long >(value);
        sal_Int32 temp2 = static_cast< sal_Int32 >(temp1);
        ret = CSS::uno::makeAny(temp2);
    } else if (value.type() == typeid(QuantLib::Size)) {
        QuantLib::Size temp1 = boost::any_cast< QuantLib::Size >(value);
        sal_Int32 temp2 = static_cast< sal_Int32 >(temp1);
        ret = CSS::uno::makeAny(temp2);
    } else if (value.type() == typeid(QuantLib::Natural)) {
        QuantLib::Natural temp1 = boost::any_cast< QuantLib::Natural >(value);
        sal_Int32 temp2 = static_cast< sal_Int32 >(temp1);
        ret = CSS::uno::makeAny(temp2);
    } else if (value.type() == typeid(double)) {
        double temp = boost::any_cast<double>(value);
        ret = CSS::uno::makeAny(temp);
    } else if (value.type() == typeid(QuantLib::InterestRate)) {
        QuantLib::InterestRate temp1 = boost::any_cast<QuantLib::InterestRate>(value);
        ret = CSS::uno::makeAny(temp1.rate());
    } else if (value.type() == typeid(QuantLib::Calendar)) {
    	  QuantLib::Calendar temp1 = boost::any_cast<QuantLib::Calendar>(value);
		  ret = stlStringToCalcAny( temp1.name().c_str() );
    } else if (value.type() == typeid(QuantLib::BusinessDayConvention)) {
    	  QuantLib::BusinessDayConvention temp1 = boost::any_cast<QuantLib::BusinessDayConvention>(value);
		  std::ostringstream strs;    	  
		  strs << temp1;
		  std::string str = strs.str();
		  ret = stlStringToCalcAny( str.c_str() );
    } else if (value.type() == typeid(QuantLib::DateGeneration::Rule)) {
    	  QuantLib::DateGeneration::Rule temp1 = boost::any_cast<QuantLib::DateGeneration::Rule>(value);
		  std::ostringstream strs;    	  
		  strs << temp1;
		  std::string str = strs.str();
		  ret = stlStringToCalcAny( str.c_str() );
    } else if (value.type() == typeid(QuantLib::Period)) {
    	  QuantLib::Period temp1 = boost::any_cast<QuantLib::Period>(value);
		  std::ostringstream strs;    	  
		  strs << temp1;
		  std::string str = strs.str();
		  ret = stlStringToCalcAny( str.c_str() );
    } else if (value.type() == typeid(QuantLib::Compounding)) {
    	  QuantLib::Compounding temp1 = boost::any_cast<QuantLib::Compounding>(value);
    	  std::string comp;
    	  
    	  switch(temp1) {
    	     case QuantLib::Simple:
    	     	comp = "Simple";
    		break;
    	     case QuantLib::Compounded:
    	     	comp = "Compounded";
    		break;
    	     case QuantLib::Continuous:
    	     	comp = "Continuous";
    		break;
    	     default:
    	     	comp = "ContThanComp";
    	  }
		  ret = stlStringToCalcAny( comp.c_str() );
    } else if (value.type() == typeid(QuantLib::Frequency)) {
    	  QuantLib::Frequency temp1 = boost::any_cast<QuantLib::Frequency>(value);
		  std::ostringstream strs;    	  
		  strs << temp1;
		  std::string str = strs.str();
		  ret = stlStringToCalcAny( str.c_str() );
    } else if (value.type() == typeid(QuantLib::Settlement::Type)) {
    	  QuantLib::Settlement::Type temp1 = boost::any_cast<QuantLib::Settlement::Type>(value);
		  std::ostringstream strs;    	  
		  strs << temp1;
		  std::string str = strs.str();
		  ret = stlStringToCalcAny( str.c_str() );
    } else if (value.type() == typeid(QuantLib::VanillaSwap::Type)) {
  	      QuantLib::VanillaSwap::Type temp1 = boost::any_cast<QuantLib::VanillaSwap::Type>(value);
		  std::ostringstream strs;    	  
		  strs << temp1;
		  std::string str = strs.str();
		  ret = stlStringToCalcAny( str.c_str() );
    } else if (value.type() == typeid(QuantLib::ZeroCouponInflationSwap::Type)) {
	      QuantLib::ZeroCouponInflationSwap::Type temp1 = boost::any_cast<QuantLib::ZeroCouponInflationSwap::Type>(value);
		  std::ostringstream strs;    	  
		  strs << temp1;
		  std::string str = strs.str();
		  ret = stlStringToCalcAny( str.c_str() );
    } else if (value.type() == typeid(std::string)) {
        std::string temp = boost::any_cast<std::string>(value);
        ret = stlStringToCalcAny(temp);
    } else if (value.type() == typeid(ObjectHandler::property_t)) {
        ObjectHandler::property_t temp1 = boost::any_cast<ObjectHandler::property_t>(value);

		bool found = false;
		if(!found) {
			try {
				std::string temp2 = ObjectHandler::convert2<std::string>(temp1);
				ret = stlStringToCalcAny(temp2);
				found = true;
			} catch (const std::exception &e) {
			}
		}
		if(!found) {
			try {
				bool temp2 = ObjectHandler::convert2<bool>(temp1);
				sal_Int32 temp3 = static_cast< sal_Int32 >(temp2);
				ret = CSS::uno::makeAny(temp3);
				found = true;
			} catch (const std::exception &e) {
			}
		}
		if(!found) {
			try {
				ObjectHandler::empty_property_tag temp2 = ObjectHandler::convert2<ObjectHandler::empty_property_tag>(temp1);
				ret = stlStringToCalcAny(std::string(""));
				found = true;
			} catch (const std::exception &e) {
			}
		}
		if(!found) {
			try {
				long temp2 = ObjectHandler::convert2<long>(temp1);
				sal_Int32 temp3 = static_cast< sal_Int32 >(temp2);
				ret = CSS::uno::makeAny(temp3);
				found = true;
			} catch (const std::exception &e) {
			}
		}
		if(!found) {
			try {
				double temp2 = ObjectHandler::convert2<double>(temp1);
					ret = CSS::uno::makeAny(temp2);
				found = true;
			} catch (const std::exception &e) {
			}
		}
		if(!found)
				ret = stlStringToCalcAny(std::string("Cannot convert property_t!"));
        
   } else if (value.type() == typeid(boost::any)) {
//        boost::any a2 = boost::any_cast< boost::any >(value);
//        return boostAnyToCalcAny(a2);
//        ret = CSS::uno::makeAny(STRFROMASCII("unknown type"));
        throw ObjectHandler::Exception("scalarToCalc: unable to interpret value");
    } else if (value.type() == typeid(std::vector< int >)
           ||  value.type() == typeid(std::vector< long >)
           ||  value.type() == typeid(std::vector< double >)
           ||  value.type() == typeid(std::vector< bool >)
           ||  value.type() == typeid(std::vector< std::string >)
           ||  value.type() == typeid(std::vector< boost::any >)) {
        ret = CSS::uno::makeAny(STRFROMASCII("<VECTOR>"));
    } else if (value.type() == typeid(std::vector< std::vector< int > >)
           ||  value.type() == typeid(std::vector< std::vector< long > >)
           ||  value.type() == typeid(std::vector< std::vector< double > >)
           ||  value.type() == typeid(std::vector< std::vector< bool > >)
           ||  value.type() == typeid(std::vector< std::vector< std::string > >)
           ||  value.type() == typeid(std::vector< std::vector< boost::any > >)) {
        ret = CSS::uno::makeAny(STRFROMASCII("<MATRIX>"));
    } else
//        ret = CSS::uno::makeAny(STRFROMASCII("unknown type"));
        throw ObjectHandler::Exception("scalarToCalc: unable to interpret value");
}
/*
void scalarToCalc(ANY &ret, const ObjectHandler::property_t &value) {
        if (double* val = boost::get<double>(&value)) {
            ret = CSS::uno::makeAny(*val);
	} else if (std::string* val = boost::get<std::string>(&value)) {
	    ret = stlStringToCalcAny(*val);
        } else if (long* val = boost::get<long>(&value)) {
            ret = CSS::uno::makeAny(*val);
        } else if (bool* val = boost::get<bool>(&value)) {
            sal_Int32 b2 = static_cast< sal_Int32 >(*val);
            ret = CSS::uno::makeAny(b2);
        } else 
	        throw ObjectHandler::Exception("[Attempt to log a value of unknown datatype ]");
} */

void scalarToCalc(long &ret, const QuantLib::Date &in) {
    ret = in.serialNumber();
}

void scalarToCalc(long &ret, const int &in) {
    ret = (long)(in);
}

void scalarToCalc(int /*sal_Int32*/ &ret, const QuantLib::Date &in) {
    ret = in.serialNumber();
}

void scalarToCalc(double &ret, const double &in) {
	ret = in;
}


#ifndef WIN32
void scalarToCalc(long &ret, const QuantLib::Natural &in) {
    ret = (long)(in);
}

void scalarToCalc(long &ret, const QuantLib::Size &in) {
    ret = (long)(in);
}
#endif 

void scalarToCalc(STRING &ret, const QuantLib::Calendar &in) {
  ret = STRFROMASCII( in.name().c_str() );
}

void scalarToCalc(STRING &ret, const QuantLib::InterestRate  &in) {
  std::ostringstream strs;
  double res = (double)(in.rate());
  
  strs << res;
  std::string str = strs.str();
  
  ret = STRFROMASCII( str.c_str() );
}

void scalarToCalc(STRING &ret, const QuantLib::BusinessDayConvention  &in) {
  std::ostringstream strs;
  
  strs << in;
  std::string str = strs.str();
  
  ret = STRFROMASCII( str.c_str() );
}

void scalarToCalc(STRING &ret, const QuantLib::DateGeneration::Rule  &in) {
  std::ostringstream strs;
  
  strs << in;
  std::string str = strs.str();
  
  ret = STRFROMASCII( str.c_str() );
}

void scalarToCalc(STRING &ret, const QuantLib::Period  &in) {
  std::ostringstream strs;
  
  strs << in;
  std::string str = strs.str();
  
  ret = STRFROMASCII( str.c_str() );
}

void scalarToCalc(STRING &ret, const QuantLib::Compounding  &in) {
  std::string comp;
  
  switch(in) {
     case QuantLib::Simple:
     	comp = "Simple";
	break;
     case QuantLib::Compounded:
     	comp = "Compounded";
	break;
     case QuantLib::Continuous:
     	comp = "Continuous";
	break;
     default:
     	comp = "ContThanComp";
  }
  
  ret = STRFROMASCII( comp.c_str() );
}

void scalarToCalc(STRING &ret, const QuantLib::DayCounter  &in) {
  ret = STRFROMASCII( in.name().c_str() );
}

void scalarToCalc(STRING &ret, const QuantLib::Frequency  &in) {
  std::ostringstream strs;
  
  strs << in;
  std::string str = strs.str();
  
  ret = STRFROMASCII( str.c_str() );
}

void scalarToCalc(STRING &ret, const QuantLib::Settlement::Type  &in) {
  std::ostringstream strs;
  
  strs << in;
  std::string str = strs.str();
  
  ret = STRFROMASCII( str.c_str() );
}

void scalarToCalc(STRING &ret, const QuantLib::VanillaSwap::Type  &in) {
  std::ostringstream strs;
  
  strs << in;
  std::string str = strs.str();
  
  ret = STRFROMASCII( str.c_str() );
}

void scalarToCalc(STRING &ret, const QuantLib::ZeroCouponInflationSwap::Type  &in) {
  std::ostringstream strs;
  
  strs << in;
  std::string str = strs.str();
  
  ret = STRFROMASCII( str.c_str() );
}

// conversions from Calc datatypes to native C++ datatypes

void calcToScalar(bool &ret, const sal_Int32 &value) {
    ret = value != 0;
}

void calcToScalar(QuantLib::Natural &ret, sal_Int32 &value) {
  long temp;
  value >>= temp;
  ret = temp;
}

// void calcToScalar(QuantLib::Integer &ret, sal_Int32 &value) {
//   long temp;
//   value >>= temp;
//   ret = temp;
// }

void calcToScalar(long int &ret, const int &value) {
  ret = (long int)(value);
}

void calcToScalar(int &ret, sal_Int32 &value) {
  long temp;
  value >>= temp;
  ret = temp;
}

void calcToScalar(unsigned int &ret, long const &value) {
  ret = (unsigned int)(value);
}

void calcToScalar(unsigned int &ret, int const &value) {
  ret = (unsigned int)(value);
}

#ifndef WIN32
void calcToScalar(QuantLib::Size &ret, const sal_Int32 &value) {
  ret = (QuantLib::Size)(value);
}

void calcToScalar(QuantLib::Size &ret, const ANY &value) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = static_cast < QuantLib::Size > (temp);
    } else if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        long temp;
        value >>= temp;
        ret = temp;
    } else {
        std::ostringstream msg;
        msg << "unrecognized type: " << ouStringToStlString(typeName);
        throw ObjectHandler::Exception(msg.str());
    }
}
#endif 

void calcToScalar(long &ret, const ANY &value) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = (long)(temp);
    } else if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        long temp;
        value >>= temp;
        ret = temp;
    } else
        ret = 0;
}

void calcToScalar(QuantLib::Natural &ret, const ANY &value) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = static_cast < QuantLib::Natural > (temp);
    } else if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        long temp;
        value >>= temp;
        ret = temp;
    } else {
        std::ostringstream msg;
        msg << "unrecognized type: " << ouStringToStlString(typeName);
        throw ObjectHandler::Exception(msg.str());
    }
}


void calcToScalar(QuantLib::Integer &ret, const ANY &value) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = static_cast < QuantLib::Integer > (temp);
    } else if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        long temp;
        value >>= temp;
        ret = temp;
    } else {
        std::ostringstream msg;
        msg << "unrecognized type: " << ouStringToStlString(typeName);
        throw ObjectHandler::Exception(msg.str());
    }
}

void calcToScalar(QuantLib::Real &ret, const double id) {
    ret = id;
}

void calcToScalar(QuantLib::Period &ret, const STRING &id) {
    std::string idCpp = ouStringToStlString(id);
    ret = QuantLib::PeriodParser::parse(idCpp);
}


void calcToScalar(QuantLib::Period &ret, const ANY &value) {
    STRING t = value.getValueTypeName();
	if (t.equalsIgnoreAsciiCase(STRFROMANSI("STRING"))) {
	        //std::cout << "ANY value type STRING" << std::endl;
	        STRING temp;
	        value >>= temp;
	        ret = QuantLib::PeriodParser::parse(ouStringToStlString(temp));
	    /*} else if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
	        ret = std::string("<MATRIX>");*/
	    } else {
	    	ret = QuantLib::Period(0, QuantLib::Days);
	    }
}


void calcToScalar(double &ret, const ANY &value) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = temp;
    } else if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        long temp;
        value >>= temp;
        ret = (double)(temp);
    } else
        ret = 0.0;
}

void calcToScalar(bool &ret, const ANY &value) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("BOOL"))) {
        bool temp;
        value >>= temp;
        ret = (temp != 0);
    } else
        ret = false;
}

void calcToScalar(QuantLib::Date &ret, const sal_Int32 &date) {
    ret = QuantLib::Date(date);
}

void calcToScalar(QuantLib::Date &ret, const ANY &date) {
    long dateLong;
    calcToScalar(dateLong, date);
    // FIXME 
    if (dateLong == 0)
      ret = QuantLib::Date();
    else
      ret = QuantLib::Date(dateLong);
}

void calcToScalar(ObjectHandler::property_t &ret, const ANY &value) {
    STRING t = value.getValueTypeName();
    //std::cout << "calcToScalar ANY to property_t called" << std::endl;
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("VOID"))) {
        //std::cout << "ANY value type VOID" << std::endl;
        ret = ObjectHandler::property_t();
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        //std::cout << "ANY value type LONG" << std::endl;
        long temp;
        value >>= temp;
        ret = ObjectHandler::property_t(temp);
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        //std::cout << "ANY value type DOUBLE" << std::endl;
        double temp;
        value >>= temp;
        ret = ObjectHandler::property_t(temp);
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("STRING"))) {
        //std::cout << "ANY value type STRING" << std::endl;
        STRING temp;
        value >>= temp;
        ret = ObjectHandler::property_t(ouStringToStlString(temp));
    /*} else if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
        ret = std::string("<MATRIX>");*/
    } else {
        OH_FAIL("unrecognized type: " << ouStringToStlString(t));
    }
}

void calcToScalar(std::string &ret, const ANY &value) {
    STRING temp;
    value >>= temp;
    ret = ouStringToStlString(temp);
}

void calcToScalar(boost::any &ret, const ANY &value) {
    STRING t = value.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("VOID"))) {
        ret = boost::any();
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        long temp;
        value >>= temp;
        ret = temp;
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = temp;
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("STRING"))) {
        STRING temp;
        value >>= temp;
        ret = ouStringToStlString(temp);
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
        ret = std::string("<MATRIX>");
    } else {
        std::ostringstream msg;
        msg << "unrecognized type: " << ouStringToStlString(t);
        ret = msg.str();
    }
}

void calcToVector(std::vector< QuantLib::Date > &ret, const SEQSEQ( ANY )& ss) {
    for (int i=0; i<ss.getLength(); ++i) {
        for (int j=0; j<ss[i].getLength(); ++j) {
            QuantLib::Date temp;
            calcToScalar(temp, ss[i][j]);
            ret.push_back(temp);
        }
    }
}

void calcToVector(std::vector< bool > &ret, const SEQSEQ( sal_Int32 )& ss) {
    for (int i=0; i<ss.getLength(); ++i) {
        for (int j=0; j<ss[i].getLength(); ++j) {
            bool temp;
            calcToScalar(temp, ss[i][j]);
            ret.push_back(temp);
        }
    }
}

void calcToVector(std::vector< double > &ret, const SEQSEQ( double )& ss) {
    for (int i=0; i<ss.getLength(); ++i) {
        for (int j=0; j<ss[i].getLength(); ++j) {
            double temp;
            calcToScalar(temp, ss[i][j]);
            ret.push_back(temp);
        }
    }
}

void calcToVector(std::vector< ObjectHandler::property_t > &ret, const SEQSEQ( ANY )& ss) {
    for (int i=0; i<ss.getLength(); ++i) {
        for (int j=0; j<ss[i].getLength(); ++j) {
            ObjectHandler::property_t temp;
            calcToScalar(temp, ss[i][j]);
            ret.push_back(temp);
        }
    }
}

void calcToVector(std::vector< std::string > &ret, const SEQSEQ( ANY )& ss) {
    for (int i=0; i<ss.getLength(); ++i) {
        for (int j=0; j<ss[i].getLength(); ++j) {
            std::string temp;
            calcToScalar(temp, ss[i][j]);
            ret.push_back(temp);
        }
    }
}

void vectorToCalc(SEQSEQ( sal_Int32 ) &ret, const std::vector < QuantLib::Date > &v) {
    ret.realloc(v.size());
    for (unsigned int i=0; i<v.size(); ++i) {
        SEQ( sal_Int32 ) s(1);
        scalarToCalc(s[0], v[i]);
        ret[i] = s;
    }
}



