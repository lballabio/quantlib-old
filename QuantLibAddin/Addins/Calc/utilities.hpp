#ifndef utilitiesxx_h
#define utilitiesxx_h

ANY anyToANY(const ObjHandler::any_ptr &a); // convert boost::any to Calc Any
SEQSEQ( ANY ) getArray(ObjHandler::obj_ptr object,
                       STRING handle);
string OUStringToString(const STRING& s1);
ANY stringToANY(const string &s);

#endif
