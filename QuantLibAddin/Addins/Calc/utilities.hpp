#ifndef utilitiesxx_h
#define utilitiesxx_h

ANY anyToANY(const any_ptr &a); // convert boost::any to Calc Any
SEQSEQ( ANY ) getArray(obj_ptr object, STRING handle);
string OUStringToString(const STRING& s1);

#endif
