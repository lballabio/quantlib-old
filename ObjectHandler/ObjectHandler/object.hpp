
/*
 Copyright (C) 2004 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef object_h
#define object_h

#include <boost/shared_ptr.hpp>
#include <boost/any.hpp>
#include <map>
#include <vector>
#include <string>
using namespace std;

typedef boost::shared_ptr<boost::any> any_ptr;
typedef map<string, any_ptr> ValueList;

class Object {
public:
	Object();
	virtual ~Object();
	vector < string > getFieldNames();
	any_ptr getValue(const string &fieldName);
	virtual const boost::shared_ptr<void> getReference() = 0;
protected:
	ValueList valueList_;
	vector < string > fieldNames_;
};

#endif
