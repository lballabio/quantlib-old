/*
 This code taken from "Financial Instrucment Pricing Using C++"
	 by Daniel Duffy
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

template <class Name, class Type> class Property {
private:
	Name nam;	// The name of the property
	Type con;	// The value or contents of the property
public:
	// Constructors and destructor
//	Property();
//	Property(const Name& name);
	Property(const Name& name, const Type& t);
//	Property(const Property<Name, Type>& source);

//	virtual ~Property{};

	// Accessing function operators (use operator overloading)
	virtual Type operator() () const;
	virtual void operator() (const Type& t);

	virtual Name name() const;

//	Property<Name, Type>& operator = (const Property<Name, Type>& source);

	// Compare two properties
//	bool operator == (const Property<Name, Type>& prop2);
};
