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

#include <ObjectHandler/property.hpp>

template <class Name, class Type>
	Property < Name, Type>::Property(const Name& name, const Type& t)
{ // Constructor

	nam = name;
	con = t;
}

template <class Name, class Type>
	Type Property<Name, Type>::operator() () const
{	// Getting the current value of the property
	return con;
}

template <class Name, class Type>
void Property<Name, Type>::operator() (const Type& t)
{	// Setting the value (contents) to a given value
	con = t;
}

template <class Name, class Type>
Name Property<Name, Type>::name() const
{
	return nam;
}
