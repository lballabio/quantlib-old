/*
 Original code from "Financial Instrucment Pricing Using C++" by Daniel Duffy

 Copyright notice pending
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
