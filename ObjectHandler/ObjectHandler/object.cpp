#include "object.hpp"
#include "utilities.hpp"

Object::Object()
{
}

Object::~Object()
{
}

vector < string > Object::getFieldNames()
{
	return fieldNames_;
}

any_ptr Object::getValue(const string &fieldName)
{
	return valueList_[toUpper(fieldName)];
}
