#include "objecthandler.hpp"
#include <iostream>
#include <sstream>

ObjectHandler objectHandler; // FIXME

ObjectHandler::ObjectHandler() {
}

ObjectHandler::~ObjectHandler() {
}

void ObjectHandler::storeObject(const string &handle, const obj_ptr &object) {
	objectList_[handle] = object;
}

obj_ptr ObjectHandler::retrieveObject(const string &handle) {
	return objectList_[handle];
}

