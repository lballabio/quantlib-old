#ifndef objecthandler_h
#define objecthandler_h

#include "utilities.hpp"

typedef boost::shared_ptr<Object> obj_ptr;
typedef map<string, obj_ptr> ObjectList;

class ObjectHandler {
public:
	void storeObject(const string &handle, const obj_ptr &object);
	obj_ptr retrieveObject(const string &handle);
	ObjectHandler();
	~ObjectHandler();
private:
	ObjectList objectList_;
};

#endif
