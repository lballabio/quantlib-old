#include <interface.hpp>
#include <ObjectHandler/exception.hpp>

extern ObjectHandler objectHandler;

Properties WIDGET_MAKE(
		const std::string &handle,
		const std::string &s,
		const int &i) {
	obj_ptr object(new ObjectWidget(s, i));
	objectHandler.storeObject(handle, object);
	return object->getProperties();
}
                                                                                                 
Properties WIDGET_UPDATE(
		const std::string &handle,
		const std::string &s,
		const int &i) {
	boost::shared_ptr<ObjectWidget> object =
		boost::dynamic_pointer_cast<ObjectWidget>
		(objectHandler.retrieveObject(handle));
	if (!object)
		throw Exception("WIDGET_UPDATE: unable to retrieve object " + handle);
	object->update(s, i);
	return object->getProperties();
}

void QL_LOGFILE(
		const std::string &logFileName) {
	setLogFile(logFileName);
}

void QL_LOGMESSAGE(
		const std::string &msg) {
	logMessage(msg);
}
