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
