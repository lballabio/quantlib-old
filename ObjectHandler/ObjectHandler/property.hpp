
/*
 Original code from "Financial Instrument Pricing Using C++" by Daniel Duffy

 Copyright notice pending
*/

#ifndef property_hpp
#define property_hpp

namespace ObjHandler {

    template <class Name, class Type> class Property {
    private:
        Name nam;    // The name of the property
        Type con;    // The value or contents of the property
    public:
    //  Constructors and destructor
    //  Property();
    //  Property(const Name& name);
        Property(const Name& name, const Type& t) {
            nam = name;
            con = t;
        }
    //  Property(const Property<Name, Type>& source);

        virtual ~Property() {};

    //  Accessing function operators (use operator overloading)
        virtual Type operator() () const { return con; }
        virtual void operator() (const Type& t) { con = t; }

        virtual Name name() const { return nam; }

    //  Property<Name, Type>& operator = (const Property<Name, Type>& source);

    //  Compare two properties
    //  bool operator == (const Property<Name, Type>& prop2);
    };

}

#endif
