
/*
 Original code from "Financial Instrument Pricing Using C++" by Daniel Duffy

 Copyright notice pending
*/

/*! \file
    \brief Property class - abstract implementation of an Object attribute
*/

#ifndef oh_property_hpp
#define oh_property_hpp

namespace ObjHandler {

    //! Template class representing an Object attribute.
    /*! Attributes of class Object are implemented
        as a vector of Property name / value pairs.
    */
    template <class Name, class Type> class Property {
      private:
        Name nam;    // The name of the property
        Type con;    // The value or contents of the property
      public:
        //! \name Constructors / Destructors
        //@{
        //  Property();
        //  Property(const Name& name);
        //! Construct a Property with given name and value.
        Property(const Name& name, const Type& t) {
            nam = name;
            con = t;
        }
        //  Property(const Property<Name, Type>& source);

        virtual ~Property() {};
        //@}
        //! \name Accessing function operators
        //@{
        //! Get the Property's value
        virtual Type operator() () const { return con; }
        //! Set the Property's value
        virtual void operator() (const Type& t) { con = t; }
        //! Get the Property's name
        virtual Name name() const { return nam; }
        //@}

        //  Property<Name, Type>& operator = (const Property<Name, Type>& source);

        //  Compare two properties
        //  bool operator == (const Property<Name, Type>& prop2);
    };

}

#endif

