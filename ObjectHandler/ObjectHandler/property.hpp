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
