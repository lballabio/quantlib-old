/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers
 Copyright (C) 2008 Nazcatech sprl Belgium

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/*! \file
\brief Class Processor - Restore an Object's state after deserialization
*/

#ifndef oh_processor_hpp
#define oh_processor_hpp

#include <map>
#include <string>
#include <list>

#include <boost/shared_ptr.hpp>
#include <oh/objectwrapper.hpp>

namespace ObjectHandler {

    typedef std::pair<std::string, boost::shared_ptr<Object> > StrObjectPair;
    typedef std::set<std::string> Category;
    typedef std::list<StrObjectPair> HandlesList;

    //! Restore the state of an Object after deserialization.
    /*! An abstract base class which implements the hooks which allow implementation
        of any post-serialization behavior that may be required.

        Each class which inherits from Object requires an associated Processor
        class.  If a given Object subclass does not have a Processor class
        explicitly configured for it then the DefaultProcessor class is used.
        To override this behavior, implement a custom class which inherits either
        from Processor or DefaultProcessor.

        The code to associate the custom Processor class with the given Object
        subclass is implemented in the constructor of class SerializationFactory.
        If you are using gensrc then you can autogenerate the required code
        by editing the XML function metadata for the relevant constructor and
        adding tag processorName.
    */
    class Processor {
    public:
        //! \name Structors
        //@{
        //! Empty virtual destructor.
        virtual ~Processor() {}
        //@}

        //! \name Processing
        //@{
        //! Perform any immediate post-serialization processing that is required.
        /*! This function is called immediately after the given Object
            is deserialized.  This function should make no assumptions
            about the existence of Objects other than the one being processed.
        */
        virtual std::string process(const SerializationFactory&,
            const boost::shared_ptr<ValueObject> &valueObject,
            bool overwriteExisting) const = 0;
        //! Perform post-serialization processing that may require other Objects.
        /*! This function is called after all Objects have been deserialized.
            Use this function when the post-processing of a given Object
            depends on the existence of other Objects.
        */
        virtual void postProcess() const = 0;
        //@}
    };

    //! Default behavior for post serialization processing of an Object.
    /*! This class performs the minimum post serialization processing
        required for any Object, which is to create the Object from its
        associated ValueObject.
    */
    class DefaultProcessor : public Processor {

        // Create the Object from its associated ValueObject.
        virtual std::string process(const SerializationFactory& factory,
            const boost::shared_ptr<ValueObject> &valueObject,
            bool overwriteExisting) const {

                StrObjectPair object = factory.createObject(valueObject, overwriteExisting);
                return object.first;
        }

        // Post processing - not implemented.
        virtual void postProcess() const {}
    };

    typedef boost::shared_ptr<Processor> ProcessorPtr;

    //! Manage all of the Processor objects which are defined for this environment.
    class DLL_API ProcessorFactory {

    public:
        ProcessorFactory();
        virtual ~ProcessorFactory() { instance_ = 0; }

        static ProcessorFactory& instance();

        virtual bool storeProcessor(std::string name, ProcessorPtr& ptr);

		ProcessorPtr getProcessor(const boost::shared_ptr<ValueObject> &valueObject);

        /*static int categoryIndex(const boost::shared_ptr<ValueObject>& obj, bool forComparison = false) {
            int endIndex = forComparison? LAST_SORTABLE_CATEGORY_INDEX: nrCategories;
            for(int i = 0; i < endIndex; ++i)
                if(categories[i].find(obj->className()) != categories[i].end())
                    return i;

            return nrCategories;
        }*/

		virtual void postProcess();

    protected:
        //! A pointer to the ProcessorFactory instance, used to support the Singleton pattern.
        static ProcessorFactory *instance_;
    };

}

#endif
