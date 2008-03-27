/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers

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
    \brief Class ObjectXL - Customization of the Object class for the Excel platform
*/

#ifndef oh_objectxl_hpp
#define oh_objectxl_hpp

#include <oh/object.hpp>

namespace ObjectHandler {

    class CallingRange;

    //! Customization of the Object class for the Excel platform.
    /*! It is not possible to use simple inheritance from the Object class as that
        would impact application libraries who also inherit directly from Object.
        Therefore this class comprises a rudimentary implementation of the Decorator
        design pattern: ObjectXL both inherits from Object, and holds a reference to
        an instance of an Object.  Clients of ObjectXL can access ObjectXL via the
        standard Object interface, ObjectXL forwards the calls to the contained Object,
        where necessary adding additional support for the Excel platform e.g.:
        - Retain a reference to the cell which constructed the Object
        - Suffix the Object's ID with an update count e.g. my_object#00123
    */
    class ObjectXL : public Object {
    public:

        //! \name Structors and static members
        //@{

        //! Constructor - accepts ID and reference to contained Object.
        /*! Unlike Object, ObjectXL knows its own ID as additional functionality
            is built around this value.
        */
        ObjectXL(const std::string &id, const boost::shared_ptr<Object> &object);

        //! Destructor - de-register this object with its CallingRange object.
        virtual ~ObjectXL();

        //! Convert a full ID to a normal one.
        /*! Instances of class Object are identified by their ID.
            ObjectXL recognizes this "normal" ID and also a "full" ID which has
            been suffixed with the cell's update count e.g. my_object#00123.
            This function accepts a string which may be either a normal or full ID,
            if full the suffix is removed, if normal the value is returned unmodified.
        */
        static DLL_API std::string ObjectXL::getStub(const std::string &objectID);

        //! Simple factory for ObjectXL objects.
        /*! This function is only required when ObjectHandler functionality is
            split across multiple XLLs which are linked together at runtime.
            The normal ObjectXL constructor cannot be invoked across DLL boundaries
            and this function provides a workaround for the problem.
        */
//#ifdef COMPILING_XLL_DYNAMIC
//        static DLL_API boost::shared_ptr<ObjectXL> create(
//            const std::string &id, const boost::shared_ptr<Object> &object);
//#endif
        //@}

        //! \name Inspectors
        //@{
        //! Retrieve the object's ID.
        const std::string &id() const { return id_; }
        //! Retrieve the object's ID as reformatted for the Excel platform.
        const std::string &idFull() const { return idFull_; }
        //! Return a copy of the reference to the Object contained by ObjectXL.
        boost::shared_ptr<Object> object() const { return object_; }
        //@}

        //! \name CallingRange Management
        //@{
        //! Set a reference to the calling cell.
        /*! If this object is constructed with a call from Excel VBA (rather than a cell
            formula) then this function is not called and the CallingRange reference
            remains empty.
        */
        void setCallingRange(const boost::shared_ptr<CallingRange> &callingRange);
        //! Get the key identifying the CallingRange object.
        /*! Returns the string "VBA" if this object was constructed by VBA code.
        */
        std::string callerKey() const;
        //! Get the address of the calling cell.
        /*! Returns the string "VBA" if this object was constructed by VBA code.
        */
        std::string callerAddress() const;
        //@}

        //! \name Logging
        //@{
        //! Write this object to the given output stream.
        /*! Called by the logging framework.  Enhanced from the base class function
            to provide additional information specific to the Excel platform.
        */
        virtual void dump(std::ostream &out);
        //@}

        //! \name Permanent Objects
        //@{
        //! Query the value of the "permanent" flag.
        /*! The request is forwarded to the Object contained by this ObjectXL.
        */
        const virtual bool &permanent() const { return object_->permanent(); }
        //@}

    private:
        // Reference to the Object contained by ObjectXL.
        boost::shared_ptr<Object> object_;
        // Reference to the worksheet cell in which this object resides.
        boost::shared_ptr<CallingRange> callingRange_;
        // This object's ID, in the same format used for the base Object class.
        std::string id_;
        // This object's ID, reformatted for Excel with the update count e.g. my_object#00123.
        // For objects created from VBA, the full ID is the same as the normal one.
        std::string idFull_;
    };

}

#endif

