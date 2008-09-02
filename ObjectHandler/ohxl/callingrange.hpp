/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2008 Eric Ehlers
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
    \brief Class CallingRange - Processing for the host cell of an Object
*/

#ifndef ohxl_callingrange_hpp
#define ohxl_callingrange_hpp

#include <oh/iless.hpp>
#include <ohxl/objectwrapperxl.hpp>
#include <ohxl/functioncall.hpp>
#include <string>
#include <map>
#include <boost/weak_ptr.hpp>

namespace ObjectHandler {

    //! Processing for the host cell of an Object.
    /*! The CallingRange constructor assigns a hidden Excel name to the cell which
        issued the command to construct the object.  The ObjectWrapperXL class retains a reference
        to this CallingRange object and the hidden name can be used to link back to the
        calling range even after events such as rename of the book or sheet, cut and paste, etc.

        The CallingRange object can also be queried to indicate whether the associated Excel range
        remains valid, this facilitates garbage collection of objects orphaned by the deletion
        of the calling cell.
    */
    class CallingRange {
        friend std::ostream &operator<<(std::ostream&, const boost::shared_ptr<CallingRange>&);
    public:
        //! \name Structors and static members
        //@{
        //! Constructor - assigns a name to the calling range.
        CallingRange();
        //! Destructor - presently takes no action.
        ~CallingRange();
        //! Returns the size in bytes of the values used to identify ranges.
        static int keyWidth() { return KEY_WIDTH; }
        //@}

        //! \name Object Management
        //@{
        //! Indicate that the given object is resident in this range.
        void registerObject(const std::string &objectID, boost::weak_ptr<ObjectWrapperXL> objectWrapperXL);
        //! Remove the given object from the list of resident objects.
        void unregisterObject(const std::string &objectID);
        //! Delete all objects associated with this range, e.g. if the range has been deleted.
        void clearResidentObjects(bool deletePermanent);
        //! Indicate whether any objects presently reside in this range.
        bool empty() { return residentObjects_.empty(); }
        //@}

        //! \name Inspectors
        //@{
        //! The unique key assigned internally to this calling range.
        const std::string &key() const { return key_; }
        //! The address of the range as a string.
        /*! The address is derived anew each time this function is called.
            This allows for any changes resulting e.g. from cut and paste operations.
            However the call is expensive therefore it is only to be invoked during
            non-critical operations.
        */
        std::string addressString() const;
        //! Determine whether the calling range is still valid.
        bool valid() const;
        //! The number of times this cell has been refreshed.
        std::string updateCount();
        //get the number of times  this cell has been refreshed.
        std::string getUpdateCount();
        //@}

        //! Convert a full ID to a normal one.
        /*! Instances of class Object are identified by their ID.
            ObjectWrapperXL recognizes this "normal" ID and also a "full" ID which has
            been suffixed with the cell's update count e.g. my_object#00123.
            This function accepts a string which may be either a normal or full ID,
            if full the suffix is removed, if normal the value is returned unmodified.
        */
        static DLL_API std::string getStub(const std::string &objectID);

        std::string initializeID(const std::string &objectID);
        std::string updateID(const std::string &objectID);

    private:
        static int keyCount_;
        static std::string getKeyCount();
        static const int KEY_WIDTH;
        std::string key_;
        int updateCount_;
        typedef std::map<std::string, boost::weak_ptr<ObjectWrapperXL>, my_iless > ObjectXLMap;
        ObjectXLMap residentObjects_;
        CallerType::Type callerType_;
    };

    std::ostream &operator<<(std::ostream&, const boost::shared_ptr<CallingRange>&);
}

#endif

