
/*
 Copyright (C) 2006, 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/*! \file
    \brief Class CallingRange - Processing for the host cell of an Object.
*/

#ifndef ohxl_callingrange_hpp
#define ohxl_callingrange_hpp

#include <oh/iless.hpp>
#include <ohxl/objectxl.hpp>
#include <string>
#include <map>

namespace ObjectHandler {

    //! Processing for the host cell of an Object.
    /*! The CallingRange constructor assigns a hidden Excel name to the cell which
        issued the command to construct the object.  The ObjectXL class retains a reference
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

        //! \name Management of resident objects
        //@{
        //! Indicate that the given object is resident in this range.
        void registerObject(boost::shared_ptr<ObjectXL> objectXL);
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
        //@}

    private:
        static int keyCount_;
        static std::string getKeyCount();
        static const int KEY_WIDTH;
        std::string key_;
        int updateCount_;
        typedef std::map<std::string, boost::shared_ptr<ObjectXL>, my_iless > ObjectXLMap;
        ObjectXLMap residentObjects_;
    };

    std::ostream &operator<<(std::ostream&, const boost::shared_ptr<CallingRange>&);
}

#endif

