
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
    \brief Class Configuration - Global configuration information
*/

#ifndef ohxl_configuration_hpp
#define ohxl_configuration_hpp

#include <oh/singleton.hpp>

namespace ObjectHandler {

    //! Global configuration information specific to the Excel session.
    class Configuration : public Singleton<Configuration> {
        friend class Singleton<Configuration>;
    public:
        //! \name Initialization
        //@{
        //! Constructor - sets initialized flag to false.
        Configuration() : initialized_(false) {}
        //! Initialize the global Configuration Singleton object.
        /*! The app must call this function at startup e.g. 
                Configuration::instance().init()
            Failure to do so will lead to runtime errors later as ObjectHandler
            functions attempt to access the configuration information.
        */
        void init();
        //@}

        //! \name Inspectors
        //@{
        //! Get the row character for this installation of Excel.
        /*! This is "R" for English versions of Excel but may be a different
            letter in other languages.
        */
        const char &rowCharacter();
        //! Get the column character for this installation of Excel.
        /*! This is "C" for English versions of Excel but may be a different
            letter in other languages.
        */
        const char &colCharacter();
        //@}
    private:
        bool initialized_;
        char rowCharacter_;
        char colCharacter_;
    };

}

#endif

