/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006, 2007 Eric Ehlers

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
    \brief EnumRegistry Classes - Concrete implementations of class Registry
*/

#ifndef oh_enumregistry_hpp
#define oh_enumregistry_hpp

#include <oh/ohdefines.hpp>
#include <oh/singleton.hpp>
#include <oh/enumerations/registry.hpp>

namespace ObjectHandler {

    //! A concrete instantiation of the Registry class, for enumerated types.
    class DLL_API EnumTypeRegistry : public Registry<std::string> {
    public:
        //! \name Structors and static members
        //@{
        //! Constructor - initialize the singleton.
        EnumTypeRegistry() { instance_ = this; }
        //! Destructor - de-initialize the singleton.
        virtual ~EnumTypeRegistry() { instance_ = 0; }
        //! Client applications access the global object via a call to EnumTypeRegistry::instance().
        static EnumTypeRegistry &instance();
        //@}

        //! \name Management of Enumerations
        //@{
        //! Return a non-const reference to the type map for use by the base class.
        virtual EnumTypeRegistry::AllTypeMap& getAllTypesMap() const;
        //@}
    private:
        //! A pointer to the EnumTypeRegistry instance, used to support the Singleton pattern.
        static EnumTypeRegistry *instance_;
    };

    //! A concrete instantiation of the Registry class, for enumerated classes.
    class DLL_API EnumClassRegistry : public Registry<std::string> {
    public:
        //! \name Structors and static members
        //@{
        //! Constructor - initialize the singleton.
        EnumClassRegistry() { instance_ = this; }
        //! Destructor - de-initialize the singleton.
        virtual ~EnumClassRegistry() { instance_ = 0; }
        //! Client applications access the global object via a call to EnumClassRegistry::instance().
        static EnumClassRegistry &instance();
        //@}

        //! \name Management of Enumerations
        //@{
        //! Return a non-const reference to the type map for use by the base class.
        virtual EnumClassRegistry::AllTypeMap& getAllTypesMap() const;
        //@}
    private:
        //! A pointer to the EnumClassRegistry instance, used to support the Singleton pattern.
        static EnumClassRegistry *instance_;
    };

    //! A concrete instantiation of the Registry class, for "pairs".
    /*! "Pairs" are enumerated classes which are keyed not by a simple std::string but by
        \code
            std::pair<std::string, std::string>
        \endcode
        This additional flexibility is required for some application domains.
    */
    class DLL_API EnumPairRegistry : public Registry<KeyPair> {
    public:
        //! \name Structors and static members
        //@{
        //! Constructor - initialize the singleton.
        EnumPairRegistry() { instance_ = this; }
        //! Destructor - de-initialize the singleton.
        virtual ~EnumPairRegistry() { instance_ = 0; }
        //! Client applications access the global object via a call to EnumPairRegistry::instance().
        static EnumPairRegistry &instance();
        //@}

        //! \name Management of Enumerations
        //@{
        //! Return a non-const reference to the type map for use by the base class.
        virtual EnumPairRegistry::AllTypeMap& getAllTypesMap() const;
        //@}
    private:
        //! A pointer to the EnumPairRegistry instance, used to support the Singleton pattern.
        static EnumPairRegistry *instance_;
    };

}

#endif

