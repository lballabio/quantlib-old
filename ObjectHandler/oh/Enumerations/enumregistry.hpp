
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
    \brief EnumRegistry Classes - Concrete implementations of class Registry.
*/

#ifndef oh_enumregistry_hpp
#define oh_enumregistry_hpp

#include <oh/ohdefines.hpp>
#include <oh/singleton.hpp>
#include <oh/Enumerations/registry.hpp>

namespace ObjectHandler {

    //! A concrete instantiation of the Registry class, for enumerated types.
    class DLL_API EnumTypeRegistry : public Registry<std::string> {
    public:
        //! \name Structors and static members
        //@{
        //! Constructor - initialize the singleton.
        EnumTypeRegistry();
        //! Destructor - de-initialize the singleton.
        virtual ~EnumTypeRegistry();
        //! Client applications access the global object via a call to Repository::instance().
        static EnumTypeRegistry &instance();
        //@}

        virtual void registerType(const std::string &mapID, const std::string &typeID, void *type) const;
        virtual const AllTypeMap& getAllTypesMap() const;
        virtual std::vector<std::string> getAllRegisteredTypes() const;
        virtual std::vector<std::string> getTypeElements(const std::string &id) const;
        void deleteTypeMap(const std::string &mapID);

    private:
        //! A pointer to the Registry instance, used to support the Singleton pattern.
        static EnumTypeRegistry *instance_;
    };

    //! A concrete instantiation of the Registry class, for enumerated classes.
    class DLL_API EnumClassRegistry : public Registry<std::string> {
    public:
        //! \name Structors and static members
        //@{
        //! Constructor - initialize the singleton.
        EnumClassRegistry();
        //! Destructor - de-initialize the singleton.
        virtual ~EnumClassRegistry();
        //! Client applications access the global object via a call to Repository::instance().
        static EnumClassRegistry &instance();
        //@}

        virtual void registerType(const std::string &mapID, const std::string &typeID, void *type) const;
        virtual const AllTypeMap& getAllTypesMap() const;
        virtual std::vector<std::string> getAllRegisteredTypes() const;
        virtual std::vector<std::string> getTypeElements(const std::string &id) const;

    private:
        //! A pointer to the Registry instance, used to support the Singleton pattern.
        static EnumClassRegistry *instance_;
    };

    //! A concrete instantiation of the Registry class, for "pairs".
    /*! "Pairs" are enumerated classes which are keyed not by strings but by
            std::pair<std::string, std::string>
        This additional flexibility is required for some application domains.
    */
    class DLL_API EnumPairRegistry : public Registry<KeyPair> {
    public:
        //! \name Structors and static members
        //@{
        //! Constructor - initialize the singleton.
        EnumPairRegistry();
        //! Destructor - de-initialize the singleton.
        virtual ~EnumPairRegistry();
        //! Client applications access the global object via a call to Repository::instance().
        static EnumPairRegistry &instance();
        //@}

        virtual void registerType(const std::string &mapID, const KeyPair &typeID, void *type) const;
        virtual const AllTypeMap& getAllTypesMap() const;
        virtual std::vector<std::string> getAllRegisteredTypes() const;
        virtual std::vector<std::string> getTypeElements(const std::string &id) const;

    private:
        //! A pointer to the Registry instance, used to support the Singleton pattern.
        static EnumPairRegistry *instance_;
    };

}

#endif

