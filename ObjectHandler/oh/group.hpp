/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers
 Copyright (C) 2011 Ferdinando Ametrano

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
    \brief class Group - A collection of Objects
*/

#ifndef oh_group_hpp
#define oh_group_hpp

#include <oh/object.hpp>
#include <vector>

namespace ObjectHandler {

    //! A collection of Objects.
    /*! This class was implemented to facilitate serialization of a collection
        of Objects.  In practice this functionality has not proved necessary so
        this class is not fully implemented.
    */
    class Group : public Object {
    public:
        Group(const boost::shared_ptr<ValueObject>& properties,
              const std::vector<std::string> &list,
              bool permanent)
        : Object(properties, permanent), list_(list)
        {
            OH_REQUIRE(!list.empty(), "Input list is empty");
        }
        Group(const boost::shared_ptr<ValueObject>& properties,
              const std::vector<boost::shared_ptr<ObjectHandler::Group> > &groups,
              bool permanent)
        : Object(properties, permanent)
        {
            OH_REQUIRE(!groups.empty(), "Group list is empty");
            list_ = groups[0]->list();
            for (size_t i=1; i<groups.size(); ++i) {
                const std::vector<std::string> & newList = groups[i]->list();
                list_.insert(list_.end(), newList.begin(), newList.end());
            }
        }
        const std::vector<std::string> &list() { return list_; }
        size_t size() { return list_.size(); }
    private:
        std::vector<std::string> list_;
    };

}

#endif
