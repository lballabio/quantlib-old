/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004, 2005, 2006 StatPro Italia srl
 Copyright (C) 2015 Peter Caspers

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

/*! \file indexmanager.hpp
    \brief global repository for past index fixings
*/

#ifndef quantlib_index_manager_hpp
#define quantlib_index_manager_hpp

#include <ql/timeseries.hpp>
#include <ql/patterns/singleton.hpp>
#include <ql/utilities/observablevalue.hpp>
#include <boost/algorithm/string/case_conv.hpp>

using boost::algorithm::to_upper_copy;
using std::string;

namespace QuantLib {

//! global repository for past index fixings
/*! \note index names are case insensitive */
template <class T = Real>
class IndexManager_t : public Singleton<IndexManager_t<T> > {
    friend class Singleton<IndexManager_t<T> >;

  private:
    IndexManager_t() {}

  public:
    //! returns whether historical fixings were stored for the index
    bool hasHistory(const std::string &name) const;
    //! returns the (possibly empty) history of the index fixings
    const TimeSeries<T> &getHistory(const std::string &name) const;
    //! stores the historical fixings of the index
    void setHistory(const std::string &name, const TimeSeries<T> &);
    //! observer notifying of changes in the index fixings
    boost::shared_ptr<Observable> notifier(const std::string &name) const;
    //! returns all names of the indexes for which fixings were stored
    std::vector<std::string> histories() const;
    //! clears the historical fixings of the index
    void clearHistory(const std::string &name);
    //! clears all stored fixings
    void clearHistories();

  private:
    typedef std::map<std::string, ObservableValue<TimeSeries<T> > > history_map;
    mutable history_map data_;
};

typedef IndexManager_t<Real> IndexManager;

// implementation

template <class T>
bool IndexManager_t<T>::hasHistory(const string &name) const {
    return data_.find(to_upper_copy(name)) != data_.end();
}

template <class T>
const TimeSeries<T> &IndexManager_t<T>::getHistory(const string &name) const {
    return data_[to_upper_copy(name)].value();
}

template <class T>
void IndexManager_t<T>::setHistory(const string &name,
                                   const TimeSeries<T> &history) {
    data_[to_upper_copy(name)] = history;
}

template <class T>
boost::shared_ptr<Observable>
IndexManager_t<T>::notifier(const string &name) const {
    return data_[to_upper_copy(name)];
}

template <class T> std::vector<string> IndexManager_t<T>::histories() const {
    std::vector<string> temp;
    temp.reserve(data_.size());
    for (typename history_map::const_iterator i = data_.begin(); i != data_.end(); ++i)
        temp.push_back(i->first);
    return temp;
}

template <class T> void IndexManager_t<T>::clearHistory(const string &name) {
    data_.erase(to_upper_copy(name));
}

template <class T> void IndexManager_t<T>::clearHistories() { data_.clear(); }
}

#endif
