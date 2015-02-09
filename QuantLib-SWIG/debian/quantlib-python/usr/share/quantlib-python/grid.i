
/*
 Copyright (C) 2004, 2005 StatPro Italia srl
 Copyright (C) 2005 Johan Witters

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

#ifndef quantlib_grid_i
#define quantlib_grid_i

%include common.i
%include types.i
%include stl.i

%{
using QuantLib::TimeGrid;
%}

class TimeGrid {
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    %rename(__len__)   size;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("length")  size;
    #elif defined(SWIGJAVA)
    %rename("getSize")   size;
    %rename("elementAt") ref;
    #endif
  public:
    // empty time-grid
    TimeGrid() {}
    // regularly spaced time-grid
    TimeGrid(Time end, Size steps);
    %extend {
        // time-grid with mandatory time points
        TimeGrid(const std::vector<Time>& times) {
            return new TimeGrid(times.begin(), times.end());
        }
        // time-grid with mandatory time points and steps
        TimeGrid(const std::vector<Time>& times, Size steps) {
            return new TimeGrid(times.begin(), times.end(), steps);
        }
    }
    Size size() const;
    %extend {
        #if defined(SWIGPYTHON) || defined(SWIGRUBY)
        Time __getitem__(Integer i) {
            Integer size_ = static_cast<Integer>(self->size());
            if (i>=0 && i<size_) {
                return (*self)[i];
            } else if (i<0 && -i<=size_) {
                return (*self)[size_+i];
            } else {
                throw std::out_of_range("time-grid index out of range");
            }
        }
        Time dt(Integer i) const {
            Integer size_ = static_cast<Integer>(self->size());
            if (i>=0 && i<size_) {
                return self->dt(i);
            } else if (i<0 && -i<=size_) {
                return self->dt(size_+i);
            } else {
                throw std::out_of_range("time-grid index out of range");
            }
        }
        #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE) || defined(SWIGJAVA)
        Time ref(Size i) {
            if (i<self->size())
                return (*self)[i];
            else
                throw std::out_of_range("time-grid index out of range");
        }
        Time dt(Size i) {
            if (i<self->size())
                return self->dt(i);
            else
                throw std::out_of_range("time-grid index out of range");
        }
        #endif
    }
};


#endif
