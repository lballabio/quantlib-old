
/*
 Copyright (C) 2005 Eric Ehlers

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

/* Sessions not supported for this platform.  This file implements
   a stub function which maintains the default behavior if 
   sessions have been enabled in QuantLib.  */

#include <ql/Patterns/singleton.hpp>

#ifdef QL_ENABLE_SESSIONS

QuantLib::Integer QuantLib::sessionId() {
    return 0;
}

#endif

