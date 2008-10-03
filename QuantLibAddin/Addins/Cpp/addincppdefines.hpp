
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

#ifndef addincpp_defines_hpp
#define addincpp_defines_hpp

/*! \def LOG_MESSAGE(message)
    Log the given message.
*/

#define LOG_MESSAGE(message) \
do { \
    std::ostringstream _oh_msg_stream; \
    _oh_msg_stream << message; \
    ohLogMessage(_oh_msg_stream.str(), 4L, ObjectHandler::property_t()); \
} while (false)

/*! \def LOG_ERROR(message)
    Log the given error message.
*/

#define LOG_ERROR(message) \
do { \
    std::ostringstream _oh_msg_stream; \
    _oh_msg_stream << message; \
    ohLogMessage(_oh_msg_stream.str(), 1L, ObjectHandler::property_t()); \
} while (false)

#endif

