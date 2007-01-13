
/*
 Copyright (C) 2004, 2005, 2006 Eric Ehlers
 Copyright (C) 2006 Plamen Neykov

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
    \brief ObjectHandler utilities
*/

#ifndef oh_utilities_hpp
#define oh_utilities_hpp

#include <oh/objhandlerdefines.hpp>
#include <oh/exception.hpp>
#include <boost/any.hpp>
#include <string>
#include <vector>

namespace ObjHandler {

    //! Returns ObjectHandler version string
    std::string version();

    /** \name Logging framework
     *  These functions wrap calls to the Logger class
        simplifying the syntax for the client application
        and alleviating the need for the client application
        to #include log4cxx headers.
    */
    //@{

    //! Wrapper for function Logger::instance().setLogFile().
    /*! Specify name of log file.
    */
    std::string setLogFile(
        const std::string &logFileName,
        const int &logLevel = 4);

    //! Wrapper for function Logger::instance().logMessage()
    /*! Write a message to the log file.
    */
    DLL_API void logMessage(
        const std::string &message,
        const int &level = 4);

    //! Wrapper for function Logger::instance().setLogLevel().
    /*! Set logging threshold.
    */
    void setLogLevel(const int &logLevel);

    //! Wrapper for function Logger::instance().setConsole().
    /*! Fork log messages to stdout.
    */
    void setConsole(const int &console = 0,
                    const int &logLevel = 4);

    //! Write Object with given ID to log file.
    /*! Writes a warning message to log file
        if no object is found with given ID.
    */
    void logObject(const std::string &objectID);

    //! Write all Objects to log file.
    /*! Takes no action if ObjectHandler
        repository is empty.
    */
    void logAllObjects();

    // some basic utilities

    std::vector<std::string> split(const std::string &line,
                                   const std::string &delim,
                                   bool token_compress);

    template< typename T>
    std::vector<T> filter(const std::vector<T> &in,
                          const std::vector<bool> &flags) {
        std::size_t size = in.size();
        OH_REQUIRE(size==flags.size(),
                   "size mismatch between value vector (" << size << 
                   ") and flag vector (" << flags.size() << ")");

        std::vector<T> out;
        out.reserve(size); //excess capacity

        for (std::size_t i=0; i<size; ++i)
            if (flags[i]) out.push_back(in[i]);

        return out;
    }

    //@}
}

#endif
