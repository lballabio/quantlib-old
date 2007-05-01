
/*
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers
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

#include <oh/ohdefines.hpp>
#include <oh/exception.hpp>
#include <boost/any.hpp>
#include <string>
#include <vector>

namespace ObjectHandler {

    //! Returns ObjectHandler version string.
    std::string version();

    /** \name Logging
     *  These functions wrap calls to the Logger class
        simplifying the syntax for the client application
        and alleviating the need for the client application
        to #include log4cxx headers.
    */
    //@{

    //! Start logging to file of given name.
    /*! Wraps function Logger::instance().setLogFile().
    */
    std::string setLogFile(
        const std::string &logFileName,
        const int &logLevel = 4);

    //! Write a message to the log file.
    /*! Wraps function Logger::instance().logMessage().
    */
    DLL_API void logMessage(
        const std::string &message,
        const int &level = 4);

    //! Set the logging threshold.
    /*! Wraps function Logger::instance().setLogLevel().

        It is seldom necessary to call this function as the default
        logging level is suitable for most purposes.
    */
    void setLogLevel(const int &logLevel);

    //! Fork log messages to stdout.
    /*! Wraps function Logger::instance().setConsole().

        This function is used in command-line environments and causes
        messages to appear both on the terminal and in the log file.
    */
    void setConsole(const int &console = 0,
                    const int &logLevel = 4);

    //! Write Object with given ID to log file.
    /*! Writes a warning message to the log file
        if no Object is found with the given ID.
    */
    void logObject(const std::string &objectID);

    //! Write all Objects to the log file.
    /*! Takes no action if the ObjectHandler
        repository is empty.
    */
    void logAllObjects();
    //@}

    /** \name Utilities
     *  Miscellaneous utility functions.
    */
    //@{
    //! Split a string into substrings using the given delimiter.
    std::vector<std::string> split(const std::string &line,
                                   const std::string &delim,
                                   bool token_compress);

    //! Filter a list of data given a list of flags.
    /*! Accept a list of values and a list of boolean flags.  Return
        the list of values for which the corresponding flag is true.
    */
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

