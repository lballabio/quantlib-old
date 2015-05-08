/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006, 2007, 2008 Eric Ehlers

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
    \brief Logger class wrapper for logging framework
*/

#ifndef oh_logger_hpp
#define oh_logger_hpp

#include <oh/singleton.hpp>

// Some older builds of log4cxx did not include file log4cxxdefines.h.
// If the #include below fails, it means either that you have the wrong
// version of log4cxx, or that your log4cxx installation (if any) cannot
// be found at all.
#include <log4cxx/log4cxxdefines.h>
#if LOG4CXX_VERSION != 0x001000f4
    #error using an incorrect version of log4cxx, please update.
#endif

#include <log4cxx/logger.h>
#include <log4cxx/simplelayout.h>
#include <log4cxx/fileappender.h>
#include <log4cxx/consoleappender.h>

namespace ObjectHandler {

    //! Wrapper for the logging framework.
    /*! Native exceptions generated within the logging framework
        are propagated as exceptions of ObjectHandler's Exception
        class (derived from std::exception).
    */
    class Logger : public Singleton<Logger> {
        friend class Singleton<Logger>;
    public:
        //! \name Logging Framework
        //@{
        //! Specify file to which messages should be logged.
        /*! Redirect logging to named file.  Logging to any
            previously specified logfile is discontinued.
            If the specified file doesn't already exist it is created.
            If the specified file does already exist, new messages are
            appended to it.  If the specified file cannot be opened, 
            an error is thrown.
            This function accepts an additional optional argument
            logLevel which is passed as an argument to setLogLevel 
            (see below). logLevel defaults to 4 (info).
        */
        void setFile(const std::string &logFileName,
                     const int &logLevel = 4);
        //! Direct logging to the console (stdout)
        /*! Logging to the console is disabled by default.
            Call this function with a parameter of 1 to enable
            logging to the console or 0 (the default) to disable it.
            This function accepts an additional optional argument
            logLevel which is passed as an argument to setLogLevel 
            (see below). logLevel defaults to 4 (info).
        */
        void setConsole(const int &console = 0,
                        const int &logLevel = 4);
        //! Specify threshold for logging messages.
        /*! Accepts one parameter logLevel with the following values:
            \li 0 - logging disabled
            \li 1 - fatal
            \li 2 - error
            \li 3 - warn
            \li 4 - info
            \li 5 - debug

            The logging level is cumulative e.g. a level of 3 means that
            messages of levels warn, error and fatal are logged.
            If the specified level is not in the range 0-5
            an error is thrown.
        */
        void setLevel(const int &logLevel);
        //! Log a message.
        /*! The optional level parameter specifies the logging level
            for the message, if omitted this defaults to 4 (info).
        */
        void writeMessage(const std::string &message,
                          const int &level = 4);
        //@}

        /** \name logFile and logLevel
        *  Miscellaneous utility functions.
        */
        //@{
        //!get log file
        const std::string file() const;
        //!get log level
        const int level() const;
        //@}


        virtual ~Logger(){}
    private:
        Logger();
        //log4cxx::LoggerPtr _logger;
        //log4cxx::LayoutPtr _layout;
        //log4cxx::AppenderPtr _fileAppender;
        //log4cxx::AppenderPtr _consoleAppender;
        log4cxx::LayoutPtr getLayout();

        //log4cxx::AppenderPtr fileAppender_;
        std::string filename_;
    };

}

#endif
