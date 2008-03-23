/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
Copyright (C) 2005, 2006, 2007 Eric Ehlers

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
	#include <oh/config.hpp>
#endif
#include <oh/logger.hpp>
#include <oh/ohdefines.hpp>
#include <oh/exception.hpp>
#include <log4cxx/helpers/exception.h>
/* Use BOOST_MSVC instead of _MSC_VER since some other vendors (Metrowerks,
for example) also #define _MSC_VER
*/
#ifdef BOOST_MSVC
#  define BOOST_LIB_DIAGNOSTIC
#  include <log4cxx/auto_link.hpp>
#  undef BOOST_LIB_DIAGNOSTIC
#endif
#include <ostream>
#include <boost/filesystem.hpp>
#include <log4cxx/helpers/transcoder.h>

#include <iostream>
using namespace log4cxx;


namespace ObjectHandler {

	Logger::Logger() {

		try {
			log4cxx::LoggerPtr _logger = log4cxx::Logger::getRootLogger();
			//_logger->setLevel(Level::OFF);
			_logger->setLevel(Level::getOff());
			//_layout = LayoutPtr(new SimpleLayout());
			   

		} catch (helpers::Exception &e) {
			//OH_FAIL("Logger::Logger: error initializing: " + e.getMessage());
			std::string str = "Logger::Logger: error initializing: ";
			str += e.what();
			OH_FAIL(str);
		}

	}

	log4cxx::LayoutPtr Logger::getLayout(){
		//static log4cxx::LayoutPtr _layout = NULL;
		//if(_layout == NULL){
		static log4cxx::LayoutPtr _layout = 0;
		if(_layout == 0){
			_layout = LayoutPtr(new SimpleLayout());
		}

		return _layout;
	}

	void Logger::setLogFile(
		const std::string &logFileName,
		const int &logLevel) {

			// Create a boost path object from the std::string.
			boost::filesystem::path path(logFileName);

			// If a parent directory has been specified then ensure it exists.
			if (path.has_branch_path()) {
				OH_REQUIRE(boost::filesystem::exists(path.branch_path()),
					"Invalid path : " << logFileName);
			}

			try {

				log4cxx::LoggerPtr _logger = log4cxx::Logger::getRootLogger();
				static log4cxx::AppenderPtr _fileAppender;

				_logger->removeAppender(_fileAppender);

				LogString fileName;
				log4cxx::helpers::Transcoder::decode(logFileName, fileName);

				_fileAppender = AppenderPtr(new FileAppender(getLayout(),  fileName));
				_logger->addAppender(_fileAppender);
				setLogLevel(logLevel);

			} catch (helpers::Exception &e) {
				//OH_FAIL("Logger::setLogFile: unable to set logfile: " + e.getMessage());
				std::string str = "Logger::setLogFile: unable to set logfile: ";
				str += e.what();
				OH_FAIL(str);
			}


	}

	void Logger::setConsole(
		const int &console,
		const int &logLevel) {
			try {
				log4cxx::LoggerPtr _logger = log4cxx::Logger::getRootLogger();
				static log4cxx::AppenderPtr _consoleAppender;
				_logger->removeAppender(_consoleAppender);
				if (console) {
					_consoleAppender = AppenderPtr(new ConsoleAppender(getLayout()));
					_logger->addAppender(_consoleAppender);
				}
				setLogLevel(logLevel);
			} catch (helpers::Exception &e) {
				//OH_FAIL("Logger::setLogFile: unable to set logfile: " + e.getMessage());
				std::string str = "Logger::setLogFile: unable to set logfile: ";
				str += e.what();
				OH_FAIL(str);
			}
	}

	void Logger::setLogLevel(const int &logLevel) {
		try {
			log4cxx::LoggerPtr _logger = log4cxx::Logger::getRootLogger();
			
			switch (logLevel) {
				case 0:
					//_logger->setLevel(Level::OFF);
					_logger->setLevel(Level::getOff());
					break;
				case 1:
					//_logger->setLevel(Level::FATAL);
					_logger->setLevel(Level::getFatal());
					break;
				case 2:
					//_logger->setLevel(Level::ERROR);
					_logger->setLevel(Level::getError());
					break;
				case 3:
					//_logger->setLevel(Level::WARN);
					_logger->setLevel(Level::getWarn());
					break;
				case 4:
					//_logger->setLevel(Level::INFO);
					_logger->setLevel(Level::getInfo());
					break;
				case 5:
					//_logger->setLevel(Level::DEBUG);
					_logger->setLevel(Level::getDebug());
					break;
				default:
					OH_FAIL("Logger::setLogLevel: invalid logLevel: " << logLevel);
			}
		} catch (helpers::Exception &e) {
			//OH_FAIL("Logger::Logger: error initializing: " + e.getMessage());
			std::string str = "Logger::Logger: error initializing: ";
			str += e.what();
			OH_FAIL(str);
		}
	}

	void Logger::logMessage(
		const std::string &message,
		const int &level) {
			// client applications call this function from within their
			// catch() clauses so this function must not throw.
			try {
				log4cxx::LoggerPtr _logger = log4cxx::Logger::getRootLogger();
				
				switch (level) {
				case 1:
					LOG4CXX_FATAL(_logger, message);
					break;
				case 2:
					LOG4CXX_ERROR(_logger, message);
					break;
				case 3:
					LOG4CXX_WARN(_logger, message);
					break;
				case 4:
					LOG4CXX_INFO(_logger, message);
					break;
				case 5:
					LOG4CXX_DEBUG(_logger, message);
					break;
				}
			} catch (...) {}
	}

}

