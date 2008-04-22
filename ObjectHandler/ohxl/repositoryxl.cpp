/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006, 2007, 2008 Eric Ehlers
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2008 Nazcatech sprl Belgium

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

#include <oh/exception.hpp>
#include <oh/utilities.hpp>
#include <ohxl/repositoryxl.hpp>
#include <ohxl/functioncall.hpp>
#include <ohxl/callingrange.hpp>
#include <ohxl/rangereference.hpp>
#include <ohxl/convert_oper.hpp>
#include <boost/algorithm/string.hpp>
/* Use BOOST_MSVC instead of _MSC_VER since some other vendors (Metrowerks,
for example) also #define _MSC_VER
*/
#ifdef BOOST_MSVC
#  define BOOST_LIB_DIAGNOSTIC
#  include <xlsdk/auto_link.hpp>
#  undef BOOST_LIB_DIAGNOSTIC
#endif
#include <iomanip>
#include <sstream>
#include <string>

namespace ObjectHandler {

	// Below are three structures which must be declared as static variables rather than
	// class members because std::map cannot be exported across DLL boundaries.

	// The object map declared in the cpp file for the base Repository class.
	extern Repository::ObjectMap objectMap_;

	// A map to associate error messages with Excel range addresses.
	typedef std::map<std::string, boost::shared_ptr<RangeReference> > ErrorMessageMap;
	ErrorMessageMap errorMessageMap_;

	// Excel cell ranges in which objects have been constructed,
	// keyed by a unique ID which is assigned to each range.
	typedef std::map<std::string, boost::shared_ptr<CallingRange> > RangeMap;
	RangeMap callingRanges_;

	RepositoryXL &RepositoryXL::instance() {
		if (instance_) {
			RepositoryXL *ret = dynamic_cast<RepositoryXL*>(instance_);
			if (ret) return *ret;
		}
		OH_FAIL("Attempt to reference uninitialized RepositoryXL object");
	}

	std::string RepositoryXL::storeObject(
		const std::string &objectIDRaw,
		const boost::shared_ptr<Object> &object,
		bool overwrite) {

		boost::shared_ptr<CallingRange> callingRange = getCallingRange();
        std::string objectID = callingRange->initializeID(objectIDRaw);

		boost::shared_ptr<ObjectWrapperXL> objectWrapperXL;
		ObjectMap::const_iterator result = objectMap_.find(objectID);
		if (result == objectMap_.end()) {
		    objectWrapperXL = boost::shared_ptr<ObjectWrapperXL> (
    			new ObjectWrapperXL(objectID, object, callingRange));
			objectMap_[objectID] = objectWrapperXL;
            callingRange->registerObject(objectID, objectWrapperXL);
        } else {
		    objectWrapperXL = boost::static_pointer_cast<ObjectWrapperXL>(result->second);
            if (objectWrapperXL->callerKey() != callingRange->key()) {
		        OH_REQUIRE(overwrite, "Cannot create object with ID '" << objectID <<
			        "' in cell " << callingRange->addressString() <<
			        " because an object with that ID already resides in cell " <<
			        objectWrapperXL->callerAddress());
			    objectWrapperXL->resetCaller(callingRange);
                callingRange->registerObject(objectID, objectWrapperXL);
            }
			objectWrapperXL->reset(object);
        }

		registerObserver(objectWrapperXL);
		return objectWrapperXL->idFull();
	}

	void RepositoryXL::registerObserver( 
		boost::shared_ptr<ObjectWrapper> objectWrapper){

			const std::set<std::string>& relationObs = objectWrapper->object()->properties()->getRelationObs();
			std::set<std::string>::const_iterator iter = relationObs.begin();
			for(; iter != relationObs.end();  iter++){
				boost::shared_ptr<ObjectWrapper> objServable = getObjectWrapper(CallingRange::getStub(*iter));
				objectWrapper->registerWith(objServable);
			}
	}

	boost::shared_ptr<Object> RepositoryXL::retrieveObjectImpl(
		const std::string &objectID)  {
        return Repository::retrieveObjectImpl(CallingRange::getStub(objectID));
	}

	void RepositoryXL::deleteObject(const std::string &objectID) {
		Repository::deleteObject(CallingRange::getStub(objectID));
	}

	void RepositoryXL::deleteObject(const std::vector<std::string> &objectIDs) {
		OH_REQUIRE(!objectIDs.empty(), "List of Object IDs for deletion is empty");
		for (std::vector<std::string>::const_iterator i = objectIDs.begin();
			i != objectIDs.end(); ++i)
			Repository::deleteObject(CallingRange::getStub(*i));
	}

	bool RepositoryXL::objectExists(const std::string &objectID) const {
		return Repository::objectExists(CallingRange::getStub(objectID));
	}

	void RepositoryXL::setError(
		const std::string &message,
		const boost::shared_ptr<FunctionCall> &functionCall) {

			std::ostringstream logMessage, cellMessage;
			cellMessage << functionCall->functionName() << " - " << message;

			std::string refStr = functionCall->refStr();
			std::string refStrUpper = boost::algorithm::to_upper_copy(refStr);
			ErrorMessageMap::const_iterator i = errorMessageMap_.find(refStrUpper);
			if (i == errorMessageMap_.end()) {
				boost::shared_ptr<RangeReference> rangeReference(new RangeReference(refStrUpper));
				rangeReference->setErrorMessage(cellMessage.str());
				errorMessageMap_[refStrUpper] = rangeReference;
			} else {
				i->second->setErrorMessage(cellMessage.str());
			}
	}

	void RepositoryXL::logError(
		const std::string &message,
		const boost::shared_ptr<FunctionCall> &functionCall) {

			// This function is called during error handling and must not throw.

			try {

				if (functionCall) {

					functionCall->setError();
					std::ostringstream fullMessage;
					if (functionCall->callerType() == CallerType::Cell) {
						setError(message, functionCall);
						fullMessage << functionCall->addressString() << " - ";
					} else if (functionCall->callerType() == CallerType::VBA || functionCall->callerType() == CallerType::Menu) {
						vbaError_ = message;
						fullMessage << "VBA - ";
					}

					fullMessage << functionCall->functionName() << " - " << message;
					logMessage(fullMessage.str(), 2);

				} else {
					logMessage(message, 2);
				}

			} catch(...) {}
	}

	std::string RepositoryXL::retrieveError(const XLOPER *xRangeRef) {

		OH_REQUIRE(xRangeRef->xltype == xltypeRef || xRangeRef->xltype == xltypeSRef,
			"Input parameter is not a range reference.");
		Xloper xRangeText;
		Excel(xlfReftext, &xRangeText, 1, xRangeRef);
		std::string refStr = ConvertOper(xRangeText());
		std::string refStrUpper = boost::algorithm::to_upper_copy(refStr);

		ErrorMessageMap::const_iterator i = errorMessageMap_.find(refStrUpper);
		if (i != errorMessageMap_.end())
			return i->second->errorMessage();

		RangeReference selectionReference(refStrUpper);
		for (i = errorMessageMap_.begin(); i != errorMessageMap_.end(); ++i) {
			if (i->second->contains(selectionReference))
				return i->second->errorMessage();
		}

		return "";
	}

	void RepositoryXL::clearError() {
		std::string refStr = FunctionCall::instance().refStr();
		errorMessageMap_.erase(boost::algorithm::to_upper_copy(refStr));
	}

	void RepositoryXL::dumpObject(const std::string &objectID, std::ostream &out) {
		ObjectMap::const_iterator result = objectMap_.find(CallingRange::getStub(objectID));
		if (result == objectMap_.end()) {
			out << "no object in repository with ID = " << objectID << std::endl;
		} else {
			out << "log dump of object with ID = " << objectID << std::endl << result->second;
		}
	}

	void RepositoryXL::collectGarbage(const bool &deletePermanent) {

		RangeMap::iterator i = callingRanges_.begin();
		while (i != callingRanges_.end()) {
			boost::shared_ptr<CallingRange> callingRange = i->second;
			if (callingRange->valid()) {
				++i;
			} else {
				callingRange->clearResidentObjects(deletePermanent);
				if (callingRange->empty())
					callingRanges_.erase(i++);
				else
					++i;
			}
		}
	}

	boost::shared_ptr<CallingRange> RepositoryXL::getCallingRange() {
        std::string callerName = FunctionCall::instance().callerName();
        if (callerName == "VBA") {
            // Called from VBA - check whether the corresponding calling range
            // object exists and create it if not.
			RangeMap::const_iterator i = callingRanges_.find(callerName);
            if (i == callingRanges_.end()) {
			    boost::shared_ptr<CallingRange> callingRange(new CallingRange);
			    callingRanges_[callingRange->key()] = callingRange;
			    return callingRange;
            } else {
			    return i->second;
            }
        // Called from a worksheet formula
        } else if (callerName.empty()) {
			// Calling range not yet named - create a new CallingRange object
			boost::shared_ptr<CallingRange> callingRange(new CallingRange);
			callingRanges_[callingRange->key()] = callingRange;
			return callingRange;
        } else {
			// Calling range already named - return associated CallingRange object
			RangeMap::const_iterator i = callingRanges_.find(callerName);
			OH_REQUIRE(i != callingRanges_.end(), "No calling range named " << callerName);
			return i->second;
        }
	}

	void RepositoryXL::dump(std::ostream& out) {
		Repository::dump(out);

		out << std::endl << "calling ranges:";
		if (callingRanges_.empty()) {
			out << " none." << std::endl;
		} else {
			out << std::endl << std::endl;
			for (RangeMap::const_iterator i = callingRanges_.begin();
				i != callingRanges_.end(); ++i) {
					out << i->second;
			}
		}

		out << std::endl << "Error messages:";
		if (errorMessageMap_.empty()) {
			out << " none." << std::endl;
		} else {
			out << std::endl << std::endl;
			for (ErrorMessageMap::const_iterator i = errorMessageMap_.begin();
				i != errorMessageMap_.end(); ++i) {
					out << std::left << std::setw(50) << i->first << i->second->errorMessage() << std::endl;
			}
		}

		out << std::endl << std::endl << "VBA error message: " << vbaError_ << std::endl;
	}

	std::vector<std::string> RepositoryXL::callerAddress(const std::vector<std::string> &objectList) {

		std::vector<std::string> ret;

		for (std::vector<std::string>::const_iterator i = objectList.begin();
			i != objectList.end(); ++i) {
				std::string idStrip = CallingRange::getStub(*i);
				const boost::shared_ptr<ObjectWrapperXL>& objectWrapperXL = boost::static_pointer_cast<ObjectWrapperXL>(
					getObjectWrapper(idStrip));
				ret.push_back(objectWrapperXL->callerAddress());
		}

		return ret;
	}

	std::vector<std::string> RepositoryXL::callerKey(const std::vector<std::string> &objectList) {

		std::vector<std::string> ret;

		for (std::vector<std::string>::const_iterator i = objectList.begin();
			i != objectList.end(); ++i) {
				std::string idStrip = CallingRange::getStub(*i);
				const boost::shared_ptr<ObjectWrapperXL>& objectWrapperXL = boost::static_pointer_cast<ObjectWrapperXL>(
					getObjectWrapper(idStrip));
				ret.push_back(objectWrapperXL->callerKey());
		}

		return ret;
	}

}
