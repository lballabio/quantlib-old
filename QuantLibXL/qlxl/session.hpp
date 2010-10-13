
/*
 Copyright (C) 2005 Eric Ehlers

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

#ifndef qla_xl_session_hpp
#define qla_xl_session_hpp

#include <ql/userconfig.hpp>
#include <ql/types.hpp>
#include <string>
#include <map>

#ifdef QL_ENABLE_SESSIONS

    #include <oh/singleton.hpp>

    namespace QuantLibAddin {

        class Session : public ObjectHandler::Singleton<Session> {
            friend class ObjectHandler::Singleton<Session>;
        public:
            Session() : sessionIdFountain_(0) {}
            void setSessionId();
            const QuantLib::Integer &getSessionId();
        private:
            QuantLib::Integer sessionId_;
            QuantLib::Integer sessionIdFountain_;
            std::map < std::string, QuantLib::Integer > sessionMap_;
            std::string bookFromAddress(const std::string &address);
        };

    }

    #define SET_SESSION_ID QuantLibAddin::Session::instance().setSessionId();

#else

    #define SET_SESSION_ID

#endif

#endif
