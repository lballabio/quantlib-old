
/*
 Copyright (C) 2007 Eric Ehlers

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

#ifndef ohxl_configuration_hpp
#define ohxl_configuration_hpp

#include <oh/singleton.hpp>

namespace ObjectHandler {

    class Configuration : public Singleton<Configuration> {
        friend class Singleton<Configuration>;
    public:
        Configuration() : initialized_(false) {}
        void init();
        const char &rowCharacter();
        const char &colCharacter();
    private:
        bool initialized_;
        char rowCharacter_;
        char colCharacter_;
    };

}

#endif

