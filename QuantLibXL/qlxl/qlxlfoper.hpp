/*
 Copyright (C) 2002 Ferdinando Ametrano

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/
/*! \file qlxlfoper.hpp
    \brief XlfOper specialization for QuantLib classes

    \fullpath
    qlxl/%qlxlfoper.hpp
*/

// $Id$

#ifndef qlxl_qlxlfoper_h
#define qlxl_qlxlfoper_h

class QlXlfOper : public XlfOper {
public:
    QlXlfOper(XlfOper xlfOper);
private:
    XlfOper xlfOper_;
};

QlXlfOper::QlXlfOper(XlfOper xlfOper)
: xlfOper_(xlfOper) {}

#endif
