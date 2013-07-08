/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*!
 Copyright (C) 2005, 2006, 2007, 2009 StatPro Italia srl

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

#include "customutilities.hpp"
#include <ql/quantlib.hpp>
#include <boost/timer.hpp>
#include <boost/shared_ptr.hpp>
#include <iostream>
#include <iomanip>

//using namespace QuantLib;
using namespace std;
using boost::shared_ptr;

//****************************************************************************

namespace FiniteDifferences {

// spatial grid class
class Grid: public Array {
public:
	Grid(double center, double dx, int steps);
};
// time grid class
class TimeGrid: public std::vector<double> {
public:
	TimeGrid() {
	}
// Regularly spaced time-grid
	TimeGrid(double end, int steps);
// double grid with mandatory time-points (regularly spaced between them)
	TimeGrid(const std::list<double>& times, int steps);
	int findIndex(double t) const;
	double dt(int i) const;
};
// inline definitions
inline Grid::Grid(double center, double dx, int steps) :
		Array(steps) {
	for (int i = 0; i < steps; i++)
		(*this)[i] = center + (i - steps / 2.0) * dx;
}
inline TimeGrid::TimeGrid(double end, int steps) {
	double dt = end / steps;
	for (int i = 0; i <= steps; i++)
		push_back(dt * i);
}
inline TimeGrid::TimeGrid(const std::list<double>& times, int steps) :
		std::vector<double>(0) {
	double last = times.back();
	double dtMax;
// The resulting timegrid have points at times listed in the input
// list. Between these points, there are inner-points which are
// regularly spaced.
	if (steps == 0) {
		std::vector<double> diff;
		std::back_insert_iterator < std::vector<double> > ii(diff);
		std::adjacent_difference(times.begin(), times.end(), ii);
		if (diff.front() == 0.0)
			diff.erase(diff.begin());
		dtMax = *(std::min_element(diff.begin(), diff.end()));
	} else {
		dtMax = last / steps;
	}
	double periodBegin = 0.0;
	std::list<double>::const_iterator t;
	for (t = times.begin(); t != times.end(); t++) {
		double periodEnd = *t;
		if (periodBegin >= periodEnd)
			continue;
		int nSteps = (int) ((periodEnd - periodBegin) / dtMax + 1.0);
		double dt = (periodEnd - periodBegin) / nSteps;
		for (int n = 0; n < nSteps; n++)
			push_back(periodBegin + n * dt);
		periodBegin = periodEnd;
	}
	push_back(periodBegin); // Note periodBegin = periodEnd
}
inline int TimeGrid::findIndex(double t) const {
	const_iterator result = std::find(begin(), end(), t);
    //QL_REQUIRE(result!=end(), “Using inadequate tree”);
	return result - begin();
}
inline double TimeGrid::dt(int i) const {
	return (*this)[i + 1] - (*this)[i];
}

class TridiagonalOperator {
// unary operators
	friend TridiagonalOperator operator+(const TridiagonalOperator&);
	friend TridiagonalOperator operator-(const TridiagonalOperator&);
// binary operators
	friend TridiagonalOperator operator+(const TridiagonalOperator&,
			const TridiagonalOperator&);
	friend TridiagonalOperator operator-(const TridiagonalOperator&,
			const TridiagonalOperator&);
	friend TridiagonalOperator operator*(double, const TridiagonalOperator&);
	friend TridiagonalOperator operator*(const TridiagonalOperator&, double);
	friend TridiagonalOperator operator/(const TridiagonalOperator&, double);
public:
	typedef Array arrayType;
// constructors
	TridiagonalOperator(Size size = 0);
	TridiagonalOperator(const Array& low, const Array& mid, const Array& high);
	TridiagonalOperator(const TridiagonalOperator& L);
	TridiagonalOperator& operator=(const TridiagonalOperator& L);
// apply operator to a given array
	Array applyTo(const Array& v) const;
// solve linear system for a given right-hand side
	Array solveFor(const Array& rhs) const;
// solve linear system with SOR approach.,m
	Array SOR(const Array& rhs, double tol) const;
// identity instance
	static TridiagonalOperator identity(Size size);
	Size size() const;
	bool isdoubleDependent();
	void setFirstRow(double, double);
	void setMidRow(Size, double, double, double);
	void setMidRows(double, double, double);
	void setLastRow(double, double);
	void setdouble(double t);
	void setTime(double t) {
		time_ = t;
	}
	bool isTimeDependent() {
		return isTimeDependent_;
	}
// encapsulation of double-setting logic
	class doubleSetter {
	public:
        //virtual ∼ doubleSetter() {}
		virtual void setdouble(double t, TridiagonalOperator& L) const = 0;
	};
protected:
	Array diagonal_, belowDiagonal_, aboveDiagonal_;
	Handle<doubleSetter> doubleSetter_;
	double time_;
	bool isTimeDependent_;
};
inline TridiagonalOperator::TridiagonalOperator(const TridiagonalOperator& L) {
	belowDiagonal_ = L.belowDiagonal_;
	diagonal_ = L.diagonal_;
	aboveDiagonal_ = L.aboveDiagonal_;
	doubleSetter_ = L.doubleSetter_;
}

/**********************************************************************************
 operator= : overloaded assignment operator
 Copies the elements of one Tridiagonal operator to another
 [in] Array from : array to copy from
 [out] TridiagonalOperator&: copy of tridiagonal operator
 **********************************************************************************/
inline TridiagonalOperator& TridiagonalOperator::operator=(
		const TridiagonalOperator& L) {
	belowDiagonal_ = L.belowDiagonal_;
	diagonal_ = L.diagonal_;
	aboveDiagonal_ = L.aboveDiagonal_;
	doubleSetter_ = L.doubleSetter_;
	return *this;
}
inline Size TridiagonalOperator::size() const {
	return diagonal_.size();
}
inline bool TridiagonalOperator::isdoubleDependent() {
	return !doubleSetter_.isNull();
}
// set values of first row of matrix
inline void TridiagonalOperator::setFirstRow(double valB, double valC) {
	diagonal_[0] = valB;
	aboveDiagonal_[0] = valC;
}
// set values of middle row of matrix
inline void TridiagonalOperator::setMidRow(Size i, double valA, double valB,
		double valC) {
    //QL_REQUIRE(i>=1 && i<=size()-2, “out of range in TridiagonalSystem::setMidRow”);
	belowDiagonal_[i - 1] = valA;
	diagonal_[i] = valB;
	aboveDiagonal_[i] = valC;
}
// set values of middle rows of matrix
inline void TridiagonalOperator::setMidRows(double valA, double valB,
		double valC) {
	for (int i = 1; i <= size() - 2; i++) {
		belowDiagonal_[i - 1] = valA;
		diagonal_[i] = valB;
		aboveDiagonal_[i] = valC;
	}
}
inline void TridiagonalOperator::setLastRow(double valA, double valB) {
	belowDiagonal_[size() - 2] = valA;
	diagonal_[size() - 1] = valB;
}
inline void TridiagonalOperator::setdouble(double t) {
	if (!doubleSetter_.isNull())
		doubleSetter_->setdouble(t, *this);
}
inline TridiagonalOperator operator+(const TridiagonalOperator& D) {
	return D;
}
inline TridiagonalOperator operator-(const TridiagonalOperator& D) {
	Array low = -D.belowDiagonal_, mid = -D.diagonal_, high = -D.aboveDiagonal_;
	TridiagonalOperator result(low, mid, high);
	return result;
}
inline TridiagonalOperator operator+(const TridiagonalOperator& D1,
		const TridiagonalOperator& D2) {
	Array low = D1.belowDiagonal_ + D2.belowDiagonal_, mid = D1.diagonal_
			+ D2.diagonal_, high = D1.aboveDiagonal_ + D2.aboveDiagonal_;
	TridiagonalOperator result(low, mid, high);
	return result;
}
inline TridiagonalOperator operator-(const TridiagonalOperator& D1,
		const TridiagonalOperator& D2) {
	Array low = D1.belowDiagonal_ - D2.belowDiagonal_, mid = D1.diagonal_
			- D2.diagonal_, high = D1.aboveDiagonal_ - D2.aboveDiagonal_;
	TridiagonalOperator result(low, mid, high);
	return result;
}
inline TridiagonalOperator operator*(double a, const TridiagonalOperator& D) {
	Array low = D.belowDiagonal_ * a, mid = D.diagonal_ * a, high =
			D.aboveDiagonal_ * a;
	TridiagonalOperator result(low, mid, high);
	return result;
}
inline TridiagonalOperator operator*(const TridiagonalOperator& D, double a) {
	Array low = D.belowDiagonal_ * a, mid = D.diagonal_ * a, high =
			D.aboveDiagonal_ * a;
	TridiagonalOperator result(low, mid, high);
	return result;
}
inline TridiagonalOperator operator/(const TridiagonalOperator& D, double a) {
	Array low = D.belowDiagonal_ / a, mid = D.diagonal_ / a, high =
			D.aboveDiagonal_ / a;
	TridiagonalOperator result(low, mid, high);
	return result;
}

TridiagonalOperator::TridiagonalOperator(Size size) {
	if (size >= 3) {
		diagonal_ = Array(size);
		lowerDiagonal_ = Array(size - 1);
		upperDiagonal_ = Array(size - 1);
	} else if (size == 0) {
		diagonal_ = Array(0);
		lowerDiagonal_ = Array(0);
		upperDiagonal_ = Array(0);
	} else {
        throw;
    //Error(“invalid size for tridiagonal operator” “(must be null or >= 3)”);
}
}

/**********************************************************************************
 TridiagonalOperator: constructor
 5.6 Object-Oriented Finite-Difference Implementation 215
 **********************************************************************************/
TridiagonalOperator::TridiagonalOperator(const Array& low, const Array& mid,
		const Array& high) :
		diagonal_(mid), lowerDiagonal_(low), upperDiagonal_(high) {
    //QL_ENSURE(low.size() == mid.size()-1, “wrong size for lower diagonal vector”);
    //QL_ENSURE(high.size() == mid.size()-1, “wrong size for upper diagonal vector”);
}

/**********************************************************************************
 applyTo : applies tridiagonal operator to grid points
 [in]: Array& v: : grid points
 [out]: Array : results of operation
 **********************************************************************************/
Array TridiagonalOperator::applyTo(const Array& v) const {
//	QL_REQUIRE(v.size()==size(),
//			“TridiagonalOperator::applyTo: vector of the wrong size (“ +
//					IntegerFormatter::toString(v.size()) + “instead of “ +
//					IntegerFormatter::toString(size()) + “)” );
	Array result(size());
// matricial product
	result[0] = diagonal_[0] * v[0] + upperDiagonal_[0] * v[1];
	for (Size j = 1; j <= size() - 2; j++)
		result[j] = lowerDiagonal_[j - 1] * v[j - 1] + diagonal_[j] * v[j]
				+ upperDiagonal_[j] * v[j + 1];
	result[size() - 1] = lowerDiagonal_[size() - 2] * v[size() - 2]
			+ diagonal_[size() - 1] * v[size() - 1];
	return result;
}

/**********************************************************************************
 solve for : solves the tridiagonal system
 [in]: Array& rhs: : rhs of system
 [out]: Array : solution of rhs of tridiagonal system
 **********************************************************************************/
Array TridiagonalOperator::solveFor(const Array& rhs) const {
//	QL_REQUIRE(rhs.size()==size(),
//			“TridiagonalOperator::solveFor: rhs has the wrong size”);
	Array result(size()), tmp(size());
	double bet = diagonal_[0];
//	QL_REQUIRE(bet != 0.0, “TridiagonalOperator::solveFor: division by zero”);
	result[0] = rhs[0] / bet;
	Size j;
	for (j = 1; j <= size() - 1; j++) {
		tmp[j] = upperDiagonal_[j - 1] / bet;
		bet = diagonal_[j] - lowerDiagonal_[j - 1] * tmp[j];
//		QL_ENSURE(bet != 0.0, “TridiagonalOperator::solveFor: division by zero”);
		result[j] = (rhs[j] - lowerDiagonal_[j - 1] * result[j - 1]) / bet;
	}
// cannot be j>=0 with Size j
	for (j = size() - 2; j > 0; j--)
		result[j] -= tmp[j + 1] * result[j + 1];
	result[0] -= tmp[1] * result[1];
	return result;
}

// Abstract boundary condition class for finite difference problems
template<class Operator>
class BoundaryCondition {
public:
// types and enumerations
	typedef Operator operatorType;
	typedef typename Operator::arrayType arrayType;
	enum Side {
		None, Upper, Lower
	};
// destructor
    //virtual ∼ BoundaryCondition() {}
// interface
// This method modifies an operator L before it is
// applied to an array u so that v = Lu will
// satisfy the given condition.
	virtual void applyBeforeApplying(operatorType&) const = 0;
// This method modifies an array u so that it satisfies
// the given condition.
	virtual void applyAfterApplying(arrayType&) const = 0;
// This method modifies an operator L before the linear
// system Lu’ = u is solved so that u’ will
// satisfy the given condition.
	virtual void applyBeforeSolving(operatorType&, arrayType& rhs) const = 0;
// This method modifies an array so that it satisfies the given condition.
	virtual void applyAfterSolving(arrayType&) const = 0;
// This method sets the current time for time-dependent boundary conditions.
	virtual void setTime(Time t) = 0;
};

// Neumann boundary condition (i.e., constant derivative)
class NeumannBC: public BoundaryCondition<TridiagonalOperator> {
public:
	NeumannBC(double value, Side side);
// interface
	void applyBeforeApplying(TridiagonalOperator&) const;
	void applyAfterApplying(Array&) const;
	void applyBeforeSolving(TridiagonalOperator&, Array& rhs) const;
	void applyAfterSolving(Array&) const;
	void setTime(Time t) {
	}
private:
	double value_;
	Side side_;
};

// DirichletBC boundary condition (i.e., constant value)
class DirichletBC: public BoundaryCondition<TridiagonalOperator> {
public:
	DirichletBC(double value, Side side);
// interface
	void applyBeforeApplying(TridiagonalOperator&) const;
	void applyAfterApplying(Array&) const;
	void applyBeforeSolving(TridiagonalOperator&, Array& rhs) const;
	void applyAfterSolving(Array&) const;
	void setTime(Time t) {
	}
private:
	double value_;
	Side side_;
};

NeumannBC::NeumannBC(double value, NeumannBC::Side side) :
		value_(value), side_(side) {
}

/******************************************************************************
 applyBeforeApplying : apply Neumann boundary conditions before applying
 Tridiag Operator
 [in] : TridiagonalOperator& L : tridiag operator
 [out]: none
 ******************************************************************************/
void NeumannBC::applyBeforeApplying(TridiagonalOperator& L) const {
	switch (side_) {
	case Lower:
		L.setFirstRow(-1.0, 1.0);
		break;
	case Upper:
        L.setLastRow(-1.0, 1.0);
		break;
	default:
        throw;
        //Error(“Unknown side for Neumann boundary condition”);
    }
}

/**********************************************************************************
 applyAfterApplying : apply Neumann boundary conditions after applying Triadiagonal
 Operator
 [in] : Array& L : array of values
 [out]: none
 **********************************************************************************/
void NeumannBC::applyAfterApplying(Array& u) const {
	switch (side_) {
	case Lower:
		u[0] = u[1] - value_;
		break;
	case Upper:
		u[u.size() - 1] = u[u.size() - 2] + value_;
		break;
	default:
        throw;
        //Error(“Unknown side for Neumann boundary condition”);
    }
}

/**********************************************************************************
 applyAfterApplying : apply Neumann boundary conditions before solving system
 [in] : TridiagonalOperator& L : tridiagonal operator
 Array& L : array of values
 [out]: none
 **********************************************************************************/
void NeumannBC::applyBeforeSolving(TridiagonalOperator& L, Array& rhs) const {
	switch (side_) {
	case Lower:
		L.setFirstRow(-1.0, 1.0);
		rhs[0] = value_;
		break;
	case Upper:
		L.setLastRow(-1.0, 1.0);
		rhs[rhs.size() - 1] = value_;
		break;
	default:
        throw;
        //Error	(“Unknown side for Neumann boundary condition”);
    }
}

void NeumannBC::applyAfterSolving(Array&) const {
}

// Dirichlet conditions
DirichletBC::DirichletBC(double value, DirichletBC::Side side)
    : value_(value), side_(side) {}

/**********************************************************************************
 applyBeforeApplying : apply Dirichlet boundary conditions before solving system
 [in] : TridiagonalOperator& L : tridiagonal operator
 [out]: none
 **********************************************************************************/
void DirichletBC::applyBeforeApplying(TridiagonalOperator& L) const {
	switch (side_) {
	case Lower:
		L.setFirstRow(1.0, 0.0);
		break;
	case Upper:
		L.setLastRow(0.0, 1.0);
		break;
	default:
        throw;
        //Error	(“Unknown side for Neumann boundary condition”);
    }
}

/**********************************************************************************
 applyAfterApplying : apply Dirichlet boundary conditions after applying
 Triadiagonal Operator
 [in] : Array& L : array of values
 [out]: none
 **********************************************************************************/
void DirichletBC::applyAfterApplying(Array& u) const {
	switch (side_) {
	case Lower:
		u[0] = value_;
		break;
	case Upper:
		u[u.size() - 1] = value_;
		break;
	default:
        throw;
        //Error	(“Unknown side for Neumann boundary condition”);
    }
}

/**********************************************************************************
 p.220
 applyAfterApplying : apply Dirichlet boundary conditions before solving system
 [in] : TridiagonalOperator& L : tridiagonal operator
 Array& L : array of values
 [out]: none
 **********************************************************************************/
void DirichletBC::applyBeforeSolving(TridiagonalOperator& L, Array& rhs) const {
	switch (side_) {
	case Lower:
		L.setFirstRow(1.0, 0.0);
		rhs[0] = value_;
		break;
	case Upper:
		L.setLastRow(0.0, 1.0);
		rhs[rhs.size() - 1] = value_;
		break;
	default:
        throw;
        //Error	(“Unknown side for Neumann boundary condition”);
    }
}

void DirichletBC::applyAfterSolving(Array&) const {
}

///**********************************************************************************
// SOR : solve tridiagonal system with SOR technique
// [in] Array& rhs : initial guess for solution
// double tol : error tolerance
// [out] Array : solution of tridiagonal system
// **********************************************************************************/
//Array TridiagonalOperator::SOR(const Array& rhs, double tol) const {
//    //QL_REQUIRE(rhs.size()==size(), "TridiagonalOperator::solveFor: rhs has the wrong size");
//    QL_REQUIRE(rhs.size()==size(), "TridiagonalOperator::solveFor: rhs has the wrong size");

//    // initial guess
//	Array result = rhs;
//    // solve tridiagonal system with SOR technique
//	Size sorIteration, i;
//	double omega = 1.5; // omega
//	double err = 2.0 * tol; // error
//	double temp; // temporarily stores SOR values
//	for (sorIteration = 0; err > tol; sorIteration++) {
////		QL_REQUIRE(sorIteration<100000,
////                "TTridiagonalOperator::SOR: tolerance [" + DoubleFormatter::toString(tol) +
////				"] not reached in " + IntegerFormatter::toString(sorIteration) +
////				" iterations. The error still is " + DoubleFormatter::toString(err));
//        QL_REQUIRE(sorIteration<100000, "iteration must be less than 100,000");
//		err = 0.0;
//		for (i = 1; i < size() - 2; i++) {
//			temp = omega
//					* (rhs[i] - upperDiagonal_[i] * result[i + 1]
//							- diagonal_[i] * result[i]
//							- lowerDiagonal_[i - 1] * result[i - 1])
//					/ diagonal_[i];
//			err += temp * temp;
//			result[i] += temp;
//		}
//	}
//	return result;
//}

///**********************************************************************************
// solveCrankNicolson: values an option using the Crank Nicolson scheme in (5.39)
// [in]: double price : asset price
// double strike : strike price
// double vol : volatility
// double rate : risk-free rate
// double div : dividend yield
// double T : maturity
// int N : number of time steps
// int M : number of space steps
// char type : (C)all or (P)ut
// [out] : double option price
// **********************************************************************************/
//double CrankNicolson::solveCrankNicolson(double price, double strike, double T,
//		double vol, double rate, double div, long M, long N, char type) {
//	double b[50] = { 0.0 }; // vector of Z(i,j)’s
//	double c1[50] = { 0.0 };
//	double d[50] = { 0.0 };
//	double x[100] = { 0.0 };
//	double dx = 0.0; // state step size
//	double drift = rate - div - vol * vol / 2; // drift rate
//	double pu, pm, pd; // risk neutral probabilities
//	int i, j;
//	double a = 0.0;
//	double deltat = T / N; // time step size
//	cout.setf(ios::showpoint);
//	cout.precision(2);
//	dx = vol * sqrt(3 * deltat / 2);

//    // we multiply by 0.5 because we are using Crank-Nicolson
//	a = 0.5 * (deltat / (dx * dx));

//    // compute probabilities
//	pu = -0.5 * deltat * ((vol * vol) / (dx * dx) + drift / dx);
//    pm = 1 + deltat * (vol * vol / (dx * dx)) + rate * deltat;
//    pd = -0.5 * deltat * ((vol * vol) / (dx * dx) - drift / dx);

//    // calculate coefficients
//	for (j = -M; j <= M; j++) {
//		S[N][j] = price * exp(j * dx);
//		S[0][j] = price;
//	}
//    // compute stock prices
//	for (i = 1; i < N; i++) {
//		for (j = -M; j <= M; j++)
//			S[i][j] = S[i - 1][j] * exp(j * dx);
//	}
//    // calculate payoffs
//    if (type == 'P') {
//		for (j = -M; j <= M; j++) {
//			P[N][j] = max(strike - S[N][j], 0);
//		}
//        // calculate boundary conditions
//		for (i = 0; i < N; i++) {
//			P[i][-M] = P[i][-M + 1] + 1 * (S[i][-M + 1] - S[i][-M]); // derivative boundary
//            // condition
//			P[i][M] = 0;
//		}
//	} else // if type == ‘C’
//	{
//        // calculate boundary conditions
//		for (j = -M; j <= M; j++) {
//			P[N][j] = max(S[N][j] - strike, 0);
//		}
//        // calculate boundary conditions
//		for (i = 0; i < N; i++) {
//			P[i][-M] = 0;
//			P[i][M] = P[i][M - 1] + (S[i][M] - S[i][M - 1]); // derivative boundary condition
//		}
//	}
//	for (j = -M + 1; j < M; j++)
//		b[j] = (1 - a) * P[N][j] + a * (P[N][j + 1] + P[N][j - 1]);
//	b[-M + 1] = b[-M + 1] + a * P[N][-M];
//    b[M - 1] = b[M - 1] + a * P[N][M];
//	solveCNTridiagonal(N, M, pu, pm, pd, d, c1, b, type, strike);

//    // print out mesh
//	for (i = 0; i <= N; i++)
//        std::cout << " " << T - deltat*i <<std::endl;
//    std::cout << "\n\n " << std::endl;
//	for (j = M; j >= -M; j--) {
//        std::cout << " " << S[N][j];
//		for (i = 0; i <= N; i++) {
//            std::cout << " " << std::endl;
//			if (j != -M)
//                std::cout << " " <<P[i][j];
//			else
//                std::cout << " " << P[N][-M];
//            std::cout << "\n";
//		}
//	}
//    std::cout << "\n " << std::endl;
//	return P[0][0];
//}

///**********************************************************************************
// solveCNTridiagonal : solves the Crank Nicolson tridiagonal system of equations
// [in]: int N : number of time steps
// int M : number of state steps
// double pu : up probability
// double pm : middle probability
// double pd : down probability
// double* d: : array used in solving tridiagonal system
// double* c1: : array used in solving tridiagonal system
// double* d1: : array of Z(i,j)’s
// char type : (C)all or (P)ut
// double strike : strike price
// [out] : double : option price
// **********************************************************************************/
//void CrankNicolson::solveCNTridiagonal(int N, int M, double pu, double pm,
//		double pd, double *d, double *c1, double *d1, char type,
//		double strike) {
//	int i, j;
//	for (j = -M; j <= M; j++)
//		d[j] = P[N][j];

//    // set values at boundary points
//	d1[-M] = d[-M] / pm;
//	d1[-M + 1] = d[-M + 1] / pm;
//	c1[-M] = pd / pm;
//    c1[-M + 1] = pd / pm;
//	for (j = -M + 1; j <= M - 2; j++)
//		c1[j + 1] = pd / (pm - pu * c1[j]);
//	for (j = -M + 1; j <= M - 1; j++)
//		d1[j + 1] = (d[j + 1] - pu * d1[j - 1]) / (pm - pu * c1[j]);

//    // solve tridiagonal system
//	for (i = N - 1; i >= 0; i--) {
//		for (j = -M + 1; j <= M - 1; j++) {
//			if (i != N - 1)
//				d[j] = P[i + 1][j];
//			if (j == -M + 1)
//				d1[-M + 1] = d[-M + 1] / pm;

//			d1[j + 1] = (d[j + 1] - pu * d1[j - 1]) / (pm - pu * c1[j]);
//			P[i][-j] = d1[-j] - c1[-j] * P[i][-j + 1];

//            // check early exercise
//            if (type == 'P') {
//				if (P[i][-j] < strike - S[N][-j])
//					P[i][-j] = strike - S[N][-j];
//			} else {
//				if (P[i][-j] < S[N][-j] - strike)
//					P[i][-j] = S[N][-j] - strike;
//			}
//		}
//	}
//}

}

int main(int, char*[]) {

	try {

		boost::timer timer;
		std::cout << std::endl;

        LARGE_TITLE("London (2005) Derivatives Modeling: Finite Difference Method");



		// End test
		Real seconds = timer.elapsed();
		Integer hours = int(seconds / 3600);
		seconds -= hours * 3600;
		Integer minutes = int(seconds / 60);
		seconds -= minutes * 60;
		std::cout << " \nRun completed in ";
		if (hours > 0)
			std::cout << hours << " h ";
		if (hours > 0 || minutes > 0)
			std::cout << minutes << " m ";
		std::cout << std::fixed << std::setprecision(0) << seconds << " s\n"
				<< std::endl;
		return 0;

	} catch (std::exception& e) {
		std::cerr << e.what() << std::endl;
		return 1;
	} catch (...) {
		std::cerr << "unknown error" << std::endl;
		return 1;
	}
}
