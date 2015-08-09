/*
 Copyright (C) 2015 Klaus Spanderen


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

package examples;

import org.quantlib.{Array => QArray, _}

object Optimizer {
    def main(args: Array[String]) : Unit = {

        try {
            System.loadLibrary("QuantLibJNI")
        } 
        catch {
            case ex: UnsatisfiedLinkError => {
                  println("please check your LD_LIBRARY_PATH variable")
                throw ex
            }
        }
        
        val costFunction = new CostFunctionDelegate {
            override def value(x: QArray) : Double = {
                val v = values(x)
                (0L until v.size()) map { i => v.get(i)*v.get(i) } sum
            }
            
            override def values(x: QArray): QArray = {
                val r = new QArray(2)
                r.set(0L, math.exp(x.get(0))-1d)
                r.set(1L, x.get(1) + x.get(0))  
                r
            }
        }

        def optimize(m: OptimizationMethod, name: String) = {
            val result = new Optimizer().solve(costFunction, new NoConstraint, m, 
                new EndCriteria(400, 40, 1.0e-8, 1.0e-8, 1.0e-8), new QArray(2L, 1d))
            
            println(name + "\t" + "%5.3g".format(costFunction.value(result)))
        }
        
        println("Optimization method   Target value")
        optimize(new LevenbergMarquardt,    "Levenberg-Marquardt")
        optimize(new Simplex(0.1),          "Simplex            ")
        optimize(new ConjugateGradient,     "ConjugateGradient  ")
        optimize(new SteepestDescent,       "SteepestDescent    ")
        optimize(new BFGS,                  "BFGS               ")
        optimize(new DifferentialEvolution, "DifferentialEvolution")
    }
}