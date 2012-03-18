/*
 Copyright (C) 2011 Klaus Spanderen


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

object RandomNumbers {
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

        val rng = new GaussianRandomGenerator(
                                    new UniformRandomGenerator(12345))
        
        val fmt = "%8d  %10.6f       %10.6f      %10.6f %10.6f\n"
        printf("  #rngs\t  mean(expected)  error(expected)\tmean\t  error\n");
        (1 to 10).foreach(i => {
            val nRngs = 1024*(1 << i)
            val st = new IncrementalStatistics()

            (0 until nRngs).foreach(_ => st.add(rng.nextValue()))

            printf(fmt, nRngs, 0.0, 1/scala.math.sqrt(nRngs),
                   st.mean(), st.errorEstimate())
        } )
    }
}