=begin
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
=end

# $Id$

require 'QuantLib'
require 'test/unit/testcase'
require 'test/unit/ui/console/testrunner'

class OldPricerTest < Test::Unit::TestCase
  include QuantLib
  def name
    case @method_name
      when 'testBarrierPricer'
        "Testing old-style barrier option pricer"
      when 'testBinaryPricer'
        "Testing old-style binary option pricer"
      when 'testCliquetPricer'
        "Testing old-style cliquet option pricer"
      when 'testDividendEuropeanPricer'
        "Testing old-style European option pricer with dividends"
      when 'testFdEuropeanPricer'
        "Testing old-style finite-difference European option pricer"
      when 'testAmericanPricers'
        "Testing old-style American-type pricers"
      when 'testMcSingleFactorPricers'
        "Testing old-style Monte Carlo single-factor pricers"
      when 'testMcMultiFactorPricers'
        "Testing old-style Monte Carlo multi-factor pricers"
    end
  end
  def relativeError(x1,x2,reference)
    if reference != 0.0
      (x1-x2).abs/reference
    else
      10e+10
    end
  end
  def testBarrierPricer
    maxErrorAllowed = 5e-5
    maxStraddleErrorAllowed = 5e-4
    underPrice = 100
    rebate = 3
    resTime = 0.5
    rRate = 0.08
    divRate = 0.04
    # this table is from:
    # "Option pricing formulas", E.G. Haug, McGraw-Hill 1998
    # pag 72
    values = [
      # barrType,  vol,strike, barrier, [   Call,     Put]
      ["DownOut", 0.25,    90,      95, [ 9.0246,  2.2798]],
      ["DownOut", 0.25,   100,      95, [ 6.7924,  2.2947]],
      ["DownOut", 0.25,   110,      95, [ 4.8759,  2.6252]],
      ["DownOut", 0.25,    90,     100, [ 3.0000,  3.0000]],
      ["DownOut", 0.25,   100,     100, [ 3.0000,  3.0000]],
      ["DownOut", 0.25,   110,     100, [ 3.0000,  3.0000]],
      ["UpOut",   0.25,    90,     105, [ 2.6789,  3.7760]],
      ["UpOut",   0.25,   100,     105, [ 2.3580,  5.4932]],
      ["UpOut",   0.25,   110,     105, [ 2.3453,  7.5187]],

      ["DownIn",  0.25,    90,      95, [ 7.7627,  2.9586]],
      ["DownIn",  0.25,   100,      95, [ 4.0109,  6.5677]],
      ["DownIn",  0.25,   110,      95, [ 2.0576, 11.9752]],
      ["DownIn",  0.25,    90,     100, [13.8333,  2.2845]],
      ["DownIn",  0.25,   100,     100, [ 7.8494,  5.9085]],
      ["DownIn",  0.25,   110,     100, [ 3.9795, 11.6465]],
      ["UpIn",    0.25,    90,     105, [14.1112,  1.4653]],
      ["UpIn",    0.25,   100,     105, [ 8.4482,  3.3721]],
      ["UpIn",    0.25,   110,     105, [ 4.5910,  7.0846]],

      ["DownOut", 0.30,    90,      95, [ 8.8334,  2.4170]],
      ["DownOut", 0.30,   100,      95, [ 7.0285,  2.4258]],
      ["DownOut", 0.30,   110,      95, [ 5.4137,  2.6246]],
      ["DownOut", 0.30,    90,     100, [ 3.0000,  3.0000]],
      ["DownOut", 0.30,   100,     100, [ 3.0000,  3.0000]],
      ["DownOut", 0.30,   110,     100, [ 3.0000,  3.0000]],
      ["UpOut",   0.30,    90,     105, [ 2.6341,  4.2293]],
      ["UpOut",   0.30,   100,     105, [ 2.4389,  5.8032]],
      ["UpOut",   0.30,   110,     105, [ 2.4315,  7.5649]],

      ["DownIn",  0.30,    90,      95, [ 9.0093,  3.8769]],
      ["DownIn",  0.30,   100,      95, [ 5.1370,  7.7989]],
      ["DownIn",  0.30,   110,      95, [ 2.8517, 13.3078]],
      ["DownIn",  0.30,    90,     100, [14.8816,  3.3328]],
      ["DownIn",  0.30,   100,     100, [ 9.2045,  7.2636]],
      ["DownIn",  0.30,   110,     100, [ 5.3043, 12.9713]],
      ["UpIn",    0.30,    90,     105, [15.2098,  2.0658]],
      ["UpIn",    0.30,   100,     105, [ 9.7278,  4.4226]],
      ["UpIn",    0.30,   110,     105, [ 5.8350,  8.3686]]
    ]

    values.each do |barrType, vol, strike, barrier, results|
      opCall = BarrierOption.new(barrType, "Call", underPrice,
                                 strike, divRate, rRate, resTime,
                                 vol, barrier, rebate)
      calculated = opCall.value
      expected   = results[0]
      error = (calculated - expected).abs
      unless error <= maxErrorAllowed
        flunk(<<-MESSAGE

    #{barrType} Call #{strike} #{barrier}
        value:    #{calculated}
        expected: #{expected}
        error:    #{error}

              MESSAGE
              )
      end
      opPut = BarrierOption.new(barrType, "Put", underPrice,
                                strike, divRate, rRate, resTime,
                                vol, barrier, rebate)
      calculated = opPut.value
      expected   = results[1]
      error = (calculated - expected).abs
      unless error <= maxErrorAllowed
        flunk(<<-MESSAGE

    #{barrType} Put #{strike} #{barrier}
        value:    #{calculated}
        expected: #{expected}
        error:    #{error}

              MESSAGE
              )
      end
      opStraddle = BarrierOption.new(barrType, "Straddle",
                                     underPrice, strike, divRate, rRate,
                                     resTime, vol, barrier, rebate)
      calculated = opStraddle.value
      expected   = results[0] + results[1]
      error = (calculated - expected).abs
      unless error <= maxStraddleErrorAllowed
        flunk(<<-MESSAGE

    #{barrType} Straddle #{strike} #{barrier}
        value:    #{calculated}
        expected: #{expected}
        error:    #{error}

              MESSAGE
              )
      end
    end
  end
  def testBinaryPricer
    tolerance = {
      'delta'  => 5e-5,
      'gamma'  => 5e-5,
      'theta'  => 5e-5,
      'rho'    => 5e-5,
      'divRho' => 5e-5,
      'vega'   => 5e-5
    }

    test_data = []
    ['Call','Put','Straddle'].each { |type|
    [100].each { |underlying|
    [0.01, 0.05, 0.15].each { |rRate|
    [0.04, 0.05, 0.06].each { |qRate|
    [1.0].each { |resTime|
    [50, 99.5, 100, 100.5, 150].each { |strike|
    [0.11, 0.5, 1.2].each { |vol|
        test_data.push [type,underlying,rRate,qRate,resTime,strike,vol]
    }}}}}}}

    test_data.each do |type,u,r,q,t,k,v|
      dS = u/10000.0
      dT = t/10000.0
      dVol = v/10000.0
      dR = r/10000.0
      dQ = q/10000.0
      opt = BinaryOption.new(type,u,k,q,r,t,v)
      opt_val = opt.value
      if opt_val > 1e-6
        optPs = BinaryOption.new(type, u+dS, k, q   , r,    t,    v)
        optMs = BinaryOption.new(type, u-dS, k, q   , r,    t,    v)
        optPt = BinaryOption.new(type, u   , k, q   , r,    t+dT, v)
        optMt = BinaryOption.new(type, u   , k, q   , r,    t-dT, v)
        optPr = BinaryOption.new(type, u   , k, q   , r+dR, t   , v)
        optMr = BinaryOption.new(type, u   , k, q   , r-dR, t   , v)
        optPq = BinaryOption.new(type, u   , k, q+dQ, r   , t   , v)
        optMq = BinaryOption.new(type, u   , k, q-dQ, r   , t   , v)
        optPv = BinaryOption.new(type, u   , k, q   , r   , t   , v+dVol)
        optMv = BinaryOption.new(type, u   , k, q   , r   , t   , v-dVol)

        expected = {
          'delta'  => opt.delta,
          'gamma'  => opt.gamma,
          'theta'  => opt.theta,
          'rho'    => opt.rho,
          'divRho' => opt.dividendRho,
          'vega'   => opt.vega
        }
                
        calculated = {
          'delta'  =>  (optPs.value-optMs.value)/(2*dS),
          'gamma'  =>  (optPs.delta-optMs.delta)/(2*dS),
          'theta'  => -(optPt.value-optMt.value)/(2*dT),
          'rho'    =>  (optPr.value-optMr.value)/(2*dR),
          'divRho' =>  (optPq.value-optMq.value)/(2*dQ),
          'vega'   =>  (optPv.value-optMv.value)/(2*dVol)
        }

        ['delta','gamma','rho','divRho','theta','vega'].each do |greek|
          expct = expected[greek]
          calcl = calculated[greek]
          unless relativeError(expct,calcl,u) <= tolerance[greek]
            flunk(<<-MESSAGE

    Option details: #{type} #{u} #{k} #{q} #{r} #{t} #{v}
        calculated #{greek} : #{calcl}
        expected   #{greek} : #{expct}

                  MESSAGE
                  )
          end
        end
      end
    end
  end
  def testCliquetPricer
    spot = 60
    moneyness = 1.1
    divYield = [0.04, 0.04]
    rRate = [0.08, 0.08]
    dates = [0.25, 1.00]
    vol = [0.30, 0.30]
    cliquet = CliquetOption.new("Call", spot, moneyness,
                                divYield, rRate, dates, vol)
    
    # Haug, pag 37
    storedValue = 4.4064
    pvalue = cliquet.value

    unless (pvalue-storedValue).abs <= 1e-4
      flunk(<<-MESSAGE

    calculated value: #{pvalue}
    stored value:     #{storedValue}

            MESSAGE
            )
    end
  end
  def testDividendEuropeanPricer
    nstp = 150
    ngrd = nstp+1

    div   = [3.92, 4.21]
    dates = [0.333, 0.667]

    tolerance = {
      'delta' => 1e-4,
      'gamma' => 1e-4,
      'theta' => 1e-4,
      'rho'   => 1e-4,
      'vega'  => 1e-4
    }

    test_data = []
    ['Call','Put','Straddle'].each { |type|
    [100].each { |underlying|
    [0.01, 0.1, 0.3].each { |rRate|
    [0.0, 0.05, 0.15].each { |qRate|
    [1.0, 2.0].each { |resTime|
    [50, 99.5, 100, 100.5, 150].each { |strike|
    [0.04, 0.2, 0.7].each { |vol|
        test_data.push [type,underlying,rRate,qRate,resTime,strike,vol]
    }}}}}}}

    factory = FdDividendEuropeanOption
    test_data.each do |type,u,r,q,t,k,v|
      du = u/10000.0
      dT = t/nstp
      dv = v/10000.0
      dr = r/10000.0
      option = factory.new(type,u,k,q,r,t,v,div,dates)
      if option.value > 0.00001*u
        optPs = factory.new(type,u+du,k,q,r   ,t   ,v,   div,dates)
        optMs = factory.new(type,u-du,k,q,r   ,t   ,v,   div,dates)
        optPt = factory.new(type,u   ,k,q,r   ,t+dT,v,   div,
                            dates.map { |d| d+dT })
        optMt = factory.new(type,u   ,k,q,r   ,t-dT,v,   div,
                            dates.map { |d| d-dT })
        optPr = factory.new(type,u   ,k,q,r+dr,t   ,v,   div,dates)
        optMr = factory.new(type,u   ,k,q,r-dr,t   ,v,   div,dates)
        optPv = factory.new(type,u   ,k,q,r   ,t   ,v+dv,div,dates)
        optMv = factory.new(type,u   ,k,q,r   ,t   ,v-dv,div,dates)

        expected = {
          "delta" => option.delta,
          "gamma" => option.gamma,
          "theta" => option.theta,
          "rho"   => option.rho,
          "vega"  => option.vega
        }

        calculated = {
          "delta" =>  (optPs.value-optMs.value)/(2*du),
          "gamma" =>  (optPs.delta-optMs.delta)/(2*du),
          "theta" => -(optPt.value-optMt.value)/(2*dT),
          "rho"   =>  (optPr.value-optMr.value)/(2*dr),
          "vega"  =>  (optPv.value-optMv.value)/(2*dv)
        }

        ['delta','gamma','rho','theta','vega'].each do |greek|
          expct = expected[greek]
          calcl = calculated[greek]
          unless relativeError(expct,calcl,u) <= tolerance[greek]
            flunk(<<-MESSAGE

    Option details: #{type} #{u} #{k} #{q} #{r} #{t} #{v}
        calculated #{greek} : #{calcl}
        expected   #{greek} : #{expct}

                  MESSAGE
                  )
          end
        end
      end
    end
  end
  def testFdEuropeanPricer
    u = 100
    strikeMin = 60
    strikeRange = 100
    rRateRange = 0.18
    qRateRange = 0.02
    volRange = 1.2
    timeMin = 0.5
    timeRange = 2.0

    tolerance = 1e-2
    totCases = 200

    rng = UniformRandomGenerator.new(56789012)
    totCases.times do
      k = strikeMin + strikeRange * rng.next().value()
      q =              qRateRange * rng.next().value()
      r =              rRateRange * rng.next().value()
      v =                volRange * rng.next().value()
      t = timeMin   +   timeRange * rng.next().value()

      ['Call', 'Put', 'Straddle'].each do |type|
        anValue = EuropeanOption.new(type,u,k,q,r,t,v).value()
        numValue = FdEuropean.new(type,u,k,q,r,t,v,100,400).value()
        unless (anValue - numValue).abs <= tolerance
          flunk(<<-MESSAGE

    Option details: #{type} #{u} #{k} #{q} #{r} #{t} #{v}
        calculated: #{numValue}
        expected:   #{anValue}

                MESSAGE
                )
        end
      end
    end
  end
  def testAmericanPricers
    nstp = 145
    ngrd = nstp + 1

    tolerance = {
      'delta'  => 2e-3,
      'gamma'  => 2e-3,
      'theta'  => 2e-3,
      'rho'    => 2e-3,
      'divRho' => 2e-3,
      'vega'   => 2e-3
    }

    test_data = []
    [FdAmericanOption, FdShoutOption].each { |pricer|
    ['Call','Put','Straddle'].each { |type|
    [100].each { |underlying|
    [0.01, 0.05, 0.15].each { |rRate|
    [0.04, 0.05, 0.06].each { |qRate|
    [1.0].each { |resTime|
    [50, 100, 150].each { |strike|
    [0.05, 0.5, 1.2].each { |vol|
        test_data.push [pricer,type,underlying,rRate,qRate,resTime,strike,vol]
    }}}}}}}}

    test_data.each do |pricer,type,u,r,q,t,k,v|
      # check Greeks
      du = u/10000.0
      dv = v/10000.0
      dr = r/10000.0
      dq = q/10000.0

      option = pricer.new(type,u,k,q,r,t,v,nstp,ngrd)
      if option.value > u*1e-5
        optPs = pricer.new(type,u+du,k,q,   r,   t,v,   nstp,ngrd)
        optMs = pricer.new(type,u-du,k,q,   r,   t,v,   nstp,ngrd)
        optPr = pricer.new(type,u,   k,q,   r+dr,t,v,   nstp,ngrd)
        optMr = pricer.new(type,u,   k,q,   r-dr,t,v,   nstp,ngrd)
        optPq = pricer.new(type,u,   k,q+dq,r,   t,v,   nstp,ngrd)
        optMq = pricer.new(type,u,   k,q-dq,r,   t,v,   nstp,ngrd)
        optPv = pricer.new(type,u,   k,q,   r,   t,v+dv,nstp,ngrd)
        optMv = pricer.new(type,u,   k,q,   r,   t,v-dv,nstp,ngrd)

        calculated = {
          "delta"  => option.delta,
          "gamma"  => option.gamma,
          "theta"  => option.theta,
          "rho"    => option.rho,
          "divRho" => option.dividendRho,
          "vega"   => option.vega
        }
                
        expected = {
          "delta"  => (optPs.value-optMs.value)/(2*du),
          "gamma"  => (optPs.delta-optMs.delta)/(2*du),
          "theta"  => r*option.value - (r-q)*u*option.delta \
                      - 0.5*v*v*u*u*option.gamma(),
          "rho"    => (optPr.value-optMr.value)/(2*dr),
          "divRho" => (optPq.value-optMq.value)/(2*dq),
          "vega"   => (optPv.value-optMv.value)/(2*dv)
        }

        ["delta","gamma","theta","rho","divRho","vega"].each do |greek|
          expct = expected[greek]
          calcl = calculated[greek]
          unless relativeError(expct,calcl,u) <= tolerance[greek]
            flunk(<<-MESSAGE

    Option details: #{type} #{u} #{k} #{q} #{r} #{t} #{v}
        calculated #{greek} : #{calcl}
        expected   #{greek} : #{expct}

                  MESSAGE
                  )
          end
        end
      end
    end
  end
  def testMcSingleFactorPricers
    seed = 3456789
    fixedSamples = 100
    minimumTol = 0.01
    
    # data from "Implementing Derivatives Model",
    # Clewlow, Strickland, pag.118-123
    cases = [
      [DiscreteGeometricAPO, "Call", 100.0, 100.0, 0.03, 0.06,
        [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0],
        0.2, 5.34255485619]
    ]
    cases.each do |pricer, optionType, underlying, strike,
        dividendYield, riskFreeRate, timeIncrements,
        volatility, storedValue|
      p = pricer.new(optionType, underlying, strike, dividendYield,
                     riskFreeRate, timeIncrements, volatility)
      pvalue = p.value
      unless (pvalue-storedValue).abs <= 1e-10
        flunk(<<-MESSAGE

    in batch 1:
        calculated : #{pvalue}
        expected   : #{storedValue}

              MESSAGE
              )
      end
    end
    
    # data from "Option Pricing Formulas", Haug, pag.96-97
    cases = [
      [EuropeanOption,         "Put", 80.0, 85.0,
        -0.03, 0.05, 0.25, 0.2, 5.21858890396],
      [ContinuousGeometricAPO, "Put", 80.0, 85.0,
        -0.03, 0.05, 0.25, 0.2, 4.69221973405]
    ]

    cases.each do |pricer, optionType, underlying, strike,
        dividendYield, riskFreeRate, residualTime,
        volatility, storedValue|
      p = pricer.new(optionType, underlying, strike, dividendYield,
                     riskFreeRate, residualTime, volatility)
      pvalue = p.value
      unless (pvalue-storedValue).abs <= 1e-10
        flunk(<<-MESSAGE

    in batch 2:
        calculated : #{pvalue}
        expected   : #{storedValue}

              MESSAGE
              )
      end
    end

    # trying to approximate the continous version with the discrete version
    cases = [
      [DiscreteGeometricAPO, "Put", 80.0, 85.0,
        -0.03, 0.05, 0.25, 90000, 0.2, 4.6922231469]
    ]

    cases.each do |pricer, optionType, underlying, strike,
        dividendYield, riskFreeRate, residualTime, timesteps,
        volatility, storedValue|
      dt=residualTime/timesteps
      timeIncrements = (0...timesteps).map { |i| (i+1)*dt }
      p = pricer.new(optionType, underlying, strike, dividendYield,
                     riskFreeRate, timeIncrements, volatility)
      pvalue = p.value
      unless (pvalue-storedValue).abs <= 1e-10
        flunk(<<-MESSAGE

    in batch 3:
        calculated : #{pvalue}
        expected   : #{storedValue}

              MESSAGE
              )
      end
    end

    cases = [
      [McEuropean,      "Put", 80.0, 85.0,
        -0.03, 0.05, 0.25, 0.2, 5.9135872358, false],
      [McEuropean,      "Put", 80.0, 85.0,
        -0.03, 0.05, 0.25, 0.2, 5.42005964479, true],
      [McEuropean,     "Call", 80.0, 85.0,
        -0.03, 0.05, 0.25, 0.2, 1.98816310759, false],
      [McEuropean,     "Call", 80.0, 85.0,
        -0.03, 0.05, 0.25, 0.2, 2.12098432917, true],
      [McEuropean, "Straddle", 80.0, 85.0,
        -0.03, 0.05, 0.25, 0.2, 7.90175034339, false],
      [McEuropean, "Straddle", 80.0, 85.0,
        -0.03, 0.05, 0.25, 0.2, 7.54104397396, true]
    ]
    cases.each do |pricer, optionType, underlying, strike,
             dividendYield, riskFreeRate, residualTime,
             volatility, storedValue, antithetic|
      p = pricer.new(optionType, underlying, strike, dividendYield,
                     riskFreeRate, residualTime, volatility,
                     antithetic, seed)
      pvalue = p.valueWithSamples(fixedSamples)
      unless (pvalue-storedValue).abs <= 1e-10
        flunk(<<-MESSAGE

    in batch 4:
        calculated : #{pvalue}
        expected   : #{storedValue}

              MESSAGE
              )
      end
      tol = p.errorEstimate/pvalue
      tol = [tol/2.0, minimumTol].min
      pvalue = p.value(tol)
      accuracy = p.errorEstimate/pvalue
      unless accuracy <= tol
        flunk(<<-MESSAGE

    in batch 4:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

              MESSAGE
              )
      end
    end
    
    # data from "Asian Option", Levy, 1997
    # in "Exotic Options: The State of the Art",
    # edited by Clewlow, Strickland
    cases = [
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0, 2, 0.13, 1.38418414762, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0, 4, 0.13, 1.57691714387, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0, 8, 0.13, 1.66062743445, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0, 12, 0.13, 1.68847081883, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0, 26, 0.13, 1.72955964448, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0, 52, 0.13, 1.73372169316, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0, 100, 0.13, 1.74918801089, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0, 250, 0.13, 1.75421310915, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0, 500, 0.13, 1.75158383443, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0, 1000, 0.13, 1.7516211018 , true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 2, 0.13, 1.83665087164, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 4, 0.13, 2.00560271429, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 8, 0.13, 2.07789721712, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 12, 0.13, 2.09622556625, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 26, 0.13, 2.14229795212, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 52, 0.13, 2.14470270916, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 100, 0.13, 2.1595414574, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 250, 0.13, 2.1600769002, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 500, 0.13, 2.159867044 , true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0,1000, 0.13, 2.1595163439, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 2, 0.13, 2.63315092584, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 4, 0.13, 2.76723962361, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 8, 0.13, 2.83124836881, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 12, 0.13, 2.84290301412, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 26, 0.13, 2.88179560417, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 52, 0.13, 2.88447044543, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 100, 0.13, 2.8998532960, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 250, 0.13, 2.9004729606, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 500, 0.13, 2.8981341216, true, true],
      [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0,1000, 0.13, 2.8970336244, true, true]
    ]
    cases.each do |pricer, optionType, underlying, strike,
        dividendYield, riskFreeRate, first, length, fixings,
        volatility, storedValue, antithetic, controlVariate|
      dt=(length)/(fixings-1)
      timeIncrements = (0...fixings).map { |i| i*dt+first }
      p = pricer.new(optionType, underlying, strike, dividendYield,
                     riskFreeRate, timeIncrements, volatility,
                     antithetic, controlVariate, seed)
      pvalue = p.valueWithSamples(fixedSamples)
      unless (pvalue-storedValue).abs <= 1e-10
        flunk(<<-MESSAGE

    in batch 5:
        calculated : #{pvalue}
        expected   : #{storedValue}

              MESSAGE
              )
      end
      tol = p.errorEstimate/pvalue
      tol = [tol/2.0, minimumTol].min
      pvalue = p.value(tol)
      accuracy = p.errorEstimate/pvalue
      unless accuracy <= tol
        flunk(<<-MESSAGE

    in batch 5:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

              MESSAGE
              )
      end
    end

    # data from "Asian Option", Levy, 1997
    # in "Exotic Options: The State of the Art",
    # edited by Clewlow, Strickland
    cases = [
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0,    2, 0.13, 1.51917595129, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0,    4, 0.13, 1.67940165674, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0,    8, 0.13, 1.75371215251, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0,   12, 0.13, 1.77595318693, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0,   26, 0.13, 1.8143053663 , true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0,   52, 0.13, 1.82269246898, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0,  100, 0.13, 1.83822402464, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0,  250, 0.13, 1.83875059026, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0,  500, 0.13, 1.83750703638, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 0.0, 11.0/12.0, 1000, 0.13, 1.83887181884, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 2, 0.13, 1.51154400089, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 4, 0.13, 1.67103508506, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 8, 0.13, 1.7452968407 , true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 12, 0.13, 1.76667074564, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 26, 0.13, 1.80528400613, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 52, 0.13, 1.81400883891, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 100, 0.13, 1.8292290145, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 250, 0.13, 1.8293711177, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0, 500, 0.13, 1.8282619319, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 1.0/12.0, 11.0/12.0,1000, 0.13, 1.8296784665, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 2, 0.13, 1.49648170891, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 4, 0.13, 1.65443100462, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 8, 0.13, 1.72817806731, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 12, 0.13, 1.74877367895, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 26, 0.13, 1.78733801988, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 52, 0.13, 1.79624826757, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 100, 0.13, 1.8111418688, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 250, 0.13, 1.8110115259, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 500, 0.13, 1.8100231194, true, true],
      [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
        0.06, 0.025, 3.0/12.0, 11.0/12.0, 1000, 0.13, 1.8114576031, true, true]
    ]
    cases.each do |pricer, optionType, underlying, strike,
        dividendYield, riskFreeRate, first, length, fixings,
        volatility, storedValue, antithetic, controlVariate|
      dt=(length)/(fixings-1)
      timeIncrements = (0...fixings).map { |i| i*dt+first }
      p = pricer.new(optionType, underlying, dividendYield,
                     riskFreeRate, timeIncrements, volatility,
                     antithetic, controlVariate, seed)
      pvalue = p.valueWithSamples(fixedSamples)
      unless (pvalue-storedValue).abs <= 1e-10
        flunk(<<-MESSAGE

    in batch 6:
        calculated : #{pvalue}
        expected   : #{storedValue}

              MESSAGE
              )
      end
      tol = p.errorEstimate/pvalue
      tol = [tol/2.0, minimumTol].min
      pvalue = p.value(tol)
      accuracy = p.errorEstimate/pvalue
      unless accuracy <= tol
        flunk(<<-MESSAGE

    in batch 6:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

              MESSAGE
              )
      end
    end
  end
  def testMcMultiFactorPricers
    cor = [[1.00, 0.50, 0.30, 0.10],
           [0.50, 1.00, 0.20, 0.40],
           [0.30, 0.20, 1.00, 0.60],
           [0.10, 0.40, 0.60, 1.00]]
    volatilities = [0.30,  0.35,  0.25,  0.20]
    cov = covariance(volatilities, cor)
    dividendYields = [0.01, 0.05, 0.04, 0.03]
    riskFreeRate = 0.05
    resTime = 1.0
    # degenerate portfolio
    perfectCorrelation = [[1.00, 1.00, 1.00, 1.00],
                          [1.00, 1.00, 1.00, 1.00],
                          [1.00, 1.00, 1.00, 1.00],
                          [1.00, 1.00, 1.00, 1.00]]
    sameAssetVols = [0.30,  0.30,  0.30,  0.30]
    sameAssetCovariance = covariance(sameAssetVols,
                                     perfectCorrelation)
    sameAssetDividend = [0.030,  0.030,  0.030,  0.030]
    seed = 86421
    fixedSamples = 100
    minimumTol = 0.01
    
    # McEverest
    p = McEverest.new(dividendYields, cov, riskFreeRate, resTime, false, seed)
    storedValue = 0.743448
    pvalue = p.valueWithSamples(fixedSamples)
    unless (pvalue-storedValue).abs <= 1e-5
      flunk(<<-MESSAGE

    McEverest:
        calculated : #{pvalue}
        expected   : #{storedValue}

            MESSAGE
            )
    end
    tol = p.errorEstimate/pvalue
    tol = [tol/2.0, minimumTol].min
    pvalue = p.value(tol)
    accuracy = p.errorEstimate/pvalue
    unless accuracy <= tol
      flunk(<<-MESSAGE

    McEverest:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

            MESSAGE
            )
    end
    p = McEverest.new(dividendYields, cov, riskFreeRate, resTime, true, seed)
    storedValue = 0.756979
    pvalue = p.valueWithSamples(fixedSamples)
    unless (pvalue-storedValue).abs <= 1e-5
      flunk(<<-MESSAGE

    McEverest:
        calculated : #{pvalue}
        expected   : #{storedValue}

            MESSAGE
            )
    end
    tol = p.errorEstimate/pvalue
    tol = [tol/2.0, minimumTol].min
    pvalue = p.value(tol)
    accuracy = p.errorEstimate/pvalue
    unless accuracy <= tol
      flunk(<<-MESSAGE

    McEverest:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

            MESSAGE
            )
    end

    # McBasket
    sameAssetValues = [25,  25,   25,  25]
    type = 'Call'
    strike = 100
    p = McBasket.new(type, sameAssetValues, strike, sameAssetDividend,
                     sameAssetCovariance, riskFreeRate, resTime, false, seed)
    # european would be 12.4426495605
    storedValue = 10.448445
    pvalue = p.valueWithSamples(fixedSamples)
    unless (pvalue-storedValue).abs <= 1e-5
      flunk(<<-MESSAGE

    McBasket:
        calculated : #{pvalue}
        expected   : #{storedValue}

            MESSAGE
            )
    end
    tol = p.errorEstimate/pvalue
    tol = [tol/2.0, minimumTol].min
    pvalue = p.value(tol)
    accuracy = p.errorEstimate/pvalue
    unless accuracy <= tol
      flunk(<<-MESSAGE

    McBasket:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

            MESSAGE
            )
    end
    p = McBasket.new(type, sameAssetValues, strike, sameAssetDividend,
                     sameAssetCovariance, riskFreeRate, resTime, true, seed)
    # european would be 12.4426495605
    storedValue = 12.294677
    pvalue = p.valueWithSamples(fixedSamples)
    unless (pvalue-storedValue).abs <= 1e-5
      flunk(<<-MESSAGE

    McBasket:
        calculated : #{pvalue}
        expected   : #{storedValue}

            MESSAGE
            )
    end
    tol = p.errorEstimate/pvalue
    tol = [tol/2.0, minimumTol].min
    pvalue = p.value(tol)
    accuracy = p.errorEstimate/pvalue
    unless accuracy <= tol
      flunk(<<-MESSAGE

    McBasket:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

            MESSAGE
            )
    end

    # McMaxBasket
    assetValues = [ 100,  110,   90,  105]
    p = McMaxBasket.new(assetValues, dividendYields, cov, 
                        riskFreeRate, resTime, false, seed)
    storedValue = 120.733780
    pvalue = p.valueWithSamples(fixedSamples)
    unless (pvalue-storedValue).abs <= 1e-5
      flunk(<<-MESSAGE

    McMaxBasket:
        calculated : #{pvalue}
        expected   : #{storedValue}

            MESSAGE
            )
    end
    tol = p.errorEstimate/pvalue
    tol = [tol/2.0, minimumTol].min
    pvalue = p.value(tol)
    accuracy = p.errorEstimate/pvalue
    unless accuracy <= tol
      flunk(<<-MESSAGE

    McMaxBasket:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

            MESSAGE
            )
    end
    p = McMaxBasket.new(assetValues, dividendYields, cov, 
                        riskFreeRate, resTime, true, seed)
    storedValue = 123.520909
    pvalue = p.valueWithSamples(fixedSamples)
    unless (pvalue-storedValue).abs <= 1e-5
      flunk(<<-MESSAGE

    McMaxBasket:
        calculated : #{pvalue}
        expected   : #{storedValue}

            MESSAGE
            )
    end
    tol = p.errorEstimate/pvalue
    tol = [tol/2.0, minimumTol].min
    pvalue = p.value(tol)
    accuracy = p.errorEstimate/pvalue
    unless accuracy <= tol
      flunk(<<-MESSAGE

    McMaxBasket:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

            MESSAGE
            )
    end

    # McPagoda
    portfolio   = [0.15, 0.20, 0.35, 0.30]
    fraction = 0.62
    roof = 0.20
    timeIncrements = [0.25, 0.5, 0.75, 1]
    p = McPagoda.new(portfolio, fraction, roof, dividendYields, cov, 
                     riskFreeRate, timeIncrements, false, seed);
    storedValue =  0.0343898
    pvalue = p.valueWithSamples(fixedSamples)
    unless (pvalue-storedValue).abs <= 1e-5
      flunk(<<-MESSAGE

    McPagoda:
        calculated : #{pvalue}
        expected   : #{storedValue}

            MESSAGE
            )
    end
    tol = p.errorEstimate/pvalue
    tol = [tol/2.0, minimumTol].min
    pvalue = p.value(tol)
    accuracy = p.errorEstimate/pvalue
    unless accuracy <= tol
      flunk(<<-MESSAGE

    McPagoda:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

            MESSAGE
            )
    end
    p = McPagoda.new(portfolio, fraction, roof, dividendYields, cov, 
                     riskFreeRate, timeIncrements, true, seed);
    storedValue = 0.0386095
    pvalue = p.valueWithSamples(fixedSamples)
    unless (pvalue-storedValue).abs <= 1e-5
      flunk(<<-MESSAGE

    McPagoda:
        calculated : #{pvalue}
        expected   : #{storedValue}

            MESSAGE
            )
    end
    tol = p.errorEstimate/pvalue
    tol = [tol/2.0, minimumTol].min
    pvalue = p.value(tol)
    accuracy = p.errorEstimate/pvalue
    unless accuracy <= tol
      flunk(<<-MESSAGE

    McPagoda:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

            MESSAGE
            )
    end

    # McHimalaya
    strike = 101
    p = McHimalaya.new(assetValues, dividendYields, cov, riskFreeRate, 
                       strike, timeIncrements, false, seed)
    storedValue = 5.0768499
    pvalue = p.valueWithSamples(fixedSamples)
    unless (pvalue-storedValue).abs <= 1e-5
      flunk(<<-MESSAGE

    McHimalaya:
        calculated : #{pvalue}
        expected   : #{storedValue}

            MESSAGE
            )
    end
    tol = p.errorEstimate/pvalue
    tol = [tol/2.0, minimumTol].min
    pvalue = p.value(tol)
    accuracy = p.errorEstimate/pvalue
    unless accuracy <= tol
      flunk(<<-MESSAGE

    McHimalaya:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

            MESSAGE
            )
    end
    p = McHimalaya.new(assetValues, dividendYields, cov, riskFreeRate, 
                       strike, timeIncrements, true, seed)
    storedValue = 6.2478050
    pvalue = p.valueWithSamples(fixedSamples)
    unless (pvalue-storedValue).abs <= 1e-5
      flunk(<<-MESSAGE

    McHimalaya:
        calculated : #{pvalue}
        expected   : #{storedValue}

            MESSAGE
            )
    end
    tol = p.errorEstimate/pvalue
    tol = [tol/2.0, minimumTol].min
    pvalue = p.value(tol)
    accuracy = p.errorEstimate/pvalue
    unless accuracy <= tol
      flunk(<<-MESSAGE

    McHimalaya:
        accuracy reached    : #{accuracy}
        tolerance requested : #{tol}

            MESSAGE
            )
    end
  end
end


if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(OldPricerTest)
end
