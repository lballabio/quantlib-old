=begin
 Copyright (C) 2000, 2001, 2002 RiskMap srl

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
require 'runit/testcase'
require 'runit/cui/testrunner'

class OldPricerTest < RUNIT::TestCase
  include QuantLib
  def name
    case @method
      when 'testBarrierPricer'
        "Testing old-style barrier option pricer..."
      when 'testBinaryPricer'
        "Testing old-style binary option pricer..."
      when 'testCliquetPricer'
        "Testing old-style cliquet option pricer..."
      when 'testDividendEuropeanPricer'
        "Testing old-style European option pricer with dividends..."
      when 'testFdEuropeanPricer'
        "Testing old-style finite-difference European option pricer..."
      when 'testAmericanPricers'
        "Testing old-style American-type pricers..."
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
        assert_fail(<<-MESSAGE

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
        assert_fail(<<-MESSAGE

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
        assert_fail(<<-MESSAGE

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
            assert_fail(<<-MESSAGE

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
      assert_fail(<<-MESSAGE

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
            assert_fail(<<-MESSAGE

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
          assert_fail(<<-MESSAGE

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
            assert_fail(<<-MESSAGE

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
end

if $0 == __FILE__
  RUNIT::CUI::TestRunner.run(OldPricerTest.suite)
end

