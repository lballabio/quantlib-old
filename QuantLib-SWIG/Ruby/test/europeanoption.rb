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
require 'test/unit'
require 'test/unit/ui/console/testrunner'

class EuropeanOptionTest < Test::Unit::TestCase
  include QuantLib
  def setup
    puts
    case @method_name
      when 'testGreeks'
        print "Testing European option greeks.."
      when 'testImpliedVol'
        print "Testing European option implied volatility.."
      when 'testBinomialEngines'
        print "Testing binomial European engines against analytic results.."
    end
    STDOUT.flush
  end
  def relativeError(x1,x2,reference)
    if reference != 0.0
      (x1-x2).abs/reference
    else
      10e+10
    end
  end
  def makeOption(type,underlying,strike,divCurve,rfCurve,
                 exDate,volCurve,engineType='analytic')
    case engineType
      when 'analytic'
        engine = EuropeanAnalyticEngine.new
      when 'jr'
        engine = EuropeanBinomialEngine.new('jr',800)
      when 'crr'
        engine = EuropeanBinomialEngine.new('crr',800)
    end
    VanillaOption.new(type,MarketElementHandle.new(underlying),strike,
                      divCurve,rfCurve, EuropeanExercise.new(exDate),
                      volCurve,engine)
  end
  def makeFlatCurve(forward)
    today = Date.todaysDate
    settlement = Calendar.new("TARGET").advance(today,2,'days')
    TermStructureHandle.new(
        FlatForward.new(today, settlement, MarketElementHandle.new(forward),
                        DayCounter.new('act/365')))
  end
  def makeFlatVolatility(volatility)
    today = Date.todaysDate
    settlement = Calendar.new("TARGET").advance(today,2,'days')
    BlackVolTermStructureHandle.new(
        BlackConstantVol.new(settlement, MarketElementHandle.new(volatility),
                             DayCounter.new('act/365')))
  end
  def testGreeks
    calendar = Calendar.new('TARGET')
    # errors allowed for Greeks
    error = {
      'delta' => 1.0e-4,
      'gamma' => 1.0e-4,
      'theta' => 1.0e-2,
      'rho'   => 1.0e-4,
      'dividendRho' => 1.0e-4,
      'vega'  => 1.0e-4
    }
    results = {}

    test_options = []
    ['Call','Put','Straddle'].each { |type|
    [50, 99.5, 100, 100.5, 150].each { |strike|
    [calendar.roll(Date.todaysDate.plusYears(1))].each { |exDate|
        test_options.push [type,strike,exDate]
    }}}
    
    test_data = []
    [100].each { |under|
    [0.04, 0.05, 0.06].each { |qRate|
    [0.01, 0.05, 0.15].each { |rRate|
    [0.11, 0.5, 1.2].each { |vol|
        test_data.push [under,qRate,rRate,vol]
    }}}}

    underlying = SimpleMarketElement.new(0.0)
    volatility = SimpleMarketElement.new(0.0)
    volCurve = makeFlatVolatility(volatility)
    qRate = SimpleMarketElement.new(0.0)
    divCurve = makeFlatCurve(qRate)
    rRate = SimpleMarketElement.new(0.0)
    rfCurve = makeFlatCurve(rRate)
        
    test_options.each do |type,strike,exDate|

      opt = makeOption(type,underlying,strike,
                       divCurve,rfCurve,exDate,volCurve)

      # time-shifted exercise dates
      exDateP = calendar.advance(exDate,1,'day')
      exDateM = calendar.advance(exDate,-1,'day')
      dT = exDateP.serialNumber-exDateM.serialNumber
      opt_p = makeOption(type, underlying , strike,
                         divCurve,  rfCurve,
                         exDateP ,  volCurve)
      opt_m = makeOption(type, underlying , strike,
                         divCurve,  rfCurve,
                         exDateM ,  volCurve)
            
      test_data.each do |u,q,r,v|
        
        underlying.value = u
        volatility.value = v
        qRate.value = q
        rRate.value = r
                
        value  = opt.NPV

        if value>0.00001*u
          # perturb underlying and get delta and gamma
          du = u/10000.0
          underlying.value = u+du
          value_p = opt.NPV
          delta_p = opt.delta
          underlying.value = u-du
          value_m = opt.NPV
          delta_m = opt.delta
          underlying.value = u
          results['delta'] = (value_p-value_m)/(2*du)
          results['gamma']= (delta_p-delta_m)/(2*du)

          # perturb rates and get rho and dividend rho
          dr = r/10000.0
          rRate.value = r+dr
          value_p = opt.NPV
          rRate.value = r-dr
          value_m = opt.NPV
          rRate.value = r
          results['rho'] = (value_p-value_m)/(2*dr)
                
          dq = q/10000.0
          qRate.value = q+dq
          value_p = opt.NPV
          qRate.value = q-dq
          value_m = opt.NPV
          qRate.value = q
          results['dividendRho'] = (value_p-value_m)/(2*dq)

          # perturb volatility and get vega
          dv = v/10000.0
          volatility.value = v+dv
          value_p = opt.NPV
          volatility.value = v-dv
          value_m = opt.NPV
          volatility.value = v
          results['vega'] = (value_p-value_m)/(2*dv)

          # get theta from time-shifted options
          results['theta'] =-(opt_p.NPV-opt_m.NPV)/(dT/365.0)

          ['delta','gamma','theta','rho','dividendRho','vega'].each do |greek|
            unless relativeError(opt.send(greek),results[greek],u) <= \
                   error[greek]

              flunk(<<-MESSAGE

    Option details: #{type} #{u} #{strike} #{q} #{r} #{exDate} #{v}
        value  = #{value}
        #{greek} = #{opt.send(greek)}, #{greek}Num = #{results[greek]}

                    MESSAGE
                    )
            end
          end
        end
      end
    end
  end
  def testImpliedVol
    maxEvaluations = 100
    tolerance = 1.0e-6

    test_options = []
    ['Call','Put','Straddle'].each { |type|
    [50, 99.5, 100, 100.5, 150].each { |strike|
    [36,180,360,1080].each { |days|
        exDate = Date.todaysDate() + days
        test_options.push [type,strike,exDate]
    }}}
    
    test_data = []
    [80, 95, 99.9, 100, 100.1, 105, 120].each { |under|
    [0.01, 0.05, 0.10].each { |qRate|
    [0.01, 0.05, 0.10].each { |rRate|
    [0.01, 0.2, 0.3, 0.7, 0.9].each { |vol|
        test_data.push [under,qRate,rRate,vol]
    }}}}

    underlying = SimpleMarketElement.new(0.0)
    volatility = SimpleMarketElement.new(0.0)
    volCurve = makeFlatVolatility(volatility)
    qRate = SimpleMarketElement.new(0.0)
    divCurve = makeFlatCurve(qRate)
    rRate = SimpleMarketElement.new(0.0)
    rfCurve = makeFlatCurve(rRate)
        
    test_options.each do |type,strike,exDate|

      opt = makeOption(type,underlying,strike,
                       divCurve,rfCurve,exDate,volCurve)
      
      test_data.each do |u,q,r,v|
        
        underlying.value = u
        volatility.value = v
        qRate.value = q
        rRate.value = r
                
        value  = opt.NPV

        if value != 0
          # shift guess somehow
          volatility.value = v*1.5
          begin
            implVol = opt.impliedVolatility(value,tolerance,maxEvaluations)
          rescue Exception => e
            flunk(<<-MESSAGE
                        
    Option details: #{type} #{u} #{strike} #{q} #{r} #{exDate} #{v}
        #{e}
        trying to calculate implied vol from value #{value}
          
                  MESSAGE
                  )
          end
          if (implVol-v).abs > tolerance
            # the difference might not matter
            volatility.value = implVol
            unless (opt.NPV-value).abs/u <= 1.0e-6
              flunk(<<-MESSAGE
                        
    Option details: #{type} #{u} #{strike} #{q} #{r} #{exDate} #{v}
        original volatility: #{v}
        price:               #{value}
        implied volatility:  #{implVol}
        corresponding price: #{opt.NPV}

                    MESSAGE
                    )
            end
          end
        end
      end
    end
  end
  def testBinomialEngines
    calendar = Calendar.new('TARGET')

    tolerance = 0.1

    test_options = []
    ['Call','Put','Straddle'].each { |type|
    [50, 100, 150].each { |strike|
    [calendar.roll(Date.todaysDate.plusYears(1))].each { |exDate|
        test_options.push [type,strike,exDate]
    }}}
    
    test_data = []
    [100].each { |under|
    [0.00, 0.05].each { |qRate|
    [0.01, 0.05, 0.15].each { |rRate|
    [0.11, 0.5, 1.2].each { |vol|
        test_data.push [under,qRate,rRate,vol]
    }}}}

    underlying = SimpleMarketElement.new(0.0)
    volatility = SimpleMarketElement.new(0.0)
    volCurve = makeFlatVolatility(volatility)
    qRate = SimpleMarketElement.new(0.0)
    divCurve = makeFlatCurve(qRate)
    rRate = SimpleMarketElement.new(0.0)
    rfCurve = makeFlatCurve(rRate)
        
    test_options.each do |type,strike,exDate|

      opt1 = makeOption(type,underlying,strike,
                        divCurve,rfCurve,exDate,volCurve)
      opt2 = makeOption(type,underlying,strike,
                        divCurve,rfCurve,exDate,volCurve,'jr')
      opt3 = makeOption(type,underlying,strike,
                        divCurve,rfCurve,exDate,volCurve,'crr')

      test_data.each do |u,q,r,v|
        
        underlying.value = u
        volatility.value = v
        qRate.value = q
        rRate.value = r

        unless (opt1.NPV-opt2.NPV).abs/u <= tolerance
          flunk(<<-MESSAGE
                      
    Option details: #{type} #{u} #{strike} #{q} #{r} #{exDate} #{v}
        analytic value: #{opt1.NPV}
        binomial (JR):  #{opt2.NPV}

                MESSAGE
                )
        end
        unless (opt1.NPV-opt3.NPV).abs/u <= tolerance
          flunk(<<-MESSAGE
                      
    Option details: #{type} #{u} #{strike} #{q} #{r} #{exDate} #{v}
        analytic value: #{opt1.NPV}
        binomial (CRR): #{opt3.NPV}

                MESSAGE
                )
        end
      end
    end
  end
end


if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(EuropeanOptionTest)
end

