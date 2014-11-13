"""
 Copyright (C) 2011 Lluis Pujol Bajador

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
"""

from QuantLib import *
import unittest

class AssetSwapTest(unittest.TestCase):
    def setUp(self):
        # initial setup
        self.termStructure = RelinkableYieldTermStructureHandle()
        self.swapSettlementDays = 2
        self.faceAmount = 100.0
        self.fixedConvention = Unadjusted
        self.compounding = Continuous
        self.fixedFrequency = Annual
        self.floatingFrequency = Semiannual
        self.iborIndex = Euribor(Period(self.floatingFrequency), self.termStructure)
        self.calendar = self.iborIndex.fixingCalendar()
        self.swapIndex=  SwapIndex("EuriborSwapIsdaFixA", Period(10,Years), self.swapSettlementDays,
                          self.iborIndex.currency(), self.calendar,
                          Period(self.fixedFrequency), self.fixedConvention,
                          self.iborIndex.dayCounter(), self.iborIndex)
        self.spread = 0.0
        self.nonnullspread = 0.003
        self.today = Date(24,April,2007)
        Settings.instance().evaluationDate = self.today
        self.termStructure.linkTo(FlatForward(self.today, 0.05, Actual365Fixed()))
        self.yieldCurve = FlatForward(self.today, 0.05, Actual365Fixed())
        self.pricer = BlackIborCouponPricer()
        self.swaptionVolatilityStructure = SwaptionVolatilityStructureHandle(ConstantSwaptionVolatility(self.today, NullCalendar(),Following,
                                           0.2, Actual365Fixed()))
        self.meanReversionQuote = QuoteHandle(SimpleQuote(0.01))
        self.cmspricer = AnalyticHaganPricer(self.swaptionVolatilityStructure,
                                GFunctionFactory.Standard,
                                self.meanReversionQuote)
        
    def testConsistency(self) :
        """Testing consistency between fair price and fair spread..."""
        bondCalendar = TARGET()
        settlementDays = 3

    ## Fixed Underlying bond (Isin: DE0001135275 DBR 4 01/04/37)
    ## maturity doesn't occur on a business day

        bondSchedule = Schedule(Date(4,January,2005),
                          Date(4,January,2037),
                          Period(Annual), bondCalendar,
                          Unadjusted, Unadjusted,
                          DateGeneration.Backward, False)
        bond = FixedRateBond(settlementDays, self.faceAmount,
                      bondSchedule,[0.04],
                      ActualActual(ActualActual.ISDA),
                      Following,
                      100.0, Date(4,January,2005))

        payFixedRate = True
        bondPrice = 95.0
        isPar = True
        parAssetSwap = AssetSwap(payFixedRate,
                             bond, bondPrice,
                             self.iborIndex, self.spread,
                             Schedule(),
                             self.iborIndex.dayCounter(),
                             isPar)

        swapEngine =  DiscountingSwapEngine(self.termStructure,
                                  True,
                                  bond.settlementDate(),
                                  Settings.instance().evaluationDate)

        parAssetSwap.setPricingEngine(swapEngine)
        fairCleanPrice = parAssetSwap.fairCleanPrice()
        fairSpread = parAssetSwap.fairSpread()

        tolerance = 1.0e-13
   
        assetSwap2 = AssetSwap(payFixedRate, bond, fairCleanPrice,
                             self.iborIndex, self.spread,
                             Schedule(),
                             self.iborIndex.dayCounter(),
                             isPar)
        
        assetSwap2.setPricingEngine(swapEngine)
        self.assertFalse(abs(assetSwap2.NPV())>tolerance,
        "\npar asset swap fair clean price doesn't zero the NPV: "
        + "\n  clean price:      " + str(bondPrice)
        + "\n  fair clean price: " + str(fairCleanPrice)
        + "\n  NPV:              " + str(assetSwap2.NPV())
        + "\n  tolerance:        " + str(tolerance))
    
        self.assertFalse(abs(assetSwap2.fairCleanPrice() - fairCleanPrice)>tolerance,
        "\npar asset swap fair clean price doesn't equal input "
        + "clean price at zero NPV: "
        + "\n  input clean price: " + str(fairCleanPrice)
        + "\n  fair clean price:  " + str(assetSwap2.fairCleanPrice())
        + "\n  NPV:               " + str(assetSwap2.NPV())
        + "\n  tolerance:         " + str(tolerance))
    
        self.assertFalse(abs(assetSwap2.fairSpread() - self.spread)>tolerance,
        "\npar asset swap fair spread doesn't equal input spread "
        + "at zero NPV: " 
        + "\n  input spread: " + str(self.spread )
        + "\n  fair spread:  " + str(assetSwap2.fairSpread() )
        + "\n  NPV:          " + str(assetSwap2.NPV() )
        + "\n  tolerance:    " + str(tolerance))

        assetSwap3 = AssetSwap(payFixedRate,
                             bond, bondPrice,
                             self.iborIndex, fairSpread,
                             Schedule(),
                             self.iborIndex.dayCounter(),
                             isPar)
        assetSwap3.setPricingEngine(swapEngine)
        self.assertFalse(abs(assetSwap3.NPV())>tolerance,
        "\npar asset swap fair spread doesn't zero the NPV: "
        + "\n  spread:      " + str(self.spread)
        + "\n  fair spread: " + str(fairSpread)
        + "\n  NPV:         " + str(assetSwap3.NPV())
        + "\n  tolerance:   " + str(tolerance))
        
        self.assertFalse(abs(assetSwap3.fairCleanPrice() - bondPrice)>tolerance,
        "\npar asset swap fair clean price doesn't equal input "
        + "clean price at zero NPV: "
        + "\n  input clean price: " + str(bondPrice)
        + "\n  fair clean price:  " + str(assetSwap3.fairCleanPrice())
        + "\n  NPV:               " + str(assetSwap3.NPV())
        + "\n  tolerance:         " + str(tolerance))
    
        self.assertFalse(abs(assetSwap3.fairSpread() - fairSpread)>tolerance,
        "\npar asset swap fair spread doesn't equal input spread at"
        + " zero NPV: "
        + "\n  input spread: " + str(fairSpread)
        + "\n  fair spread:  " + str(assetSwap3.fairSpread())
        + "\n  NPV:          " + str(assetSwap3.NPV())
        + "\n  tolerance:    " + str(tolerance))
    

        ## let's change the npv date
        swapEngine = DiscountingSwapEngine(self.termStructure,
                              True,
                              bond.settlementDate(),
                              bond.settlementDate())

        parAssetSwap.setPricingEngine(swapEngine)
        ## fair clean price and fair spread should not change
        self.assertFalse(abs(parAssetSwap.fairCleanPrice() - fairCleanPrice)>tolerance,
            "\npar asset swap fair clean price changed with NpvDate:"
            + "\n expected clean price: " + str(fairCleanPrice)
            + "\n fair clean price:     " + str(parAssetSwap.fairCleanPrice())
            + "\n tolerance:            " + str(tolerance))
    
        self.assertFalse(abs(parAssetSwap.fairSpread() - fairSpread)>tolerance, 
            "\npar asset swap fair spread changed with NpvDate:"
            + "\n  expected spread: " + str(fairSpread)
            + "\n  fair spread:     " + str(parAssetSwap.fairSpread())
            + "\n  tolerance:       " + str(tolerance))
    
        assetSwap2 = AssetSwap(payFixedRate,    
                           bond, fairCleanPrice,
                           self.iborIndex, self.spread,
                           Schedule(),
                           self.iborIndex.dayCounter(),
                           isPar)
        assetSwap2.setPricingEngine(swapEngine)
        self.assertFalse(abs(assetSwap2.NPV())>tolerance, 
            "\npar asset swap fair clean price doesn't zero the NPV: "
            + "\n  clean price:      " + str(bondPrice)
            + "\n  fair clean price: " + str(fairCleanPrice)
            + "\n  NPV:              " + str(assetSwap2.NPV())
            + "\n  tolerance:        " + str(tolerance))
    
        self.assertFalse(abs(assetSwap2.fairCleanPrice() - fairCleanPrice)>tolerance,
            "\npar asset swap fair clean price doesn't equal input "
            + "clean price at zero NPV: "
            + "\n  input clean price: " + str(fairCleanPrice)
            + "\n  fair clean price:  " + str(assetSwap2.fairCleanPrice())
            + "\n  NPV:               " + str(assetSwap2.NPV())
            + "\n  tolerance:         " + str(tolerance))
    
        self.assertFalse(abs(assetSwap2.fairSpread() - self.spread)>tolerance,
            "\npar asset swap fair spread doesn't equal input spread at zero NPV: "
            + "\n  input spread: " + str(self.spread)
            + "\n  fair spread:  " + str(assetSwap2.fairSpread())
            + "\n  NPV:          " + str(assetSwap2.NPV())
            + "\n  tolerance:    " + str(tolerance))

        assetSwap3 = AssetSwap(payFixedRate,
                           bond, bondPrice,
                           self.iborIndex, fairSpread,
                           Schedule(),
                           self.iborIndex.dayCounter(),
                           isPar)
        assetSwap3.setPricingEngine(swapEngine)
        self.assertFalse(abs(assetSwap3.NPV())>tolerance, 
            "\npar asset swap fair spread doesn't zero the NPV: "
            + "\n  spread:      " + str(self.spread)
            + "\n  fair spread: " + str(fairSpread)
            + "\n  NPV:         " + str(assetSwap3.NPV())
            + "\n  tolerance:   " + str(tolerance))
    
        self.assertFalse(abs(assetSwap3.fairCleanPrice() - bondPrice)>tolerance,
            "\npar asset swap fair clean price doesn't equal input "
            + "clean price at zero NPV: "
            + "\n  input clean price: " + str(bondPrice)
            + "\n  fair clean price:  " + str(assetSwap3.fairCleanPrice())
            + "\n  NPV:               " + str(assetSwap3.NPV())
            + "\n  tolerance:         " + str(tolerance))
    
        self.assertFalse(abs(assetSwap3.fairSpread() - fairSpread)>tolerance,
            "\npar asset swap fair spread doesn't equal input spread at zero NPV: "
            + "\n  input spread: " + str(fairSpread)
            + "\n  fair spread:  " + str(assetSwap3.fairSpread())
            + "\n  NPV:          " + str(assetSwap3.NPV())
            + "\n  tolerance:    " + str(tolerance))
    

        ## now market asset swap
        isPar = False
        mktAssetSwap = AssetSwap (payFixedRate,
                               bond, bondPrice,
                               self.iborIndex, self.spread,
                               Schedule(),
                               self.iborIndex.dayCounter(),
                               isPar)

        swapEngine = DiscountingSwapEngine(self.termStructure,
                                  True,
                                  bond.settlementDate(),
                                  Settings.instance().evaluationDate)

        mktAssetSwap.setPricingEngine(swapEngine)
        fairCleanPrice = mktAssetSwap.fairCleanPrice()
        fairSpread = mktAssetSwap.fairSpread()

        assetSwap4 = AssetSwap (payFixedRate,
                             bond, fairCleanPrice,
                             self.iborIndex, self.spread,
                             Schedule(),
                             self.iborIndex.dayCounter(),
                             isPar)
        assetSwap4.setPricingEngine(swapEngine)
        self.assertFalse(abs(assetSwap4.NPV())>tolerance, 
            "\nmarket asset swap fair clean price doesn't zero the NPV: "
            + "\n  clean price:      " + str(bondPrice)
            + "\n  fair clean price: " + str(fairCleanPrice)
            + "\n  NPV:              " + str(assetSwap4.NPV())
            + "\n  tolerance:        " + str(tolerance))
        
        self.assertFalse(abs(assetSwap4.fairCleanPrice() - fairCleanPrice)>tolerance,
            "\nmarket asset swap fair clean price doesn't equal input "
            + "clean price at zero NPV: "   
            + "\n  input clean price: " + str(fairCleanPrice)
            + "\n  fair clean price:  " + str(assetSwap4.fairCleanPrice())
            + "\n  NPV:               " + str(assetSwap4.NPV())
            + "\n  tolerance:         " + str(tolerance))
        
        self.assertFalse(abs(assetSwap4.fairSpread() - self.spread)>tolerance,
            "\nmarket asset swap fair spread doesn't equal input spread"
            + " at zero NPV: "
            + "\n  input spread: " + str(self.spread)
            + "\n  fair spread:  " + str(assetSwap4.fairSpread())
            + "\n  NPV:          " + str(assetSwap4.NPV())
            + "\n  tolerance:    " + str(tolerance))
        

        assetSwap5 = AssetSwap(payFixedRate,
                             bond, bondPrice,
                             self.iborIndex, fairSpread,
                             Schedule(),
                             self.iborIndex.dayCounter(),
                             isPar)
        assetSwap5.setPricingEngine(swapEngine)
        self.assertFalse(abs(assetSwap5.NPV())>tolerance,
            "\nmarket asset swap fair spread doesn't zero the NPV: "
            + "\n  spread:      " + str(self.spread)
            + "\n  fair spread: " + str(fairSpread)
            + "\n  NPV:         " + str(assetSwap5.NPV())
            + "\n  tolerance:   " + str(tolerance))
        
        self.assertFalse(abs(assetSwap5.fairCleanPrice() - bondPrice)>tolerance, 
            "\nmarket asset swap fair clean price doesn't equal input "
            + "clean price at zero NPV: "
            + "\n  input clean price: " + str(bondPrice)
            + "\n  fair clean price:  " + str(assetSwap5.fairCleanPrice())
            + "\n  NPV:               " + str(assetSwap5.NPV())
            + "\n  tolerance:         " + str(tolerance))
        
        self.assertFalse(abs(assetSwap5.fairSpread() - fairSpread)>tolerance,
            "\nmarket asset swap fair spread doesn't equal input spread at zero NPV: "
            + "\n  input spread: " + str(fairSpread)
            + "\n  fair spread:  " + str(assetSwap5.fairSpread())
            + "\n  NPV:          " + str(assetSwap5.NPV())
            + "\n  tolerance:    " + str(tolerance))
        

        ## let's change the npv date
        swapEngine = DiscountingSwapEngine(self.termStructure,
                                  True,
                                  bond.settlementDate(),
                                  bond.settlementDate())

        mktAssetSwap.setPricingEngine(swapEngine)
        ## fair clean price and fair spread should not change
        self.assertFalse(abs(mktAssetSwap.fairCleanPrice() - fairCleanPrice)>tolerance, 
            "\nmarket asset swap fair clean price changed with NpvDate:"
            + "\n  expected clean price: " + str(fairCleanPrice)
            + "\n  fair clean price:  " + str(mktAssetSwap.fairCleanPrice())
            + "\n  tolerance:         " + str(tolerance))
        
        self.assertFalse(abs(mktAssetSwap.fairSpread() - fairSpread)>tolerance,
            "\nmarket asset swap fair spread changed with NpvDate:"
            + "\n  expected spread: " + str(fairSpread)
            + "\n  fair spread:  " + str(mktAssetSwap.fairSpread())
            + "\n  tolerance:    " + str(tolerance))
        

        assetSwap4 = AssetSwap(payFixedRate,
                               bond, fairCleanPrice,
                               self.iborIndex, self.spread,
                               Schedule(),
                               self.iborIndex.dayCounter(),
                               isPar)
        assetSwap4.setPricingEngine(swapEngine)
        self.assertFalse(abs(assetSwap4.NPV())>tolerance,
            "\nmarket asset swap fair clean price doesn't zero the NPV: "
            + "\n  clean price:      " + str(bondPrice)
            + "\n  fair clean price: " + str(fairCleanPrice)
            + "\n  NPV:              " + str(assetSwap4.NPV())
            + "\n  tolerance:        " + str(tolerance))
        
        self.assertFalse(abs(assetSwap4.fairCleanPrice() - fairCleanPrice)>tolerance,
            "\nmarket asset swap fair clean price doesn't equal input "
            + "clean price at zero NPV: "
            + "\n  input clean price: " + str(fairCleanPrice)
            + "\n  fair clean price:  " + str(assetSwap4.fairCleanPrice())
            + "\n  NPV:               " + str(assetSwap4.NPV())
            + "\n  tolerance:         " + str(tolerance))
        
        self.assertFalse(abs(assetSwap4.fairSpread() - self.spread)>tolerance,
            "\nmarket asset swap fair spread doesn't equal input spread at zero NPV: "
            + "\n  input spread: " + str(self.spread)
            + "\n  fair spread:  " + str(assetSwap4.fairSpread())
            + "\n  NPV:          " + str(assetSwap4.NPV())
            + "\n  tolerance:    " + str(tolerance))
        
        assetSwap5 = AssetSwap(payFixedRate,
                               bond, bondPrice,
                               self.iborIndex, fairSpread,
                               Schedule(),
                               self.iborIndex.dayCounter(),
                               isPar)
        assetSwap5.setPricingEngine(swapEngine)
        self.assertFalse(abs(assetSwap5.NPV())>tolerance,
            "\nmarket asset swap fair spread doesn't zero the NPV: "    
            + "\n  spread:      " + str(self.spread)
            + "\n  fair spread: " + str(fairSpread)
            + "\n  NPV:         " + str(assetSwap5.NPV())
            + "\n  tolerance:   " + str(tolerance))
        
        self.assertFalse(abs(assetSwap5.fairCleanPrice() - bondPrice)>tolerance,
            "\nmarket asset swap fair clean price doesn't equal input "
            + "clean price at zero NPV: "
            + "\n  input clean price: " + str(bondPrice)
            + "\n  fair clean price:  " + str(assetSwap5.fairCleanPrice())
            + "\n  NPV:               " + str(assetSwap5.NPV())
            + "\n  tolerance:         " + str(tolerance))
        
        self.assertFalse(abs(assetSwap5.fairSpread() - fairSpread)>tolerance,
            "\nmarket asset swap fair spread doesn't equal input spread at zero NPV: "
            + "\n  input spread: " + str(fairSpread)
            + "\n  fair spread:  " + str(assetSwap5.fairSpread())
            + "\n  NPV:          " + str(assetSwap5.NPV())
            + "\n  tolerance:    " + str(tolerance))
        



    def testImpliedValue(self):
        """Testing implied bond value against asset-swap fair price with null spread..."""
        bondCalendar = TARGET()
        settlementDays = 3
        fixingDays = 2
        payFixedRate = True
        parAssetSwap = True
        inArrears = False

        ## Fixed Underlying bond (Isin: DE0001135275 DBR 4 01/04/37)
        ## maturity doesn't occur on a business day

        fixedBondSchedule1 = Schedule(Date(4,January,2005),
                                    Date(4,January,2037),
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBond1 = FixedRateBond(settlementDays, self.faceAmount,
                                               fixedBondSchedule1,
                                               [0.04],
                                               ActualActual(ActualActual.ISDA),
                                               Following,
                                               100.0, Date(4,January,2005))

        bondEngine = DiscountingBondEngine(self.termStructure)
        swapEngine = DiscountingSwapEngine(self.termStructure, False)
        fixedBond1.setPricingEngine(bondEngine)

        fixedBondPrice1 = fixedBond1.cleanPrice()
        fixedBondAssetSwap1 = AssetSwap(payFixedRate,
                                      fixedBond1, fixedBondPrice1,
                                      self.iborIndex, self.spread,
                                      Schedule(),
                                      self.iborIndex.dayCounter(),
                                      parAssetSwap)
        fixedBondAssetSwap1.setPricingEngine(swapEngine)
        fixedBondAssetSwapPrice1 = fixedBondAssetSwap1.fairCleanPrice()
        tolerance = 1.0e-13
        
        error1 = abs(fixedBondAssetSwapPrice1-fixedBondPrice1)

        self.assertFalse(error1>tolerance, 
            "wrong zero spread asset swap price for fixed bond:"
            + "\n  bond's clean price:    " + str(fixedBondPrice1)
            + "\n  asset swap fair price: " + str(fixedBondAssetSwapPrice1)
            + "\n  error:                 " + str(error1)
            + "\n  tolerance:             " + str(tolerance))
        
        ## Fixed Underlying bond (Isin: IT0006527060 IBRD 5 02/05/19)
        ## maturity occurs on a business day

        fixedBondSchedule2 = Schedule(Date(5,February,2005),
                                    Date(5,February,2019),
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBond2 = FixedRateBond(settlementDays, self.faceAmount,
                                               fixedBondSchedule2,
                                               [0.05],
                                               Thirty360(Thirty360.BondBasis),
                                               Following,
                                               100.0, Date(5,February,2005))

        fixedBond2.setPricingEngine(bondEngine)

        fixedBondPrice2 = fixedBond2.cleanPrice()
        fixedBondAssetSwap2 = AssetSwap(payFixedRate,
                                      fixedBond2, fixedBondPrice2,
                                      self.iborIndex, self.spread,
                                      Schedule(),
                                      self.iborIndex.dayCounter(),
                                      parAssetSwap)
        fixedBondAssetSwap2.setPricingEngine(swapEngine)
        fixedBondAssetSwapPrice2 = fixedBondAssetSwap2.fairCleanPrice()
        error2 = abs(fixedBondAssetSwapPrice2-fixedBondPrice2)

        self.assertFalse(error2>tolerance,  
            "wrong zero spread asset swap price for fixed bond:"
            + "\n  bond's clean price:    " + str(fixedBondPrice2)
            + "\n  asset swap fair price: " + str(fixedBondAssetSwapPrice2)
            + "\n  error:                 " + str(error2)
            + "\n  tolerance:             " + str(tolerance))
        

        ## FRN Underlying bond (Isin: IT0003543847 ISPIM 0 09/29/13)
        ## maturity doesn't occur on a business day

        floatingBondSchedule1 = Schedule(Date(29,September,2003),
                                       Date(29,September,2013),
                                       Period(Semiannual), bondCalendar,
                                       Unadjusted, Unadjusted,
                                       DateGeneration.Backward, False)

        floatingBond1 =FloatingRateBond(settlementDays, self.faceAmount,
                                               floatingBondSchedule1,
                                               self.iborIndex, Actual360(),
                                               Following, fixingDays,
                                               [1],
                                               [0.0056],
                                               [],
                                               [],
                                               inArrears,
                                               100.0, Date(29,September,2003))

        floatingBond1.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond1.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(27,March,2007), 0.0402)
        floatingBondPrice1 = floatingBond1.cleanPrice()
        floatingBondAssetSwap1 = AssetSwap(payFixedRate,
                                         floatingBond1, floatingBondPrice1,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        floatingBondAssetSwap1.setPricingEngine(swapEngine)
        floatingBondAssetSwapPrice1 = floatingBondAssetSwap1.fairCleanPrice()
        error3 = abs(floatingBondAssetSwapPrice1-floatingBondPrice1)

        self.assertFalse(error3>tolerance, 
            "wrong zero spread asset swap price for floater:"
            + "\n  bond's clean price:    " + str(floatingBondPrice1)
            + "\n  asset swap fair price: " + str(floatingBondAssetSwapPrice1)
            + "\n  error:                 " + str(error3)
            + "\n  tolerance:             " + str(tolerance))
        

        ## FRN Underlying bond (Isin: XS0090566539 COE 0 09/24/18)
        ## maturity occurs on a business day

        floatingBondSchedule2 = Schedule(Date(24,September,2004),
                                       Date(24,September,2018),
                                       Period(Semiannual), bondCalendar,
                                       ModifiedFollowing, ModifiedFollowing,
                                       DateGeneration.Backward, False)
        floatingBond2 = FloatingRateBond(settlementDays, self.faceAmount,
                                               floatingBondSchedule2,
                                               self.iborIndex, Actual360(),
                                               ModifiedFollowing, fixingDays,
                                               [1],
                                               [0.0025],
                                               [],
                                               [],
                                               inArrears,
                                               100.0, Date(24,September,2004))

        floatingBond2.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond2.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(22,March,2007), 0.04013)
        currentCoupon=0.04013+0.0025
        floatingCurrentCoupon= floatingBond2.nextCouponRate()
        error4= abs(floatingCurrentCoupon-currentCoupon)
        self.assertFalse(error4>tolerance, 
            "wrong current coupon is returned for floater bond:"
            + "\n  bond's calculated current coupon:      " + str(currentCoupon)
            + "\n  current coupon asked to the bond: " + str(floatingCurrentCoupon)
            + "\n  error:                 " + str(error4)
            + "\n  tolerance:             " + str(tolerance))
        

        floatingBondPrice2 = floatingBond2.cleanPrice()
        floatingBondAssetSwap2 = AssetSwap(payFixedRate,
                                         floatingBond2, floatingBondPrice2,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        floatingBondAssetSwap2.setPricingEngine(swapEngine)
        floatingBondAssetSwapPrice2 = floatingBondAssetSwap2.fairCleanPrice()
        error5 = abs(floatingBondAssetSwapPrice2-floatingBondPrice2)

        self.assertFalse(error5>tolerance, 
            "wrong zero spread asset swap price for floater:"
            + "\n  bond's clean price:    " + str(floatingBondPrice2)
            + "\n  asset swap fair price: " + str(floatingBondAssetSwapPrice2)
            + "\n  error:                 " + str(error5)
            + "\n  tolerance:             " + str(tolerance))
        

        ## CMS Underlying bond (Isin: XS0228052402 CRDIT 0 8/22/20)
        ## maturity doesn't occur on a business day

        cmsBondSchedule1 = Schedule(Date(22,August,2005),
                                  Date(22,August,2020),
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBond1 = CmsRateBond(settlementDays, self.faceAmount,
                                              cmsBondSchedule1,
                                              self.swapIndex, Thirty360(),
                                              Following, fixingDays,
                                              [1.0],
                                              [0.0],
                                              [0.055],
                                              [0.025],
                                              inArrears,
                                              100.0, Date(22,August,2005))

        cmsBond1.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond1.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(18,August,2006), 0.04158)
        cmsBondPrice1 = cmsBond1.cleanPrice()
        cmsBondAssetSwap1 = AssetSwap(payFixedRate,
                                    cmsBond1, cmsBondPrice1,
                                    self.iborIndex, self.spread,
                                    Schedule(),
                                    self.iborIndex.dayCounter(),
                                    parAssetSwap)
        cmsBondAssetSwap1.setPricingEngine(swapEngine)
        cmsBondAssetSwapPrice1 = cmsBondAssetSwap1.fairCleanPrice()
        error6 = abs(cmsBondAssetSwapPrice1-cmsBondPrice1)

        self.assertFalse(error6>tolerance, 
            "wrong zero spread asset swap price for cms bond:"
            + "\n  bond's clean price:    " + str(cmsBondPrice1)
            + "\n  asset swap fair price: " + str(cmsBondAssetSwapPrice1)
            + "\n  error:                 " + str(error6)
            + "\n  tolerance:             " + str(tolerance))
        

         ## CMS Underlying bond (Isin: XS0218766664 ISPIM 0 5/6/15)
         ## maturity occurs on a business day

        cmsBondSchedule2 =  Schedule(Date(6,May,2005),
                                  Date(6,May,2015),
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBond2 = CmsRateBond(settlementDays, self.faceAmount, cmsBondSchedule2,
                        self.swapIndex, Thirty360(),
                        Following, fixingDays,
                        [0.84], [0.0],
                        [], [],
                        inArrears,
                        100.0, Date(6,May,2005))

        cmsBond2.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond2.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(4,May,2006), 0.04217)
        cmsBondPrice2 = cmsBond2.cleanPrice()
        cmsBondAssetSwap2 = AssetSwap(payFixedRate,
                                    cmsBond2, cmsBondPrice2,
                                    self.iborIndex, self.spread,
                                    Schedule(),
                                    self.iborIndex.dayCounter(),
                                    parAssetSwap)
        cmsBondAssetSwap2.setPricingEngine(swapEngine)
        cmsBondAssetSwapPrice2 = cmsBondAssetSwap2.fairCleanPrice()
        error7 = abs(cmsBondAssetSwapPrice2-cmsBondPrice2)

        self.assertFalse(error7>tolerance, 
            "wrong zero spread asset swap price for cms bond:"
            + "\n  bond's clean price:    " + str(cmsBondPrice2)
            + "\n  asset swap fair price: " + str(cmsBondAssetSwapPrice2)
            + "\n  error:                 " + str(error7)
            + "\n  tolerance:             " + str(tolerance))
        

        ## Zero Coupon bond (Isin: DE0004771662 IBRD 0 12/20/15)
        ## maturity doesn't occur on a business day

        zeroCpnBond1 = ZeroCouponBond(settlementDays, bondCalendar, self.faceAmount,
                           Date(20,December,2015),
                           Following,
                           100.0, Date(19,December,1985))

        zeroCpnBond1.setPricingEngine(bondEngine)

        zeroCpnBondPrice1 = zeroCpnBond1.cleanPrice()
        zeroCpnAssetSwap1 = AssetSwap(payFixedRate,
                                    zeroCpnBond1, zeroCpnBondPrice1,
                                    self.iborIndex, self.spread,
                                    Schedule(),
                                    self.iborIndex.dayCounter(),
                                    parAssetSwap)
        zeroCpnAssetSwap1.setPricingEngine(swapEngine)
        zeroCpnBondAssetSwapPrice1 = zeroCpnAssetSwap1.fairCleanPrice()
        error8 = abs(cmsBondAssetSwapPrice1-cmsBondPrice1)

        self.assertFalse(error8>tolerance, 
            "wrong zero spread asset swap price for zero cpn bond:"
            + "\n  bond's clean price:    " + str(zeroCpnBondPrice1)
            + "\n  asset swap fair price: " + str(zeroCpnBondAssetSwapPrice1)
            + "\n  error:                 " + str(error8)
            + "\n  tolerance:             " + str(tolerance))
        

        ## Zero Coupon bond (Isin: IT0001200390 ISPIM 0 02/17/28)
        ## maturity occurs on a business day

        zeroCpnBond2 = ZeroCouponBond(settlementDays, bondCalendar, self.faceAmount,
                           Date(17,February,2028),
                           Following,
                           100.0, Date(17,February,1998))

        zeroCpnBond2.setPricingEngine(bondEngine)

        zeroCpnBondPrice2 = zeroCpnBond2.cleanPrice()
        zeroCpnAssetSwap2 = AssetSwap(payFixedRate,
                                    zeroCpnBond2, zeroCpnBondPrice2,
                                    self.iborIndex, self.spread,
                                    Schedule(),
                                    self.iborIndex.dayCounter(),
                                    parAssetSwap)
        zeroCpnAssetSwap2.setPricingEngine(swapEngine)
        zeroCpnBondAssetSwapPrice2 = zeroCpnAssetSwap2.fairCleanPrice()
        error9 = abs(cmsBondAssetSwapPrice2-cmsBondPrice2)

        self.assertFalse(error9>tolerance, 
            "wrong zero spread asset swap price for zero cpn bond:"
            + "\n  bond's clean price:      " + str(zeroCpnBondPrice2)
            + "\n  asset swap fair price:   " + str(zeroCpnBondAssetSwapPrice2)
            + "\n  error:                   " + str(error9)
            + "\n  tolerance:               " + str(tolerance))
        


    def testMarketASWSpread(self) :
        """Testing relationship between market asset swap and par asset swap..."""
        bondCalendar = TARGET()
        settlementDays = 3
        fixingDays = 2
        payFixedRate = True
        parAssetSwap = True
        mktAssetSwap = False
        inArrears = False

        ## Fixed Underlying bond (Isin: DE0001135275 DBR 4 01/04/37)
        ## maturity doesn't occur on a business day

        fixedBondSchedule1 = Schedule (Date(4,January,2005),
                                    Date(4,January,2037),
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBond1 = FixedRateBond(settlementDays, self.faceAmount, fixedBondSchedule1,
                          [0.04],
                          ActualActual(ActualActual.ISDA), Following,
                          100.0, Date(4,January,2005))

        bondEngine = DiscountingBondEngine(self.termStructure)
        swapEngine = DiscountingSwapEngine(self.termStructure,False)
        fixedBond1.setPricingEngine(bondEngine)

        fixedBondMktPrice1 = 89.22  ## market price observed on 7th June 2007
        fixedBondMktFullPrice1=fixedBondMktPrice1+fixedBond1.accruedAmount()
        fixedBondParAssetSwap1 = AssetSwap(payFixedRate,
                                         fixedBond1, fixedBondMktPrice1,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        fixedBondParAssetSwap1.setPricingEngine(swapEngine)
        fixedBondParAssetSwapSpread1 = fixedBondParAssetSwap1.fairSpread()
        fixedBondMktAssetSwap1 = AssetSwap(payFixedRate,
                                         fixedBond1, fixedBondMktPrice1,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         mktAssetSwap)
        fixedBondMktAssetSwap1.setPricingEngine(swapEngine)
        fixedBondMktAssetSwapSpread1 = fixedBondMktAssetSwap1.fairSpread()

        tolerance = 1.0e-13
        error1 = abs(fixedBondMktAssetSwapSpread1-
                      100*fixedBondParAssetSwapSpread1/fixedBondMktFullPrice1)

        self.assertFalse (error1>tolerance, 
            "wrong asset swap spreads for fixed bond:"
            + "\n  market ASW spread: " + str(fixedBondMktAssetSwapSpread1)
            + "\n  par ASW spread:    " + str(fixedBondParAssetSwapSpread1)
            + "\n  error:             " + str(error1)
            + "\n  tolerance:         " + str(tolerance))
        

        ## Fixed Underlying bond (Isin: IT0006527060 IBRD 5 02/05/19)
        ## maturity occurs on a business day

        fixedBondSchedule2 = Schedule(Date(5,February,2005),
                                    Date(5,February,2019),
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBond2 =FixedRateBond(settlementDays, self.faceAmount, fixedBondSchedule2,
                          [0.05],
                          Thirty360(Thirty360.BondBasis), Following,
                          100.0, Date(5,February,2005))

        fixedBond2.setPricingEngine(bondEngine)

        fixedBondMktPrice2 = 99.98  ## market price observed on 7th June 2007
        fixedBondMktFullPrice2 = fixedBondMktPrice2+fixedBond2.accruedAmount()
        fixedBondParAssetSwap2 = AssetSwap (payFixedRate,
                                         fixedBond2, fixedBondMktPrice2,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        fixedBondParAssetSwap2.setPricingEngine(swapEngine)
        fixedBondParAssetSwapSpread2 = fixedBondParAssetSwap2.fairSpread()
        fixedBondMktAssetSwap2 = AssetSwap(payFixedRate,
                                         fixedBond2, fixedBondMktPrice2,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         mktAssetSwap)
        fixedBondMktAssetSwap2.setPricingEngine(swapEngine)
        fixedBondMktAssetSwapSpread2 = fixedBondMktAssetSwap2.fairSpread()
        error2 = abs(fixedBondMktAssetSwapSpread2-
                      100*fixedBondParAssetSwapSpread2/fixedBondMktFullPrice2)

        self.assertFalse(error2>tolerance,
            "wrong asset swap spreads for fixed bond:"
            + "\n  market ASW spread: " + str(fixedBondMktAssetSwapSpread2)
            + "\n  par ASW spread:    " + str(fixedBondParAssetSwapSpread2)
            + "\n  error:             " + str(error2)
            + "\n  tolerance:         " + str(tolerance))
        

        ## FRN Underlying bond (Isin: IT0003543847 ISPIM 0 09/29/13)
        ## maturity doesn't occur on a business day

        floatingBondSchedule1 = Schedule(Date(29,September,2003),
                                       Date(29,September,2013),
                                       Period(Semiannual), bondCalendar,
                                       Unadjusted, Unadjusted,
                                       DateGeneration.Backward, False)

        floatingBond1 = FloatingRateBond(settlementDays, self.faceAmount,
                             floatingBondSchedule1,
                             self.iborIndex, Actual360(),
                             Following, fixingDays,
                             [1], [0.0056],
                             [],[],
                             inArrears,
                             100.0, Date(29,September,2003))

        floatingBond1.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond1.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(27,March,2007), 0.0402)
        ## market price observed on 7th June 2007
        floatingBondMktPrice1 = 101.64 
        floatingBondMktFullPrice1 = floatingBondMktPrice1+floatingBond1.accruedAmount()
        floatingBondParAssetSwap1 = AssetSwap(payFixedRate,
                                            floatingBond1, floatingBondMktPrice1,
                                            self.iborIndex, self.spread,
                                            Schedule(),
                                            self.iborIndex.dayCounter(),
                                            parAssetSwap)
        floatingBondParAssetSwap1.setPricingEngine(swapEngine)
        floatingBondParAssetSwapSpread1 = floatingBondParAssetSwap1.fairSpread()
        floatingBondMktAssetSwap1 = AssetSwap(payFixedRate,
                                            floatingBond1, floatingBondMktPrice1,
                                            self.iborIndex, self.spread,
                                            Schedule(),
                                            self.iborIndex.dayCounter(),
                                            mktAssetSwap)
        floatingBondMktAssetSwap1.setPricingEngine(swapEngine)
        floatingBondMktAssetSwapSpread1 = floatingBondMktAssetSwap1.fairSpread()
        error3 =  abs(floatingBondMktAssetSwapSpread1-
                      100*floatingBondParAssetSwapSpread1/floatingBondMktFullPrice1)

        self.assertFalse(error3>tolerance, 
            "wrong asset swap spreads for floating bond:"
            + "\n  market ASW spread: " + str(floatingBondMktAssetSwapSpread1)
            + "\n  par ASW spread:    " + str(floatingBondParAssetSwapSpread1)
            + "\n  error:             " + str(error3)
            + "\n  tolerance:         " + str(tolerance))
        

        ## FRN Underlying bond (Isin: XS0090566539 COE 0 09/24/18)
        ## maturity occurs on a business day

        floatingBondSchedule2 = Schedule (Date(24,September,2004),
                                       Date(24,September,2018),
                                       Period(Semiannual), bondCalendar,
                                       ModifiedFollowing, ModifiedFollowing,
                                       DateGeneration.Backward, False)
        floatingBond2  = FloatingRateBond(settlementDays, self.faceAmount,
                             floatingBondSchedule2,
                             self.iborIndex, Actual360(),
                             ModifiedFollowing, fixingDays,
                             [1], [0.0025],
                             [], [],
                             inArrears,
                             100.0, Date(24,September,2004))

        floatingBond2.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond2.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(22,March,2007), 0.04013)
        ## market price observed on 7th June 2007
        floatingBondMktPrice2 = 101.248 
        floatingBondMktFullPrice2 = floatingBondMktPrice2+floatingBond2.accruedAmount()
        floatingBondParAssetSwap2 = AssetSwap (payFixedRate,
                                            floatingBond2, floatingBondMktPrice2,
                                            self.iborIndex, self.spread,
                                            Schedule(),
                                            self.iborIndex.dayCounter(),
                                            parAssetSwap)
        floatingBondParAssetSwap2.setPricingEngine(swapEngine)
        floatingBondParAssetSwapSpread2 = floatingBondParAssetSwap2.fairSpread()
        floatingBondMktAssetSwap2 = AssetSwap(payFixedRate,
                                            floatingBond2, floatingBondMktPrice2,
                                            self.iborIndex, self.spread,
                                            Schedule(),
                                            self.iborIndex.dayCounter(),
                                            mktAssetSwap)
        floatingBondMktAssetSwap2.setPricingEngine(swapEngine)
        floatingBondMktAssetSwapSpread2 = floatingBondMktAssetSwap2.fairSpread()
        error4 = abs(floatingBondMktAssetSwapSpread2-
                      100*floatingBondParAssetSwapSpread2/floatingBondMktFullPrice2)

        self.assertFalse(error4>tolerance , 
            "wrong asset swap spreads for floating bond:"
            + "\n  market ASW spread: " + str(floatingBondMktAssetSwapSpread2)
            + "\n  par ASW spread:    " + str(floatingBondParAssetSwapSpread2)
            + "\n  error:             " + str(error4)
            + "\n  tolerance:         " + str(tolerance))
        

        ## CMS Underlying bond (Isin: XS0228052402 CRDIT 0 8/22/20)
        ## maturity doesn't occur on a business day

        cmsBondSchedule1 = Schedule(Date(22,August,2005),
                                  Date(22,August,2020),
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBond1 = CmsRateBond(settlementDays, self.faceAmount, cmsBondSchedule1,
                        self.swapIndex, Thirty360(),
                        Following, fixingDays,
                        [1,1.0], [0.0],
                        [0.055], [0.025],
                        inArrears,
                        100.0, Date(22,August,2005))

        cmsBond1.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond1.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(18,August,2006), 0.04158)
        cmsBondMktPrice1 = 88.45  ## market price observed on 7th June 2007
        cmsBondMktFullPrice1 = cmsBondMktPrice1+cmsBond1.accruedAmount()
        cmsBondParAssetSwap1 = AssetSwap(payFixedRate,
                                       cmsBond1, cmsBondMktPrice1,
                                       self.iborIndex, self.spread,
                                       Schedule(),
                                       self.iborIndex.dayCounter(),
                                       parAssetSwap)
        cmsBondParAssetSwap1.setPricingEngine(swapEngine)
        cmsBondParAssetSwapSpread1 = cmsBondParAssetSwap1.fairSpread()
        cmsBondMktAssetSwap1 = AssetSwap(payFixedRate,
                                       cmsBond1, cmsBondMktPrice1,
                                       self.iborIndex, self.spread,
                                       Schedule(),
                                       self.iborIndex.dayCounter(),
                                       mktAssetSwap)
        cmsBondMktAssetSwap1.setPricingEngine(swapEngine)
        cmsBondMktAssetSwapSpread1 = cmsBondMktAssetSwap1.fairSpread()
        error5 = abs(cmsBondMktAssetSwapSpread1-
                      100*cmsBondParAssetSwapSpread1/cmsBondMktFullPrice1)

        self.assertFalse(error5>tolerance, 
            "wrong asset swap spreads for cms bond:"
            + "\n  market ASW spread: " + str(cmsBondMktAssetSwapSpread1)
            + "\n  par ASW spread:    " + str(cmsBondParAssetSwapSpread1)
            + "\n  error:             " + str(error5)
            + "\n  tolerance:         " + str(tolerance))
        

         ## CMS Underlying bond (Isin: XS0218766664 ISPIM 0 5/6/15)
         ## maturity occurs on a business day

        cmsBondSchedule2 = Schedule(Date(6,May,2005),
                                  Date(6,May,2015),
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBond2 = CmsRateBond(settlementDays, self.faceAmount, cmsBondSchedule2,
                        self.swapIndex, Thirty360(),
                        Following, fixingDays,
                        [0.84], [0.0],
                        [], [],
                        inArrears,
                        100.0, Date(6,May,2005))

        cmsBond2.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond2.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(4,May,2006), 0.04217)
        cmsBondMktPrice2 = 94.08  ## market price observed on 7th June 2007
        cmsBondMktFullPrice2 = cmsBondMktPrice2+cmsBond2.accruedAmount()
        cmsBondParAssetSwap2 = AssetSwap(payFixedRate,
                                       cmsBond2, cmsBondMktPrice2,
                                       self.iborIndex, self.spread,
                                       Schedule(),
                                       self.iborIndex.dayCounter(),
                                       parAssetSwap)
        cmsBondParAssetSwap2.setPricingEngine(swapEngine)
        cmsBondParAssetSwapSpread2 = cmsBondParAssetSwap2.fairSpread()
        cmsBondMktAssetSwap2 = AssetSwap(payFixedRate,
                                       cmsBond2, cmsBondMktPrice2,
                                       self.iborIndex, self.spread,
                                       Schedule(),
                                       self.iborIndex.dayCounter(),
                                       mktAssetSwap)
        cmsBondMktAssetSwap2.setPricingEngine(swapEngine)
        cmsBondMktAssetSwapSpread2 = cmsBondMktAssetSwap2.fairSpread()
        error6 = abs(cmsBondMktAssetSwapSpread2-
                      100*cmsBondParAssetSwapSpread2/cmsBondMktFullPrice2)

        self.assertFalse(error6>tolerance,  
            "wrong asset swap spreads for cms bond:"
            + "\n  market ASW spread: " + str(cmsBondMktAssetSwapSpread2)
            + "\n  par ASW spread:    " + str(cmsBondParAssetSwapSpread2)
            + "\n  error:             " + str(error6)
            + "\n  tolerance:         " + str(tolerance))
        

        ## Zero Coupon bond (Isin: DE0004771662 IBRD 0 12/20/15)
        ## maturity doesn't occur on a business day

        zeroCpnBond1 = ZeroCouponBond(settlementDays, bondCalendar, self.faceAmount,
                           Date(20,December,2015),
                           Following,
                           100.0, Date(19,December,1985))

        zeroCpnBond1.setPricingEngine(bondEngine)

        ## market price observed on 12th June 2007
        zeroCpnBondMktPrice1 = 70.436 
        zeroCpnBondMktFullPrice1 = zeroCpnBondMktPrice1+zeroCpnBond1.accruedAmount()
        zeroCpnBondParAssetSwap1 = AssetSwap(payFixedRate,zeroCpnBond1,
                                           zeroCpnBondMktPrice1,
                                           self.iborIndex, self.spread,
                                           Schedule(),
                                           self.iborIndex.dayCounter(),
                                           parAssetSwap)
        zeroCpnBondParAssetSwap1.setPricingEngine(swapEngine)
        zeroCpnBondParAssetSwapSpread1 = zeroCpnBondParAssetSwap1.fairSpread()
        zeroCpnBondMktAssetSwap1 = AssetSwap(payFixedRate,zeroCpnBond1,
                                           zeroCpnBondMktPrice1,
                                           self.iborIndex, self.spread,
                                           Schedule(),
                                           self.iborIndex.dayCounter(),
                                           mktAssetSwap)
        zeroCpnBondMktAssetSwap1.setPricingEngine(swapEngine)
        zeroCpnBondMktAssetSwapSpread1 = zeroCpnBondMktAssetSwap1.fairSpread()
        error7 = abs(zeroCpnBondMktAssetSwapSpread1-
                      100*zeroCpnBondParAssetSwapSpread1/zeroCpnBondMktFullPrice1)

        self.assertFalse(error7>tolerance, 
            "wrong asset swap spreads for zero cpn bond:"
            + "\n  market ASW spread: " + str(zeroCpnBondMktAssetSwapSpread1)
            + "\n  par ASW spread:    " + str(zeroCpnBondParAssetSwapSpread1)
            + "\n  error:             " + str(error7)
            + "\n  tolerance:         " + str(tolerance))
        

        ## Zero Coupon bond (Isin: IT0001200390 ISPIM 0 02/17/28)
        ## maturity occurs on a business day

        zeroCpnBond2 =ZeroCouponBond(settlementDays, bondCalendar, self.faceAmount,
                           Date(17,February,2028),
                           Following,
                           100.0, Date(17,February,1998))

        zeroCpnBond2.setPricingEngine(bondEngine)

        ## zeroCpnBondPrice2 = zeroCpnBond2.cleanPrice()

        ## market price observed on 12th June 2007
        zeroCpnBondMktPrice2 = 35.160 
        zeroCpnBondMktFullPrice2 = zeroCpnBondMktPrice2+zeroCpnBond2.accruedAmount()
        zeroCpnBondParAssetSwap2 = AssetSwap(payFixedRate,zeroCpnBond2,
                                           zeroCpnBondMktPrice2,
                                           self.iborIndex, self.spread,
                                           Schedule(),
                                           self.iborIndex.dayCounter(),
                                           parAssetSwap)
        zeroCpnBondParAssetSwap2.setPricingEngine(swapEngine)
        zeroCpnBondParAssetSwapSpread2 = zeroCpnBondParAssetSwap2.fairSpread()
        zeroCpnBondMktAssetSwap2 = AssetSwap(payFixedRate,zeroCpnBond2,
                                           zeroCpnBondMktPrice2,
                                           self.iborIndex, self.spread,
                                           Schedule(),
                                           self.iborIndex.dayCounter(),
                                           mktAssetSwap)
        zeroCpnBondMktAssetSwap2.setPricingEngine(swapEngine)
        zeroCpnBondMktAssetSwapSpread2 = zeroCpnBondMktAssetSwap2.fairSpread()
        error8 = abs(zeroCpnBondMktAssetSwapSpread2-
                      100*zeroCpnBondParAssetSwapSpread2/zeroCpnBondMktFullPrice2)

        self.assertFalse(error8>tolerance, 
            "wrong asset swap spreads for zero cpn bond:"
            + "\n  market ASW spread: " + str(zeroCpnBondMktAssetSwapSpread2)
            + "\n  par ASW spread:    " + str(zeroCpnBondParAssetSwapSpread2)
            + "\n  error:             " + str(error8)
            + "\n  tolerance:         " + str(tolerance))
        



    def testZSpread(self) :
        """Testing clean and dirty price with null Z-spread against theoretical prices..."""
        
        bondCalendar = TARGET()
        settlementDays = 3
        fixingDays = 2
        inArrears = False

        ## Fixed bond (Isin: DE0001135275 DBR 4 01/04/37)
        ## maturity doesn't occur on a business day

        fixedBondSchedule1 = Schedule(Date(4,January,2005),
                                    Date(4,January,2037),
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBond1 = FixedRateBond(settlementDays, self.faceAmount, fixedBondSchedule1,
                          [0.04],
                          ActualActual(ActualActual.ISDA), Following,
                          100.0, Date(4,January,2005))

        bondEngine = DiscountingBondEngine(self.termStructure)
        fixedBond1.setPricingEngine(bondEngine)

        fixedBondImpliedValue1 = fixedBond1.cleanPrice()
        fixedBondSettlementDate1= fixedBond1.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YC...
        fixedBondCleanPrice1 = cleanPriceFromZSpread(fixedBond1,self.yieldCurve, self.spread,
             Actual365Fixed(), self.compounding, Annual,
             fixedBondSettlementDate1)

        tolerance = 1.0e-13
        error1 = abs(fixedBondImpliedValue1-fixedBondCleanPrice1)
        self.assertFalse(error1>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(fixedBondImpliedValue1)
            + "\n  par asset swap spread: " + str(fixedBondCleanPrice1)
            + "\n  error:                 " + str(error1)
            + "\n  tolerance:             " + str(tolerance))
        
        ## Fixed bond (Isin: IT0006527060 IBRD 5 02/05/19)
        ## maturity occurs on a business day

        fixedBondSchedule2 = Schedule (Date(5,February,2005),
                                    Date(5,February,2019),
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBond2 = FixedRateBond(settlementDays, self.faceAmount, fixedBondSchedule2,
                          [0.05],
                          Thirty360(Thirty360.BondBasis), Following,
                          100.0, Date(5,February,2005))

        fixedBond2.setPricingEngine(bondEngine)

        fixedBondImpliedValue2 = fixedBond2.cleanPrice()
        fixedBondSettlementDate2= fixedBond2.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        fixedBondCleanPrice2 = cleanPriceFromZSpread(
             fixedBond2, self.yieldCurve, self.spread,
             Actual365Fixed(), self.compounding, Annual,
             fixedBondSettlementDate2)
        error3 = abs(fixedBondImpliedValue2-fixedBondCleanPrice2)
        self.assertFalse(error3>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(fixedBondImpliedValue2)
            + "\n  par asset swap spread: " + str(fixedBondCleanPrice2)
            + "\n  error:                 " + str(error3)
            + "\n  tolerance:             " + str(tolerance))
        

        ## FRN bond (Isin: IT0003543847 ISPIM 0 09/29/13)
        ## maturity doesn't occur on a business day

        floatingBondSchedule1 = Schedule(Date(29,September,2003),
                                       Date(29,September,2013),
                                       Period(Semiannual), bondCalendar,
                                       Unadjusted, Unadjusted,
                                       DateGeneration.Backward, False)

        floatingBond1 = FloatingRateBond(settlementDays, self.faceAmount,
                             floatingBondSchedule1,
                             self.iborIndex, Actual360(),
                             Following, fixingDays,
                             [1], [0.0056],
                             [], [],
                             inArrears,
                             100.0, Date(29,September,2003))

        floatingBond1.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond1.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(27,March,2007), 0.0402)
        floatingBondImpliedValue1 = floatingBond1.cleanPrice()
        floatingBondSettlementDate1= floatingBond1.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        floatingBondCleanPrice1 = cleanPriceFromZSpread(
            floatingBond1, self.yieldCurve, self.spread,
            Actual365Fixed(), self.compounding, Semiannual,
            fixedBondSettlementDate1)
        error5 = abs(floatingBondImpliedValue1-floatingBondCleanPrice1)
        self.assertFalse(error5>tolerance,  
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(floatingBondImpliedValue1)
            + "\n  par asset swap spread: " + str(floatingBondCleanPrice1)
            + "\n  error:                 " + str(error5)
            + "\n  tolerance:             " + str(tolerance))
        

        ## FRN bond (Isin: XS0090566539 COE 0 09/24/18)
        ## maturity occurs on a business day

        floatingBondSchedule2 = Schedule(Date(24,September,2004),
                                       Date(24,September,2018),
                                       Period(Semiannual), bondCalendar,
                                       ModifiedFollowing, ModifiedFollowing,
                                       DateGeneration.Backward, False)
        floatingBond2 = FloatingRateBond(settlementDays, self.faceAmount,
                             floatingBondSchedule2,
                             self.iborIndex, Actual360(),
                             ModifiedFollowing, fixingDays,
                             [1], [0.0025],
                             [], [],
                             inArrears,
                             100.0, Date(24,September,2004))

        floatingBond2.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond2.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(22,March,2007), 0.04013)
        floatingBondImpliedValue2 = floatingBond2.cleanPrice()
        floatingBondSettlementDate2= floatingBond2.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        floatingBondCleanPrice2 = cleanPriceFromZSpread(
            floatingBond2, self.yieldCurve,
            self.spread, Actual365Fixed(), self.compounding, Semiannual,
            fixedBondSettlementDate1)
        error7 = abs(floatingBondImpliedValue2-floatingBondCleanPrice2)
        self.assertFalse(error7>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(floatingBondImpliedValue2)
            + "\n  par asset swap spread: " + str(floatingBondCleanPrice2)
            + "\n  error:                 " + str(error7)
            + "\n  tolerance:             " + str(tolerance))
        

        #### CMS bond (Isin: XS0228052402 CRDIT 0 8/22/20)
        #### maturity doesn't occur on a business day

        cmsBondSchedule1 = Schedule(Date(22,August,2005),
                                  Date(22,August,2020),
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBond1 = CmsRateBond(settlementDays, self.faceAmount, cmsBondSchedule1,
                        self.swapIndex, Thirty360(),
                        Following, fixingDays,
                        [1.0], [0.0],
                        [0.055], [0.025],
                        inArrears,
                        100.0, Date(22,August,2005))

        cmsBond1.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond1.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(18,August,2006), 0.04158)
        cmsBondImpliedValue1 = cmsBond1.cleanPrice()
        cmsBondSettlementDate1= cmsBond1.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        cmsBondCleanPrice1 = cleanPriceFromZSpread(
            cmsBond1, self.yieldCurve, self.spread,
            Actual365Fixed(), self.compounding, Annual,
            cmsBondSettlementDate1)
        error9 = abs(cmsBondImpliedValue1-cmsBondCleanPrice1)
        self.assertFalse(error9>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(cmsBondImpliedValue1)
            + "\n  par asset swap spread: " + str(cmsBondCleanPrice1)
            + "\n  error:                 " + str(error9)
            + "\n  tolerance:             " + str(tolerance))
        

        ## CMS bond (Isin: XS0218766664 ISPIM 0 5/6/15)
        ## maturity occurs on a business day

        cmsBondSchedule2 = Schedule(Date(6,May,2005),
                                  Date(6,May,2015),
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBond2 = CmsRateBond(settlementDays, self.faceAmount, cmsBondSchedule2,
                        self.swapIndex, Thirty360(),
                        Following, fixingDays,
                        [0.84], [0.0],
                        [], [],
                        inArrears,
                        100.0, Date(6,May,2005))

        cmsBond2.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond2.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(4,May,2006), 0.04217)
        cmsBondImpliedValue2 = cmsBond2.cleanPrice()
        cmsBondSettlementDate2= cmsBond2.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        cmsBondCleanPrice2 = cleanPriceFromZSpread(
             cmsBond2, self.yieldCurve, self.spread,
             Actual365Fixed(), self.compounding, Annual,
             cmsBondSettlementDate2)
        error11 = abs(cmsBondImpliedValue2-cmsBondCleanPrice2)
        self.assertFalse(error11>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(cmsBondImpliedValue2)
            + "\n  par asset swap spread: " + str(cmsBondCleanPrice2)
            + "\n  error:                 " + str(error11)
            + "\n  tolerance:             " + str(tolerance))
        

        ## Zero-Coupon bond (Isin: DE0004771662 IBRD 0 12/20/15)
        ## maturity doesn't occur on a business day

        zeroCpnBond1 = ZeroCouponBond(settlementDays, bondCalendar, self.faceAmount,
                           Date(20,December,2015),
                           Following,
                           100.0, Date(19,December,1985))

        zeroCpnBond1.setPricingEngine(bondEngine)

        zeroCpnBondImpliedValue1 = zeroCpnBond1.cleanPrice()
        zeroCpnBondSettlementDate1= zeroCpnBond1.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        zeroCpnBondCleanPrice1 = cleanPriceFromZSpread(zeroCpnBond1,
                                  self.yieldCurve,
                                  self.spread,
                                  Actual365Fixed(),
                                  self.compounding, Annual,
                                  zeroCpnBondSettlementDate1)
        error13 = abs(zeroCpnBondImpliedValue1-zeroCpnBondCleanPrice1)
        self.assertFalse(error13>tolerance, 
            "wrong clean price for zero coupon bond:"
            + "\n  zero cpn implied value: " + str(zeroCpnBondImpliedValue1)
            + "\n  zero cpn price: " + str(zeroCpnBondCleanPrice1)
            + "\n  error:                 " + str(error13)
            + "\n  tolerance:             " + str(tolerance))
        

        ## Zero Coupon bond (Isin: IT0001200390 ISPIM 0 02/17/28)
        ## maturity doesn't occur on a business day

        zeroCpnBond2 = ZeroCouponBond(settlementDays, bondCalendar, self.faceAmount,
                           Date(17,February,2028),
                           Following,
                           100.0, Date(17,February,1998))

        zeroCpnBond2.setPricingEngine(bondEngine)

        zeroCpnBondImpliedValue2 = zeroCpnBond2.cleanPrice()
        zeroCpnBondSettlementDate2= zeroCpnBond2.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        zeroCpnBondCleanPrice2 = cleanPriceFromZSpread(zeroCpnBond2,
                                  self.yieldCurve,
                                  self.spread,
                                  Actual365Fixed(),
                                  self.compounding, Annual,
                                  zeroCpnBondSettlementDate2)
        error15 = abs(zeroCpnBondImpliedValue2-zeroCpnBondCleanPrice2)
        self.assertFalse(error15>tolerance,
            "wrong clean price for zero coupon bond:"
            + "\n  zero cpn implied value: " + str(zeroCpnBondImpliedValue2)
            + "\n  zero cpn price: " + str(zeroCpnBondCleanPrice2)
            + "\n  error:                 " + str(error15)
            + "\n  tolerance:             " + str(tolerance))
        


    def testGenericBondImplied(self):
        """Testing implied generic-bond value against asset-swap fair price with null spread..."""

        bondCalendar = TARGET()
        settlementDays = 3
        fixingDays = 2
        payFixedRate = True
        parAssetSwap = True
        inArrears = False

        ## Fixed Underlying bond (Isin: DE0001135275 DBR 4 01/04/37)
        ## maturity doesn't occur on a business day
        fixedBondStartDate1 = Date(4,January,2005)
        fixedBondMaturityDate1 = Date(4,January,2037)
        fixedBondSchedule1 = Schedule(fixedBondStartDate1,
                                    fixedBondMaturityDate1,
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBondLeg1 = list(FixedRateLeg(fixedBondSchedule1,
                        ActualActual(ActualActual.ISDA),
                        [self.faceAmount],
                        [0.04]))
        fixedbondRedemption1 = bondCalendar.adjust(fixedBondMaturityDate1,
                                                        Following)
        fixedBondLeg1.append(SimpleCashFlow(100.0, fixedbondRedemption1))
        fixedBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 fixedBondMaturityDate1, fixedBondStartDate1,
                 tuple(fixedBondLeg1))
        bondEngine = DiscountingBondEngine(self.termStructure)
        swapEngine = DiscountingSwapEngine(self.termStructure,True)
        fixedBond1.setPricingEngine(bondEngine)

        fixedBondPrice1 = fixedBond1.cleanPrice()
        fixedBondAssetSwap1 = AssetSwap(payFixedRate,
                                      fixedBond1, fixedBondPrice1,
                                      self.iborIndex, self.spread,
                                      Schedule(),
                                      self.iborIndex.dayCounter(),
                                      parAssetSwap)
        fixedBondAssetSwap1.setPricingEngine(swapEngine)
        fixedBondAssetSwapPrice1 = fixedBondAssetSwap1.fairCleanPrice()
        tolerance = 1.0e-13
        error1 = abs(fixedBondAssetSwapPrice1-fixedBondPrice1)

        self.assertFalse(error1>tolerance, 
            "wrong zero spread asset swap price for fixed bond:"
            + "\n  bond's clean price:    " + str(fixedBondPrice1)
            + "\n  asset swap fair price: " + str(fixedBondAssetSwapPrice1)
            + "\n  error:                 " + str(error1)
            + "\n  tolerance:             " + str(tolerance))


        ## Fixed Underlying bond (Isin: IT0006527060 IBRD 5 02/05/19)
        ## maturity occurs on a business day
        fixedBondStartDate2 = Date(5,February,2005)
        fixedBondMaturityDate2 = Date(5,February,2019)
        fixedBondSchedule2 = Schedule(fixedBondStartDate2,
                                    fixedBondMaturityDate2,
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBondLeg2 = list(FixedRateLeg(fixedBondSchedule2,Thirty360(Thirty360.BondBasis),
                                      [self.faceAmount],[0.05]))
        fixedbondRedemption2 = bondCalendar.adjust(fixedBondMaturityDate2,Following)
        fixedBondLeg2.append(SimpleCashFlow(100.0, fixedbondRedemption2))
        fixedBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 fixedBondMaturityDate2, fixedBondStartDate2, tuple(fixedBondLeg2))
        fixedBond2.setPricingEngine(bondEngine)

        fixedBondPrice2 = fixedBond2.cleanPrice()
        fixedBondAssetSwap2 = AssetSwap(payFixedRate,
                                      fixedBond2, fixedBondPrice2,
                                      self.iborIndex, self.spread,
                                      Schedule(),
                                      self.iborIndex.dayCounter(),
                                      parAssetSwap)
        fixedBondAssetSwap2.setPricingEngine(swapEngine)
        fixedBondAssetSwapPrice2 = fixedBondAssetSwap2.fairCleanPrice()
        error2 = abs(fixedBondAssetSwapPrice2-fixedBondPrice2)

        self.assertFalse(error2>tolerance,
            "wrong zero spread asset swap price for fixed bond:"
            + "\n  bond's clean price:    " + str(fixedBondPrice2)
            + "\n  asset swap fair price: " + str(fixedBondAssetSwapPrice2)
            + "\n  error:                 " + str(error2)
            + "\n  tolerance:             " + str(tolerance))

        ## FRN Underlying bond (Isin: IT0003543847 ISPIM 0 09/29/13)
        ## maturity doesn't occur on a business day
        floatingBondStartDate1 = Date(29,September,2003)
        floatingBondMaturityDate1 = Date(29,September,2013)
        floatingBondSchedule1 = Schedule(floatingBondStartDate1,
                                       floatingBondMaturityDate1,
                                       Period(Semiannual), bondCalendar,
                                       Unadjusted, Unadjusted,
                                       DateGeneration.Backward, False)
        floatingBondLeg1 = list(IborLeg([self.faceAmount],floatingBondSchedule1, self.iborIndex,
                                       Actual360(),ModifiedFollowing, [fixingDays],[],[0.0056],[],[],inArrears))
        floatingbondRedemption1 = bondCalendar.adjust(floatingBondMaturityDate1, Following)
        floatingBondLeg1.append(SimpleCashFlow(100.0, floatingbondRedemption1))
        floatingBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 floatingBondMaturityDate1, floatingBondStartDate1,
                 tuple(floatingBondLeg1))
        floatingBond1.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond1.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(27,March,2007), 0.0402)
        floatingBondPrice1 = floatingBond1.cleanPrice()
        floatingBondAssetSwap1 = AssetSwap (payFixedRate,
                                         floatingBond1, floatingBondPrice1,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        floatingBondAssetSwap1.setPricingEngine(swapEngine)
        floatingBondAssetSwapPrice1 = floatingBondAssetSwap1.fairCleanPrice()
        error3 = abs(floatingBondAssetSwapPrice1-floatingBondPrice1)

        self.assertFalse(error3>tolerance, 
            "wrong zero spread asset swap price for floater:"
            + "\n  bond's clean price:    " + str(floatingBondPrice1)
            + "\n  asset swap fair price: " + str(floatingBondAssetSwapPrice1)
            + "\n  error:                 " + str(error3)
            + "\n  tolerance:             " + str(tolerance))
        

        ## FRN Underlying bond (Isin: XS0090566539 COE 0 09/24/18)
        ## maturity occurs on a business day
        floatingBondStartDate2 = Date(24,September,2004)
        floatingBondMaturityDate2 = Date(24,September,2018)
        floatingBondSchedule2 =Schedule(floatingBondStartDate2,
                                       floatingBondMaturityDate2,
                                       Period(Semiannual), bondCalendar,
                                       ModifiedFollowing, ModifiedFollowing,
                                       DateGeneration.Backward, False)
        floatingBondLeg2 = list(IborLeg([self.faceAmount],floatingBondSchedule2, self.iborIndex,
                                        Actual360(),ModifiedFollowing,[fixingDays],[],[0.0025],[],[],inArrears))
        floatingbondRedemption2 = bondCalendar.adjust(floatingBondMaturityDate2, ModifiedFollowing)
        floatingBondLeg2.append(SimpleCashFlow(100.0, floatingbondRedemption2))
        floatingBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 floatingBondMaturityDate2, floatingBondStartDate2,
                 tuple(floatingBondLeg2))
        floatingBond2.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond2.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(22,March,2007), 0.04013)
        currentCoupon=0.04013+0.0025
        floatingCurrentCoupon= floatingBond2.nextCouponRate()
        error4= abs(floatingCurrentCoupon-currentCoupon)
        self.assertFalse(error4>tolerance,
            "wrong current coupon is returned for floater bond:"
            + "\n  bond's calculated current coupon:      " + str(currentCoupon)
            + "\n  current coupon asked to the bond: " + str(floatingCurrentCoupon)
            + "\n  error:                 " + str(error4)
            + "\n  tolerance:             " + str(tolerance))
        

        floatingBondPrice2 = floatingBond2.cleanPrice()
        floatingBondAssetSwap2 = AssetSwap(payFixedRate,
                                         floatingBond2, floatingBondPrice2,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        floatingBondAssetSwap2.setPricingEngine(swapEngine)
        floatingBondAssetSwapPrice2 = floatingBondAssetSwap2.fairCleanPrice()
        error5 = abs(floatingBondAssetSwapPrice2-floatingBondPrice2)

        self.assertFalse(error5>tolerance, 
            "wrong zero spread asset swap price for floater:"
            + "\n  bond's clean price:    " + str(floatingBondPrice2)
            + "\n  asset swap fair price: " + str(floatingBondAssetSwapPrice2)
            + "\n  error:                 " + str(error5)
            + "\n  tolerance:             " + str(tolerance))
        

        ## CMS Underlying bond (Isin: XS0228052402 CRDIT 0 8/22/20)
        ## maturity doesn't occur on a business day
     
        cmsBondStartDate1 = Date(22,August,2005)
        cmsBondMaturityDate1 = Date(22,August,2020)
        cmsBondSchedule1 = Schedule(cmsBondStartDate1,
                                  cmsBondMaturityDate1,
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBondLeg1 = list(CmsLeg([self.faceAmount],cmsBondSchedule1, self.swapIndex,
                             Thirty360(),Following,[fixingDays],[],[0.055],[0.025],[],inArrears))
        cmsbondRedemption1 = bondCalendar.adjust(cmsBondMaturityDate1, Following)
        cmsBondLeg1.append(SimpleCashFlow(100.0, cmsbondRedemption1))
        cmsBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 cmsBondMaturityDate1, cmsBondStartDate1, tuple(cmsBondLeg1))
        cmsBond1.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond1.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(18,August,2006), 0.04158)
        cmsBondPrice1 = cmsBond1.cleanPrice()
        cmsBondAssetSwap1 = AssetSwap(payFixedRate,
                                    cmsBond1, cmsBondPrice1,
                                    self.iborIndex, self.spread,
                                    Schedule(),
                                    self.iborIndex.dayCounter(),
                                    parAssetSwap)
        cmsBondAssetSwap1.setPricingEngine(swapEngine)
        cmsBondAssetSwapPrice1 = cmsBondAssetSwap1.fairCleanPrice()
        error6 = abs(cmsBondAssetSwapPrice1-cmsBondPrice1)

        self.assertFalse(error6>tolerance, 
            "wrong zero spread asset swap price for cms bond:"
            + "\n  bond's clean price:    " + str(cmsBondPrice1)
            + "\n  asset swap fair price: " + str(cmsBondAssetSwapPrice1)
            + "\n  error:                 " + str(error6)
            + "\n  tolerance:             " + str(tolerance))
        

        ## CMS Underlying bond (Isin: XS0218766664 ISPIM 0 5/6/15)
        ## maturity occurs on a business day
        cmsBondStartDate2 = Date(6,May,2005)
        cmsBondMaturityDate2 = Date(6,May,2015)
        cmsBondSchedule2 = Schedule(cmsBondStartDate2,
                                  cmsBondMaturityDate2,
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBondLeg2 = list(CmsLeg([self.faceAmount],cmsBondSchedule2, self.swapIndex,
                                  Thirty360(),Following,[fixingDays],[0.84],[],[],[],inArrears))
        cmsbondRedemption2 = bondCalendar.adjust(cmsBondMaturityDate2, Following)
        cmsBondLeg2.append(SimpleCashFlow(100.0, cmsbondRedemption2))
        cmsBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 cmsBondMaturityDate2, cmsBondStartDate2, tuple(cmsBondLeg2))
        cmsBond2.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond2.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(4,May,2006), 0.04217)
        cmsBondPrice2 = cmsBond2.cleanPrice()
        cmsBondAssetSwap2 = AssetSwap(payFixedRate,
                                    cmsBond2, cmsBondPrice2,
                                    self.iborIndex, self.spread,
                                    Schedule(),
                                    self.iborIndex.dayCounter(),
                                    parAssetSwap)
        cmsBondAssetSwap2.setPricingEngine(swapEngine)
        cmsBondAssetSwapPrice2 = cmsBondAssetSwap2.fairCleanPrice()
        error7 = abs(cmsBondAssetSwapPrice2-cmsBondPrice2)

        self.assertFalse(error7>tolerance, 
            "wrong zero spread asset swap price for cms bond:"
            + "\n  bond's clean price:    " + str(cmsBondPrice2)
            + "\n  asset swap fair price: " + str(cmsBondAssetSwapPrice2)
            + "\n  error:                 " + str(error7)
            + "\n  tolerance:             " + str(tolerance))
        

        ## Zero Coupon bond (Isin: DE0004771662 IBRD 0 12/20/15)
        ## maturity doesn't occur on a business day
        
        zeroCpnBondStartDate1 = Date(19,December,1985)
        zeroCpnBondMaturityDate1 = Date(20,December,2015)
        zeroCpnBondRedemption1 = bondCalendar.adjust(zeroCpnBondMaturityDate1,
                                                          Following)
        zeroCpnBondLeg1 = Leg([SimpleCashFlow(100.0, zeroCpnBondRedemption1)])
        zeroCpnBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 zeroCpnBondMaturityDate1, zeroCpnBondStartDate1, zeroCpnBondLeg1)
        zeroCpnBond1.setPricingEngine(bondEngine)

        zeroCpnBondPrice1 = zeroCpnBond1.cleanPrice()
        zeroCpnAssetSwap1 = AssetSwap (payFixedRate,
                                    zeroCpnBond1, zeroCpnBondPrice1,
                                    self.iborIndex, self.spread,
                                    Schedule(),
                                    self.iborIndex.dayCounter(),
                                    parAssetSwap)
        zeroCpnAssetSwap1.setPricingEngine(swapEngine)
        zeroCpnBondAssetSwapPrice1 = zeroCpnAssetSwap1.fairCleanPrice()
        error8 = abs(zeroCpnBondAssetSwapPrice1-zeroCpnBondPrice1)

        self.assertFalse(error8>tolerance, 
            "wrong zero spread asset swap price for zero cpn bond:"
            + "\n  bond's clean price:    " + str(zeroCpnBondPrice1)
            + "\n  asset swap fair price: " + str(zeroCpnBondAssetSwapPrice1)
            + "\n  error:                 " + str(error8)
            + "\n  tolerance:             " + str(tolerance))
        

        ## Zero Coupon bond (Isin: IT0001200390 ISPIM 0 02/17/28)
        ## maturity occurs on a business day
        zeroCpnBondStartDate2 = Date(17,February,1998)
        zeroCpnBondMaturityDate2 = Date(17,February,2028)
        zerocpbondRedemption2 = bondCalendar.adjust(zeroCpnBondMaturityDate2,
                                                          Following)
        zeroCpnBondLeg2 = Leg([SimpleCashFlow(100.0, zerocpbondRedemption2)])
        zeroCpnBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 zeroCpnBondMaturityDate2, zeroCpnBondStartDate2, zeroCpnBondLeg2)
        zeroCpnBond2.setPricingEngine(bondEngine)

        zeroCpnBondPrice2 = zeroCpnBond2.cleanPrice()
        zeroCpnAssetSwap2 = AssetSwap(payFixedRate,
                                    zeroCpnBond2, zeroCpnBondPrice2,
                                    self.iborIndex, self.spread,
                                    Schedule(),
                                    self.iborIndex.dayCounter(),
                                    parAssetSwap)
        zeroCpnAssetSwap2.setPricingEngine(swapEngine)
        zeroCpnBondAssetSwapPrice2 = zeroCpnAssetSwap2.fairCleanPrice()
        error9 = abs(cmsBondAssetSwapPrice2-cmsBondPrice2)

        self.assertFalse(error9>tolerance, 
            "wrong zero spread asset swap price for zero cpn bond:"
            + "\n  bond's clean price:    " + str(zeroCpnBondPrice2)
            + "\n  asset swap fair price: " + str(zeroCpnBondAssetSwapPrice2)
            + "\n  error:                 " + str(error9)
            + "\n  tolerance:             " + str(tolerance))
        



    def testMASWWithGenericBond(self):
        """Testing market asset swap against par asset swap with generic bond..."""
        bondCalendar = TARGET()
        settlementDays = 3
        fixingDays = 2
        payFixedRate = True
        parAssetSwap = True
        mktAssetSwap = False
        inArrears = False

        ## Fixed Underlying bond (Isin: DE0001135275 DBR 4 01/04/37)
        ## maturity doesn't occur on a business day

        fixedBondStartDate1 = Date(4,January,2005)
        fixedBondMaturityDate1 = Date(4,January,2037)
        fixedBondSchedule1 = Schedule(fixedBondStartDate1,
                                    fixedBondMaturityDate1,
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBondLeg1 = list(FixedRateLeg(fixedBondSchedule1, ActualActual(ActualActual.ISDA), [self.faceAmount], [0.04]))
        fixedbondRedemption1 = bondCalendar.adjust(fixedBondMaturityDate1, Following)
        fixedBondLeg1.append(SimpleCashFlow(100.0, fixedbondRedemption1))
        fixedBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 fixedBondMaturityDate1, fixedBondStartDate1,
                 fixedBondLeg1)
        bondEngine = DiscountingBondEngine(self.termStructure)
        swapEngine = DiscountingSwapEngine(self.termStructure, False)
        fixedBond1.setPricingEngine(bondEngine)

        fixedBondMktPrice1 = 89.22  ## market price observed on 7th June 2007
        fixedBondMktFullPrice1=fixedBondMktPrice1+fixedBond1.accruedAmount()
        fixedBondParAssetSwap1 = AssetSwap (payFixedRate,
                                         fixedBond1, fixedBondMktPrice1,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        fixedBondParAssetSwap1.setPricingEngine(swapEngine)
        fixedBondParAssetSwapSpread1 = fixedBondParAssetSwap1.fairSpread()
        fixedBondMktAssetSwap1 = AssetSwap(payFixedRate,
                                         fixedBond1, fixedBondMktPrice1,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         mktAssetSwap)
        fixedBondMktAssetSwap1.setPricingEngine(swapEngine)
        fixedBondMktAssetSwapSpread1 = fixedBondMktAssetSwap1.fairSpread()

        tolerance = 1.0e-13
        error1 = abs(fixedBondMktAssetSwapSpread1-
                      100*fixedBondParAssetSwapSpread1/fixedBondMktFullPrice1)

        self.assertFalse(error1>tolerance,
            "wrong asset swap spreads for fixed bond:"
            + "\n  market asset swap spread: " + str(fixedBondMktAssetSwapSpread1)
            + "\n  par asset swap spread:    " + str(fixedBondParAssetSwapSpread1)
            + "\n  error:                    " + str(error1)
            + "\n  tolerance:                " + str(tolerance))

        ## Fixed Underlying bond (Isin: IT0006527060 IBRD 5 02/05/19)
        ## maturity occurs on a business day

        fixedBondStartDate2 = Date(5,February,2005)
        fixedBondMaturityDate2 = Date(5,February,2019)
        fixedBondSchedule2 = Schedule(fixedBondStartDate2,
                                    fixedBondMaturityDate2,
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBondLeg2 = list(FixedRateLeg(fixedBondSchedule2, Thirty360(Thirty360.BondBasis),[self.faceAmount],[0.05]))
        fixedbondRedemption2 = bondCalendar.adjust(fixedBondMaturityDate2, Following)
        fixedBondLeg2.append(SimpleCashFlow(100.0, fixedbondRedemption2))
        fixedBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 fixedBondMaturityDate2, fixedBondStartDate2, fixedBondLeg2)
        fixedBond2.setPricingEngine(bondEngine)

        fixedBondMktPrice2 = 99.98  ## market price observed on 7th June 2007
        fixedBondMktFullPrice2 = fixedBondMktPrice2+fixedBond2.accruedAmount()
        fixedBondParAssetSwap2 = AssetSwap(payFixedRate,
                                         fixedBond2, fixedBondMktPrice2,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        fixedBondParAssetSwap2.setPricingEngine(swapEngine)
        fixedBondParAssetSwapSpread2 = fixedBondParAssetSwap2.fairSpread()
        fixedBondMktAssetSwap2 = AssetSwap(payFixedRate,
                                         fixedBond2, fixedBondMktPrice2,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         mktAssetSwap)
        fixedBondMktAssetSwap2.setPricingEngine(swapEngine)
        fixedBondMktAssetSwapSpread2 = fixedBondMktAssetSwap2.fairSpread()
        error2 = abs(fixedBondMktAssetSwapSpread2-
                      100*fixedBondParAssetSwapSpread2/fixedBondMktFullPrice2)

        self.assertFalse(error2>tolerance,
            "wrong asset swap spreads for fixed bond:"
            + "\n  market asset swap spread: " + str(fixedBondMktAssetSwapSpread2)
            + "\n  par asset swap spread:    " + str(fixedBondParAssetSwapSpread2)
            + "\n  error:                    " + str(error2)
            + "\n  tolerance:                " + str(tolerance))

        ## FRN Underlying bond (Isin: IT0003543847 ISPIM 0 09/29/13)
        ## maturity doesn't occur on a business day

        floatingBondStartDate1 = Date(29,September,2003)
        floatingBondMaturityDate1 = Date(29,September,2013)
        floatingBondSchedule1 = Schedule(floatingBondStartDate1,
                                       floatingBondMaturityDate1,
                                       Period(Semiannual), bondCalendar,
                                       Unadjusted, Unadjusted,
                                       DateGeneration.Backward, False)
        floatingBondLeg1 = list(IborLeg([self.faceAmount],floatingBondSchedule1, self.iborIndex,Actual360(),Following,
                                   [fixingDays], [],[0.0056],[],[],inArrears))
        floatingbondRedemption1 = bondCalendar.adjust(floatingBondMaturityDate1, Following)
        floatingBondLeg1.append(SimpleCashFlow(100.0, floatingbondRedemption1))
        floatingBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                        floatingBondMaturityDate1, floatingBondStartDate1,
                        floatingBondLeg1)
        floatingBond1.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond1.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(27,March,2007), 0.0402)
        ## market price observed on 7th June 2007
        floatingBondMktPrice1 = 101.64 
        floatingBondMktFullPrice1 = floatingBondMktPrice1+floatingBond1.accruedAmount()
        floatingBondParAssetSwap1 = AssetSwap(payFixedRate,
                                            floatingBond1, floatingBondMktPrice1,
                                            self.iborIndex, self.spread,
                                            Schedule(),
                                            self.iborIndex.dayCounter(),
                                            parAssetSwap)
        floatingBondParAssetSwap1.setPricingEngine(swapEngine)
        floatingBondParAssetSwapSpread1 = floatingBondParAssetSwap1.fairSpread()
        floatingBondMktAssetSwap1 = AssetSwap(payFixedRate,
                                            floatingBond1, floatingBondMktPrice1,
                                            self.iborIndex, self.spread,
                                            Schedule(),
                                            self.iborIndex.dayCounter(),
                                            mktAssetSwap)
        floatingBondMktAssetSwap1.setPricingEngine(swapEngine)
        floatingBondMktAssetSwapSpread1 = floatingBondMktAssetSwap1.fairSpread()
        error3 = abs(floatingBondMktAssetSwapSpread1-
                      100*floatingBondParAssetSwapSpread1/floatingBondMktFullPrice1)

        self.assertFalse(error3>tolerance,
                "wrong asset swap spreads for floating bond:"
                + "\n  market asset swap spread: " + str(floatingBondMktAssetSwapSpread1)
                + "\n  par asset swap spread:    " + str(floatingBondParAssetSwapSpread1)
                + "\n  error:                    " + str(error3)
                + "\n  tolerance:                " + str(tolerance))

        ## FRN Underlying bond (Isin: XS0090566539 COE 0 09/24/18)
        ## maturity occurs on a business day

        floatingBondStartDate2 = Date(24,September,2004)
        floatingBondMaturityDate2 = Date(24,September,2018)
        floatingBondSchedule2 = Schedule(floatingBondStartDate2,
                                       floatingBondMaturityDate2,
                                       Period(Semiannual), bondCalendar,
                                       ModifiedFollowing, ModifiedFollowing,
                                       DateGeneration.Backward, False)
        floatingBondLeg2 = list(IborLeg([self.faceAmount],floatingBondSchedule2, self.iborIndex, Actual360(),
                                   ModifiedFollowing, [fixingDays], [], [0.0025] , [],[], inArrears))
        floatingbondRedemption2 = bondCalendar.adjust(floatingBondMaturityDate2, ModifiedFollowing)
        floatingBondLeg2.append(SimpleCashFlow(100.0, floatingbondRedemption2))
        floatingBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                         floatingBondMaturityDate2, floatingBondStartDate2,
                         floatingBondLeg2)
        floatingBond2.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond2.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(22,March,2007), 0.04013)
        ## market price observed on 7th June 2007
        floatingBondMktPrice2 = 101.248 
        floatingBondMktFullPrice2 = floatingBondMktPrice2+floatingBond2.accruedAmount()
        floatingBondParAssetSwap2 = AssetSwap(payFixedRate,
                                            floatingBond2, floatingBondMktPrice2,
                                            self.iborIndex, self.spread,
                                            Schedule(),
                                            self.iborIndex.dayCounter(),
                                            parAssetSwap)
        floatingBondParAssetSwap2.setPricingEngine(swapEngine)
        floatingBondParAssetSwapSpread2 = floatingBondParAssetSwap2.fairSpread()
        floatingBondMktAssetSwap2 = AssetSwap(payFixedRate,
                                            floatingBond2, floatingBondMktPrice2,
                                            self.iborIndex, self.spread,
                                            Schedule(),
                                            self.iborIndex.dayCounter(),
                                            mktAssetSwap)
        floatingBondMktAssetSwap2.setPricingEngine(swapEngine)
        floatingBondMktAssetSwapSpread2 = floatingBondMktAssetSwap2.fairSpread()
        error4 = abs(floatingBondMktAssetSwapSpread2-
                      100*floatingBondParAssetSwapSpread2/floatingBondMktFullPrice2)

        self.assertFalse(error4>tolerance,
            "wrong asset swap spreads for floating bond:"
            + "\n  market asset swap spread: " + str(floatingBondMktAssetSwapSpread2)
            + "\n  par asset swap spread:    " + str(floatingBondParAssetSwapSpread2)
            + "\n  error:                    " + str(error4)
            + "\n  tolerance:                " + str(tolerance))


        
        ## CMS Underlying bond (Isin: XS0228052402 CRDIT 0 8/22/20)
        ## maturity doesn't occur on a business day
        
        cmsBondStartDate1 = Date(22,August,2005)
        cmsBondMaturityDate1 = Date(22,August,2020)
        cmsBondSchedule1 = Schedule(cmsBondStartDate1,
                                  cmsBondMaturityDate1,
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBondLeg1 = list(CmsLeg([self.faceAmount],cmsBondSchedule1, self.swapIndex,
                                 Thirty360(),Following,[fixingDays],[],[],[0.055],[0.025],inArrears))
        cmsbondRedemption1 = bondCalendar.adjust(cmsBondMaturityDate1, Following)
        cmsBondLeg1.append(SimpleCashFlow(100.0, cmsbondRedemption1))
        cmsBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 cmsBondMaturityDate1, cmsBondStartDate1, cmsBondLeg1)
        cmsBond1.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond1.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(18,August,2006), 0.04158)
        cmsBondMktPrice1 = 88.45  ## market price observed on 7th June 2007
        cmsBondMktFullPrice1 = cmsBondMktPrice1+cmsBond1.accruedAmount()
        cmsBondParAssetSwap1 = AssetSwap(payFixedRate,
                                       cmsBond1, cmsBondMktPrice1,
                                       self.iborIndex, self.spread,
                                       Schedule(),
                                       self.iborIndex.dayCounter(),
                                       parAssetSwap)
        cmsBondParAssetSwap1.setPricingEngine(swapEngine)
        cmsBondParAssetSwapSpread1 = cmsBondParAssetSwap1.fairSpread()
        cmsBondMktAssetSwap1 = AssetSwap(payFixedRate,
                                       cmsBond1, cmsBondMktPrice1,
                                       self.iborIndex, self.spread,
                                       Schedule(),
                                       self.iborIndex.dayCounter(),
                                       mktAssetSwap)
        cmsBondMktAssetSwap1.setPricingEngine(swapEngine)
        cmsBondMktAssetSwapSpread1 = cmsBondMktAssetSwap1.fairSpread()
        error5 = abs(cmsBondMktAssetSwapSpread1-
                      100*cmsBondParAssetSwapSpread1/cmsBondMktFullPrice1)

        self.assertFalse(error5>tolerance,
            "wrong asset swap spreads for cms bond:"
            + "\n  market asset swap spread: " + str(cmsBondMktAssetSwapSpread1)
            + "\n  par asset swap spread:    " + str(100*cmsBondParAssetSwapSpread1/cmsBondMktFullPrice1)
            + "\n  error:                    " + str(error5)
            + "\n  tolerance:                " + str(tolerance))

         ## CMS Underlying bond (Isin: XS0218766664 ISPIM 0 5/6/15)
         ## maturity occurs on a business day

        cmsBondStartDate2 = Date(6,May,2005)
        cmsBondMaturityDate2 = Date(6,May,2015)
        cmsBondSchedule2 = Schedule(cmsBondStartDate2,
                                  cmsBondMaturityDate2,
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBondLeg2 = list(CmsLeg([self.faceAmount],cmsBondSchedule2, self.swapIndex,
                             Thirty360(),Following,[fixingDays],[0.84],[],[],[],inArrears))
        cmsbondRedemption2 = bondCalendar.adjust(cmsBondMaturityDate2, Following)
        cmsBondLeg2.append(SimpleCashFlow(100.0, cmsbondRedemption2))
        cmsBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                     cmsBondMaturityDate2, cmsBondStartDate2, cmsBondLeg2)
        cmsBond2.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond2.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(4,May,2006), 0.04217)
        cmsBondMktPrice2 = 94.08  ## market price observed on 7th June 2007
        cmsBondMktFullPrice2 = cmsBondMktPrice2+cmsBond2.accruedAmount()
        cmsBondParAssetSwap2 = AssetSwap(payFixedRate,
                                       cmsBond2, cmsBondMktPrice2,
                                       self.iborIndex, self.spread,
                                       Schedule(),
                                       self.iborIndex.dayCounter(),
                                       parAssetSwap)
        cmsBondParAssetSwap2.setPricingEngine(swapEngine)
        cmsBondParAssetSwapSpread2 = cmsBondParAssetSwap2.fairSpread()
        cmsBondMktAssetSwap2 = AssetSwap(payFixedRate,
                                       cmsBond2, cmsBondMktPrice2,
                                       self.iborIndex, self.spread,
                                       Schedule(),
                                       self.iborIndex.dayCounter(),
                                       mktAssetSwap)
        cmsBondMktAssetSwap2.setPricingEngine(swapEngine)
        cmsBondMktAssetSwapSpread2 = cmsBondMktAssetSwap2.fairSpread()
        error6 = abs(cmsBondMktAssetSwapSpread2-
                      100*cmsBondParAssetSwapSpread2/cmsBondMktFullPrice2)

        self.assertFalse(error6>tolerance, 
            "wrong asset swap spreads for cms bond:"
            + "\n  market asset swap spread: " + str(cmsBondMktAssetSwapSpread2)
            + "\n  par asset swap spread:    " + str(cmsBondParAssetSwapSpread2)
            + "\n  error:                    " + str(error6)
            + "\n  tolerance:                " + str(tolerance))

        ## Zero Coupon bond (Isin: DE0004771662 IBRD 0 12/20/15)
        ## maturity doesn't occur on a business day

        zeroCpnBondStartDate1 = Date(19,December,1985)
        zeroCpnBondMaturityDate1 = Date(20,December,2015)
        zeroCpnBondRedemption1 = bondCalendar.adjust(zeroCpnBondMaturityDate1,
                                                          Following)
        zeroCpnBondLeg1 = Leg([SimpleCashFlow(100.0, zeroCpnBondRedemption1)])
        zeroCpnBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 zeroCpnBondMaturityDate1, zeroCpnBondStartDate1, zeroCpnBondLeg1)
        zeroCpnBond1.setPricingEngine(bondEngine)

        ## market price observed on 12th June 2007
        zeroCpnBondMktPrice1 = 70.436 
        zeroCpnBondMktFullPrice1 = zeroCpnBondMktPrice1+zeroCpnBond1.accruedAmount()
        zeroCpnBondParAssetSwap1 = AssetSwap(payFixedRate,zeroCpnBond1,
                                           zeroCpnBondMktPrice1,
                                           self.iborIndex, self.spread,
                                           Schedule(),
                                           self.iborIndex.dayCounter(),
                                           parAssetSwap)
        zeroCpnBondParAssetSwap1.setPricingEngine(swapEngine)
        zeroCpnBondParAssetSwapSpread1 = zeroCpnBondParAssetSwap1.fairSpread()
        zeroCpnBondMktAssetSwap1 = AssetSwap(payFixedRate,zeroCpnBond1,
                                           zeroCpnBondMktPrice1,
                                           self.iborIndex, self.spread,
                                           Schedule(),
                                           self.iborIndex.dayCounter(),
                                           mktAssetSwap)
        zeroCpnBondMktAssetSwap1.setPricingEngine(swapEngine)
        zeroCpnBondMktAssetSwapSpread1 = zeroCpnBondMktAssetSwap1.fairSpread()
        error7 = abs(zeroCpnBondMktAssetSwapSpread1-
                      100*zeroCpnBondParAssetSwapSpread1/zeroCpnBondMktFullPrice1)

        self.assertFalse(error7>tolerance,
            "wrong asset swap spreads for zero cpn bond:"
            + "\n  market asset swap spread: " + str(zeroCpnBondMktAssetSwapSpread1)
            + "\n  par asset swap spread:    " + str(zeroCpnBondParAssetSwapSpread1)
            + "\n  error:                    " + str(error7)
            + "\n  tolerance:                " + str(tolerance))

        ## Zero Coupon bond (Isin: IT0001200390 ISPIM 0 02/17/28)
        ## maturity occurs on a business day

        zeroCpnBondStartDate2 = Date(17,February,1998)
        zeroCpnBondMaturityDate2 = Date(17,February,2028)
        zerocpbondRedemption2 = bondCalendar.adjust(zeroCpnBondMaturityDate2,
                                                          Following)
        zeroCpnBondLeg2 = Leg([SimpleCashFlow(100.0, zerocpbondRedemption2)])
        zeroCpnBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 zeroCpnBondMaturityDate2, zeroCpnBondStartDate2, zeroCpnBondLeg2)
        zeroCpnBond2.setPricingEngine(bondEngine)

        ## zeroCpnBondPrice2 = zeroCpnBond2.cleanPrice()
        ## market price observed on 12th June 2007
        zeroCpnBondMktPrice2 = 35.160 
        zeroCpnBondMktFullPrice2 = zeroCpnBondMktPrice2+zeroCpnBond2.accruedAmount()
        zeroCpnBondParAssetSwap2 = AssetSwap(payFixedRate,zeroCpnBond2,
                                           zeroCpnBondMktPrice2,
                                           self.iborIndex, self.spread,
                                           Schedule(),
                                           self.iborIndex.dayCounter(),
                                           parAssetSwap)
        zeroCpnBondParAssetSwap2.setPricingEngine(swapEngine)
        zeroCpnBondParAssetSwapSpread2 = zeroCpnBondParAssetSwap2.fairSpread()
        zeroCpnBondMktAssetSwap2 = AssetSwap(payFixedRate,zeroCpnBond2,
                                           zeroCpnBondMktPrice2,
                                           self.iborIndex, self.spread,
                                           Schedule(),
                                           self.iborIndex.dayCounter(),
                                           mktAssetSwap)
        zeroCpnBondMktAssetSwap2.setPricingEngine(swapEngine)
        zeroCpnBondMktAssetSwapSpread2 = zeroCpnBondMktAssetSwap2.fairSpread()
        error8 = abs(zeroCpnBondMktAssetSwapSpread2-
                      100*zeroCpnBondParAssetSwapSpread2/zeroCpnBondMktFullPrice2)

        self.assertFalse(error8>tolerance, 
            "wrong asset swap spreads for zero cpn bond:"
            + "\n  market asset swap spread: " + str(zeroCpnBondMktAssetSwapSpread2)
            + "\n  par asset swap spread:    " + str(zeroCpnBondParAssetSwapSpread2)
            + "\n  error:                    " + str(error8)
            + "\n  tolerance:                " + str(tolerance))



    def testZSpreadWithGenericBond(self) :
        """Testing clean and dirty price with null Z-spread against theoretical prices..."""
        
        bondCalendar = TARGET()
        settlementDays = 3
        fixingDays = 2
        inArrears = False

        ## Fixed Underlying bond (Isin: DE0001135275 DBR 4 01/04/37)
        ## maturity doesn't occur on a business day

        fixedBondStartDate1 = Date(4,January,2005)
        fixedBondMaturityDate1 = Date(4,January,2037)
        fixedBondSchedule1 = Schedule(fixedBondStartDate1,
                                    fixedBondMaturityDate1,
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBondLeg1 = list(FixedRateLeg(fixedBondSchedule1, ActualActual(ActualActual.ISDA),  [self.faceAmount], [0.04]))
        fixedbondRedemption1 = bondCalendar.adjust(fixedBondMaturityDate1, Following)
        fixedBondLeg1.append(SimpleCashFlow(100.0, fixedbondRedemption1))
        fixedBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 fixedBondMaturityDate1, fixedBondStartDate1,
                 fixedBondLeg1)
        bondEngine = DiscountingBondEngine(self.termStructure)
        fixedBond1.setPricingEngine(bondEngine)

        fixedBondImpliedValue1 = fixedBond1.cleanPrice()
        fixedBondSettlementDate1 = fixedBond1.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        fixedBondCleanPrice1 = cleanPriceFromZSpread(fixedBond1, self.yieldCurve, self.spread,
             Actual365Fixed(), self.compounding, Annual, fixedBondSettlementDate1)
        tolerance = 1.0e-13
        error1 = abs(fixedBondImpliedValue1-fixedBondCleanPrice1)
        self.assertFalse(error1>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(fixedBondImpliedValue1)
            + "\n  par asset swap spread: " + str(fixedBondCleanPrice1)
            + "\n  error:                 " + str(error1)
            + "\n  tolerance:             " + str(tolerance))
        

        ## Fixed Underlying bond (Isin: IT0006527060 IBRD 5 02/05/19)
        ## maturity occurs on a business day

        fixedBondStartDate2 = Date(5,February,2005)
        fixedBondMaturityDate2 = Date(5,February,2019)
        fixedBondSchedule2 = Schedule(fixedBondStartDate2,
                                    fixedBondMaturityDate2,
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBondLeg2 = list(FixedRateLeg(fixedBondSchedule2, Thirty360(Thirty360.BondBasis),
                                     [self.faceAmount],[0.05]))
        fixedbondRedemption2 = bondCalendar.adjust(fixedBondMaturityDate2, Following)
        fixedBondLeg2.append(SimpleCashFlow(100.0, fixedbondRedemption2))
        fixedBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 fixedBondMaturityDate2, fixedBondStartDate2, fixedBondLeg2)
        fixedBond2.setPricingEngine(bondEngine)

        fixedBondImpliedValue2 = fixedBond2.cleanPrice()
        fixedBondSettlementDate2= fixedBond2.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve

        fixedBondCleanPrice2 = cleanPriceFromZSpread(fixedBond2, self.yieldCurve, self.spread,
             Actual365Fixed(), self.compounding, Annual, fixedBondSettlementDate2)
        error3 = abs(fixedBondImpliedValue2-fixedBondCleanPrice2)
        self.assertFalse(error3>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(fixedBondImpliedValue2)
            + "\n  par asset swap spread: " + str(fixedBondCleanPrice2)
            + "\n  error:                 " + str(error3)
            + "\n  tolerance:             " + str(tolerance))
        

        ## FRN Underlying bond (Isin: IT0003543847 ISPIM 0 09/29/13)
        ## maturity doesn't occur on a business day

        floatingBondStartDate1 = Date(29,September,2003)
        floatingBondMaturityDate1 = Date(29,September,2013)
        floatingBondSchedule1 = Schedule(floatingBondStartDate1,
                                       floatingBondMaturityDate1,
                                       Period(Semiannual), bondCalendar,
                                       Unadjusted, Unadjusted,
                                       DateGeneration.Backward, False)
        floatingBondLeg1 = list(IborLeg([self.faceAmount],floatingBondSchedule1, self.iborIndex,
                                   Actual360(),Following,[fixingDays], [],[0.0056],[],[], inArrears))
        floatingbondRedemption1 = bondCalendar.adjust(floatingBondMaturityDate1, Following)
        floatingBondLeg1.append(SimpleCashFlow(100.0, floatingbondRedemption1))
        floatingBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 floatingBondMaturityDate1, floatingBondStartDate1,
                 floatingBondLeg1)
        floatingBond1.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond1.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(27,March,2007), 0.0402)
        floatingBondImpliedValue1 = floatingBond1.cleanPrice()
        floatingBondSettlementDate1= floatingBond1.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        floatingBondCleanPrice1 = cleanPriceFromZSpread(floatingBond1, self.yieldCurve, 
            self.spread, Actual365Fixed(), self.compounding, Semiannual,
            fixedBondSettlementDate1)
        error5 = abs(floatingBondImpliedValue1-floatingBondCleanPrice1)
        self.assertFalse(error5>tolerance,
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(floatingBondImpliedValue1)
            + "\n  par asset swap spread: " + str(floatingBondCleanPrice1)
            + "\n  error:                 " + str(error5)
            + "\n  tolerance:             " + str(tolerance))
    

        ## FRN Underlying bond (Isin: XS0090566539 COE 0 09/24/18)
        ## maturity occurs on a business day

        floatingBondStartDate2 = Date(24,September,2004)
        floatingBondMaturityDate2 = Date(24,September,2018)
        floatingBondSchedule2 = Schedule(floatingBondStartDate2,
                                       floatingBondMaturityDate2,
                                       Period(Semiannual), bondCalendar,
                                       ModifiedFollowing, ModifiedFollowing,
                                       DateGeneration.Backward, False)
        floatingBondLeg2 = list(IborLeg([self.faceAmount],floatingBondSchedule2, self.iborIndex,
                                        Actual360(),ModifiedFollowing, [fixingDays],[],[0.0025],[],[], inArrears))
        floatingbondRedemption2 = bondCalendar.adjust(floatingBondMaturityDate2, ModifiedFollowing)
        floatingBondLeg2.append(SimpleCashFlow(100.0, floatingbondRedemption2))
        floatingBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 floatingBondMaturityDate2, floatingBondStartDate2, floatingBondLeg2)
        floatingBond2.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond2.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(22,March,2007), 0.04013)
        floatingBondImpliedValue2 = floatingBond2.cleanPrice()
        floatingBondSettlementDate2= floatingBond2.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        floatingBondCleanPrice2 = cleanPriceFromZSpread(floatingBond2, self.yieldCurve, 
            self.spread, Actual365Fixed(), self.compounding, Semiannual, fixedBondSettlementDate1)
        error7 = abs(floatingBondImpliedValue2-floatingBondCleanPrice2)
        self.assertFalse(error7>tolerance,
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(floatingBondImpliedValue2)
            + "\n  par asset swap spread: " + str(floatingBondCleanPrice2)
            + "\n  error:                 " + str(error7)
            + "\n  tolerance:             " + str(tolerance))
        

        ## CMS Underlying bond (Isin: XS0228052402 CRDIT 0 8/22/20)
        ## maturity doesn't occur on a business day

        cmsBondStartDate1 = Date(22,August,2005)
        cmsBondMaturityDate1 = Date(22,August,2020)
        cmsBondSchedule1 = Schedule(cmsBondStartDate1,
                                  cmsBondMaturityDate1,
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBondLeg1 = list(CmsLeg([self.faceAmount],cmsBondSchedule1, self.swapIndex,
                             Thirty360(),Following,[fixingDays],[],[],[0.055],[0.025],inArrears))
        cmsbondRedemption1 = bondCalendar.adjust(cmsBondMaturityDate1, Following)
        cmsBondLeg1.append(SimpleCashFlow(100.0, cmsbondRedemption1))
        cmsBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 cmsBondMaturityDate1, cmsBondStartDate1, cmsBondLeg1)
        cmsBond1.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond1.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(18,August,2006), 0.04158)
        cmsBondImpliedValue1 = cmsBond1.cleanPrice()
        cmsBondSettlementDate1= cmsBond1.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        cmsBondCleanPrice1 = cleanPriceFromZSpread(cmsBond1, self.yieldCurve, self.spread,
                                 Actual365Fixed(), self.compounding, Annual, cmsBondSettlementDate1)
        error9 = abs(cmsBondImpliedValue1-cmsBondCleanPrice1)
        self.assertFalse(error9>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(cmsBondImpliedValue1)
            + "\n  par asset swap spread: " + str(cmsBondCleanPrice1)
            + "\n  error:                 " + str(error9)
            + "\n  tolerance:             " + str(tolerance))
        

        ## CMS Underlying bond (Isin: XS0218766664 ISPIM 0 5/6/15)
        ## maturity occurs on a business day
       
        cmsBondStartDate2 = Date(6,May,2005)
        cmsBondMaturityDate2 = Date(6,May,2015)
        cmsBondSchedule2 = Schedule(cmsBondStartDate2,
                                  cmsBondMaturityDate2,
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBondLeg2 = list(CmsLeg([self.faceAmount],cmsBondSchedule2, self.swapIndex,
                                  Thirty360(),Following,[fixingDays],[0.84],[],[],[],inArrears))
        cmsbondRedemption2 = bondCalendar.adjust(cmsBondMaturityDate2, Following)
        cmsBondLeg2.append(SimpleCashFlow(100.0, cmsbondRedemption2))
        cmsBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 cmsBondMaturityDate2, cmsBondStartDate2, cmsBondLeg2)
        cmsBond2.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond2.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(4,May,2006), 0.04217)
        cmsBondImpliedValue2 = cmsBond2.cleanPrice()
        cmsBondSettlementDate2= cmsBond2.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        cmsBondCleanPrice2 = cleanPriceFromZSpread(cmsBond2, self.yieldCurve, self.spread,
             Actual365Fixed(), self.compounding, Annual, cmsBondSettlementDate2)
        error11 = abs(cmsBondImpliedValue2-cmsBondCleanPrice2)
        self.assertFalse(error11>tolerance,
            "wrong clean price for fixed bond:"
            + "\n  market asset swap spread: " + str(cmsBondImpliedValue2)
            + "\n  par asset swap spread: " + str(cmsBondCleanPrice2)
            + "\n  error:                 " + str(error11)
            + "\n  tolerance:             " + str(tolerance))
        

        ## Zero Coupon bond (Isin: DE0004771662 IBRD 0 12/20/15)
        ## maturity doesn't occur on a business day

        zeroCpnBondStartDate1 = Date(19,December,1985)
        zeroCpnBondMaturityDate1 = Date(20,December,2015)
        zeroCpnBondRedemption1 = bondCalendar.adjust(zeroCpnBondMaturityDate1,
                                                          Following)
        zeroCpnBondLeg1 = Leg([SimpleCashFlow(100.0, zeroCpnBondRedemption1)])
        zeroCpnBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                         zeroCpnBondMaturityDate1, zeroCpnBondStartDate1, zeroCpnBondLeg1)
        zeroCpnBond1.setPricingEngine(bondEngine)

        zeroCpnBondImpliedValue1 = zeroCpnBond1.cleanPrice()
        zeroCpnBondSettlementDate1= zeroCpnBond1.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        zeroCpnBondCleanPrice1 = cleanPriceFromZSpread(zeroCpnBond1, self.yieldCurve,
                                  self.spread,
                                  Actual365Fixed(),
                                  self.compounding, Annual,
                                  zeroCpnBondSettlementDate1)
        error13 = abs(zeroCpnBondImpliedValue1-zeroCpnBondCleanPrice1)
        self.assertFalse(error13>tolerance,
            "wrong clean price for zero coupon bond:"
            + "\n  zero cpn implied value: " + str(zeroCpnBondImpliedValue1)
            + "\n  zero cpn price: " + str(zeroCpnBondCleanPrice1)
            + "\n  error:                 " + str(error13)
            + "\n  tolerance:             " + str(tolerance))
        

        ## Zero Coupon bond (Isin: IT0001200390 ISPIM 0 02/17/28)
        ## maturity occurs on a business day

        zeroCpnBondStartDate2 = Date(17,February,1998)
        zeroCpnBondMaturityDate2 = Date(17,February,2028)
        zerocpbondRedemption2 = bondCalendar.adjust(zeroCpnBondMaturityDate2, Following)
        zeroCpnBondLeg2 = Leg([SimpleCashFlow(100.0, zerocpbondRedemption2)])
        zeroCpnBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 zeroCpnBondMaturityDate2, zeroCpnBondStartDate2, zeroCpnBondLeg2)
        zeroCpnBond2.setPricingEngine(bondEngine)

        zeroCpnBondImpliedValue2 = zeroCpnBond2.cleanPrice()
        zeroCpnBondSettlementDate2= zeroCpnBond2.settlementDate()
        ## standard market conventions:
        ## bond's frequency + coumpounding and daycounter of the YieldCurve
        zeroCpnBondCleanPrice2 = cleanPriceFromZSpread(zeroCpnBond2,
                                  self.yieldCurve,
                                  self.spread,
                                  Actual365Fixed(),
                                  self.compounding, Annual,
                                  zeroCpnBondSettlementDate2)
        error15 = abs(zeroCpnBondImpliedValue2-zeroCpnBondCleanPrice2)

        self.assertFalse(error15>tolerance,
            "wrong clean price for zero coupon bond:"
            + "\n  zero cpn implied value: " + str(zeroCpnBondImpliedValue2)
            + "\n  zero cpn price: " + str(zeroCpnBondCleanPrice2)
            + "\n  error:                 " + str(error15)
            + "\n  tolerance:             " + str(tolerance))
        



    def testSpecializedBondVsGenericBond(self) :
        """Testing clean and dirty prices for specialized bond against equivalent generic bond..."""

        bondCalendar = TARGET()
        settlementDays = 3
        fixingDays = 2
        inArrears = False

        ## Fixed Underlying bond (Isin: DE0001135275 DBR 4 01/04/37)
        ## maturity doesn't occur on a business day
        fixedBondStartDate1 = Date(4,January,2005)
        fixedBondMaturityDate1 = Date(4,January,2037)
        fixedBondSchedule1 = Schedule(fixedBondStartDate1,
                                    fixedBondMaturityDate1,
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBondLeg1 = list(FixedRateLeg(fixedBondSchedule1, ActualActual(ActualActual.ISDA), [self.faceAmount],
                                     [0.04]))
        fixedbondRedemption1 = bondCalendar.adjust(fixedBondMaturityDate1, Following)
        fixedBondLeg1.append(SimpleCashFlow(100.0, fixedbondRedemption1))
        ## generic bond
        fixedBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                         fixedBondMaturityDate1, fixedBondStartDate1,
                         fixedBondLeg1)
        bondEngine = DiscountingBondEngine(self.termStructure)
        fixedBond1.setPricingEngine(bondEngine)

        ## equivalent specialized fixed rate bond
        fixedSpecializedBond1 = FixedRateBond(settlementDays, self.faceAmount, fixedBondSchedule1,
                          [0.04], ActualActual(ActualActual.ISDA), Following,
                          100.0, Date(4,January,2005))
        fixedSpecializedBond1.setPricingEngine(bondEngine)

        fixedBondTheoValue1 = fixedBond1.cleanPrice()
        fixedSpecializedBondTheoValue1 = fixedSpecializedBond1.cleanPrice()
        tolerance = 1.0e-13
        error1 = abs(fixedBondTheoValue1-fixedSpecializedBondTheoValue1)

        self.assertFalse(error1>tolerance,
            "wrong clean price for fixed bond:"
            + "\n  specialized fixed rate bond's theo clean price: "  + str(fixedBondTheoValue1)
            + "\n  generic equivalent bond's theo clean price: "  + str(fixedSpecializedBondTheoValue1)
            + "\n  error:                 " + str(error1)
            + "\n  tolerance:             " + str(tolerance))
        
        fixedBondTheoDirty1 = fixedBondTheoValue1+fixedBond1.accruedAmount()
        fixedSpecializedTheoDirty1 = fixedSpecializedBondTheoValue1+ fixedSpecializedBond1.accruedAmount()
        error2 = abs(fixedBondTheoDirty1-fixedSpecializedTheoDirty1)

        self.assertFalse(error2>tolerance,
            "wrong dirty price for fixed bond:"
            + "\n  specialized fixed rate bond's theo dirty price: " +  str(fixedBondTheoDirty1)
            + "\n  generic equivalent bond's theo dirty price: " + str(fixedSpecializedTheoDirty1)
            + "\n  error:                 " + str(error2)
            + "\n  tolerance:             " + str(tolerance))
        

        ## Fixed Underlying bond (Isin: IT0006527060 IBRD 5 02/05/19)
        ## maturity occurs on a business day
        fixedBondStartDate2 = Date(5,February,2005)
        fixedBondMaturityDate2 = Date(5,February,2019)
        fixedBondSchedule2 = Schedule(fixedBondStartDate2,
                                    fixedBondMaturityDate2,
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBondLeg2 = list(FixedRateLeg(fixedBondSchedule2, Thirty360(Thirty360.BondBasis),[self.faceAmount],[0.05]))
        fixedbondRedemption2 = bondCalendar.adjust(fixedBondMaturityDate2, Following)
        fixedBondLeg2.append(SimpleCashFlow(100.0, fixedbondRedemption2))

        ## generic bond
        fixedBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 fixedBondMaturityDate2, fixedBondStartDate2, fixedBondLeg2)
        fixedBond2.setPricingEngine(bondEngine)

        ## equivalent specialized fixed rate bond
        fixedSpecializedBond2 = FixedRateBond(settlementDays, self.faceAmount, fixedBondSchedule2,
                          [0.05],Thirty360(Thirty360.BondBasis), Following,
                          100.0, Date(5,February,2005))
        fixedSpecializedBond2.setPricingEngine(bondEngine)

        fixedBondTheoValue2 = fixedBond2.cleanPrice()
        fixedSpecializedBondTheoValue2 = fixedSpecializedBond2.cleanPrice()

        error3 = abs(fixedBondTheoValue2-fixedSpecializedBondTheoValue2)
        self.assertFalse(error3>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  specialized fixed rate bond's theo clean price: "   + str(fixedBondTheoValue2)
            + "\n  generic equivalent bond's theo clean price: " + str(fixedSpecializedBondTheoValue2)
            + "\n  error:                 " + str(error3)
            + "\n  tolerance:             " + str(tolerance))
        
        fixedBondTheoDirty2 = fixedBondTheoValue2+ fixedBond2.accruedAmount()
        fixedSpecializedBondTheoDirty2 = fixedSpecializedBondTheoValue2+ fixedSpecializedBond2.accruedAmount()

        error4 = abs(fixedBondTheoDirty2-fixedSpecializedBondTheoDirty2)
        self.assertFalse(error4>tolerance, 
            "wrong dirty price for fixed bond:"
            + "\n  specialized fixed rate bond's dirty clean price: "  + str(fixedBondTheoDirty2)
            + "\n  generic equivalent bond's theo dirty price: "  + str(fixedSpecializedBondTheoDirty2)
            + "\n  error:                 " + str(error4)
            + "\n  tolerance:             " + str(tolerance))
        

        ## FRN Underlying bond (Isin: IT0003543847 ISPIM 0 09/29/13)
        ## maturity doesn't occur on a business day
        floatingBondStartDate1 = Date(29,September,2003)
        floatingBondMaturityDate1 = Date(29,September,2013)
        floatingBondSchedule1 = Schedule(floatingBondStartDate1,
                                       floatingBondMaturityDate1,
                                      Period(Semiannual), bondCalendar,
                                       Unadjusted, Unadjusted,
                                       DateGeneration.Backward, False)
        floatingBondLeg1 = list(IborLeg([self.faceAmount],floatingBondSchedule1, self.iborIndex,
                                   Actual360(),Following,[fixingDays],[],[0.0056],[],[],inArrears))
        floatingbondRedemption1 = bondCalendar.adjust(floatingBondMaturityDate1, Following)
        floatingBondLeg1.append(SimpleCashFlow(100.0, floatingbondRedemption1))
        ## generic bond
        floatingBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 floatingBondMaturityDate1, floatingBondStartDate1,
                 floatingBondLeg1)
        floatingBond1.setPricingEngine(bondEngine)

        ## equivalent specialized floater
        floatingSpecializedBond1 = FloatingRateBond(settlementDays, self.faceAmount,
                                floatingBondSchedule1,
                                self.iborIndex, Actual360(),
                                Following, fixingDays,
                                [1],
                                [0.0056],
                                [], [],
                                inArrears,
                                100.0, Date(29,September,2003))
        floatingSpecializedBond1.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond1.cashflows(), self.pricer)
        setCouponPricer(floatingSpecializedBond1.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(27,March,2007), 0.0402)
        floatingBondTheoValue1 = floatingBond1.cleanPrice()
        floatingSpecializedBondTheoValue1 = floatingSpecializedBond1.cleanPrice()

        error5 = abs(floatingBondTheoValue1-
                                floatingSpecializedBondTheoValue1)
        self.assertFalse(error5>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  generic fixed rate bond's theo clean price: " + str(floatingBondTheoValue1)
            + "\n  equivalent specialized bond's theo clean price: " + str(floatingSpecializedBondTheoValue1)
            + "\n  error:                 " + str(error5)
            + "\n  tolerance:             " + str(tolerance))
        
        floatingBondTheoDirty1 = floatingBondTheoValue1+ floatingBond1.accruedAmount()
        floatingSpecializedBondTheoDirty1 = floatingSpecializedBondTheoValue1 + floatingSpecializedBond1.accruedAmount()
        error6 = abs(floatingBondTheoDirty1-
                                floatingSpecializedBondTheoDirty1)
        self.assertFalse(error6>tolerance,
            "wrong dirty price for frn bond:"
            + "\n  generic frn bond's dirty clean price: " + str(floatingBondTheoDirty1)
            + "\n  equivalent specialized bond's theo dirty price: " + str(floatingSpecializedBondTheoDirty1)
            + "\n  error:                 " + str(error6)
            + "\n  tolerance:             " + str(tolerance))
   

        ## FRN Underlying bond (Isin: XS0090566539 COE 0 09/24/18)
        ## maturity occurs on a business day
        floatingBondStartDate2 = Date(24,September,2004)
        floatingBondMaturityDate2 = Date(24,September,2018)
        floatingBondSchedule2 = Schedule(floatingBondStartDate2,
                                       floatingBondMaturityDate2,
                                       Period(Semiannual), bondCalendar,
                                       ModifiedFollowing, ModifiedFollowing,
                                       DateGeneration.Backward, False)
        floatingBondLeg2 = list(IborLeg([self.faceAmount], floatingBondSchedule2, self.iborIndex,
                                       Actual360(),ModifiedFollowing,[fixingDays], [],[0.0025],[],[],inArrears))
        floatingbondRedemption2 = bondCalendar.adjust(floatingBondMaturityDate2, ModifiedFollowing)
        floatingBondLeg2.append(SimpleCashFlow(100.0, floatingbondRedemption2))
        ## generic bond
        floatingBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 floatingBondMaturityDate2, floatingBondStartDate2,
                 floatingBondLeg2)
        floatingBond2.setPricingEngine(bondEngine)

        ## equivalent specialized floater
        floatingSpecializedBond2 = FloatingRateBond(settlementDays, self.faceAmount,
                             floatingBondSchedule2,
                             self.iborIndex, Actual360(),
                             ModifiedFollowing, fixingDays,
                             [1], [0.0025],
                             [], [],
                             inArrears,
                             100.0, Date(24,September,2004))
        floatingSpecializedBond2.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond2.cashflows(), self.pricer)
        setCouponPricer(floatingSpecializedBond2.cashflows(), self.pricer)

        self.iborIndex.addFixing(Date(22,March,2007), 0.04013)

        floatingBondTheoValue2 = floatingBond2.cleanPrice()
        floatingSpecializedBondTheoValue2 = floatingSpecializedBond2.cleanPrice()

        error7 = abs(floatingBondTheoValue2-floatingSpecializedBondTheoValue2)
        self.assertFalse(error7>tolerance, 
            "wrong clean price for floater bond:"
            + "\n  generic floater bond's theo clean price: " + str(floatingBondTheoValue2)
            + "\n  equivalent specialized bond's theo clean price: " + str(floatingSpecializedBondTheoValue2)
            + "\n  error:                 " + str(error7)
            + "\n  tolerance:             " + str(tolerance))
        
        floatingBondTheoDirty2 = floatingBondTheoValue2+ floatingBond2.accruedAmount()
        floatingSpecializedTheoDirty2 = floatingSpecializedBondTheoValue2+ floatingSpecializedBond2.accruedAmount()

        error8 = abs(floatingBondTheoDirty2-floatingSpecializedTheoDirty2)
        self.assertFalse(error8>tolerance, 
            "wrong dirty price for floater bond:"
            + "\n  generic floater bond's theo dirty price: " + str(floatingBondTheoDirty2)
            + "\n  equivalent specialized  bond's theo dirty price: " + str(floatingSpecializedTheoDirty2)
            + "\n  error:                 " + str(error8)
            + "\n  tolerance:             " + str(tolerance))
        


        ## CMS Underlying bond (Isin: XS0228052402 CRDIT 0 8/22/20)
        ## maturity doesn't occur on a business day
        cmsBondStartDate1 = Date(22,August,2005)
        cmsBondMaturityDate1 = Date(22,August,2020)
        cmsBondSchedule1 = Schedule(cmsBondStartDate1,
                                  cmsBondMaturityDate1,
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBondLeg1 = list(CmsLeg([self.faceAmount],cmsBondSchedule1, self.swapIndex,
                                 Thirty360(), Following, [fixingDays], [],[],[0.055],[0.025],inArrears))
        cmsbondRedemption1 = bondCalendar.adjust(cmsBondMaturityDate1, Following)
        cmsBondLeg1.append(SimpleCashFlow(100.0, cmsbondRedemption1))
        ## generic cms bond
        cmsBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 cmsBondMaturityDate1, cmsBondStartDate1, cmsBondLeg1)
        cmsBond1.setPricingEngine(bondEngine)

        ## equivalent specialized cms bond
        cmsSpecializedBond1 = CmsRateBond(settlementDays, self.faceAmount, cmsBondSchedule1,
                    self.swapIndex, Thirty360(),
                    Following, fixingDays,
                    [1.0], [0.0],
                    [0.055],[0.025],
                    inArrears,
                    100.0, Date(22,August,2005))
        cmsSpecializedBond1.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond1.cashflows(), self.cmspricer)
        setCouponPricer(cmsSpecializedBond1.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(18,August,2006), 0.04158)
        cmsBondTheoValue1 = cmsBond1.cleanPrice()
        cmsSpecializedBondTheoValue1 = cmsSpecializedBond1.cleanPrice()
        error9 = abs(cmsBondTheoValue1-cmsSpecializedBondTheoValue1)
        self.assertFalse(error9>tolerance, 
            "wrong clean price for cms bond:"
            + "\n  generic cms bond's theo clean price: " + str(cmsBondTheoValue1)
            + "\n  equivalent specialized bond's theo clean price: " + str(cmsSpecializedBondTheoValue1)
            + "\n  error:                 " + str(error9)
            + "\n  tolerance:             " + str(tolerance))
        
        cmsBondTheoDirty1 = cmsBondTheoValue1+cmsBond1.accruedAmount()
        cmsSpecializedBondTheoDirty1 = cmsSpecializedBondTheoValue1+ cmsSpecializedBond1.accruedAmount()
        error10 = abs(cmsBondTheoDirty1-cmsSpecializedBondTheoDirty1)
        self.assertFalse(error10>tolerance,
            "wrong dirty price for cms bond:"
            + "\n generic cms bond's theo dirty price: " + str(cmsBondTheoDirty1)
            + "\n  specialized cms bond's theo dirty price: " + str(cmsSpecializedBondTheoDirty1)
            + "\n  error:                 " + str(error10)
            + "\n  tolerance:             " + str(tolerance))
        

        ## CMS Underlying bond (Isin: XS0218766664 ISPIM 0 5/6/15)
        ## maturity occurs on a business day
        cmsBondStartDate2 = Date(6,May,2005)
        cmsBondMaturityDate2 = Date(6,May,2015)
        cmsBondSchedule2 = Schedule(cmsBondStartDate2,
                                  cmsBondMaturityDate2,
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBondLeg2 = list(CmsLeg([self.faceAmount],cmsBondSchedule2, self.swapIndex,
                            Thirty360(),Following,[fixingDays],[0.84],[],[],[],inArrears))
        cmsbondRedemption2 = bondCalendar.adjust(cmsBondMaturityDate2, Following)
        cmsBondLeg2.append(SimpleCashFlow(100.0, cmsbondRedemption2))
        ## generic bond
        cmsBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 cmsBondMaturityDate2, cmsBondStartDate2, cmsBondLeg2)
        cmsBond2.setPricingEngine(bondEngine)
        
        ## equivalent specialized cms bond
        cmsSpecializedBond2 = CmsRateBond(settlementDays, self.faceAmount, cmsBondSchedule2,
                    self.swapIndex, Thirty360(),
                    Following, fixingDays,
                    [0.84], [0.0],
                    [], [],
                    inArrears,
                    100.0, Date(6,May,2005))
        cmsSpecializedBond2.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond2.cashflows(), self.cmspricer)
        setCouponPricer(cmsSpecializedBond2.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(4,May,2006), 0.04217)
        cmsBondTheoValue2 = cmsBond2.cleanPrice()
        cmsSpecializedBondTheoValue2 = cmsSpecializedBond2.cleanPrice()

        error11 = abs(cmsBondTheoValue2-cmsSpecializedBondTheoValue2)
        self.assertFalse(error11>tolerance, 
            "wrong clean price for cms bond:"
            + "\n  generic cms bond's theo clean price: " + str(cmsBondTheoValue2)
            + "\n  cms bond's theo clean price: " + str(cmsSpecializedBondTheoValue2)
            + "\n  error:                 " + str(error11) 
            + "\n  tolerance:             " + str(tolerance))
        
        cmsBondTheoDirty2 = cmsBondTheoValue2+cmsBond2.accruedAmount()
        cmsSpecializedBondTheoDirty2 = cmsSpecializedBondTheoValue2+cmsSpecializedBond2.accruedAmount()
        error12 = abs(cmsBondTheoDirty2-cmsSpecializedBondTheoDirty2)
        self.assertFalse(error12>tolerance,
            "wrong dirty price for cms bond:"
            + "\n  generic cms bond's dirty price: " + str(cmsBondTheoDirty2)
            + "\n  specialized cms bond's theo dirty price: " + str(cmsSpecializedBondTheoDirty2)
            + "\n  error:                 " + str(error12)
            + "\n  tolerance:             " + str(tolerance))
        
        
        ## Zero Coupon bond (Isin: DE0004771662 IBRD 0 12/20/15)
        ## maturity doesn't occur on a business day
        zeroCpnBondStartDate1 = Date(19,December,1985)
        zeroCpnBondMaturityDate1 = Date(20,December,2015)
        zeroCpnBondRedemption1 = bondCalendar.adjust(zeroCpnBondMaturityDate1,
                                                          Following)
        zeroCpnBondLeg1 = Leg([SimpleCashFlow(100.0, zeroCpnBondRedemption1)])

        ## generic bond
        zeroCpnBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 zeroCpnBondMaturityDate1, zeroCpnBondStartDate1, zeroCpnBondLeg1)
        zeroCpnBond1.setPricingEngine(bondEngine)

        ## specialized zerocpn bond
        zeroCpnSpecializedBond1 = ZeroCouponBond(settlementDays, bondCalendar, self.faceAmount,
                      Date(20,December,2015),
                      Following,
                      100.0, Date(19,December,1985))
        zeroCpnSpecializedBond1.setPricingEngine(bondEngine)

        zeroCpnBondTheoValue1 = zeroCpnBond1.cleanPrice()
        zeroCpnSpecializedBondTheoValue1 = zeroCpnSpecializedBond1.cleanPrice()

        error13 = abs(zeroCpnBondTheoValue1-zeroCpnSpecializedBondTheoValue1)
        self.assertFalse(error13>tolerance, 
            "wrong clean price for zero coupon bond:"
            + "\n  generic zero bond's clean price: " + str(zeroCpnBondTheoValue1)
            + "\n  specialized zero bond's clean price: " + str(zeroCpnSpecializedBondTheoValue1)
            + "\n  error:                 " + str(error13)
            + "\n  tolerance:             " + str(tolerance))
        
        zeroCpnBondTheoDirty1 = zeroCpnBondTheoValue1+ zeroCpnBond1.accruedAmount()
        zeroCpnSpecializedBondTheoDirty1 = zeroCpnSpecializedBondTheoValue1+ \
            zeroCpnSpecializedBond1.accruedAmount()
        error14 = abs(zeroCpnBondTheoDirty1-zeroCpnSpecializedBondTheoDirty1)
        self.assertFalse(error14>tolerance, 
            "wrong dirty price for zero bond:"
            + "\n  generic zerocpn bond's dirty price: " + str(zeroCpnBondTheoDirty1)
            + "\n  specialized zerocpn bond's clean price: " + str(zeroCpnSpecializedBondTheoDirty1)
            + "\n  error:                 " + str(error14)
            + "\n  tolerance:             " + str(tolerance))
        

        ## Zero Coupon bond (Isin: IT0001200390 ISPIM 0 02/17/28)
        ## maturity occurs on a business day
        zeroCpnBondStartDate2 = Date(17,February,1998)
        zeroCpnBondMaturityDate2 = Date(17,February,2028)
        zerocpbondRedemption2 = bondCalendar.adjust(zeroCpnBondMaturityDate2,
                                                          Following)
        zeroCpnBondLeg2 = Leg([SimpleCashFlow(100.0, zerocpbondRedemption2)])
        ## generic bond
        zeroCpnBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 zeroCpnBondMaturityDate2, zeroCpnBondStartDate2, zeroCpnBondLeg2)
        zeroCpnBond2.setPricingEngine(bondEngine)

        ## specialized zerocpn bond
        zeroCpnSpecializedBond2 = ZeroCouponBond(settlementDays, bondCalendar, self.faceAmount,
                       Date(17,February,2028),
                       Following,
                       100.0, Date(17,February,1998))
        zeroCpnSpecializedBond2.setPricingEngine(bondEngine)

        zeroCpnBondTheoValue2 = zeroCpnBond2.cleanPrice()
        zeroCpnSpecializedBondTheoValue2 = zeroCpnSpecializedBond2.cleanPrice()

        error15 = abs(zeroCpnBondTheoValue2 -zeroCpnSpecializedBondTheoValue2)
        self.assertFalse(error15>tolerance, 
            "wrong clean price for zero coupon bond:"
            + "\n  generic zerocpn bond's clean price: " + str(zeroCpnBondTheoValue2)
            + "\n  specialized zerocpn bond's clean price: " + str(zeroCpnSpecializedBondTheoValue2)
            + "\n  error:                 " + str(error15)
            + "\n  tolerance:             " + str(tolerance))
        
        zeroCpnBondTheoDirty2 = zeroCpnBondTheoValue2+ zeroCpnBond2.accruedAmount()

        zeroCpnSpecializedBondTheoDirty2 = \
            zeroCpnSpecializedBondTheoValue2+ \
            zeroCpnSpecializedBond2.accruedAmount()

        error16 = abs(zeroCpnBondTheoDirty2-zeroCpnSpecializedBondTheoDirty2)
        self.assertFalse(error16>tolerance, 
            "wrong dirty price for zero coupon bond:"
            + "\n  generic zerocpn bond's dirty price: " + str(zeroCpnBondTheoDirty2)
            + "\n  specialized zerocpn bond's dirty price: " + str(zeroCpnSpecializedBondTheoDirty2)
            + "\n  error:                 " + str(error16)
            + "\n  tolerance:             " + str(tolerance))
        



    def testSpecializedBondVsGenericBondUsingAsw(self) :
        """Testing asset-swap prices and spreads for specialized bond against equivalent generic bond..."""
        
        bondCalendar = TARGET()
        settlementDays = 3
        fixingDays = 2
        payFixedRate = True
        parAssetSwap = True
        inArrears = False

        ## Fixed bond (Isin: DE0001135275 DBR 4 01/04/37)
        ## maturity doesn't occur on a business day
        fixedBondStartDate1 = Date(4,January,2005)
        fixedBondMaturityDate1 = Date(4,January,2037)
        fixedBondSchedule1 = Schedule(fixedBondStartDate1,
                                    fixedBondMaturityDate1,
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBondLeg1 = list(FixedRateLeg(fixedBondSchedule1,
                             ActualActual(ActualActual.ISDA), [self.faceAmount],[0.04]))
        fixedbondRedemption1 = bondCalendar.adjust(fixedBondMaturityDate1, Following)
        fixedBondLeg1.append(SimpleCashFlow(100.0, fixedbondRedemption1))
        ## generic bond
        fixedBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 fixedBondMaturityDate1, fixedBondStartDate1, fixedBondLeg1)

        bondEngine = DiscountingBondEngine(self.termStructure)
        swapEngine = DiscountingSwapEngine(self.termStructure, False)
        fixedBond1.setPricingEngine(bondEngine)

        ## equivalent specialized fixed rate bond
        fixedSpecializedBond1 = FixedRateBond(settlementDays, self.faceAmount, fixedBondSchedule1,
                          [0.04], ActualActual(ActualActual.ISDA), Following,
                          100.0, Date(4,January,2005))
        fixedSpecializedBond1.setPricingEngine(bondEngine)

        fixedBondPrice1 = fixedBond1.cleanPrice()
        fixedSpecializedBondPrice1 = fixedSpecializedBond1.cleanPrice()
        fixedBondAssetSwap1 = AssetSwap(payFixedRate,
                                      fixedBond1, fixedBondPrice1,
                                      self.iborIndex, self.nonnullspread,
                                      Schedule(),
                                      self.iborIndex.dayCounter(),
                                      parAssetSwap)
        fixedBondAssetSwap1.setPricingEngine(swapEngine)
        fixedSpecializedBondAssetSwap1 = AssetSwap(payFixedRate,
                                                 fixedSpecializedBond1,
                                                 fixedSpecializedBondPrice1,
                                                 self.iborIndex,
                                                 self.nonnullspread,
                                                 Schedule(),
                                                 self.iborIndex.dayCounter(),
                                                 parAssetSwap)
        fixedSpecializedBondAssetSwap1.setPricingEngine(swapEngine)
        fixedBondAssetSwapPrice1 = fixedBondAssetSwap1.fairCleanPrice()
        fixedSpecializedBondAssetSwapPrice1 = fixedSpecializedBondAssetSwap1.fairCleanPrice()
        tolerance = 1.0e-13
        error1 = abs(fixedBondAssetSwapPrice1-fixedSpecializedBondAssetSwapPrice1)
        self.assertFalse(error1>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  generic  fixed rate bond's  clean price: " + str(fixedBondAssetSwapPrice1)
            + "\n  equivalent specialized bond's clean price: " + str(fixedSpecializedBondAssetSwapPrice1)
            + "\n  error:                 " + str(error1)
            + "\n  tolerance:             " + str(tolerance))
        
        ## market executable price as of 4th sept 2007
        fixedBondMktPrice1= 91.832
        fixedBondASW1 = AssetSwap(payFixedRate,
                                fixedBond1, fixedBondMktPrice1,
                                self.iborIndex, self.spread,
                                Schedule(),
                                self.iborIndex.dayCounter(),
                                parAssetSwap)
        fixedBondASW1.setPricingEngine(swapEngine)
        fixedSpecializedBondASW1 = AssetSwap(payFixedRate,
                                           fixedSpecializedBond1,
                                           fixedBondMktPrice1,
                                           self.iborIndex, self.spread,
                                           Schedule(),
                                           self.iborIndex.dayCounter(),
                                           parAssetSwap)
        fixedSpecializedBondASW1.setPricingEngine(swapEngine)
        fixedBondASWSpread1 = fixedBondASW1.fairSpread()
        fixedSpecializedBondASWSpread1 = fixedSpecializedBondASW1.fairSpread()
        error2 = abs(fixedBondASWSpread1-fixedSpecializedBondASWSpread1)
        self.assertFalse(error2>tolerance, 
            "wrong asw spread  for fixed bond:"
            + "\n  generic  fixed rate bond's  asw spread: " + str(fixedBondASWSpread1)
            + "\n  equivalent specialized bond's asw spread: " + str(fixedSpecializedBondASWSpread1)
            + "\n  error:                 " + str(error2)
            + "\n  tolerance:             " + str(tolerance))
        

         ##Fixed bond (Isin: IT0006527060 IBRD 5 02/05/19)
         ##maturity occurs on a business day

        fixedBondStartDate2 = Date(5,February,2005)
        fixedBondMaturityDate2 = Date(5,February,2019)
        fixedBondSchedule2 = Schedule(fixedBondStartDate2,
                                    fixedBondMaturityDate2,
                                    Period(Annual), bondCalendar,
                                    Unadjusted, Unadjusted,
                                    DateGeneration.Backward, False)
        fixedBondLeg2 = list(FixedRateLeg(fixedBondSchedule2, Thirty360(Thirty360.BondBasis),[self.faceAmount],
                                    [0.05]))
        fixedbondRedemption2 = bondCalendar.adjust(fixedBondMaturityDate2, Following)
        fixedBondLeg2.append(SimpleCashFlow(100.0, fixedbondRedemption2))

        ## generic bond
        fixedBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 fixedBondMaturityDate2, fixedBondStartDate2, fixedBondLeg2)
        fixedBond2.setPricingEngine(bondEngine)

        ## equivalent specialized fixed rate bond
        fixedSpecializedBond2 = FixedRateBond(settlementDays, self.faceAmount, fixedBondSchedule2,
                          [0.05], Thirty360(Thirty360.BondBasis), Following,
                          100.0, Date(5,February,2005))
        fixedSpecializedBond2.setPricingEngine(bondEngine)

        fixedBondPrice2 = fixedBond2.cleanPrice()
        fixedSpecializedBondPrice2 = fixedSpecializedBond2.cleanPrice()
        fixedBondAssetSwap2 = AssetSwap(payFixedRate,
                                      fixedBond2, fixedBondPrice2,
                                      self.iborIndex, self.nonnullspread,
                                      Schedule(),
                                      self.iborIndex.dayCounter(),
                                      parAssetSwap)
        fixedBondAssetSwap2.setPricingEngine(swapEngine)
        fixedSpecializedBondAssetSwap2 = AssetSwap(payFixedRate,
                                                 fixedSpecializedBond2,
                                                 fixedSpecializedBondPrice2,
                                                 self.iborIndex,
                                                 self.nonnullspread,
                                                 Schedule(),
                                                 self.iborIndex.dayCounter(),
                                                 parAssetSwap)
        fixedSpecializedBondAssetSwap2.setPricingEngine(swapEngine)
        fixedBondAssetSwapPrice2 = fixedBondAssetSwap2.fairCleanPrice()
        fixedSpecializedBondAssetSwapPrice2 = fixedSpecializedBondAssetSwap2.fairCleanPrice()

        error3 = abs(fixedBondAssetSwapPrice2-fixedSpecializedBondAssetSwapPrice2)
        self.assertFalse(error3>tolerance, 
            "wrong clean price for fixed bond:"
            + "\n  generic  fixed rate bond's clean price: " + str(fixedBondAssetSwapPrice2)
            + "\n  equivalent specialized  bond's clean price: " + str(fixedSpecializedBondAssetSwapPrice2)
            + "\n  error:                 " + str(error3)
            + "\n  tolerance:             " + str(tolerance))
        
        ## market executable price as of 4th sept 2007
        fixedBondMktPrice2= 102.178
        fixedBondASW2 = AssetSwap(payFixedRate,
                                fixedBond2, fixedBondMktPrice2,
                                self.iborIndex, self.spread,
                                Schedule(),
                                self.iborIndex.dayCounter(),
                                parAssetSwap)
        fixedBondASW2.setPricingEngine(swapEngine)
        fixedSpecializedBondASW2 = AssetSwap(payFixedRate,
                                           fixedSpecializedBond2,
                                           fixedBondMktPrice2,
                                           self.iborIndex, self.spread,
                                           Schedule(),
                                           self.iborIndex.dayCounter(),
                                           parAssetSwap)
        fixedSpecializedBondASW2.setPricingEngine(swapEngine)
        fixedBondASWSpread2 = fixedBondASW2.fairSpread()
        fixedSpecializedBondASWSpread2 = fixedSpecializedBondASW2.fairSpread()
        error4 = abs(fixedBondASWSpread2-fixedSpecializedBondASWSpread2)
        self.assertFalse(error4>tolerance, 
            "wrong asw spread for fixed bond:"
            + "\n  generic  fixed rate bond's  asw spread: " + str(fixedBondASWSpread2)
            + "\n  equivalent specialized bond's asw spread: " + str(fixedSpecializedBondASWSpread2)
            + "\n  error:                 " + str(error4)
            + "\n  tolerance:             " + str(tolerance))
        


        ##FRN bond (Isin: IT0003543847 ISPIM 0 09/29/13)
        ##maturity doesn't occur on a business day
        floatingBondStartDate1 = Date(29,September,2003)
        floatingBondMaturityDate1 = Date(29,September,2013)
        floatingBondSchedule1 = Schedule(floatingBondStartDate1,
                                       floatingBondMaturityDate1,
                                       Period(Semiannual), bondCalendar,
                                       Unadjusted, Unadjusted,
                                       DateGeneration.Backward, False)
        floatingBondLeg1 = list(IborLeg([self.faceAmount],floatingBondSchedule1, self.iborIndex,
                                   Actual360(), Following,[fixingDays],[],[0.0056],[],[],inArrears))
        floatingbondRedemption1 = bondCalendar.adjust(floatingBondMaturityDate1, Following)
        floatingBondLeg1.append(SimpleCashFlow(100.0, floatingbondRedemption1))
        ## generic bond
        floatingBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 floatingBondMaturityDate1, floatingBondStartDate1,
                 floatingBondLeg1)
        floatingBond1.setPricingEngine(bondEngine)

        ## equivalent specialized floater
        floatingSpecializedBond1 = FloatingRateBond(settlementDays, self.faceAmount,
                                floatingBondSchedule1,
                                self.iborIndex, Actual360(),
                                Following, fixingDays,
                                [1],
                                [0.0056],
                                [], [],
                                inArrears,
                                100.0, Date(29,September,2003))
        floatingSpecializedBond1.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond1.cashflows(), self.pricer)
        setCouponPricer(floatingSpecializedBond1.cashflows(), self.pricer)
        self.iborIndex.addFixing(Date(27,March,2007), 0.0402)
        floatingBondPrice1 = floatingBond1.cleanPrice()
        floatingSpecializedBondPrice1= floatingSpecializedBond1.cleanPrice()
        floatingBondAssetSwap1 = AssetSwap(payFixedRate,
                                         floatingBond1, floatingBondPrice1,
                                         self.iborIndex, self.nonnullspread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        floatingBondAssetSwap1.setPricingEngine(swapEngine)
        floatingSpecializedBondAssetSwap1 = AssetSwap(payFixedRate,
                                                    floatingSpecializedBond1,
                                                    floatingSpecializedBondPrice1,
                                                    self.iborIndex,
                                                    self.nonnullspread,
                                                    Schedule(),
                                                    self.iborIndex.dayCounter(),
                                                    parAssetSwap)
        floatingSpecializedBondAssetSwap1.setPricingEngine(swapEngine)
        floatingBondAssetSwapPrice1 = floatingBondAssetSwap1.fairCleanPrice()
        floatingSpecializedBondAssetSwapPrice1 =  floatingSpecializedBondAssetSwap1.fairCleanPrice()

        error5 = abs(floatingBondAssetSwapPrice1-floatingSpecializedBondAssetSwapPrice1)
        self.assertFalse(error5>tolerance, 
            "wrong clean price for frnbond:"
            + "\n  generic frn rate bond's clean price: " + str(floatingBondAssetSwapPrice1)
            + "\n  equivalent specialized  bond's price: " + str(floatingSpecializedBondAssetSwapPrice1)
            + "\n  error:                 " + str(error5)
            + "\n  tolerance:             " + str(tolerance))
        
        ## market executable price as of 4th sept 2007
        floatingBondMktPrice1= 101.33
        floatingBondASW1 = AssetSwap(payFixedRate,
                                   floatingBond1, floatingBondMktPrice1,
                                   self.iborIndex, self.spread,
                                   Schedule(),
                                   self.iborIndex.dayCounter(),
                                   parAssetSwap)
        floatingBondASW1.setPricingEngine(swapEngine)
        floatingSpecializedBondASW1 = AssetSwap(payFixedRate,
                                              floatingSpecializedBond1,
                                              floatingBondMktPrice1,
                                              self.iborIndex, self.spread,
                                              Schedule(),
                                              self.iborIndex.dayCounter(),
                                              parAssetSwap)
        floatingSpecializedBondASW1.setPricingEngine(swapEngine)
        floatingBondASWSpread1 = floatingBondASW1.fairSpread()
        floatingSpecializedBondASWSpread1 = floatingSpecializedBondASW1.fairSpread()
        error6 = abs(floatingBondASWSpread1-floatingSpecializedBondASWSpread1)
        self.assertFalse(error6>tolerance, 
            "wrong asw spread for fixed bond:"
            + "\n  generic  frn rate bond's  asw spread: " + str(floatingBondASWSpread1)
            + "\n  equivalent specialized bond's asw spread: " + str(floatingSpecializedBondASWSpread1)
            + "\n  error:                 " + str(error6)
            + "\n  tolerance:             " + str(tolerance))
     
        ##FRN bond (Isin: XS0090566539 COE 0 09/24/18)
        ##maturity occurs on a business day
        floatingBondStartDate2 = Date(24,September,2004)
        floatingBondMaturityDate2 = Date(24,September,2018)
        floatingBondSchedule2 = Schedule(floatingBondStartDate2,
                                       floatingBondMaturityDate2,
                                       Period(Semiannual), bondCalendar,
                                       ModifiedFollowing, ModifiedFollowing,
                                       DateGeneration.Backward, False)
        floatingBondLeg2 = list(IborLeg([self.faceAmount],floatingBondSchedule2, self.iborIndex,
                                        Actual360(),ModifiedFollowing,[fixingDays],[],[0.0025],[],[],inArrears))
        floatingbondRedemption2 = bondCalendar.adjust(floatingBondMaturityDate2, ModifiedFollowing)
        floatingBondLeg2.append(SimpleCashFlow(100.0, floatingbondRedemption2))
        ## generic bond
        floatingBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 floatingBondMaturityDate2, floatingBondStartDate2,
                 floatingBondLeg2)
        floatingBond2.setPricingEngine(bondEngine)

        ## equivalent specialized floater
        floatingSpecializedBond2 = FloatingRateBond(settlementDays, self.faceAmount,
                             floatingBondSchedule2,
                             self.iborIndex, Actual360(),
                             ModifiedFollowing, fixingDays,
                             [1],
                             [0.0025],
                             [], [],
                             inArrears,
                             100.0, Date(24,September,2004))
        floatingSpecializedBond2.setPricingEngine(bondEngine)

        setCouponPricer(floatingBond2.cashflows(), self.pricer)
        setCouponPricer(floatingSpecializedBond2.cashflows(), self.pricer)

        self.iborIndex.addFixing(Date(22,March,2007), 0.04013)

        floatingBondPrice2 = floatingBond2.cleanPrice()
        floatingSpecializedBondPrice2= floatingSpecializedBond2.cleanPrice()
        floatingBondAssetSwap2 = AssetSwap(payFixedRate,
                                         floatingBond2, floatingBondPrice2,
                                         self.iborIndex, self.nonnullspread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        floatingBondAssetSwap2.setPricingEngine(swapEngine)
        floatingSpecializedBondAssetSwap2 = AssetSwap(payFixedRate,
                                                    floatingSpecializedBond2,
                                                    floatingSpecializedBondPrice2,
                                                    self.iborIndex,
                                                    self.nonnullspread,
                                                    Schedule(),
                                                    self.iborIndex.dayCounter(),
                                                    parAssetSwap)
        floatingSpecializedBondAssetSwap2.setPricingEngine(swapEngine)
        floatingBondAssetSwapPrice2 = floatingBondAssetSwap2.fairCleanPrice()
        floatingSpecializedBondAssetSwapPrice2 = floatingSpecializedBondAssetSwap2.fairCleanPrice()
        error7 = abs(floatingBondAssetSwapPrice2-floatingSpecializedBondAssetSwapPrice2)
        self.assertFalse(error7>tolerance, 
            "wrong clean price for frnbond:"
            + "\n  generic frn rate bond's clean price: " + str(floatingBondAssetSwapPrice2)
            + "\n  equivalent specialized frn  bond's price: " + str(floatingSpecializedBondAssetSwapPrice2)
            + "\n  error:                 " + str(error7)
            + "\n  tolerance:             " + str(tolerance))
        
        ## market executable price as of 4th sept 2007
        floatingBondMktPrice2 = 101.26
        floatingBondASW2 = AssetSwap(payFixedRate,
                                   floatingBond2, floatingBondMktPrice2,
                                   self.iborIndex, self.spread,
                                   Schedule(),
                                   self.iborIndex.dayCounter(),
                                   parAssetSwap)
        floatingBondASW2.setPricingEngine(swapEngine)
        floatingSpecializedBondASW2 = AssetSwap(payFixedRate,
                                              floatingSpecializedBond2,
                                              floatingBondMktPrice2,
                                              self.iborIndex, self.spread,
                                              Schedule(),
                                              self.iborIndex.dayCounter(),
                                              parAssetSwap)
        floatingSpecializedBondASW2.setPricingEngine(swapEngine)
        floatingBondASWSpread2 = floatingBondASW2.fairSpread()
        floatingSpecializedBondASWSpread2 = floatingSpecializedBondASW2.fairSpread()
        error8 = abs(floatingBondASWSpread2-floatingSpecializedBondASWSpread2)
        self.assertFalse(error8>tolerance, 
            "wrong asw spread for frn bond:"
            + "\n  generic  frn rate bond's  asw spread: " + str(floatingBondASWSpread2)
            + "\n  equivalent specialized bond's asw spread: " + str(floatingSpecializedBondASWSpread2)
            + "\n  error:                 " + str(error8)
            + "\n  tolerance:             " + str(tolerance))        

        ## CMS bond (Isin: XS0228052402 CRDIT 0 8/22/20)
        ## maturity doesn't occur on a business day
        
        cmsBondStartDate1 = Date(22,August,2005)
        cmsBondMaturityDate1 = Date(22,August,2020)
        cmsBondSchedule1 = Schedule(cmsBondStartDate1,
                                  cmsBondMaturityDate1,
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBondLeg1 = list(CmsLeg([self.faceAmount],cmsBondSchedule1, self.swapIndex,
                                  Thirty360(),Following,[fixingDays], [],[],[0.055],[0.025],inArrears))
        cmsbondRedemption1 = bondCalendar.adjust(cmsBondMaturityDate1,Following)
        cmsBondLeg1.append(SimpleCashFlow(100.0, cmsbondRedemption1))
        ## generic cms bond
        cmsBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 cmsBondMaturityDate1, cmsBondStartDate1, cmsBondLeg1)
        cmsBond1.setPricingEngine(bondEngine)

        ## equivalent specialized cms bond
        cmsSpecializedBond1 = CmsRateBond(settlementDays, self.faceAmount, cmsBondSchedule1,
                    self.swapIndex, Thirty360(),
                    Following, fixingDays,
                    [1.0], [0.0],
                    [0.055], [0.025],
                    inArrears,
                    100.0, Date(22,August,2005))
        cmsSpecializedBond1.setPricingEngine(bondEngine)


        setCouponPricer(cmsBond1.cashflows(), self.cmspricer)
        setCouponPricer(cmsSpecializedBond1.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(18,August,2006), 0.04158)
        cmsBondPrice1 = cmsBond1.cleanPrice()
        cmsSpecializedBondPrice1 = cmsSpecializedBond1.cleanPrice()
        cmsBondAssetSwap1 = AssetSwap(payFixedRate,cmsBond1, cmsBondPrice1,
                                    self.iborIndex, self.nonnullspread,
                                    Schedule(),self.iborIndex.dayCounter(),
                                    parAssetSwap)
        cmsBondAssetSwap1.setPricingEngine(swapEngine)
        cmsSpecializedBondAssetSwap1 = AssetSwap(payFixedRate,cmsSpecializedBond1,
                                               cmsSpecializedBondPrice1,
                                               self.iborIndex,
                                               self.nonnullspread,
                                               Schedule(),
                                               self.iborIndex.dayCounter(),
                                               parAssetSwap)
        cmsSpecializedBondAssetSwap1.setPricingEngine(swapEngine)
        cmsBondAssetSwapPrice1 = cmsBondAssetSwap1.fairCleanPrice()
        cmsSpecializedBondAssetSwapPrice1 = cmsSpecializedBondAssetSwap1.fairCleanPrice()
        error9 = abs(cmsBondAssetSwapPrice1-cmsSpecializedBondAssetSwapPrice1)
        self.assertFalse(error9>tolerance, 
            "wrong clean price for cmsbond:"
            + "\n  generic bond's clean price: " + str(cmsBondAssetSwapPrice1)
            + "\n  equivalent specialized cms rate bond's price: " + str(cmsSpecializedBondAssetSwapPrice1)
            + "\n  error:                 " + str(error9)
            + "\n  tolerance:             " + str(tolerance))
        
        cmsBondMktPrice1 = 87.02## market executable price as of 4th sept 2007
        cmsBondASW1 = AssetSwap(payFixedRate,
                              cmsBond1, cmsBondMktPrice1,
                              self.iborIndex, self.spread,
                              Schedule(),
                              self.iborIndex.dayCounter(),
                              parAssetSwap)
        cmsBondASW1.setPricingEngine(swapEngine)
        cmsSpecializedBondASW1 = AssetSwap(payFixedRate,
                                         cmsSpecializedBond1,
                                         cmsBondMktPrice1,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        cmsSpecializedBondASW1.setPricingEngine(swapEngine)
        cmsBondASWSpread1 = cmsBondASW1.fairSpread()
        cmsSpecializedBondASWSpread1 = cmsSpecializedBondASW1.fairSpread()
        error10 = abs(cmsBondASWSpread1-cmsSpecializedBondASWSpread1)
        self.assertFalse(error10>tolerance, 
            "wrong asw spread for cm bond:"
            + "\n  generic cms rate bond's  asw spread: " + str(cmsBondASWSpread1)
            + "\n  equivalent specialized bond's asw spread: " + str(cmsSpecializedBondASWSpread1)
            + "\n  error:                 " + str(error10)
            + "\n  tolerance:             " + str(tolerance))
        

          ##CMS bond (Isin: XS0218766664 ISPIM 0 5/6/15)
          ##maturity occurs on a business day
        cmsBondStartDate2 = Date(6,May,2005)
        cmsBondMaturityDate2 = Date(6,May,2015)
        cmsBondSchedule2 = Schedule(cmsBondStartDate2,
                                  cmsBondMaturityDate2,
                                  Period(Annual), bondCalendar,
                                  Unadjusted, Unadjusted,
                                  DateGeneration.Backward, False)
        cmsBondLeg2 = list(CmsLeg([self.faceAmount],cmsBondSchedule2, self.swapIndex,
                             Thirty360(), Following, [fixingDays] , [0.84],[],[],[],inArrears))
        cmsbondRedemption2 = bondCalendar.adjust(cmsBondMaturityDate2,
                                                      Following)
        cmsBondLeg2.append(SimpleCashFlow(100.0, cmsbondRedemption2))
        ## generic bond
        cmsBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 cmsBondMaturityDate2, cmsBondStartDate2, cmsBondLeg2)
        cmsBond2.setPricingEngine(bondEngine)

        ## equivalent specialized cms bond
        cmsSpecializedBond2 = CmsRateBond(settlementDays, self.faceAmount, cmsBondSchedule2,
                    self.swapIndex, Thirty360(),
                    Following, fixingDays,
                    [0.84], [0.0],
                    [], [],
                    inArrears,
                    100.0, Date(6,May,2005))
        cmsSpecializedBond2.setPricingEngine(bondEngine)

        setCouponPricer(cmsBond2.cashflows(), self.cmspricer)
        setCouponPricer(cmsSpecializedBond2.cashflows(), self.cmspricer)
        self.swapIndex.addFixing(Date(4,May,2006), 0.04217)
        cmsBondPrice2 = cmsBond2.cleanPrice()
        cmsSpecializedBondPrice2 = cmsSpecializedBond2.cleanPrice()
        cmsBondAssetSwap2 = AssetSwap(payFixedRate,cmsBond2, cmsBondPrice2,
                                    self.iborIndex, self.nonnullspread,
                                    Schedule(),
                                    self.iborIndex.dayCounter(),
                                    parAssetSwap)
        cmsBondAssetSwap2.setPricingEngine(swapEngine)
        cmsSpecializedBondAssetSwap2 = AssetSwap(payFixedRate,cmsSpecializedBond2,
                                               cmsSpecializedBondPrice2,
                                               self.iborIndex,
                                               self.nonnullspread,
                                               Schedule(),
                                               self.iborIndex.dayCounter(),
                                               parAssetSwap)
        cmsSpecializedBondAssetSwap2.setPricingEngine(swapEngine)
        cmsBondAssetSwapPrice2 = cmsBondAssetSwap2.fairCleanPrice()
        cmsSpecializedBondAssetSwapPrice2 = cmsSpecializedBondAssetSwap2.fairCleanPrice()
        error11 = abs(cmsBondAssetSwapPrice2-cmsSpecializedBondAssetSwapPrice2)
        self.assertFalse(error11>tolerance,
            "wrong clean price for cmsbond:"
            + "\n  generic  bond's clean price: " + str(cmsBondAssetSwapPrice2)
            + "\n  equivalent specialized cms rate bond's price: "  + str(cmsSpecializedBondAssetSwapPrice2)
            + "\n  error:                 " + str(error11)
            + "\n  tolerance:             " + str(tolerance))
        
        cmsBondMktPrice2 = 94.35## market executable price as of 4th sept 2007
        cmsBondASW2 = AssetSwap(payFixedRate,
                              cmsBond2, cmsBondMktPrice2,
                              self.iborIndex, self.spread,
                              Schedule(),
                              self.iborIndex.dayCounter(),
                              parAssetSwap)
        cmsBondASW2.setPricingEngine(swapEngine)
        cmsSpecializedBondASW2 = AssetSwap(payFixedRate,
                                         cmsSpecializedBond2,
                                         cmsBondMktPrice2,
                                         self.iborIndex, self.spread,
                                         Schedule(),
                                         self.iborIndex.dayCounter(),
                                         parAssetSwap)
        cmsSpecializedBondASW2.setPricingEngine(swapEngine)
        cmsBondASWSpread2 = cmsBondASW2.fairSpread()
        cmsSpecializedBondASWSpread2 = cmsSpecializedBondASW2.fairSpread()
        error12 = abs(cmsBondASWSpread2-cmsSpecializedBondASWSpread2)
        self.assertFalse(error12>tolerance,
            "wrong asw spread for cm bond:"
            + "\n  generic cms rate bond's  asw spread: " + str(cmsBondASWSpread2)
            + "\n  equivalent specialized bond's asw spread: " + str(cmsSpecializedBondASWSpread2)
            + "\n  error:                 " + str(error12)
            + "\n  tolerance:             " + str(tolerance))
        


       ##  Zero-Coupon bond (Isin: DE0004771662 IBRD 0 12/20/15)
       ##  maturity doesn't occur on a business day
        zeroCpnBondStartDate1 = Date(19,December,1985)
        zeroCpnBondMaturityDate1 = Date(20,December,2015)
        zeroCpnBondRedemption1 = bondCalendar.adjust(zeroCpnBondMaturityDate1, Following)
        zeroCpnBondLeg1 = Leg([SimpleCashFlow(100.0, zeroCpnBondRedemption1)])
        ## generic bond
        zeroCpnBond1 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 zeroCpnBondMaturityDate1, zeroCpnBondStartDate1, zeroCpnBondLeg1)
        zeroCpnBond1.setPricingEngine(bondEngine)

        ## specialized zerocpn bond
        zeroCpnSpecializedBond1 = ZeroCouponBond(settlementDays, bondCalendar, self.faceAmount,
                      Date(20,December,2015),
                      Following,
                      100.0, Date(19,December,1985))
        zeroCpnSpecializedBond1.setPricingEngine(bondEngine)

        zeroCpnBondPrice1 = zeroCpnBond1.cleanPrice()
        zeroCpnSpecializedBondPrice1 = zeroCpnSpecializedBond1.cleanPrice()
        zeroCpnBondAssetSwap1 = AssetSwap(payFixedRate,zeroCpnBond1,
                                        zeroCpnBondPrice1,
                                        self.iborIndex, self.nonnullspread,
                                        Schedule(),
                                        self.iborIndex.dayCounter(),
                                        parAssetSwap)
        zeroCpnBondAssetSwap1.setPricingEngine(swapEngine)
        zeroCpnSpecializedBondAssetSwap1 = AssetSwap(payFixedRate,
                                                   zeroCpnSpecializedBond1,
                                                   zeroCpnSpecializedBondPrice1,
                                                   self.iborIndex,
                                                   self.nonnullspread,
                                                   Schedule(),
                                                   self.iborIndex.dayCounter(),
                                                   parAssetSwap)
        zeroCpnSpecializedBondAssetSwap1.setPricingEngine(swapEngine)
        zeroCpnBondAssetSwapPrice1 = zeroCpnBondAssetSwap1.fairCleanPrice()
        zeroCpnSpecializedBondAssetSwapPrice1 = zeroCpnSpecializedBondAssetSwap1.fairCleanPrice()
        error13 = abs(zeroCpnBondAssetSwapPrice1-zeroCpnSpecializedBondAssetSwapPrice1)
        self.assertFalse(error13>tolerance, 
            "wrong clean price for zerocpn bond:"
            + "\n  generic zero cpn bond's clean price: " + str(zeroCpnBondAssetSwapPrice1)
            + "\n  specialized equivalent bond's price: " + str(zeroCpnSpecializedBondAssetSwapPrice1)
            + "\n  error:                 " + str(error13)
            + "\n  tolerance:             " + str(tolerance))
        
        ## market executable price as of 4th sept 2007
        zeroCpnBondMktPrice1 = 72.277
        zeroCpnBondASW1 = AssetSwap(payFixedRate,
                                  zeroCpnBond1,zeroCpnBondMktPrice1,
                                  self.iborIndex, self.spread,
                                  Schedule(),
                                  self.iborIndex.dayCounter(),
                                  parAssetSwap)
        zeroCpnBondASW1.setPricingEngine(swapEngine)
        zeroCpnSpecializedBondASW1 = AssetSwap(payFixedRate,
                                             zeroCpnSpecializedBond1,
                                             zeroCpnBondMktPrice1,
                                             self.iborIndex, self.spread,
                                             Schedule(),
                                             self.iborIndex.dayCounter(),
                                             parAssetSwap)
        zeroCpnSpecializedBondASW1.setPricingEngine(swapEngine)
        zeroCpnBondASWSpread1 = zeroCpnBondASW1.fairSpread()
        zeroCpnSpecializedBondASWSpread1 = zeroCpnSpecializedBondASW1.fairSpread()
        error14 = abs(zeroCpnBondASWSpread1-zeroCpnSpecializedBondASWSpread1)
        self.assertFalse(error14>tolerance,  
            "wrong asw spread for zeroCpn bond:"
            + "\n  generic zeroCpn bond's  asw spread: " + str(zeroCpnBondASWSpread1)
            + "\n  equivalent specialized bond's asw spread: " + str(zeroCpnSpecializedBondASWSpread1)
            + "\n  error:                 " + str(error14)
            + "\n  tolerance:             " + str(tolerance))
        


       ##  Zero Coupon bond (Isin: IT0001200390 ISPIM 0 02/17/28)
       ##  maturity doesn't occur on a business day
        zeroCpnBondStartDate2 = Date(17,February,1998)
        zeroCpnBondMaturityDate2 = Date(17,February,2028)
        zerocpbondRedemption2 = bondCalendar.adjust(zeroCpnBondMaturityDate2, Following)
        zeroCpnBondLeg2 = Leg([SimpleCashFlow(100.0, zerocpbondRedemption2)])
        ## generic bond
        zeroCpnBond2 = Bond(settlementDays, bondCalendar, self.faceAmount,
                 zeroCpnBondMaturityDate2, zeroCpnBondStartDate2, zeroCpnBondLeg2)
        zeroCpnBond2.setPricingEngine(bondEngine)

        ## specialized zerocpn bond
        zeroCpnSpecializedBond2 = ZeroCouponBond(settlementDays, bondCalendar, self.faceAmount,
                       Date(17,February,2028),
                       Following,
                       100.0, Date(17,February,1998))
        zeroCpnSpecializedBond2.setPricingEngine(bondEngine)

        zeroCpnBondPrice2 = zeroCpnBond2.cleanPrice()
        zeroCpnSpecializedBondPrice2 = zeroCpnSpecializedBond2.cleanPrice()

        zeroCpnBondAssetSwap2 = AssetSwap(payFixedRate,zeroCpnBond2,
                                        zeroCpnBondPrice2,
                                        self.iborIndex, self.nonnullspread,
                                        Schedule(),
                                        self.iborIndex.dayCounter(),
                                        parAssetSwap)
        zeroCpnBondAssetSwap2.setPricingEngine(swapEngine)
        zeroCpnSpecializedBondAssetSwap2 = AssetSwap(payFixedRate,
                                                   zeroCpnSpecializedBond2,
                                                   zeroCpnSpecializedBondPrice2,
                                                   self.iborIndex,
                                                   self.nonnullspread,
                                                   Schedule(),
                                                   self.iborIndex.dayCounter(),
                                                   parAssetSwap)
        zeroCpnSpecializedBondAssetSwap2.setPricingEngine(swapEngine)
        zeroCpnBondAssetSwapPrice2 = zeroCpnBondAssetSwap2.fairCleanPrice()
        zeroCpnSpecializedBondAssetSwapPrice2 = zeroCpnSpecializedBondAssetSwap2.fairCleanPrice()
        error15 = abs(zeroCpnBondAssetSwapPrice2 -zeroCpnSpecializedBondAssetSwapPrice2)
        self.assertFalse(error8>tolerance,
            "wrong clean price for zerocpn bond:"
            + "\n  generic zero cpn bond's clean price: " + str(zeroCpnBondAssetSwapPrice2)
            + "\n  equivalent specialized bond's price: " + str(zeroCpnSpecializedBondAssetSwapPrice2)
            + "\n  error:                 " + str(error15)
            + "\n  tolerance:             " + str(tolerance))
        
        ## market executable price as of 4th sept 2007
        zeroCpnBondMktPrice2 = 72.277
        zeroCpnBondASW2 = AssetSwap(payFixedRate,
                                  zeroCpnBond2,zeroCpnBondMktPrice2,
                                  self.iborIndex, self.spread,
                                  Schedule(),
                                  self.iborIndex.dayCounter(),
                                  parAssetSwap)
        zeroCpnBondASW2.setPricingEngine(swapEngine)
        zeroCpnSpecializedBondASW2 = AssetSwap(payFixedRate,
                                             zeroCpnSpecializedBond2,
                                             zeroCpnBondMktPrice2,
                                             self.iborIndex, self.spread,
                                             Schedule(),
                                             self.iborIndex.dayCounter(),
                                             parAssetSwap)
        zeroCpnSpecializedBondASW2.setPricingEngine(swapEngine)
        zeroCpnBondASWSpread2 = zeroCpnBondASW2.fairSpread()
        zeroCpnSpecializedBondASWSpread2 = zeroCpnSpecializedBondASW2.fairSpread()
        error16 = abs(zeroCpnBondASWSpread2-zeroCpnSpecializedBondASWSpread2)
        self.assertFalse(error16>tolerance, 
            "wrong asw spread for zeroCpn bond:"
            + "\n  generic zeroCpn bond's  asw spread: " + str(zeroCpnBondASWSpread2)
            + "\n  equivalent specialized bond's asw spread: " + str(zeroCpnSpecializedBondASWSpread2)
            + "\n  error:                 " + str(error16)
            + "\n  tolerance:             " + str(tolerance))
        

if __name__ == '__main__':
    print('testing QuantLib ' + QuantLib.__version__)
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(AssetSwapTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
