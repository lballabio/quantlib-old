# Example of option baskets
# Distributed under BSD License

from QuantLib import *
class BasketOptionClass:
    def __init__(self, btype):
        # global data
        self.todaysDate = Date(15,May,1998)
        Settings.instance().evaluationDate = self.todaysDate
        self.settlementDate = Date(17,May,1998)
        self.riskFreeQuote = SimpleQuote(0.05)
        self.riskFreeRate = FlatForward(self.settlementDate,
                                        QuoteHandle(self.riskFreeQuote),
                                        Actual365Fixed())

        # option parameters
        self.exercise = EuropeanExercise(Date(17,May,1999))
        self.payoff = PlainVanillaPayoff(Option.Call, 8.0)

        # market data
        self.underlying1 = SimpleQuote(10.0)
        self.volatility1 = BlackConstantVol(self.todaysDate, 0.20,
                                            Actual365Fixed())
        self.dividendYield1 = FlatForward(self.settlementDate,
                                          0.05, Actual365Fixed())
        self.underlying2 = SimpleQuote(7.0)
        self.volatility2 = BlackConstantVol(self.todaysDate,
                                            0.10, Actual365Fixed())
        self.dividendYield2 = FlatForward(self.settlementDate,
                                          0.05, Actual365Fixed())

        self.process1 = BlackScholesMertonProcess(QuoteHandle(self.underlying1),
                                                  YieldTermStructureHandle(self.dividendYield1),
                                                  YieldTermStructureHandle(self.riskFreeRate),
                                                  BlackVolTermStructureHandle(self.volatility1))

        self.process2 = BlackScholesMertonProcess(QuoteHandle(self.underlying2),
                                                  YieldTermStructureHandle(self.dividendYield2),
                                                  YieldTermStructureHandle(self.riskFreeRate),
                                                  BlackVolTermStructureHandle(self.volatility2))

        self.procs = StochasticProcessVector();
        self.procs.push_back(self.process1)
        self.procs.push_back(self.process2)

        self.matrix = Matrix(2,2)
        self.matrix[0][0] = 1.0
        self.matrix[1][1] = 1.0
        self.matrix[0][1] = 0.25
        self.matrix[1][0] = 0.25

        self.process = StochasticProcessArray(self.procs, self.matrix)
        self.engine = MCBasketEngine('lowdiscrepancy',
                                     timeStepsPerYear = 1,
                                     requiredTolerance = 0.02,
                                     seed = 42)
        if (btype == 'min'):
            bpayoff = MinBasketPayoff(self.payoff)
        elif (btype == 'max'):
            bpayoff = MaxBasketPayoff(self.payoff)
        else:
            bpayoff = AverageBasketPayoff(self.payoff, 2)
        
        self.option = BasketOption(self.process,
                                   bpayoff,
                                   self.exercise,
                                   self.engine)
    def npv(self, x,y):
        self.underlying1.setValue(x)
        self.underlying2.setValue(y)
        return self.option.NPV()
    def setQuote(r):
        self.riskFreeQuote.setValue(r)
