package examples;

import org.quantlib.QuantLib;
import org.quantlib.Actual360;
import org.quantlib.Date;
import org.quantlib.DayCounter;
import org.quantlib.FlatForward;
import org.quantlib.Month;
import org.quantlib.Settings;
import org.quantlib.TARGET;
import org.quantlib.YieldTermStructure;
import org.quantlib.YieldTermStructureHandle;
import org.quantlib.ForwardRateAgreement;
import org.quantlib.Position;
import org.quantlib.Calendar;
import org.quantlib.IborIndex;
import org.quantlib.Euribor3M;

public class FRA {

    static {
        try {
            System.loadLibrary("QuantLibJNI");
        } catch (RuntimeException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) throws Exception {

        Date todaysDate = new Date(23, Month.May, 2006);
        Settings.instance().setEvaluationDate(todaysDate);
        Date settlementDate = new Date(25, Month.May, 2006);
        Date maturityDate = new Date(23, Month.August, 2006);
        Position.Type type = Position.Type.Long;
        double strike = 0.02;
        double notional = 100.0;
        double riskFreeRate = 0.06;
        DayCounter dayCounter = new Actual360();
        Calendar calendar = new TARGET();

        // define the underlying asset and the yield/dividend/volatility curves
        YieldTermStructureHandle flatTermStructure =
            new YieldTermStructureHandle(new FlatForward(
                                  settlementDate, riskFreeRate, dayCounter));
        IborIndex euribor3m = new Euribor3M(flatTermStructure);
        Date fixingDate = new Date(19, Month.May, 2006);
        euribor3m.addFixing(fixingDate, 0.02);

        ForwardRateAgreement myFra =
            new ForwardRateAgreement(todaysDate, maturityDate,
                    type, strike, notional, euribor3m, flatTermStructure);
        System.out.println(myFra.spotValue());
    }
}

