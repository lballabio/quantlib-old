
package test;

import org.quantlib.*;

public class Hello {
    static {
        System.loadLibrary("QuantLibJNI");
    }
    public static void main(String[] args) {
        QuantLib ql = new QuantLib();
        double nd = QuantLib.nullDouble();

        System.out.println("ql nullDouble is "+nd);
        System.out.println("and sunday is "+Weekday.Sunday.toString());
    }
}

