#include <ql/quantlib.hpp>

#include <iostream>

using namespace QuantLib;

int main() {

    int mode = atoi(getenv("MODE"));

    Real etamin = atof(getenv("ETAMIN"));
    Real etamax = atof(getenv("ETAMAX"));
    Real u0min = atof(getenv("U0MIN"));
    Real u0max = atof(getenv("U0MAX"));
    Real sumin = atof(getenv("SUMIN"));
    Real sumax = atof(getenv("SUMAX"));
    Size usize = atoi(getenv("USIZE"));
    Size susize = atoi(getenv("SUSIZE"));
    Size etasize = atoi(getenv("ETASIZE"));
    Real cu = atof(getenv("CU"));
    Real csu = atof(getenv("CSU"));
    Real ce = atof(getenv("CE"));
    Real densu = atof(getenv("DENSU"));
    Real denssu = atof(getenv("DENSSU"));
    Real dense = atof(getenv("DENSE"));

    detail::betaeta_tabulation_type type;

    switch (mode) {
    case 1:
        type = detail::Cpp_M;
        break;
    case 2:
        type = detail::Cpp_p;
        break;
    case 10:
        type = detail::GnuplotEUV;
        break;
    case 11:
        type = detail::GnuplotUEV;
        break;
    case 12:
        type = detail::GnuplotVEU;
        break;
    case 13:
        type = detail::GnuplotP;
        break;
    default:
        QL_FAIL("unknown mode " << mode);
    }

    detail::betaeta_tabulate(type, std::cout, etamin, etamax, u0min, u0max,
                             sumin, sumax, usize, susize, etasize, cu, densu,
                             csu, denssu, ce, dense);

    return 0;
}
