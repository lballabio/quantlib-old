#include <iostream>
#include <cmath>

extern "C" void discount_ad_(double *zeroyield, double *dcf, double *df,
                             double *ddf);

int main() {

    double zeroyield = 0.02;
    double dcf = 10.0;
    double df, ddf;

    discount_ad_(&zeroyield, &dcf, &df, &ddf);

    std::cout << "result1 = " << df
              << " should be: " << std::exp(-zeroyield * dcf) << std::endl;

    std::cout << "result2 = " << ddf
              << " should be: " << -dcf * std::exp(-zeroyield * dcf)
              << std::endl;
}
