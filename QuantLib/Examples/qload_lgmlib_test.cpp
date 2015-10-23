#include <iostream>
#include <cmath>

//extern "C" void lgm_swaption_engine_(int *n_times, double *times, double *res);
extern "C" void lgm_swaption_engine_ad_(int *n_times, double *times, double *res, double *dres);

int main() {

    int n_times = 10;
    double times[10] = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
    double result, dresult[10];

    // =====================
    // test of plain library
    // =====================

    // lgm_swaption_engine_(&n_times, times, &result);

    // std::cout << "results (plain function):\n";
    // std::cout << "value = " << result << "\n";

    // =====================
    // test of ad library
    // =====================

    lgm_swaption_engine_ad_(&n_times, times, &result, dresult);

    std::cout << "results (ad'ized function):\n";
    std::cout << "value = " << result << "\n";
    std::cout << "dvalue = ";
    for(size_t i=0; i<10; ++i) {
        std::cout << dresult[i] << " ";
    }
    std::cout << std::endl;

}
