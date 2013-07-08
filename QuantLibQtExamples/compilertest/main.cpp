#include <iostream>
#include <ql/quantlib.hpp>

using namespace QuantLib;

int main()
{
    std::cout << "\nHello World!" << std::endl;
    Date today(29, June, 2013);
    std::cout << today << "\n\n" << std::endl;
    return 0;
}

