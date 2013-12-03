
#ifndef cpp_hw_hpp
#define cpp_hw_hpp

#include <string>

namespace QuantLibAddin {
    bool qlClose(double x, double y);
    std::string qlSimpleQuote(const std::string &objectID, double value);
    double qlSimpleQuoteValue(const std::string &objectID);
}

#endif

