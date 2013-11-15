
#include <string>

namespace QuantLibAddin {
    bool qlClose(double x, double y);
    std::string qlSimpleQuote(const std::string &objectID, double value);
    double qlSimpleQuoteValue(const std::string &objectID);
}

