#include <string>


namespace QuantLibAddin {
  bool close(double x, double y);
  std::string qlSimpleQuote(const std::string &objectID, double value);
  double qlSimpleQuotevalue(const std::string &objectID);
}


