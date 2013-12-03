
#include <iostream>
#include "AddinCpp/cpp_quote.hpp"
#include "AddinCpp/cpp_date.hpp"

int main() {
    try {
	    std::cout << "hi" << std::endl;
    	std::cout << QuantLibAddin::qlClose(1, 2) << std::endl;
        QuantLibAddin::qlSimpleQuote("quote", 1);
        std::cout << QuantLibAddin::qlSimpleQuoteValue("quote") << std::endl;
        QuantLibAddin::qlDate("date", 24950);
        std::cout << QuantLibAddin::qlDateSerialNumber("date") << std::endl;
    	std::cout << "bye" << std::endl;
    } catch (const std::exception &e) {
    	std::cout << "error " << e.what() << std::endl;
    } catch (...) {
    	std::cout << "error" << std::endl;
    }
    return 0;
}

