
#include <iostream>
#include "AddinCpp/sla.hpp"

int main() {
    try {
	    std::cout << "hi" << std::endl;
    	std::cout << QuantLibAddin::qlclose(1, 2) << std::endl;
        QuantLibAddin::qlSimpleQuote("quote", 1);
        std::cout << QuantLibAddin::qlSimpleQuotevalue("quote") << std::endl;
    	std::cout << "bye" << std::endl;
    } catch (const std::exception &e) {
    	std::cout << "error " << e.what() << std::endl;
    } catch (...) {
    	std::cout << "error" << std::endl;
    }
    return 0;
}

