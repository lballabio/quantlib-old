
#include <iostream>
#include "quantLibAddinTest.hpp"

int main() {
    try {
	    std::cout << "hi" << std::endl;
    	std::cout << QuantLibAddin::qlClose(1, 2) << std::endl;
        QuantLibAddin::qlSimpleQuote("foo", 1);
        std::cout << QuantLibAddin::qlSimpleQuoteValue("foo") << std::endl;
    	std::cout << "bye" << std::endl;
    } catch (const std::exception &e) {
    	std::cout << "error " << e.what() << std::endl;
    } catch (...) {
    	std::cout << "error" << std::endl;
    }
    return 0;
}

