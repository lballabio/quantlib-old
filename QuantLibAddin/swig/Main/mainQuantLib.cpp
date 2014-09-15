
#include <iostream>
#include <ql/math/comparison.hpp>
#include <ql/quotes/simplequote.hpp>

int main() {
    try {
    	std::cout << "hi" << std::endl;
    	std::cout << QuantLib::close(1, 2) << std::endl;
        QuantLib::SimpleQuote q(1);
    	std::cout << q.value() << std::endl;
    	std::cout << "bye" << std::endl;
    } catch (const std::exception &e) {
    	std::cout << "error " << e.what() << std::endl;
    } catch (...) {
    	std::cout << "error" << std::endl;
    }
    return 0;
}

