#include <ql/quantlib.hpp>

using namespace QuantLib;

int main(int, char* []) {

    try {

        std::cout << "CmsSwaption Example" << std::endl;


    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }
}

