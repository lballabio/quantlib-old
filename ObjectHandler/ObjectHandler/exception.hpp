#ifndef exception_h
#define exception_h

#include <exception>
#include <string>

class Exception : public std::exception {
	public:
		Exception(const std::string& message);
		~Exception() throw() {}
		const char* what() const throw ();
	private:
		std::string message_;
};

#endif
