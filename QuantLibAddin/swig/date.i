
%feature("oh:group", "date");
%feature("oh:include") %{
#include <ql/time/date.hpp>
%}

namespace QuantLib {
    class Date {
      public:
        Date(long serialNumber);
        long serialNumber();
    };
}

