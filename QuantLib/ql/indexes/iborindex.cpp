
#include <ql/indexes/iborindex.hpp>

//todo move to header, once template'zed

namespace QuantLib {

OvernightIndex::OvernightIndex(const std::string &familyName,
                               Natural settlementDays, const Currency &curr,
                               const Calendar &fixCal, const DayCounter &dc,
                               const Handle<YieldTermStructure> &h)
    : IborIndex(familyName, 1 * Days, settlementDays, curr, fixCal, Following,
                false, dc, h) {}

boost::shared_ptr<IborIndex>
OvernightIndex::clone(const Handle<YieldTermStructure> &h) const {
    return boost::shared_ptr<IborIndex>(
        new OvernightIndex(familyName(), fixingDays(), currency(),
                           fixingCalendar(), dayCounter(), h));
}

}
