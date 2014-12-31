
#include <ql/indexes/swapindex.hpp>

//todo move to header, once template'zed

namespace QuantLib {

OvernightIndexedSwapIndex::OvernightIndexedSwapIndex(
    const std::string &familyName, const Period &tenor, Natural settlementDays,
    Currency currency, const shared_ptr<OvernightIndex> &overnightIndex)
    : SwapIndex(familyName, tenor, settlementDays, currency,
                     overnightIndex->fixingCalendar(), 1 * Years,
                     ModifiedFollowing, overnightIndex->dayCounter(),
                     overnightIndex),
      overnightIndex_(overnightIndex) {}

boost::shared_ptr<OvernightIndexedSwap>
OvernightIndexedSwapIndex::underlyingSwap(const Date &fixingDate) const {

    QL_REQUIRE(fixingDate != Date(), "null fixing date");

    // caching mechanism
    if (lastFixingDate_ != fixingDate) {
        Rate fixedRate = 0.0;
        lastSwap_ = MakeOIS(tenor_, overnightIndex_, fixedRate)
                        .withEffectiveDate(valueDate(fixingDate))
                        .withFixedLegDayCount(dayCounter_);
        lastFixingDate_ = fixingDate;
    }
    return lastSwap_;
}

}
