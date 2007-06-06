This directory contains some example instruments for use with QuantLibXL.

On first opening a book, hit Ctrl-Alt-F9 to force a full recalculation.

The workbooks Swap.xls, Swaption.xls, and VanillaSwap.xls depend on
MarketData.xls.

Be aware that in QuantLib the evaluation date is a global variable, set by
formula qlSettingsSetEvaluationDate().  Two books requiring different
evaluation dates cannot be open simultaneously within a single QuantLibXL
session.  Please refer to the documentation for more information.
