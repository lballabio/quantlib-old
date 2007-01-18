
rem force deleting files in quiet mode, do not ask if ok to delete on global wildcard
del /Q %2\Workbooks\MarketData\ReutersFeed2\*.*

copy %1\Workbooks\MarketData\ReutersFeed\*.*  %2\Workbooks\MarketData\ReutersFeed2\

explorer.exe %2\Workbooks\MarketData\ReutersFeed2\

pause

rem list of functions in MarketData\ReutersFeed\*.*
rem ohTrigger
rem qlSimpleQuoteSetValue
rem qlVersion
rem qlMidSafe
rem qlSettingsEvaluationDate
rem qlIndexAddFixings
rem qlInterestRateIndexForecastFixing
rem qlIMMcode
rem qlNextIMMdates
rem qlMidEquivalent




