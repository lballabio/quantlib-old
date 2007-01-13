rem this .bat file must be saved in C:\Projects\Trunks


rem force deleting read-only files in quiet mode
del /Q X:\Offices1\CabotoXL\%1\Workbooks\MarketData\ReutersFeed\*.*
copy QuantLibXL\Workbooks\MarketData\ReutersFeed\*.*  X:\Offices1\CabotoXL\01 Production\Workbooks\MarketData\ReutersFeed\

explorer.exe X:\Offices1\CabotoXL\01 Production\

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




