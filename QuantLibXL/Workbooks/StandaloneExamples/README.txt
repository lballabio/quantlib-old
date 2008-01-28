
This directory contains some example workbooks for use with QuantLibXL.

On first opening a book, hit Ctrl-Alt-F9 to force a full recalculation.

Be aware that in QuantLib the evaluation date is a global variable, set by
formula qlSettingsSetEvaluationDate().  Two books requiring different
evaluation dates cannot be open simultaneously within a single QuantLibXL
session.  Please refer to the documentation for more information.

The latest and greatest example workbooks are YieldCurveBootstrapping.xls
and InterestRateDerivatives.xls.

VBA addin serialize_swap.xla contains some startup code which loads
MarketData.xls and VanillaSwap.xls to create a demo serialization environment,
as described in the following document:

    http://www.quantlibaddin.org/serialization.html

The other workbooks in this Standalone directory have been updated for the
0.9.0 release and illustrate various aspects of QuantLibXL functionality.

Apart from StandaloneExamples, other subdirectories of the Workbooks directory
contain workbooks that belong to the Framework application:

    http://www.quantlibxl.org/framework.html

When you run the QuantLibXL installer, you are given the option to install the
Framework.  If you choose no, the installer will still deliver a handful of
workbooks that belong to the Framework application but which might be usable in
a standalone QuantLibXL session.  If you choose yes then all of the Framework
workbooks are installed, most of them are meaningful only when opened
automatically by VBA code in response to menu events.

