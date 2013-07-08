QT      -= core gui
TEMPLATE = app
CONFIG += console
CONFIG -= qt
CONFIG += link_pri

gcc {
    message(gcc)
    QMAKE_CXXFLAGS += -std=c++11 -pthread
    #QMAKE_CXXFLAGS += -lstdc++
    #QMAKE_CXXFLAGS += -pthread
    LIBS += -pthread
}
macx-g++ {
    message(macx-g++)
}
macx-clang {
    message(macx-clang)
}

# Boost Library
macx {
    INCLUDEPATH += /opt/local/include /opt/local/include/boost
    LIBS += -L/opt/local/lib -lboost_timer-mt -lboost_thread-mt -lboost_unit_test_framework-mt
}
win32-msvc* {
    INCLUDEPATH += C:/boost/boost_1_53_0
    CONFIG(debug, debug|release) {
        LIBS += C:/boost/boost_1_53_0/lib/libboost_timer-vc100-mt-gd-1_53.lib
    } else {
        LIBS += C:/boost/boost_1_53_0/lib/libboost_timer-vc100-mt-1_53.lib
    }
}
win32-g++ {
    INCLUDEPATH += C:/boost/boost_1_53_0
    CONFIG(debug, debug|release) {
        LIBS += C:/boost/boost_1_53_0/lib/libboost_timer-mgw47-mt-d-1_53.a
    } else {
        LIBS += C:/boost/boost_1_53_0/lib/libboost_timer-mgw47-mt-1_53.a
    }
}
#unix {
#    #INCLUDEPATH += /home/algefrontal/boost/boost_1_50_0
#    INCLUDEPATH += ../../boost/boost_1_53_0
#    CONFIG(debug, debug|release) {
#        #LIBS += -L/home/algefrontal/boost/boost_1_50_0/libd -lboost_timer-mt-d
#        LIBS += -L../../boost/boost_1_53_0/lib \
#                -lboost_timer-mt-d \
#                -lboost_unit_test_framework-mt-d
#    } else {
#        #LIBS += -L/home/algefrontal/boost/boost_1_50_0/lib -lboost_timer-mt
#        LIBS += -L../../boost/boost_1_53_0/lib \
#                -lboost_timer-mt \
#                -lboost_unit_test_framework-mt
#    }
#}


# QuantLib
macx {
    INCLUDEPATH += ~/Documents/QuantLib/quantlib/QuantLib/
    CONFIG(debug, debug|release) {
        #LIBS += -L/opt/local/lib/ -lQuantLib
        #LIBS += -L/Users/osx/Documents/QuantLib/quantlib/build-QuantLib-Desktop_Qt_5_0_2_clang_64bit-Debug/lib -lquantlibd
        LIBS += -L/Users/osx/Documents/QuantLib/quantlib/QuantLib/lib -lquantlibd
    } else {
        #LIBS += -L/opt/local/lib/ -lQuantLib
        #LIBS += -L/Users/osx/Documents/QuantLib/quantlib/build-QuantLib-Desktop_Qt_5_0_2_clang_64bit-Release/lib -lquantlib
        LIBS += -L/Users/osx/Documents/QuantLib/quantlib/QuantLib/lib -lquantlib
    }

}
win32-msvc* {
    INCLUDEPATH += C:/QuantLib/QuantLibQt
    CONFIG(debug, debug|release) {
        LIBS += C:/QuantLib/QuantLibQt/lib/QuantLibQtd.lib
    } else {
        LIBS += C:/QuantLib/QuantLibQt/lib/QuantLibQt.lib
    }
}
win32-g++ {
    INCLUDEPATH += C:/QuantLib/QuantLibQt
    CONFIG(debug, debug|release) {
        LIBS += C:/QuantLib/QuantLibQt/lib/libQuantLibQtd.a
    } else {
        LIBS += C:/QuantLib/QuantLibQt/lib/libQuantLibQt.a
    }
}
#unix {
#    INCLUDEPATH += /home/deriversatile/QuantLib/QuantLibQt
#    CONFIG(debug, debug|release) {
#        LIBS += -L/home/deriversatile/QuantLib/quantlib/lib -lquantlibd
#    } else {
#        LIBS += -L/home/deriversatile/QuantLib/quantlib/lib -lquantlib
#    }
#}

win32-msvc* {
    QMAKE_CXXFLAGS += /wd4819
}


SOURCES += \
test-suite/americanoption.cpp \
test-suite/array.cpp \
test-suite/asianoptions.cpp \
test-suite/assetswap.cpp \
test-suite/autocovariances.cpp \
test-suite/barrieroption.cpp \
test-suite/basketoption.cpp \
test-suite/batesmodel.cpp \
test-suite/bermudanswaption.cpp \
test-suite/blackdeltacalculator.cpp \
test-suite/bonds.cpp \
test-suite/brownianbridge.cpp \
test-suite/calendars.cpp \
test-suite/capfloor.cpp \
test-suite/capflooredcoupon.cpp \
test-suite/cashflows.cpp \
test-suite/cdo.cpp \
test-suite/cdsoption.cpp \
test-suite/chooseroption.cpp \
test-suite/cliquetoption.cpp \
test-suite/cms.cpp \
test-suite/commodityunitofmeasure.cpp \
test-suite/compoundoption.cpp \
test-suite/convertiblebonds.cpp \
test-suite/covariance.cpp \
test-suite/creditdefaultswap.cpp \
test-suite/curvestates.cpp \
test-suite/dates.cpp \
test-suite/daycounters.cpp \
test-suite/defaultprobabilitycurves.cpp \
test-suite/digitalcoupon.cpp \
test-suite/digitaloption.cpp \
test-suite/distributions.cpp \
test-suite/dividendoption.cpp \
test-suite/europeanoption.cpp \
test-suite/everestoption.cpp \
test-suite/exchangerate.cpp \
test-suite/extendedtrees.cpp \
test-suite/factorial.cpp \
test-suite/fastfouriertransform.cpp \
test-suite/fdheston.cpp \
test-suite/fdmlinearop.cpp \
test-suite/forwardoption.cpp \
test-suite/garch.cpp \
test-suite/gaussianquadratures.cpp \
test-suite/gjrgarchmodel.cpp \
test-suite/hestonmodel.cpp \
test-suite/himalayaoption.cpp \
test-suite/hybridhestonhullwhiteprocess.cpp \
test-suite/inflation.cpp \
test-suite/inflationcapfloor.cpp \
test-suite/inflationcapflooredcoupon.cpp \
test-suite/inflationcpibond.cpp \
test-suite/inflationcpicapfloor.cpp \
test-suite/inflationcpiswap.cpp \
test-suite/inflationvolatility.cpp \
test-suite/instruments.cpp \
test-suite/integrals.cpp \
test-suite/interestrates.cpp \
test-suite/interpolations.cpp \
test-suite/jumpdiffusion.cpp \
test-suite/libormarketmodel.cpp \
test-suite/libormarketmodelprocess.cpp \
test-suite/linearleastsquaresregression.cpp \
test-suite/lookbackoptions.cpp \
test-suite/lowdiscrepancysequences.cpp \
test-suite/margrabeoption.cpp \
test-suite/marketmodel.cpp \
test-suite/marketmodel_cms.cpp \
test-suite/marketmodel_smm.cpp \
test-suite/marketmodel_smmcapletalphacalibration.cpp \
test-suite/marketmodel_smmcapletcalibration.cpp \
test-suite/marketmodel_smmcaplethomocalibration.cpp \
test-suite/markovfunctional.cpp \
test-suite/matrices.cpp \
test-suite/mclongstaffschwartzengine.cpp \
test-suite/mersennetwister.cpp \
test-suite/money.cpp \
test-suite/nthtodefault.cpp \
test-suite/ode.cpp \
test-suite/operators.cpp \
test-suite/optimizers.cpp \
test-suite/optionletstripper.cpp \
test-suite/overnightindexedswap.cpp \
test-suite/pagodaoption.cpp \
test-suite/pathgenerator.cpp \
test-suite/period.cpp \
test-suite/piecewiseyieldcurve.cpp \
test-suite/quantlibbenchmark.cpp \
test-suite/quantlibtestsuite.cpp \
test-suite/quantooption.cpp \
test-suite/quotes.cpp \
test-suite/rangeaccrual.cpp \
test-suite/riskstats.cpp \
test-suite/rngtraits.cpp \
test-suite/rounding.cpp \
test-suite/sampledcurve.cpp \
test-suite/schedule.cpp \
test-suite/shortratemodels.cpp \
test-suite/solvers.cpp \
test-suite/spreadoption.cpp \
test-suite/stats.cpp \
test-suite/surface.cpp \
test-suite/swap.cpp \
test-suite/swapforwardmappings.cpp \
test-suite/swaption.cpp \
test-suite/swaptionvolatilitycube.cpp \
test-suite/swaptionvolatilitymatrix.cpp \
test-suite/swingoption.cpp \
test-suite/termstructures.cpp \
test-suite/timeseries.cpp \
test-suite/tqreigendecomposition.cpp \
test-suite/tracing.cpp \
test-suite/transformedgrid.cpp \
test-suite/twoassetbarrieroption.cpp \
test-suite/utilities.cpp \
test-suite/variancegamma.cpp \
test-suite/varianceoption.cpp \
test-suite/varianceswaps.cpp \
test-suite/volatilitymodels.cpp \
test-suite/vpp.cpp \
test-suite/writerextensibleoption.cpp




HEADERS += \
test-suite/americanoption.hpp \
test-suite/array.hpp \
test-suite/asianoptions.hpp \
test-suite/assetswap.hpp \
test-suite/autocovariances.hpp \
test-suite/barrieroption.hpp \
test-suite/basketoption.hpp \
test-suite/batesmodel.hpp \
test-suite/bermudanswaption.hpp \
test-suite/blackdeltacalculator.hpp \
test-suite/bonds.hpp \
test-suite/brownianbridge.hpp \
test-suite/calendars.hpp \
test-suite/capfloor.hpp \
test-suite/capflooredcoupon.hpp \
test-suite/cashflows.hpp \
test-suite/cdo.hpp \
test-suite/cdsoption.hpp \
test-suite/chooseroption.hpp \
test-suite/cliquetoption.hpp \
test-suite/cms.hpp \
test-suite/commodityunitofmeasure.hpp \
test-suite/compoundoption.hpp \
test-suite/convertiblebonds.hpp \
test-suite/covariance.hpp \
test-suite/creditdefaultswap.hpp \
test-suite/curvestates.hpp \
test-suite/dates.hpp \
test-suite/daycounters.hpp \
test-suite/defaultprobabilitycurves.hpp \
test-suite/digitalcoupon.hpp \
test-suite/digitaloption.hpp \
test-suite/distributions.hpp \
test-suite/dividendoption.hpp \
test-suite/europeanoption.hpp \
test-suite/everestoption.hpp \
test-suite/exchangerate.hpp \
test-suite/extendedtrees.hpp \
test-suite/factorial.hpp \
test-suite/fastfouriertransform.hpp \
test-suite/fdheston.hpp \
test-suite/fdmlinearop.hpp \
test-suite/forwardoption.hpp \
test-suite/garch.hpp \
test-suite/gaussianquadratures.hpp \
test-suite/gjrgarchmodel.hpp \
test-suite/hestonmodel.hpp \
test-suite/himalayaoption.hpp \
test-suite/hybridhestonhullwhiteprocess.hpp \
test-suite/inflation.hpp \
test-suite/inflationcapfloor.hpp \
test-suite/inflationcapflooredcoupon.hpp \
test-suite/inflationcpibond.hpp \
test-suite/inflationcpicapfloor.hpp \
test-suite/inflationcpiswap.hpp \
test-suite/inflationvolatility.hpp \
test-suite/instruments.hpp \
test-suite/integrals.hpp \
test-suite/interestrates.hpp \
test-suite/interpolations.hpp \
test-suite/jumpdiffusion.hpp \
test-suite/libormarketmodel.hpp \
test-suite/libormarketmodelprocess.hpp \
test-suite/linearleastsquaresregression.hpp \
test-suite/lookbackoptions.hpp \
test-suite/lowdiscrepancysequences.hpp \
test-suite/margrabeoption.hpp \
test-suite/marketmodel.hpp \
test-suite/marketmodel_cms.hpp \
test-suite/marketmodel_smm.hpp \
test-suite/marketmodel_smmcapletalphacalibration.hpp \
test-suite/marketmodel_smmcapletcalibration.hpp \
test-suite/marketmodel_smmcaplethomocalibration.hpp \
test-suite/markovfunctional.hpp \
test-suite/matrices.hpp \
test-suite/mclongstaffschwartzengine.hpp \
test-suite/mersennetwister.hpp \
test-suite/money.hpp \
test-suite/nthtodefault.hpp \
test-suite/ode.hpp \
test-suite/operators.hpp \
test-suite/optimizers.hpp \
test-suite/optionletstripper.hpp \
test-suite/overnightindexedswap.hpp \
test-suite/pagodaoption.hpp \
test-suite/pathgenerator.hpp \
test-suite/period.hpp \
test-suite/piecewiseyieldcurve.hpp \
test-suite/quantooption.hpp \
test-suite/quotes.hpp \
test-suite/rangeaccrual.hpp \
test-suite/riskstats.hpp \
test-suite/rngtraits.hpp \
test-suite/rounding.hpp \
test-suite/sampledcurve.hpp \
test-suite/schedule.hpp \
test-suite/shortratemodels.hpp \
test-suite/solvers.hpp \
test-suite/spreadoption.hpp \
test-suite/stats.hpp \
test-suite/surface.hpp \
test-suite/swap.hpp \
test-suite/swapforwardmappings.hpp \
test-suite/swaption.hpp \
test-suite/swaptionvolatilitycube.hpp \
test-suite/swaptionvolatilitymatrix.hpp \
test-suite/swaptionvolstructuresutilities.hpp \
test-suite/swingoption.hpp \
test-suite/termstructures.hpp \
test-suite/timeseries.hpp \
test-suite/tqreigendecomposition.hpp \
test-suite/tracing.hpp \
test-suite/transformedgrid.hpp \
test-suite/twoassetbarrieroption.hpp \
test-suite/utilities.hpp \
test-suite/variancegamma.hpp \
test-suite/varianceoption.hpp \
test-suite/varianceswaps.hpp \
test-suite/volatilitymodels.hpp \
test-suite/vpp.hpp \
test-suite/writerextensibleoption.hpp


EVERYTHING = $$SOURCES $$HEADERS
message("The project contains the following files:")
message($$EVERYTHING)
message(Qt version: $$[QT_VERSION])
message(Qt is installed in $$[QT_INSTALL_PREFIX])
message(Qt resources can be found in the following locations:)
message(Documentation: $$[QT_INSTALL_DOCS])
message(Header files: $$[QT_INSTALL_HEADERS])
message(Libraries: $$[QT_INSTALL_LIBS])
message(Binary files (executables): $$[QT_INSTALL_BINS])
message(Plugins: $$[QT_INSTALL_PLUGINS])
message(Data files: $$[QT_INSTALL_DATA])
message(Translation files: $$[QT_INSTALL_TRANSLATIONS])
message(Settings: $$[QT_INSTALL_SETTINGS])
message(Examples: $$[QT_INSTALL_EXAMPLES])
message(Demonstrations: $$[QT_INSTALL_DEMOS])

