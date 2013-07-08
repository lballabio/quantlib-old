QT       -= core gui
TEMPLATE = subdirs
CONFIG -= app_bundle
CONFIG -= qt
#CONFIG += console

#SOURCES += main.cpp

SUBDIRS = \
BermudanSwaption \
Bonds \
CallableBonds \
CDS \
ConvertibleBonds \
DiscreteHedging \
EquityOption \
##FDLondon \
##FDLondonOO \
FiniteDifference \
FittedBondCurve \
FRA \
#Futures \
JumpDiffusion \
###LiborMarketModel \
MarketModels \
#MiniExamples \
##QLExercise \
###QuantoOption \
#RainbowOption \
Replication \
Repo \
Swap \
TestInflation \
##QLMBS \
##QLRiskParity \
###QLTestHestonModel \
###QLTestShortRateModel
