
install.packages("haven")
library(quantmod)
library(stargazer)
library(fredr)
library(tidyr)
library(PASWR2)
library(MASS)
library(repmis)
library(latex2exp)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RCurl)
library(haven)
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # My key, please don't abuse.

Pull <- readxl::read_excel("./data/Pull.xlsx")
attach(Pull)

# Real GDP (Level) > Y
# Number of Oil Rigs > X1
#Electricity	> X2
#Probability of Losing a Job >X3
#Job Openings per Unemployed Person >X4	
#Ratio of Hires to Openings	> X5
#Business Survey > X6
#Business Survey (Peak Diff) > X7
#Yield Curve	> X8
#Exonomic Policy Index	> X9
#Equity Uncertainty	> X10
#Unemployment Rate (YoY) >X11

LOGY= log(Y) 
LOGY1= log(lag(Y,1))
LOGY2=log(lag(Y,2))
LOGY3=log(lag(Y,3))
LOGY4=log(lag(Y,4))
LOGY5=log(lag(Y,5))
LOGY6=log(lag(Y,6))
LOGY7=log(lag(Y,7))

CurrentAnnualizedGDP = (LOGY-LOGY1)
PriorMonthChange= (LOGY1-LOGY2)
TwoPriorMonthChange= (LOGY2-LOGY3)
ThreePriorMonthChange= (LOGY3-LOGY4)
FourPriorMonthChange= (LOGY4-LOGY5)
FivePriorMonthChange= (LOGY5-LOGY6)
SixPriorMonthChange= (LOGY6-LOGY7)

# X1 Functional Form Test

X1Squared= X1*X1
X1Root= sqrt(X1)
TestX1=lm(AnnGDP~X1+X1Squared)
TestX1
stargazer(TestX1, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
A1=(X1+X1Squared)

# X2 Functional Form Test

X2Squared= X2*X2
X2Root= sqrt(X2)
TestX2=lm(AnnGDP~X2)
TestX2
stargazer(TestX2, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

A2=X2

# X3 Functional Form Test

X3Squared= X3*X3
X3Root= sqrt(X3)
TestX3=lm(AnnGDP~X3)
TestX3
stargazer(TestX3, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

A3=X3

# X4 Functional Form Test

X4Squared= X4*X4
X4Root= sqrt(X4)
TestX4=lm(AnnGDP~X4+X4Squared)
TestX4
stargazer(TestX4, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

A4=(X4+X4Squared)

# X5 Functional Form Test

X5Squared= X5*X5
X5Root= sqrt(X5)
TestX5=lm(AnnGDP~X5+X5Squared)
TestX5
stargazer(TestX5, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

A5=X5+X5Squared


# X6 Functional Form Test
X6Squared= X6*X6
X6Root= sqrt(X6)
TestX6=lm(AnnGDP~X6+X6Squared)
TestX6
stargazer(TestX6, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

A6=X6+X6Squared

# X7 Functional Form Test
X7Squared= X7*X7
X7Root= sqrt(X7)
TestX7=lm(AnnGDP~X7+X7Squared)
TestX7
stargazer(TestX7, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

A7=X7+X7Squared

# X8 Functional Form Test
X8Squared= X8*X8
X8Root= sqrt(X8)
TestX8=lm(AnnGDP~X8)
TestX8
stargazer(TestX8, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

A8=X8


# X9 Functional Form Test
X9Squared= X9*X9
X9Root= sqrt(X9)
TestX9=lm(AnnGDP~X9+X9Squared)
TestX9
stargazer(TestX9, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

A9=X9+X9Squared

# X10 Functional Form Test
X10Squared= X10*X10
X10Root= sqrt(X10)
TestX10=lm(AnnGDP~X10)
TestX10
stargazer(TestX10, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

A10=X10


# X11 Functional Form Test
X11Squared= X11*X11
X11Root= sqrt(X11)
TestX11=lm(AnnGDP~X11)
TestX11
stargazer(TestX11, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

A11=X11



#First Test of All Best Form Variables

CurrentGDP=lm(GDP_LOG~ A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11)
CurrentGDP
stargazer(CurrentGDP, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

#One Month Ahead Forecast 

OML1=Lag(A1,1)
OML2=Lag(A2,1)
OML3=Lag(A3,1)
OML4=Lag(A4,1)
OML5=Lag(A5,1)
OML6=Lag(A6,1)
OML7=Lag(A7,1)
OML8=Lag(A8,1)
OML9=Lag(A9,1)
OML10=Lag(A10,1)
OML11=Lag(A11,1)
PriorMonthChange= (LOGY1-LOGY2)


MonthAheadForecast= lm(CurrentAnnualizedGDP~OML1+OML9+OML10+OML11+PriorMonthChange)


MonthAheadForecast
stargazer(MonthAheadForecast, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

summary(MonthAheadForecast)


OneMonthForecast=  7.440e-04 +(-2.294e-10*OML1)+(OML9* 1.810e-08)+(-1.288e-06*OML10)+(OML11*-1.024e-05)+(7.267e-01*PriorMonthChange)
  
# Two Month Ahead Forecast

TML1=Lag(A1,2)
TML2=Lag(A2,2)
TML3=Lag(A3,2)
TML4=Lag(A4,2)
TML5=Lag(A5,2)
TML6=Lag(A6,2)
TML7=Lag(A7,2)
TML8=Lag(A8,2)
TML9=Lag(A9,2)
TML10=Lag(A10,2)
TML11=Lag(A11,2)


GDP_TML= lm(CurrentAnnualizedGDP~TML1+TML5+TML9+TML11+TwoPriorMonthChange)
GDP_TML
stargazer(GDP_TML, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)


summary(GDP_TML)
TwoMonthForecast=2.400e-04+(-4.080e-10*TML1)+(4.758e-04*TML5)*(TML9* 1.826e-08)+(-2.833e-05*TML11)+(TwoPriorMonthChange*4.835e-01)



# Three Month Ahead Forecast

THML1=Lag(A1,3)
THML2=Lag(A2,3)
THML3=Lag(A3,3)
THML4=Lag(A4,3)
THML5=Lag(A5,3)
THML6=Lag(A6,3)
THML7=Lag(A7,3)
THML8=Lag(A8,3)
THML9=Lag(A9,3)
THML10=Lag(A10,3)
THML11=Lag(A11,3)

GDP_THML= lm(CurrentAnnualizedGDP~THML1+THML5+THML11+ThreePriorMonthChange)
GDP_THML
stargazer(GDP_THML, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

summary(GDP_THML)

ThreeMonthForecast=   4.653e-04+( -5.268e-10*THML1)+(7.660e-04*THML5)+(THML11*-3.923e-05)+(1.860e-01*ThreePriorMonthChange)

#Model Output to Be Used for Random Foreast

OneMonthAhead_LN=LOGY1+OneMonthForecast
TwoMonthAhead_LN=OneMonthAhead_LN+TwoMonthForecast
ThreeMonthAhead_LN=TwoMonthAhead_LN+ThreeMonthForecast


