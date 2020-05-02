
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

# Please dont use attach because its easy to mistake cols with other dfs in multi-df envts.
# remember to detach once you are done to avoid errors down the line
attach(Pull)

LOGY= log(Y) 
LOGY1= log(lag(Y,1))

AnnGDP = (LOGY-LOGY1)*12
GDP_LOG = log(Y) 

# X1 Functional Form Test

X1Squared= X1*X1
X1Root= sqrt(X1)
TestX1=lm(AnnGDP~X1+X1Squared)
TestX1
stargazer(TestX1, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
A1=(X1+X1Squared)
summary(TestX1)
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
# Detach
detach(Pull)

# Gathering all predictors 

predictors <- cbind(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,GDP_LOG) %>%
  as.data.frame()

saveRDS(predictors, "predictors.RDS")
# function to automate lag calculation
lagged_predictors <- function(n_lags, predictor_df, n_predictors=12){
  pred_df <- sapply(1:n_predictors, function(x){
    lag(predictor_df[,x],n_lags)
  } )
  pred_df <- as.data.frame(pred_df)
  return(pred_df)
}

set.seed(1728)


# one month lag
predictors_oml <- lagged_predictors(1,predictors)
predictors_oml <- cbind(GDP_LOG, predictors_oml) %>%
  drop_na()

# sampling then testing
testIDs <- sample(1:dim(predictors_oml)[1], 0.8*dim(predictors_oml)[1])
training_predictors_oml <- predictors_oml[testIDs,]
testing_predictors_oml <- predictors_oml[-testIDs,]
gdp_oml_lm <- lm(GDP_LOG ~., data = training_predictors_oml)
summary(gdp_oml_lm)

pred_gdp_log_oml <- predict.lm(gdp_oml_lm, newdata = testing_predictors_oml)

ggplot() + 
  geom_point(aes(x = testing_predictors_oml$GDP_LOG, y = pred_gdp_log_oml)) + 
  geom_abline(intercept = 0, slope = 1) + 
  labs(x = "Actual Logged GDP",
       y = "Predicted Logged GDP")


#One Month Lag Model 

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

GDP_LOG_OneMonthLag=lag(GDP_LOG,1)

GDP_OML= lm(GDP_LOG~OML1+OML2+OML3+OML6+OML7+OML8+OML10+GDP_LOG_OneMonthLag)



GDP_OML
stargazer(GDP_OML, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

summary(GDP_OML)


OneMonthForecast = (-2.356)+( -2.633e-10 *OML1)+(OML2*--6.691e-06 )+(OML3* 3.185e-02)+(OML6* 2.175e-04)+(OML7*-4.831e+00)+(OML8* -3.630e-04 )+(-2.186e-06*OML10)+(GDP_LOG_OneMonthLag*  9.990e-01)

# Two Month Lag Forecast

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


GDP_TML= lm(GDP_LOG~TML2+TML9+TML11+OneMonthForecast)
GDP_TML
stargazer(GDP_TML, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)


summary(GDP_TML)

TwoMonthForecast=(-6.349e-06*TML2)+(2.684e-08*TML9)+(-1.174e-05*TML11)+(9.989e-01*OneMonthForecast) +(7.542e-03)



# Three Month Lag Forecast

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

GDP_THML= lm(GDP_LOG~THML5+THML6+THML7+THML11+TwoMonthForecast)
GDP_THML
stargazer(GDP_THML, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

ThreeMonthForecast= (.001*THML5)+(-.0001*THML6)+(1.780*THML7)+(-.00004*THML11)+(1.002*TwoMonthForecast)

# Four Month Lag Forecast

FML1=Lag(A1,4)
FML2=Lag(A2,4)
FML3=Lag(A3,4)
FML4=Lag(A4,4)
FML5=Lag(A5,4)
FML6=Lag(A6,4)
FML7=Lag(A7,4)
FML8=Lag(A8,4)
FML9=Lag(A9,4)
FML10=Lag(A10,4)
FML11=Lag(A11,4)

GDP_FML= lm(GDP_LOG~FML5+FML7+ThreeMonthForecast)
GDP_FML
stargazer(GDP_FML, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

FourMonthForecast= (.001*FML5)+(.429*FML7)+(ThreeMonthForecast*1.003)+1.039

#Five month Lag Forecast

FIML1=Lag(A1,5)
FIML2=Lag(A2,5)
FIML3=Lag(A3,5)
FIML4=Lag(A4,5)
FIML5=Lag(A5,5)
FIML6=Lag(A6,5)
FIML7=Lag(A7,5)
FIML8=Lag(A8,5)
FIML9=Lag(A9,5)
FIML10=Lag(A10,5)
FIML11=Lag(A11,5)

GDP_FIML= lm(GDP_LOG~FIML3+FIML4+FIML7+FIML10+FourMonthForecast)
GDP_FIML
stargazer(GDP_FIML, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

FiveMonthForecast=(.016*FIML3)+(.001*FIML4)+(-.04*FIML7)+(-.00001*FIML10)+(FourMonthForecast*1.003)+(-.03)

#Six month Lag Forecast

SML1=Lag(A1,6)
SML2=Lag(A2,6)
SML3=Lag(A3,6)
SML4=Lag(A4,6)
SML5=Lag(A5,6)
SML6=Lag(A6,6)
SML7=Lag(A7,6)
SML8=Lag(A8,6)
SML9=Lag(A9,6)
SML10=Lag(A10,6)
SML11=Lag(A11,6)

GDP_SML= lm(GDP_LOG~SML2+SML7+SML9+SML11+FiveMonthForecast)
GDP_SML
stargazer(GDP_SML, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

summary(GDP_SML)

SixMonthForecast= (.004418)+(-.000006364*SML2)+(-.04146*SML7)+(.00000002817*SML9)+(-.00001435*SML11)+(.9992*FiveMonthForecast)







