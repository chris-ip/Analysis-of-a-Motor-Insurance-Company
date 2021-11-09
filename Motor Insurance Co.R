###Open and Rename Dataset
Assignment.1...data.set <- read.csv("~/Downloads/Assignment 1 - data set.csv")
Insurance <- Assignment.1...data.set
###Coerce into factor type
Insurance$Kilometres <- as.factor(Insurance$Kilometres)
Insurance$Zone <- as.factor(Insurance$Zone)
Insurance$Bonus <- as.factor(Insurance$Bonus)
Insurance$Make <- as.factor(Insurance$Make)
###Align numbers of "Insured" and "Claims"
Insurance$Insured <- Insurance$Insured*1000
###Question A
###Call Packages
library(ggplot2)
library(Hmisc)
###Graph1
KilovsPayment <- ggplot(Insurance, aes(Kilometres, Payment))
KilovsPayment + stat_summary(fun.y = 'mean', geom = 'bar', colour = 'black', fill = 'blue') + stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.2) + labs(x = 'Kilometers traveled', y = 'Mean of Payment (Skr)') + ggtitle('Kilometers traveled VS Payment') + theme(plot.title = element_text(hjust = 0.5))
###Graph2
KilovsClaims <- ggplot(Insurance, aes(Kilometres, Claims))
KilovsClaims + stat_summary(fun.y = 'mean', geom = 'bar', colour = 'black', fill = 'blue') + stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.2) + labs(x = 'Kilometers traveled', y = 'Mean of Claims') + ggtitle('Kilometers traveled VS Claims') + theme(plot.title = element_text(hjust = 0.5))
###Graph3
ZonevsPayment <- ggplot(Insurance, aes(Zone, Payment))
ZonevsPayment + stat_summary(fun.y = 'mean', geom = 'bar', colour = 'black', fill = 'blue') + stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.2) + labs(x = 'Geographical Zone', y = 'Mean of Payment (Skr)') + ggtitle('Geographical Zone VS Payment') + theme(plot.title = element_text(hjust = 0.5))
###Graph4
ZonevsClaims <- ggplot(Insurance, aes(Zone, Claims))
ZonevsClaims + stat_summary(fun.y = 'mean', geom = 'bar', colour = 'black', fill = 'blue') + stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.2) + labs(x = 'Geographical Zone', y = 'Mean of Claims') + ggtitle('Geographical Zone VS Claims') + theme(plot.title = element_text(hjust = 0.5))
###Graph5
BonusvsPayment <- ggplot(Insurance, aes(Bonus, Payment))
BonusvsPayment + stat_summary(fun.y = 'mean', geom = 'bar', colour = 'black', fill = 'blue') + stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.2) + labs(x = 'No Claims Bonus (Year+1)', y = 'Mean of Payment (Skr)') + ggtitle('No Claims Bonus VS Payment') + theme(plot.title = element_text(hjust = 0.5))
###Graph6
BonusvsClaims <- ggplot(Insurance, aes(Bonus, Claims))
BonusvsClaims + stat_summary(fun.y = 'mean', geom = 'bar', colour = 'black', fill = 'blue') + stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.2) + labs(x = 'No Claims Bonus (Year+1)', y = 'Mean of Claims') + ggtitle('No Claims Bonus VS Claims') + theme(plot.title = element_text(hjust = 0.5))
###Graph7
MakevsPayment <- ggplot(Insurance, aes(Make, Payment))
MakevsPayment + stat_summary(fun.y = 'mean', geom = 'bar', colour = 'black', fill = 'blue') + stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.2) + labs(x = 'Car Models', y = 'Mean of Payment (Skr)') + ggtitle('Car Models VS Payment') + theme(plot.title = element_text(hjust = 0.5))
###Graph8
MakevsClaims <- ggplot(Insurance, aes(Make, Claims))
MakevsClaims + stat_summary(fun.y = 'mean', geom = 'bar', colour = 'black', fill = 'blue') + stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.2) + labs(x = 'Car Models', y = 'Mean of Claims') + ggtitle('Car Models VS Claims') + theme(plot.title = element_text(hjust = 0.5))
###Graph9
ClaimsvsPayment <- ggplot(Insurance, aes(Insured, Claims, colour = Payment))
ClaimsvsPayment + geom_point() + stat_smooth(method = "lm", col = "blue") + labs(x = 'Insured', y = 'Claims') + ggtitle('Insured VS Claims VS Payment') + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method = 'lm')
###Central Tendencies and Dispersions
summary(Insurance)
###Question B
###The Test
InsuranceCorr <- data.frame(Insurance[,c('Insured', 'Claims', 'Payment')])
cor(InsuranceCorr, use = 'everything', method = 'spearman')
###Question C
###Call Packages
library(car)
library(QuantPsyc)
library(lmtest)
###Creating Models Using Hierarchical
PaymentModel1 <- lm(Payment~Claims, data = Insurance)
summary(PaymentModel1)
PaymentModel2 <- lm(Payment~Claims + Kilometres + Zone + Bonus + Make, data = Insurance)
summary(PaymentModel2)
###Anova
anova(PaymentModel1, PaymentModel2)
###Checking for Outliers
Insurance$Strdresid.Payment <- rstandard(PaymentModel2)
Insurance$LargeResid.Payment <- Insurance$Strdresid.Payment > 2.58 | Insurance$Strdresid.Payment < -2.58
sum(Insurance$LargeResid.Payment)
###Checking for Influential Cases
par(mfrow=c(2,2))
plot(cooks.distance(PaymentModel2))
Insurance$CD.Payment <- cooks.distance(PaymentModel2)
Insurance$Large.CD.Payment <- Insurance$CD.Payment > 1
plot(hatvalues(PaymentModel2))
Insurance$Hatvalues <- hatvalues(PaymentModel2)
###Sum of Poor Residuals
sum(Insurance$Large.CD.Payment & (Insurance$Strdresid.Payment > 3.29 | Insurance$Strdresid.Payment < -3.29) & Insurance$Hatvalues > 2*(1+5)/nrow(Insurance))
###Removal of Poor Residuals
Insurance <- Insurance[!(Insurance$Large.CD.Payment & (Insurance$Strdresid.Payment > 3.29 | Insurance$Strdresid.Payment < -3.29) & Insurance$Hatvalues > 2*(1+5)/nrow(Insurance)),]
###Check Again
PaymentModel2 <- lm(Payment~Claims + Kilometres + Zone + Bonus + Make, data = Insurance)
plot(cooks.distance(PaymentModel2))
plot(hatvalues(PaymentModel2))
par(mfrow=c(1,1))
###Checking for Independent error
dwtest(PaymentModel2)
###Checking for Multicollinearity
vif(PaymentModel2)
mean(vif(PaymentModel2))
###Checking for Linearity and Homoscedasticity
par(mfrow=c(2,2))
plot(PaymentModel2)
par(mfrow=c(1,1))
###summary
summary(PaymentModel2)
###Second Model
###Open and Rename Dataset
Assignment.1...data.set <- read.csv("~/Downloads/Assignment 1 - data set.csv")
Insurance <- Assignment.1...data.set
###Coerce into factor type
Insurance$Kilometres <- as.factor(Insurance$Kilometres)
Insurance$Zone <- as.factor(Insurance$Zone)
Insurance$Bonus <- as.factor(Insurance$Bonus)
Insurance$Make <- as.factor(Insurance$Make)
###Align numbers of "Insured" and "Claims"
Insurance$Insured <- Insurance$Insured*1000
###Creating Models Using Hierarchical
PaymentModel3 <- lm(Payment~Insured, data = Insurance)
summary(PaymentModel3)
PaymentModel4 <- lm(Payment~Insured + Kilometres + Zone + Bonus + Make, data = Insurance)
summary(PaymentModel4)
###Anova
anova(PaymentModel3, PaymentModel4)
###Checking for Outliers
Insurance$Strdresid.Payment <- rstandard(PaymentModel4)
Insurance$LargeResid.Payment <- Insurance$Strdresid.Payment > 2.58 | Insurance$Strdresid.Payment < -2.58
sum(Insurance$LargeResid.Payment)
###Checking for Influential Cases
par(mfrow=c(2,2))
plot(cooks.distance(PaymentModel4))
Insurance$CD.Payment <- cooks.distance(PaymentModel4)
Insurance$Large.CD.Payment <- Insurance$CD.Payment > 1
plot(hatvalues(PaymentModel4))
Insurance$Hatvalues <- hatvalues(PaymentModel4)
###Sum of Poor Residuals
sum(Insurance$Large.CD.Payment & (Insurance$Strdresid.Payment > 3.29 | Insurance$Strdresid.Payment < -3.29) & Insurance$Hatvalues > 2*(1+5)/nrow(Insurance))
###Removal of Poor Residuals
Insurance <- Insurance[!(Insurance$Large.CD.Payment & (Insurance$Strdresid.Payment > 3.29 | Insurance$Strdresid.Payment < -3.29) & Insurance$Hatvalues > 2*(1+5)/nrow(Insurance)),]
###Check Again
PaymentModel4 <- lm(Payment~Insured + Kilometres + Zone + Bonus + Make, data = Insurance)
plot(cooks.distance(PaymentModel4))
plot(hatvalues(PaymentModel4))
par(mfrow=c(1,1))
###Checking for Independent error
dwtest(PaymentModel4)
###Checking for Multicollinearity
vif(PaymentModel4)
mean(vif(PaymentModel4))
###Checking for Linearity and Homoscedasticity
par(mfrow=c(2,2))
plot(PaymentModel4)
par(mfrow=c(1,1))
###summary
summary(PaymentModel4)
###Question D
###Open and Rename Dataset
Assignment.1...data.set <- read.csv("~/Downloads/Assignment 1 - data set.csv")
Insurance <- Assignment.1...data.set
###Coerce into factor type
Insurance$Kilometres <- as.factor(Insurance$Kilometres)
Insurance$Zone <- as.factor(Insurance$Zone)
Insurance$Bonus <- as.factor(Insurance$Bonus)
Insurance$Make <- as.factor(Insurance$Make)
###Align numbers of "Insured" and "Claims"
Insurance$Insured <- Insurance$Insured*1000
###Create model
ClaimsModel <- lm(Claims~Insured + Zone + Kilometres + Bonus + Make, data = Insurance)
ClaimsModel1 <- lm(Claims~1, data = Insurance)
###Stepwise Both
step(ClaimsModel1, direction = 'both', scope = formula(ClaimsModel))
###summary
summary(ClaimsModel)
###Checking for Outliers
Insurance$Strdresid.Claims <- rstandard(ClaimsModel)
Insurance$LargeResid.Claims <- Insurance$Strdresid.Claims > 2.58 | Insurance$Strdresid.Claims < -2.58
sum(Insurance$LargeResid.Claims)
###Checking for Influential Cases
par(mfrow=c(2,2))
plot(cooks.distance(ClaimsModel))
Insurance$CD.Claims <- cooks.distance(ClaimsModel)
Insurance$Large.CD.Claims <- Insurance$CD.Claims > 1
plot(hatvalues(ClaimsModel))
Insurance$Hatvalues <- hatvalues(ClaimsModel)
###Sum of Poor Residuals
sum(Insurance$Large.CD.Claims & (Insurance$Strdresid.Claims > 3.29 | Insurance$Strdresid.Claims < -3.29) & Insurance$Hatvalues > 2*(1+5)/nrow(Insurance))
###Removal of Poor Residuals
Insurance <- Insurance[!(Insurance$Large.CD.Claims & (Insurance$Strdresid.Claims > 3.29 | Insurance$Strdresid.Claims < -3.29) & Insurance$Hatvalues > 2*(1+5)/nrow(Insurance)),]
###Check Again
ClaimsModel <- lm(Claims~Insured + Zone + Kilometres + Bonus + Make, data = Insurance)
plot(cooks.distance(ClaimsModel))
plot(hatvalues(ClaimsModel))
par(mfrow=c(1,1))
###Checking for Independent error
dwtest(ClaimsModel)
###Checking for Multicollinearity
vif(ClaimsModel)
mean(vif(ClaimsModel))
###Checking for Linearity and Homoscedasticity
par(mfrow=c(2,2))
plot(ClaimsModel)
par(mfrow=c(1,1))
###summary
summary(ClaimsModel)
###Question E(i)
###Case1 Claims
Case1Claims <- predict(ClaimsModel, data.frame(Zone = as.factor(5), Kilometres = as.factor(2), Bonus = as.factor(2+1), Make = as.factor(3), Insured = 4621000) )
Case1Claims
###Case1 Payment
predict(PaymentModel2, data.frame(Zone = as.factor(5), Kilometres = as.factor(2), Bonus = as.factor(2+1), Make = as.factor(3), Claims = Case1Claims) )
###Question E(ii)
###Case2 Claims
Case2Claims <- predict(ClaimsModel, data.frame(Zone = as.factor(3), Kilometres = as.factor(2), Bonus = as.factor(0+1), Make = as.factor(9), Insured = 9500000) )
Case2Claims
####Case2 Payment
predict(PaymentModel2, data.frame(Zone = as.factor(3), Kilometres = as.factor(2), Bonus = as.factor(0+1), Make = as.factor(9), Claims = Case2Claims) )
###Question E(iii)
###Case3 Claims
Case3ClaimsLwr <- predict(ClaimsModel, data.frame(Zone = as.factor(2), Kilometres = as.factor(4), Bonus = as.factor(4+1), Make = as.factor(3), Insured = 17500000))
Case3ClaimsLwr
Case3ClaimsUpr <- predict(ClaimsModel, data.frame(Zone = as.factor(2), Kilometres = as.factor(4), Bonus = as.factor(4+1), Make = as.factor(3), Insured = 25416000))
Case3ClaimsUpr
###Case3 Payment
predict(PaymentModel2, data.frame(Zone = as.factor(2), Kilometres = as.factor(4), Bonus = as.factor(4+1), Make = as.factor(3), Claims = Case3ClaimsLwr) )
predict(PaymentModel2, data.frame(Zone = as.factor(2), Kilometres = as.factor(4), Bonus = as.factor(4+1), Make = as.factor(3), Claims = Case3ClaimsUpr) )
###Question F
###Recall Payment Regession Model in C
summary(PaymentModel2)
###Recall Claims Regession Model in D
summary(ClaimsModel)
###Build Model to Answer for Insured
###Open and Rename Dataset
Assignment.1...data.set <- read.csv("~/Downloads/Assignment 1 - data set.csv")
Insurance <- Assignment.1...data.set
###Coerce into factor type
Insurance$Kilometres <- as.factor(Insurance$Kilometres)
Insurance$Zone <- as.factor(Insurance$Zone)
Insurance$Bonus <- as.factor(Insurance$Bonus)
Insurance$Make <- as.factor(Insurance$Make)
###Align numbers of "Insured" and "Claims"
Insurance$Insured <- Insurance$Insured*1000
###Create model
InsuredModel <- lm(Insured~Claims + Zone + Kilometres + Bonus + Make, data = Insurance)
InsuredModel1 <- lm(Insured~1, data = Insurance)
###Stepwise Both
step(InsuredModel1, direction = 'both', scope = formula(InsuredModel))
###summary
summary(InsuredModel)
###Checking for Outliers
Insurance$Strdresid.Insured <- rstandard(InsuredModel)
Insurance$LargeResid.Insured <- Insurance$Strdresid.Insured > 2.58 | Insurance$Strdresid.Insured < -2.58
sum(Insurance$LargeResid.Insured)
###Checking for Influential Cases
par(mfrow=c(2,2))
plot(cooks.distance(InsuredModel))
Insurance$CD.Insured <- cooks.distance(InsuredModel)
Insurance$Large.CD.Insured <- Insurance$CD.Insured > 1
plot(hatvalues(InsuredModel))
Insurance$Hatvalues <- hatvalues(InsuredModel)
###Sum of Poor Residuals
sum(Insurance$Large.CD.Insured & (Insurance$Strdresid.Insured > 3.29 | Insurance$Strdresid.Insured < -3.29) & Insurance$Hatvalues > 2*(1+4)/nrow(Insurance))
###Removal of Poor Residuals
Insurance <- Insurance[!(Insurance$Large.CD.Insured & (Insurance$Strdresid.Insured > 3.29 | Insurance$Strdresid.Insured < -3.29) & Insurance$Hatvalues > 2*(1+4)/nrow(Insurance)),]
###Check Again
InsuredModel <- lm(Insured~Claims + Zone + Kilometres + Bonus + Make, data = Insurance)
plot(cooks.distance(InsuredModel))
plot(hatvalues(InsuredModel))
par(mfrow=c(1,1))
###Checking for Independent error
dwtest(InsuredModel)
###Checking for Multicollinearity
vif(InsuredModel)
mean(vif(InsuredModel))
###Checking for Linearity and Homoscedasticity
par(mfrow=c(2,2))
plot(InsuredModel)
par(mfrow=c(1,1))
#summary
summary(InsuredModel)