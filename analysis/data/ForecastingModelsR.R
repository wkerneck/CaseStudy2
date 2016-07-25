#Chulwalhar Case Study
#Forecasting with Models

require(fpp) # for time series forecasting and analysis
require(forecast) # for some other forecasting models


#Simple expontential smoothing
Model_ses <- ses(RedEtelAsIs, h=12)
summary(Model_ses)

plot(Model_ses, plot.conf=FALSE, ylab="Red Etel Exports Chulwalar  )", xlab="Year", main="", fcol="white", type="o")
lines(fitted(Model_ses), col="green", type="o")
lines(Model_ses$mean, col="blue", type="o")
legend("topleft",lty=1, col=c(1,"green"), c("data", expression(alpha == 0.671)),pch=1)


#Holt's linear trend method

Model_holt_1 <- holt(RedEtelAsIs,h=12)
summary(Model_holt_1)
plot(Model_holt_1)


#Expoential trend
Model_holt_2<- holt(RedEtelAsIs, exponential=TRUE,h=12)
summary(Model_holt_2)
plot(Model_holt_2)


#Dampened trends
Model_holt_3 <- holt(RedEtelAsIs, damped=TRUE,h=12)
summary(Model_holt_3)
plot(Model_holt_3)

Model_holt_4 <- holt(RedEtelAsIs, exponential=TRUE, damped=TRUE,h=12)
summary(Model_holt_4)
plot(Model_holt_4)

# level and slope can be plotted individually for each model. 
plot(Model_holt_1$model$state)
plot(Model_holt_2$model$state)
plot(Model_holt_3$model$state)
plot(Model_holt_4$model$state)

plot(Model_holt_1, plot.conf=FALSE, ylab="Red Etel Exports Chulwalar  )", xlab="Year", main="", fcol="white", type="o")
lines(fitted(Model_ses), col="purple", type="o")
lines(fitted(Model_holt_1), col="blue", type="o")
lines(fitted(Model_holt_2), col="red", type="o")
lines(fitted(Model_holt_3), col="green", type="o")
lines(fitted(Model_holt_4), col="orange", type="o")
lines(Model_ses$mean, col="purple", type="o")
lines(Model_holt_1$mean, col="blue", type="o")
lines(Model_holt_2$mean, col="red", type="o")
lines(Model_holt_3$mean, col="green", type="o")
lines(Model_holt_4$mean, col="orange", type="o")
legend("topleft",lty=1, col=c(1,"purple","blue","red","green","orange"), c("data", "SES","Holts auto", "Exponential", "Additive Damped", "Multiplicative Damped"),pch=1)


#Holt-Winter's seasonal method
#Holt and Winters have expanded Holt's model further to include the seasonality aspect. The parameter gamma, which is for smoothing the seasonality, was added to achieve this. The values are better than the models without seasonality. This is logical, since the data is strongly influenced by seasonality.  In the following model, none of the parameters are given so that they will be optimised automatically. There are two models: one using an additive error model method and one using a multiplicative error model. The additive model gives slightly better results than the multiplicative model.

Model_hw_1 <- hw(RedEtelAsIs ,seasonal="additive",h=12)
summary(Model_hw_1)
plot(Model_hw_1)
# This forecast is a much better fit compared the the previous models. It seems to account for the seasonaility nicely, however it looks a bit off in the overall trend

Model_hw_2 <- hw(RedEtelAsIs ,seasonal="multiplicative",h=12)
summary(Model_hw_2)
plot(Model_hw_2)

#Compare Models

Model_ses_Comparison <- c(Model_ses$model$aic,Model_ses$model$aicc, Model_ses$model$bic)
Model_holt_1_Comparison <- c(Model_holt_1$model$aic,Model_holt_1$model$aicc, Model_holt_1$model$bic)
Model_holt_2_Comparison <- c(Model_holt_2$model$aic,Model_holt_2$model$aicc, Model_holt_2$model$bic)
Model_holt_3_Comparison <- c(Model_holt_3$model$aic,Model_holt_3$model$aicc, Model_holt_3$model$bic)
Model_holt_4_Comparison <- c(Model_holt_4$model$aic,Model_holt_4$model$aicc, Model_holt_4$model$bic)
Model_hw_1_Comparison <- c(Model_hw_1$model$aic,Model_hw_1$model$aicc, Model_hw_1$model$bic)
Model_hw_2_Comparison <- c(Model_hw_2$model$aic,Model_hw_2$model$aicc, Model_hw_2$model$bic)
Comparison <- c("AIC", "AICc", "BIC")

ModelCompare <- data.frame(Comparison, Model_ses_Comparison, Model_holt_1_Comparison, Model_holt_2_Comparison, Model_holt_3_Comparison, Model_holt_4_Comparison, Model_hw_1_Comparison, Model_hw_2_Comparison)
str(ModelCompare)

plot(Model_hw_1, ylab="Red Etel Exports Chulwalar  ", plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(fitted(Model_hw_1), col="red", lty=2)
lines(fitted(Model_hw_2), col="green", lty=2)
lines(Model_hw_1$mean, type="o", col="red")
lines(Model_hw_2$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))


## Forecast Estimates for Red Etel
Model_hw_2_df <-as.data.frame(Model_hw_2) 
Model_hw_2_PointForecast <- ts(Model_hw_2_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
Model_hw_2_PointForecast
