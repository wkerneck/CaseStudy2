#Analysis of Basic Data
#Module 2
par(mfrow=c(2,1))
plot(RedEtelAsIs, col="red", main="RedEtelAsIs")
plot(RedEtelPlan, col="red", main="RedEtelPlan")
cor(RedEtelAsIs , RedEtelPlan)
RedEtelAsIs_stl <- stl(RedEtelAsIs , s.window=5)
par(mfrow=c(2,1))
plot(RedEtelAsIs_stl, col="red", main="RedEtelAsIs_stl")
plot(RedEtelAsIs_stl$time.series[,"trend"], col="red")
monthplot(RedEtelAsIs_stl$time.series[,"seasonal"], main="", ylab="Seasonal")
