# setting up
mydata <- read.csv("~/Desktop/School/Stat 231/stat231f24dataset21058539.csv")
dim(mydata)
colnames(mydata)
sum(is.na(mydata))

# beginning of analysis
SanAntonio <- subset(mydata, city=="sa")
NewOrleans <- subset(mydata, city=="no")

# 1b.
mod <- lm(vehicle.year ~ subject.age, SanAntonio)
mod
summary(mod)
confint(mod, level = 0.95)

# 1d.
plot(SanAntonio$subject.age, SanAntonio$vehicle.year, xlab = 
      "subject.age", ylab = "vehicle.year",
      main = "Scatterplot of subject.age and subject.year in San Antonio", 
      pch = 1, cex = 0.5, col = "navy",las = 1, cex.axis = 0.5)
abline(coef(mod), lwd = 2, lty = 2, col = "red")

# 1e.
stdres <- rstandard(mod)
mean(stdres)
sd(stdres)
# scatterplot
plot(SanAntonio$subject.age, stdres, main = "Standard residuals vs. subject.age", 
     xlab = "subject.age", ylab = "Standardized Residuals", 
     pch = 1, col = "navy", cex = 0.5, las = 1)
abline(h = 0, lty = 2, col = "red", lwd = 2)
# qq plot
qqnorm(stdres, main = "qqplot of standard residuals", xlab = "G(0, 1) Quantiles", 
  ylab = "Standardized Res", pch = 1, col = "navy", cex = 0.5)
qqline(stdres, lty = 2, col = "red", lwd = 2)

# 1g.
predict(mod, newdata = data.frame(subject.age = 30), interval = "confidence", level = 0.95)

# 1h.
coef(summary(mod))

# 2b.
ExpRLF <- function(theta, n, thetahat) {
  exp(n * log(thetahat/theta) + n * (1 - thetahat/theta))
}
n <- length(log(SanAntonio$subject.age))
thetahat <- mean(SanAntonio$subject.age)
n
thetahat
teststat <- -2 * log(ExpRLF(34.1, n, thetahat))
teststat
p <- 1 - pchisq(teststat, df = 1)
p


# 2d.
mydata$subject.age.log <- log(mydata$subject.age)
table(SanAntonio$vehicle.colour)
table(SanAntonio$vehicle.make)
table(SanAntonio$subject.race)

SanAntonio$race.binary <- 1 - as.numeric(SanAntonio$subject.race == "hispanic")
table(SanAntonio$race.binary)
his <- subset(mydata, SanAntonio$race.binary == 0)
nhis <- subset(mydata, SanAntonio$race.binary == 1)
summary(his$subject.age.log)
sd(his$subject.age.log)
summary(nhis$subject.age.log)
sd(nhis$subject.age.log)

# 2e.
test.results <- t.test(his$subject.age.log, nhis$subject.age.log, var.equal = TRUE)
test.results
test.results$statistic
test.results$p.value

# 3b.
predict(mod, newdata = data.frame(subject.age = 400), interval = "prediction", level = 0.95)

# 3c.
summary(lm(subject.age.log ~ subject.age, data = mydata))
