# setting up
mydata <- read.csv("~/Desktop/School/Stat 231/stat231f24dataset21058539.csv")
dim(mydata)
colnames(mydata)
sum(is.na(mydata))

skewness <- function(x) {
  (sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)
}
kurtosis <- function(x) {
  (sum((x - mean(x))^4)/length(x))/(sum((x - mean(x))^2)/length(x))^2
}

# beginning of analysis
# 1c.

library(MASS)
SanAntonio <- subset(mydata, city=="sa")
NewOrleans <- subset(mydata, city=="no")
summary(mydata$subject.age)
summary(SanAntonio$subject.age)
summary(NewOrleans$subject.age)

dim(mydata)
dim(SanAntonio)
dim(NewOrleans)

sd(SanAntonio$subject.age)
skewness(SanAntonio$subject.age)

# 1d.

truehist(SanAntonio$subject.age, xlab = "Age",
         ylab = "Relative Frequency", main = "Relative frequency histogram of subject.age",
         xlim = c(0, 100), ylim = c(0, 0.04), las = 1, col = "dodgerblue3", density = 50)

agem <- mean(SanAntonio$subject.age)
curve(dexp(x, rate = 1 / agem), col = "red", add = TRUE, lwd = 2)

# 1e.

plot(ecdf(SanAntonio$subject.age), xlab = "Age",
     main = "e.c.d.f. of subject.age", las = 1, lwd = 2, pch = NA)

curve(pexp(x, rate = 1 / agem), col = "red", add = TRUE, lwd = 2)

# 1h.

pexp(25, rate = 1 / agem, lower.tail = FALSE)

# 1i.

ExpRLF <- function(theta, n, thetahat) {
  (thetahat/theta)^n * exp(n * (1 - thetahat/theta))
}

theta <- seq(25, 45, by = 0.01)

plot(theta, ExpRLF(theta, 671, agem), xlab = expression(theta),
     ylab = expression(paste("R(", theta, ")")), type = "l", lwd = 2,
     , main = "Exponential relative likelihood function for subject.age model", las = 1)

# 2b.
table(NewOrleans$subject.race)
nrow(NewOrleans)

# 2c.
exptd_freq <- c(10.92, 209.04, 31.59,123.24,15.21)
obsvd_freq <- table(NewOrleans$subject.race)

barplot(rbind(obsvd_freq, exptd_freq), beside = T, xlab = "Races",
        ylab = "Frequency", las = 1, main = "Bar plot of observed and expected frequencies of races in New Orleans",
        col = c("orange", "dodgerblue3"), ylim = c(0, 300), density = 50, angle = c(45, 135))

legend("topright", legend = c("Observed frequency", "Expected frequency"), fill = c("orange", "dodgerblue3"),
       density = 50, angle = c(45, 135))

# 2e.

boxplot(NewOrleans$vehicle.year ~ factor(NewOrleans$subject.race, levels = 
        c("asian/pacific islander", "black", "hispanic", "white", "other")), col = c("dodgerblue1", "dodgerblue3",
       "darkorchid2", "darkorchid4"), xlab = "Race", ylab = "Year", main = "Box plots of subject.race based on vehicle.year in New Orleans")

# 2g.

qqnorm(NewOrleans$vehicle.year, pch = 1, cex = 0.5, ylab = 
         "Sample Quantiles (Vehicle Year)", main = "Gaussian Q-Q Plot based on vehicle.year in New Orleans")
qqline(NewOrleans$vehicle.year, col = "red", lwd = 2, lty = 3)
table(NewOrleans$vehicle.year)
barplot(table(NewOrleans$vehicle.year), las = 1, xlab = "Goals", ylab = "Games",
        ylim = c(0, 50), col = "dodgerblue3")
kurtosis(NewOrleans$vehicle.year)

# 3c. 

table(SanAntonio$subject.age)
nrow(SanAntonio)
adults <- subset(SanAntonio$subject.age, SanAntonio$subject.age >= 18)
table(adults)
mean(adults)

# 3d.

table(SanAntonio$vehicle.make)

boxplot(SanAntonio$vehicle.year ~ factor(SanAntonio$vehicle.colour, levels = 
  c("black", "gray", "silver", "white", "other")), col = c("dodgerblue3", "darkgray",
  "lightgray", "white","lightblue"), xlab = "Race", ylab = "Year", main = "Box plots of vehicle.colour based on vehicle.year in San Antonio")

boxplot(SanAntonio$vehicle.year ~ factor(SanAntonio$vehicle.make, levels = 
  c("chevrolet", "ford", "honda", "toyota", "other")), col = c("dodgerblue1", "dodgerblue3", "lightblue",
    "darkorchid2", "darkorchid4"), xlab = "Race", ylab = "Year", main = "Box plots of vehicle.make based on vehicle.year in San Antonio")
