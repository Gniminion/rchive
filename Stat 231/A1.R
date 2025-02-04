# setting up
mydata <- read.csv("~/Desktop/School/Stat 231/stat231f24dataset21058539.csv")
dim(mydata)
colnames(mydata)
sum(is.na(mydata))

# beginning of analysis
# 1b.

" testing out vehicle.make:
vehtable <- table(mydata$vehicle.make, mydata$city)
vehtable
prop.table(vehtable, 2)"

mydata$subject.race <- factor(mydata$subject.race, 
                              levels = c( "asian/pacific islander", "black", "hispanic", "white", "other"))
racetable <- table(mydata$subject.race, mydata$city)
racetable
prop.table(racetable, 2)

# 1c.
sanan <- subset(mydata, city=="sa")
newor <- subset(mydata, city== "no")
barplot(table(sanan$subject.race), xlab = "Cities", ylab = "Frequency", las = 1,
main = "Bar plot of subject.race in San Antonio", col = c("red", "orange", "yellow", "forestgreen",
"dodgerblue3"), ylim = c(0, 350), density = 50)
barplot(table(newor$subject.race), xlab = "Cities", ylab = "Frequency", las = 1,
main = "Bar plot of subject.race in New Orleans", col = c("red", "orange", "yellow", "forestgreen",
"dodgerblue3"), ylim = c(0, 300), density = 50)

# 2b.
mydata$subject.age.log <- log(mydata$subject.age)
SanAntonio <- subset(mydata, city=="sa")
skewness <- function(x) {
  (sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)
  }
kurtosis <- function(x) {
  (sum((x - mean(x))^4)/length(x))/(sum((x - mean(x))^2)/length(x))^2
}

mean(SanAntonio$subject.age)
median(SanAntonio$subject.age)
sd(SanAntonio$subject.age)
skewness(SanAntonio$subject.age)
kurtosis(SanAntonio$subject.age)

mean(SanAntonio$subject.age.log)
median(SanAntonio$subject.age.log)
sd(SanAntonio$subject.age.log)
skewness(SanAntonio$subject.age.log)
kurtosis(SanAntonio$subject.age.log)

# 2c.
library(MASS)
truehist(SanAntonio$subject.age, xlab = "Age",
         ylab = "Relative Frequency", main = "Relative frequency histogram of subject.age",
         xlim = c(0, 100), ylim = c(0, 0.04), las = 1, col = "dodgerblue3", density = 50)
curve(dnorm(x, mean(SanAntonio$subject.age), sd(SanAntonio$subject.age)), 
      col = "red", add = TRUE, lwd = 2)
truehist(SanAntonio$subject.age.log, xlab = "Log transformed vraiate age",
         ylab = "Relative Frequency", main = "Relative frequency histogram of subject.age.log",
         xlim = c(2.5, 5), ylim = c(0, 1.5), las = 1, col = "dodgerblue3", density = 50)
curve(dnorm(x, mean(SanAntonio$subject.age.log), sd(SanAntonio$subject.age.log)), 
      col = "red", add = TRUE, lwd = 2)

#2d.
plot(ecdf(SanAntonio$subject.age), xlab = "Age",
     main = "e.c.d.f. of subject.age", las = 1, lwd = 2, pch = NA)
curve(pnorm(x, mean(SanAntonio$subject.age), sd(SanAntonio$subject.age)), 
      col = "red", add = TRUE, lwd = 2, lty = 3)
plot(ecdf(SanAntonio$subject.age.log), xlab = "Log transformed vraiate age",
     main = "e.c.d.f. of subject.age.log", las = 1, lwd = 2, pch = NA)
curve(pnorm(x, mean(SanAntonio$subject.age.log), sd(SanAntonio$subject.age.log)), 
      col = "red", add = TRUE, lwd = 2, lty = 3)

#2e.
agem = mean(SanAntonio$subject.age)
agesd = sd(SanAntonio$subject.age)
age_lower <- (agem - 2*agesd - agem) / agesd 
age_upper <- (agem + 2*agesd - agem) / agesd 
pnorm(age_upper) - pnorm(age_lower)

agelm = mean(SanAntonio$subject.age.log)
agelsd = sd(SanAntonio$subject.age.log)
agel_lower <- (agelm - 2*agelsd - agelm) / agelsd 
agel_upper <- (agelm + 2*agelsd - agelm) / agelsd 
pnorm(agel_upper) - pnorm(agel_lower)

#2f.
plot(SanAntonio$subject.age, SanAntonio$vehicle.year, xlab = "subject.age", ylab = "vehicle.year",
     main = "Age and Vehicle Year in San Antonio", pch = 1, cex = 0.5, col = "navy",
     las = 1, cex.axis = 0.5)

#2g.
cor(SanAntonio$subject.age, SanAntonio$vehicle.year)

#3b.
vehtable <- table(mydata$vehicle.make, mydata$city)
prop.table(vehtable, 2)

#3c.
range(SanAntonio$vehicle.year)

#3d.
table(SanAntonio$vehicle.colour)
table(newor$vehicle.colour)

#3e. 
median(SanAntonio$vehicle.colour)
