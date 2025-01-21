# setting up
mydata <- read.csv("~/Desktop/School/Stat 231/stat231f24dataset21058539.csv")
dim(mydata)
colnames(mydata)
sum(is.na(mydata))

# beginning of analysis
# 1c.

freqt<-table(mydata$day.of.week)
freqt
sum(freqt)
(freqt/1061) * 100

# 1f.
theta <- seq(from = 0.14, to = 0.24, by = 0.001)
n <- 1061
y <- 198
BinLF <- function(theta, n, y) {
  (theta^y) * (1 - theta)^(n-y)
}
RLFVals <- BinLF(theta, n, y) / max(BinLF(theta, n, y))

plot(theta, RLFVals, main = "Binomial relative likelihood function for weekend model",
     xlab = expression(omega), ylab = expression(paste("R(", omega, ")")), type = "l",
     lwd = 2, las = 1)
abline(h = 0.1, col = "red", lwd = 2)

# 1g.
thetahat <- 198/1061
BinRLF <- function(x) {
  BinLF(x, n, y) / BinLF(thetahat, n, y)
}

uniroot(function(x) BinRLF(x) - 0.1, lower = 0.1, upper = thetahat)$root
uniroot(function(x) BinRLF(x) - 0.1, lower = thetahat, upper = 0.22)$root

# 1h.
BinRLF(1/7)

# 1i.
p <- pchisq(-2 * log(0.1), 1)
a <- qnorm((1 + p) / 2)
thetahat - a * sqrt((thetahat * (1 - thetahat)) / n)
thetahat + a * sqrt((thetahat * (1 - thetahat)) / n)

# 1j.
a <- qnorm((1 + thetahat) / 2)
thetahat - a * sqrt((thetahat * (1 - thetahat)) / n)
thetahat + a * sqrt((thetahat * (1 - thetahat)) / n)
approx <- exp(-a^2/2)
approx
uniroot(function(x) BinRLF(x) - approx, lower = 0.1, upper = thetahat)$root
uniroot(function(x) BinRLF(x) - approx, lower = thetahat, upper = 0.22)$root

# 2b.
SanAntonio <- subset(mydata, city=="sa")
sum(table(SanAntonio$subject.age))
mean(SanAntonio$subject.age)
mean(log(SanAntonio$subject.age))
sd(SanAntonio$subject.age)
sd(log(SanAntonio$subject.age))
quantile(SanAntonio$subject.age, c(0.05, 0.95)) 
quantile(log(SanAntonio$subject.age), c(0.05, 0.95)) 

# 2c.
newth <- mean(SanAntonio$subject.age)
c <- qnorm((1 + 0.9)/2)
newth - c * newth/sqrt(n)
newth + c * newth/sqrt(n)

# 2e.
length(log(SanAntonio$subject.age))
x_bar <- mean(log(SanAntonio$subject.age))
s <- sd(log(SanAntonio$subject.age))
lower <- x_bar - c * (s / sqrt(671))
higher <- x_bar + c * (s / sqrt(671))
lower
higher
exp(lower)
exp(higher)

# 2f.
s2 <- s^2

(1 - 0.9)/2
(1 + 0.9)/2
chia <- qchisq(0.05, 670)
chib <- qchisq(0.95, 670)

sqrt(s2 * 670 / chib)
sqrt(s2 * 670 / chia)

# 2g.
m <- mean(SanAntonio$subject.age)
a <- qnorm((1 + 0.9)/2)
n <- (a * m)^2
n
m - a * m/sqrt(n)
m + a * m/sqrt(n)

# 3c.
thetahat <- 198/1061
thetahat
(thetahat * (1 - thetahat))
sqrt((thetahat * (1 - thetahat)) / 1061)
thetahat <- (198 + 159)/1061
thetahat
(thetahat * (1 - thetahat))
sqrt((thetahat * (1 - thetahat)) / 1061)

thetahat <- (198 + 500)/1061
thetahat
(thetahat * (1 - thetahat))
sqrt((thetahat * (1 - thetahat)) / 1061)
