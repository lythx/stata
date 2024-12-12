library("vioplot")
library("car")

# 1
boxplot(boot::acme$market)
boxplot(boot::acme$acme)

# 2
vioplot(boot::acme$market)
vioplot(boot::acme$acme)

# 3
scatterplot(boot::acme$market, boot::acme$acme)

# 4
x <- seq(0, 10, 0.01)
n <- dnorm(x, 0, 3)
f <- df(x, df1=3, df2=6)
chi2 <- dchisq(x, df=3)

plot(x, n, type="n", ylim=c(0, max(n, f, chi2)))
lines(x, n, col="blue", lwd=3)
lines(x, f, col="red", lwd=3)
lines(x, chi2, col="green", lwd=3)
