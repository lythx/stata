# 1
Z = 1
# print(pnorm(Z))

# 2
sigma <- 0.2
n <- 25
x_mean <- 0.315
alpha <- 0.05

q <- 1 - alpha / 2
zalpha <- stats::qnorm(q, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
error <- zalpha * sigma / sqrt(n)
low <- x_mean - error
high <- x_mean + error
# cat("(", low, ", ", high, ")", sep="")

# 3
n <- 25
x_mean <- 45
alpha <- 0.05
s <- 11

q <- 1 - alpha / 2
talpha <- stats::qt(q, df=n-1, lower.tail = TRUE, log.p = FALSE)
error <- talpha * s / sqrt(n - 1)
low <- x_mean - error
high <- x_mean + error
# cat("(", low, ", ", high, ")", sep="")

# 4
n <- 100
x_mean <- 5.4
alpha <- 0.04
s <- 1.7

q <- 1 - alpha / 2
zalpha <- stats::qnorm(q, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
error <- zalpha * s / sqrt(n)
low <- x_mean - error
high <- x_mean + error
# cat("(", low, ", ", high, ")", sep="")

# 5
n <- 25
x_mean <- 37.3
alpha <- 0.1
s2 <- 13.5

q1 = alpha / 2
q2 = 1 - alpha / 2
chji1 = stats::qchisq(q1, df=n-1, lower.tail = TRUE, log.p =
                        FALSE)
chji2 = stats::qchisq(q2, df=n-1, lower.tail = TRUE, log.p =
                        FALSE)
low = n * s2 / chji2
high = n * s2 / chji1
cat("(", low, ", ", high, ")", sep="")
