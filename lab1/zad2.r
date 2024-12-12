library("stats")
library("e1071")
library("psych")
library("dprep")

# 1
data <- boot::acme$market
N <- length(data)
n = 1 + 3.222 * log(N)
tab = table(cut(data, n)) / length(data)
print(tab)

# 2
data <- boot::acme
year_count = list()
year_average = list()
for (i in 1:nrow(data)) {
  month = data$month[i]
  year = substring(month, nchar(month) - 1, nchar(month))
  if (is.null(year_count[[year]])) {
    year_count[[year]] = 1
    year_average[[year]] = data$market[i]
  } else {
    year_count[[year]] = year_count[[year]] + 1
    cnt = year_count[[year]]
    avg = year_average[[year]]
    year_average[[year]] = (avg * (cnt - 1) + data$market[i]) / cnt
  }
}
# print(year_count)
# print(year_average)


# 3
divide_intervals <- function(data, proportions) {
  data = sort(data)
  intervals = matrix(NA, length(proportions), 2)
  colnames(intervals) = c("start", "end")
  a = data[1]
  i = 0
  for (j in 1:length(proportions)) {
    p = proportions[j]
    elem_count = floor(p * length(data))
    i = i + elem_count
    b = data[i]
    intervals[j, 1] = a
    intervals[j, 2] = b
    a = b
  }
  intervals[length(proportions), 2] = data[length(data)]
  return(intervals)
}
da = c(seq(0, 10, 0.01), seq(0, 5, 0.01))
pr = c(0.3, 0.2, 0.1, 0.2, 0.2)
intervals <- divide_intervals(da, pr)
# print(intervals)

# 4
data <- boot::catsM$Bwt
max_bwt <- max(data)
min_bwt <- min(data)
mean_bwt <- mean(data)
length_bwt <- length(data)
range_bwt <- range(data)
diff_bwt <- diff(data)
summary_bwt <- summary(data)
median_bwt <- median(data)
quantile_bwt <- quantile(data, c(0.2, 0.3, 0.77, 0.97))
IQR_bwt <- IQR(data)
var_bwt <- var(data)
sd_bwt <- sd(data)
mad_bwt <- mad(data)
kurtosis_bwt <- kurtosis(data)
skewness_bwt <- skewness(data)
geometric_mean_bwt <- geometric.mean(data)
harmonic_mean_bwt <- harmonic.mean(data)
moda_bwt <- moda(data)

data <- boot::catsM$Hwt
max_hwt <- max(data)
min_hwt <- min(data)
mean_hwt <- mean(data)
length_hwt <- length(data)
range_hwt <- range(data)
diff_hwt <- diff(data)
summary_hwt <- summary(data)
median_hwt <- median(data)
quantile_hwt <- quantile(data, c(0.2, 0.3, 0.77, 0.97))
IQR_hwt <- IQR(data)
var_hwt <- var(data)
sd_hwt <- sd(data)
mad_hwt <- mad(data)
kurtosis_hwt <- kurtosis(data)
skewness_hwt <- skewness(data)
geometric_mean_hwt <- geometric.mean(data)
harmonic_mean_hwt <- harmonic.mean(data)
moda_hwt <- moda(data)

data <- boot::catsM$Bwt - boot::catsM$Hwt
max_diff <- max(data)
min_diff <- min(data)
mean_diff <- mean(data)
length_diff <- length(data)
range_diff <- range(data)
diff_diff <- diff(data)
summary_diff <- summary(data)
median_diff <- median(data)
quantile_diff <- quantile(data, c(0.2, 0.3, 0.77, 0.97))
IQR_diff <- IQR(data)
var_diff <- var(data)
sd_diff <- sd(data)
mad_diff <- mad(data)
kurtosis_diff <- kurtosis(data)
skewness_diff <- skewness(data)
geometric_mean_diff <- geometric.mean(data)
harmonic_mean_diff <- harmonic.mean(data)
moda_diff <- moda(data)
