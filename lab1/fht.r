N = c(1, 2, 5, 10, 50, 100)
delta = c(0.6, 0.95)

fht = matrix(NA, length(N), length(delta))

calculate_fht <- function(N, delta) {
  log01 = log(0.1)
  # rozwiniecie w szereg Maclaurina aby uniknac p!
  maclaurin_log = 0
  for (i in 1:50) {
    maclaurin_log = maclaurin_log - (-1)^i * (-(0.1^N))^i / i
  }
  return(ceiling(log(1 - delta) / maclaurin_log))
}

for (i in 1:length(N)) {
  for (j in 1:length(delta)) {
    fht[i, j] = calculate_fht(N[i], delta[j])
  }
}
dimnames(fht) = list(N, delta)
print(fht)
