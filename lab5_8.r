N_vec = c(1, 2, 5, 10, 50, 100)
delta_vec = c(0.6, 0.95)

fht = matrix(NA, length(N_vec), length(delta_vec))

# rozwiniecie w szereg Maclaurina aby uniknac p!
calculate_fht <- function(N, delta) {
  maclaurin_log = 0
  p = 0.1^N
  for (i in 1:50) {
    maclaurin_log = maclaurin_log - p^i / i
  }
  return(ceiling(log(1 - delta) / maclaurin_log))
}

for (i in 1:length(N_vec)) {
  for (j in 1:length(delta_vec)) {
    fht[i, j] = calculate_fht(N_vec[i], delta_vec[j])
  }
}
dimnames(fht) = list(N_vec, delta_vec)
print(fht)
