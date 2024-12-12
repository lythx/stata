find_S <- function(sample_D, is_inside_S, max_iterations=1e6) {
  S = c()
  for (i in 1:max_iterations) {
    point = sample_D()
    if (is_inside_S(point)) {
      append(S, point)
    }
  }
  return(S)
}
