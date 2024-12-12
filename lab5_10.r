pmultinom <- function(x, p) {
  r = length(x)
  
  recursive_sum <- function(current_x, current_index) {
    ret = dmultinom(current_x, prob=p)
    print(current_x)
    print(ret)
    
    for (i in current_index:r) {
      if (current_x[i] + 1 <= x[i]) {
        cur_x_copy = current_x
        cur_x_copy[i] = cur_x_copy[i] + 1
        ret = ret + recursive_sum(cur_x_copy, i)
      }
    }
    
    return(ret)
  }
  
  return(recursive_sum(rep(0, r), 1))
}

x <- c(2, 3, 1)
p <- c(0.3, 0.4, 0.3)
print(pmultinom(x, p))
