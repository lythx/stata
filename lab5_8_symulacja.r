S_max = 0.1
N = 5
trial_num = 100

result = c()
for (i in 1:trial_num) {
  for (j in 1:100000) {
    rand = runif(n=N, min=0, max=1)
    if (all(rand <= S_max)) {
      result = c(result, j)
      break
    }
  }
}

print(result)
print(summary(result))
print(1 - length(result[result>29]) / trial_num)

