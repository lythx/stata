S_max = 0.1
N = 1
ilosc_prob = 100
oczekiwane_fht = 900000

result = c()
for (i in 1:ilosc_prob) {
  for (j in 1:10000000) {
    rand = runif(n=N, min=0, max=1)
    if (all(rand <= S_max)) {
      result = c(result, j)
      break
    }
  }
}

print(result)
print(summary(result))
print(1 - length(result[result>29]) / ilosc_prob)

