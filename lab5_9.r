r = 3
pj = 0.3
pg = 0.2
ps = 0.5
n = 5

distribution <- matrix(nrow=0, ncol=4)
for (j in 0:n) {
  for (g in 0:n) {
    for (s in 0:n) {
      if (j + g + s == n) {
        value = (factorial(n)/(factorial(j)*factorial(g)*factorial(s)))*pj^j*pg^g*ps^s
        distribution = rbind(distribution, c(j, g, s, value))
      }
    }
  }
}

marginal_dist = matrix(0, nrow=6, ncol=3)
for (i in 1:nrow(distribution)) {
  row <- distribution[i, ]
  marginal_dist[row[1] + 1, 1] = marginal_dist[row[1] + 1, 1] + row[4]
  marginal_dist[row[2] + 1, 2] = marginal_dist[row[2] + 1, 2] + row[4]
  marginal_dist[row[3] + 1, 3] = marginal_dist[row[3] + 1, 3] + row[4]
}

cov_j_g = cov(marginal_dist[,1], marginal_dist[,2])
cov_j_s = cov(marginal_dist[,1], marginal_dist[,3])
cov_g_s = cov(marginal_dist[,2], marginal_dist[,3])

colnames(distribution) = c("jabłka", "gruszki", "śliwki", "Pr")
print(distribution)
colnames(marginal_dist) = c("jabłka", "gruszki", "śliwki")
rownames(marginal_dist) = 0:5
print(marginal_dist)
print(c("Cov(jabłko, gruszka)", cov_j_g))
print(c("Cov(jabłko, śliwka)", cov_j_s))
print(c("Cov(gruszka, śliwka)", cov_g_s))
