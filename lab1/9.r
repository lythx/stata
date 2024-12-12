# Parametry
n <- 5 # Liczba prób
categories <- c("jabłko", "gruszka", "śliwka")
probabilities <- c(0.3, 0.2, 0.5)

# Generowanie rozkładu prawdopodobieństwa (tablica)
prob_distribution <- expand.grid(jablko = 0:n,
                                 gruszka = 0:n,
                                 sliwka = 0:n)
prob_distribution <- prob_distribution[rowSums(prob_distribution) == n, ]

# Obliczanie prawdopodobieństwa dla każdej kombinacji
prob_distribution$probability <- apply(prob_distribution, 1, function(row) {
  dbinom(row["jablko"], n, probabilities[1]) *
    dbinom(row["gruszka"], n - row["jablko"], probabilities[2] / (1 - probabilities[1])) *
    dbinom(row["sliwka"], n - row["jablko"] - row["gruszka"], probabilities[3] / (1 - probabilities[1] - probabilities[2]))
})

# Rozkłady brzegowe
marginal_jablko <- aggregate(probability ~ jablko, data = prob_distribution, sum)
marginal_gruszka <- aggregate(probability ~ gruszka, data = prob_distribution, sum)
marginal_sliwka <- aggregate(probability ~ sliwka, data = prob_distribution, sum)

# Obliczanie kowariancji
covariance_matrix <- matrix(0, nrow = 3, ncol = 3,
                            dimnames = list(categories, categories))

for (i in 1:3) {
  for (j in 1:3) {
    if (i == j) {
      covariance_matrix[i, j] <- var(prob_distribution[[categories[i]]], prob_distribution$probability)
    } else {
      covariance_matrix[i, j] <- cov(prob_distribution[[categories[i]]], prob_distribution[[categories[j]]], prob_distribution$probability)
    }
  }
}

# Wyniki
cat("Rozkład brzegowy - Jabłko:\n")
print(marginal_jablko)

cat("\nRozkład brzegowy - Gruszka:\n")
print(marginal_gruszka)

cat("\nRozkład brzegowy - Śliwka:\n")
print(marginal_sliwka)

cat("\nKowariancje:\n")
print(covariance_matrix)
