# 1a
x <- 1:10
y <- c(56, 34, 123, 5, -5, 0, -4, 3, 2, 0)
A <- matrix(c(x, y), 2, length(x), TRUE)
AT <- t(A) 
ATy <- AT * y
# print(A)
# print(AT)
# print(ATy)

# 1b
A2 <- matrix(c(45, 1, 3, 1, 33, 8, 3, 8, 36), 3, 3)
b <- c(-5, -12, 7)
detA2 <- det(A2)
x2 <- solve(A2, b)
# print(A2)
# print(detA2)
# print(x2)

# 1c
c <- c(3, 4, 5)
B <- cbind(A2, c)
d <- c(10, 20, 30, 40)
G <- rbind(B, d)
# print(B)
# print(G)

# 1d
dimnames(G) = list(c("Anna", "Natalia", "Monika", "Beata"), 
                   c("Tulipan", "Róża", "Lilia", "Hiacynt"))
dimG <- dim(G)
# print(G)
# print(dimG)

# 1e
C <- matrix(1:9, 3, 3, TRUE,
            list(c("wiersz1", "wiersz2", "wiersz3"),
            c("kolumna1", "kolumna2", "kolumna3")))
# print(C)

# 1f
Z <- array(x, c(3, 3, 2))
Z2 <- 1:18
dim(Z2) <- c(3, 3, 2)
# print(Z)
# print(Z2)


# 2a
list1 <- list(vec=c("aa", "bb", "cc"), 
              mat_num=matrix(1:9, 3, 3), 
              mat_bool=matrix(c(T, T, T, F), 2, 2))
type_list1 <- typeof(list1)

dimens = dim(list1$mat_num)
mat_sqrt <- matrix(, dimens[1], dimens[2])
for (i in 1:dimens[1]) {
  for (j in 1:dimens[2]) {
    mat_sqrt[i, j] = sqrt(list1$mat_num[i, j])
  }
}
# print(list1)
# print(type_list1)
# print(mat_sqrt)

# 2b
palenie <- c(T, T, F, T, T, T, F, T, T, F)
plec <- c("K", "K", "K", "K", "M", "M", "K", "K", "K", "K")
wiek <- c(23, 21, 57, 43, 26, 35, 64, 48, 44, 50)
badanie <- data.frame(czy_pali=palenie, plec, wiek)
ilosc_kobiet <- 0
ilosc_mezczyzn <- 0
for (p in badanie$plec) {
  if (p == "K") {
    ilosc_kobiet <- ilosc_kobiet + 1
  } else {
    ilosc_mezczyzn <- ilosc_mezczyzn + 1
  }
}
# print(badanie)
# print(ilosc_kobiet)
# print(ilosc_mezczyzn)

# 3a
write.table(badanie, "data/badanie.txt")
Nowe_badanie <- read.table("data/badanie.txt")
# print(Nowe_badanie)

# 3b
write.table(boot::beaver, "data/beaver.txt")


# 4a
il_skalarny <- 0
for (i in 1:nrow(boot::beaver)) {
  il_skalarny <- il_skalarny + boot::beaver[i, 1] * boot::beaver[i, 2]
}
# print(il_skalarny)

# 4b
ile_zer <- function(v) {
  ilosc = 0
  for (x in v) {
    if (x == 0) {
      ilosc <- ilosc + 1
    }
  }
  return(ilosc);
}
# print(ile_zer(c(0, 0, 1, 2, 3, 0)))

# 4c
pierwszy_ostatni <- function(v) {
  wynik = c(NA, NA)
  for (i in 1:length(v)) {
    if (v[i]) {
      wynik[1] = i
      break
    }
  }
  for (i in seq(length(v), 1, -1)) {
    if (v[i]) {
      wynik[2] = i
      break
    }
  }
  return(wynik);
}
# print(pierwszy_ostatni(c(F, T, F, F, T, F, F)))

# 4d
moda <- function(x) {
  hashtable = list()
  for (i in 1:3) {
    for (j in 1:3) {
      for (k in 1:3) {
        key = as.character(x[i, j, k])
        if(is.null(hashtable[[key]])) {
          hashtable[[key]] = 1
        } else {
          hashtable[[key]] = hashtable[[key]] + 1
        }
      }
    }
  }
  max_val = 0
  max_key = NA
  for (key in names(hashtable)) {
    if (hashtable[[key]] > max_val) {
      max_val = hashtable[[key]]
      max_key = key
    }
  }
  return(as.double(max_key))
}
# print(moda(array(c(1:26, 8), c(3, 3, 3))))
