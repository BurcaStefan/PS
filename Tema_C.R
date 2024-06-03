#C1
# a
permutare_aleatoare = function(n) {
  U = runif(n)
  U_sorted = sort(U)
  pi = order(U_sorted)
  return(pi)
}
permutare_aleatoare(10)

# b
comparare_lexicografica = function(Wi, Wj) {
  Lij = min(length(Wi), length(Wj))
  h = 1
  while (h <= Lij) {
    if (Wi[h] < Wj[h]) return(TRUE)
    if (Wi[h] > Wj[h]) return(FALSE)
    h = h + 1
  }
  # Daca lungimile sunt diferite, adaugam biti suplimentari pana cand gasim o diferenta
  while (length(Wi) != length(Wj)) {
    if (length(Wi) < length(Wj)) {
      Wi = c(Wi, sample(c(0, 1), 1))
    } else {
      Wj = c(Wj, sample(c(0, 1), 1))
    }
    if (Wi[length(Wi)] < Wj[length(Wj)]) return(TRUE)
    if (Wi[length(Wi)] > Wj[length(Wj)]) return(FALSE)
  }
  # Daca lungimile sunt egale si cuvintele sunt egale, generam biti suplimentari pana la diferenta
  while (TRUE) {
    bit_Wi = sample(c(0, 1), 1)
    bit_Wj = sample(c(0, 1), 1)
    if (bit_Wi < bit_Wj) return(TRUE)
    if (bit_Wi > bit_Wj) return(FALSE)
  }
}

generate_random_bit_vector = function(max_length) {
  length = sample(1:max_length, 1)
  return(sample(c(0, 1), length, replace = TRUE))
}

Wi = generate_random_bit_vector(10)
Wj = generate_random_bit_vector(10)
cat("Wi: ", Wi, "\n")
cat("Wj: ", Wj, "\n")
result = comparare_lexicografica(Wi, Wj)
cat("Wi este lexicografic mai mic decat Wj: ", result, "\n")


#c
quick_sort_randomizat = function(W) {
  if (length(W) <= 1) {
    return(W)
  }
  
  pivot_index = sample(1:length(W), 1)
  pivot = W[[pivot_index]]
  
  S1 = list()
  S2 = list()
  
  for (i in 1:length(W)) {
    if (i == pivot_index) next
    if (comparare_lexicografica(W[[i]], pivot)) {
      S1 = c(S1, list(W[[i]]))
    } else {
      S2 = c(S2, list(W[[i]]))
    }
  }
  
  sorted_S1 = quick_sort_randomizat(S1)
  sorted_S2 = quick_sort_randomizat(S2)
  
  return(c(sorted_S1, list(pivot), sorted_S2))
}

generare_cuvinte_binare = function(n, k) {
  cuvinte = vector("list", n)
  for (i in 1:n) {
    cuvinte[[i]] = sample(c(0, 1), k, replace = TRUE)
  }
  return(cuvinte)
}

n = 10
k = 5
cuvinte = generare_cuvinte_binare(n, k)

cat("Cuvintele inainte de sortare:\n")
print(cuvinte)

cuvinte_sortate = quick_sort_randomizat(cuvinte)

cat("Cuvintele dupa sortare:\n")
print(cuvinte_sortate)



#d
permutare_aleatoare_cuvinte = function(n, k) {
  cuvinte = generare_cuvinte_binare(n, k)
  cuvinte_sortate = quick_sort_randomizat(cuvinte)
  
  permutare = sapply(1:length(cuvinte_sortate), function(i) {
    for (j in 1:length(cuvinte)) {
      if (identical(cuvinte_sortate[[i]], cuvinte[[j]])) {
        return(j)
      }
    }
  })
  
  return(permutare)
}

n = 10
k = 5
permutare = permutare_aleatoare_cuvinte(n, k)
cat("Permutarea aleatoare: ", permutare,"\n")




#C2
taietura_maxima = function(V, E, n) {
  A = sample(V, n)
  B = setdiff(V, A)
  
  cardinal_taietura = 0
  for (muchie in E) {
    u = muchie[1]
    v = muchie[2]
    if ((u %in% A && v %in% B) || (u %in% B && v %in% A)) {
      cardinal_taietura = cardinal_taietura + 1
    }
  }
  return(cardinal_taietura)
}

max_cardinal_taietura = function(V, E, n, num_repetitii) {
  max_cardinal = 0
  for (i in 1:num_repetitii) {
    cardinal_taietura = taietura_maxima(V, E, n)
    if (cardinal_taietura > max_cardinal) {
      max_cardinal = cardinal_taietura
    }
  }
  return(max_cardinal)
}

V = 1:6
E = list(c(1, 2), c(1, 4), c(1, 6), c(3, 2), c(3, 4), c(3, 6), c(5, 2), c(5, 4), c(5, 6))

n = 3  # Numarul de noduri in fiecare parte a bipartitiei
num_repetitii = 1000

rezultat = max_cardinal_taietura(V, E, n, num_repetitii)
cat("Cardinalul maxim al taieturii este:", rezultat, "\n")
