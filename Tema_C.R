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
  for (h in 1:Lij) {
    if (Wi[h] < Wj[h]) {
      return(TRUE)
    } else if (Wi[h] > Wj[h]) {
      return(FALSE)
    }
  }
  
  if (length(Wi) == length(Wj)) {
    return(runif(1) < 0.5)
  } else if (length(Wi) < length(Wj)) {
    return(TRUE)
  } else {
    return(FALSE)
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
quick_sort_randomizat <- function(cuvant) {
  if (length(cuvant) <= 1) {
    return(cuvant)
  }
  
  pivot_index <- sample(1:length(cuvant), 1)
  pivot <- cuvant[[pivot_index]]
  cuvant <- cuvant[-pivot_index]
  
  less <- list()
  greater <- list()
  
  for (word in cuvant) {
    if (comparare_lexicografica(word, pivot)) {
      less <- c(less, list(word))
    } else {
      greater <- c(greater, list(word))
    }
  }
  
  return(c(quick_sort_randomizat(less), list(pivot), quick_sort_randomizat(greater)))
}

generare_cuvinte_binare <- function(n, k) {
  cuvinte <- vector("list", n)
  for (i in 1:n) {
    cuvinte[[i]] <- sample(c(0, 1), k, replace = TRUE)
  }
  return(cuvinte)
}

cuvinte <- generare_cuvinte_binare(10, 5)

cat("Cuvintele înainte de sortare:\n")
print(cuvinte)

cuvinte_sortate <- quick_sort_randomizat(cuvinte)

cat("Cuvintele după sortare:\n")
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
permutare = permutare_aleatoare_cuvinte(10, 5)
cat("Permutarea aleatoare: ", permutare,"\n")




#C2
V = 1:6
E = list(c(1, 2), c(1, 6), c(2, 4), c(2, 6), c(3, 5), c(3, 6), c(4, 5), c(4, 6), c(5, 6))
n = 3 

A = sample(V, n)
B = setdiff(V, A)
cat("Nodurile din A sunt:", A, "\n")
cat("Nodurile din B sunt:", B, "\n")


taietura_maxima = function(A, B, E) {

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
    cardinal_taietura = taietura_maxima(A, B, E)
    if (cardinal_taietura > max_cardinal) {
      max_cardinal = cardinal_taietura
    }
  }
  return(max_cardinal)
}


num_repetitii = 1000

rezultat = max_cardinal_taietura(V, E, n, num_repetitii)
cat("Cardinalul maxim al taieturii este:", rezultat, "\n")
