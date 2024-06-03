#A1
#a
calculeaza_probabilitatile = function(lambda, p, n, m, k) {
  if (k < 0 || k >= m || m > n) {
    stop("Condiția 0 ≤ k < m ≤ n nu este îndeplinită.")
  }
  
  valori = k:m
  poisson_probs = mean(dpois(valori, lambda))
  geometric_probs = mean(dgeom(valori, p))
  binomial_probs = mean(dbinom(valori, n, p))
  
  cat("Poisson: ", poisson_probs, "\n");
  cat("Geometric: ", geometric_probs, "\n");
  cat("Binomial: ", binomial_probs, "\n");
}
calculeaza_probabilitatile(5,0.3,10,7,2)



#b
grafic_function = function(lambda, p, n, m, k)
{
  if (k < 0 || k >= m || m > n) {
    stop("Condiția 0 ≤ k < m ≤ n nu este îndeplinită.")
  }

  valori = k:m
  poisson_probs = dpois(valori, lambda)
  geometric_probs = dgeom(valori, p)
  binomial_probs = dbinom(valori, n, p)

  hist(poisson_probs,breaks=5,right=T,freq=T)
  hist(geometric_probs,breaks=7,right=T,freq=T)
  hist(binomial_probs,breaks=6,right=T,freq=T)
}
grafic_function(5,0.3,10,7,2)


#c
cauta_k0 = function(lambda)
{
  k0=0
  cumulative_prob=0
  
  while (cumulative_prob <= 1-10^(-6)) {
    cumulative_prob = ppois(k0, lambda)
    if (cumulative_prob > 1-10^(-6))
    {
      break
    }
    k0 = k0 + 1
  }
  
  print(k0);
}
cauta_k0(5)



#A2
#a
analiza_fisier = function(nume_fisier) {
  date = read.csv(nume_fisier, header = T)
  
  echantion_P =date[['P']]
  echantion_S =date[['S']]
  
  calculeaza_frecvente = function(echantion) {
    frecvente_abs = table(echantion)
    frecvente_rel = as.vector(frecvente_abs / length(echantion))
    cat("Frecvente_abs: ", frecvente_abs, "\n")
    cat("Frecvente_rel: ", frecvente_rel, "\n")
  }
  
  calculeaza_media = function(echantion) {
    media = mean(echantion)
    cat("Media: ", media, "\n")
  }
  
  calculeaza_frecvente(echantion_P)
  calculeaza_frecvente(echantion_S)
  calculeaza_media(echantion_P)
  calculeaza_media(echantion_S)
}
analiza_fisier("note_PS.csv")


#b
eliminare_valori_aberante = function(nume_fisier, nume_echantion) {
  date = read.csv(nume_fisier, header = T)
  
  echantion = date[[nume_echantion]]
  
  determina_aberante = function(echantion) {
    m = mean(echantion)
    s = sd(echantion)
    outliers = vector()
    j = 0
    for (i in 1:length(echantion)) 
      {
        if (echantion[i] < m - 2 * s || echantion[i] > m + 2 * s) {
          j = j + 1
          outliers[j] = echantion[i]
        }
      }
    return(outliers)
}

aberante = determina_aberante(echantion)
echantion_curatat = echantion[!echantion %in% aberante]
intervale=c(1,2,3,4,5,6,7,8,9,10)
hist(echantion_curatat, breaks =intervale)

cat("Echantion final: ",echantion_curatat,"\n")
}

eliminare_valori_aberante("note_PS.csv", "P")
eliminare_valori_aberante("note_PS.csv", "S")