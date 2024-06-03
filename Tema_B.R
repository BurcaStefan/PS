#B1
tor_vol = function(N, R, r) {
  N_C = 0
  for (i in 1:N) {
    x1 = runif(1, -R - r, R + r)
    x2 = runif(1, -R - r, R + r)
    x3 = runif(1, -r, r)
    if ((x1^2 + x2^2 - R^2)^2 + x3^2 <= r^2 * (R^2 + r^2)) {
      N_C = N_C + 1
    }
  }
  return((2 * R * r^2 * N_C) / N)
}

R=10
r=3

estimare_vol_10000 = tor_vol(10000, 10, 3)
eroare_abs_10000 = abs(estimare_vol_10000 - (2 * pi^2 * R * r^2))
eroare_rel_10000 = eroare_abs_10000 / (2 * pi^2 * R * r^2)

estimare_vol_20000 = tor_vol(20000, 10, 3)
eroare_abs_20000 = abs(estimare_vol_20000 - (2 * pi^2 * R * r^2))
eroare_rel_20000 = eroare_abs_20000 / (2 * pi^2 * R * r^2)

estimare_vol_50000 = tor_vol(50000, 10, 3)
eroare_abs_50000 = abs(estimare_vol_50000 - (2 * pi^2 * R * r^2))
eroare_rel_50000 = eroare_abs_50000 / (2 * pi^2 * R * r^2)

cat("Estimare volum (N = 10000): ", estimare_vol_10000, "\n")
cat("Eroare absolută (N = 10000): ", eroare_abs_10000, "\n")
cat("Eroare relativă (N = 10000): ", eroare_rel_10000, "\n")

cat("Estimare volum (N = 20000): ", estimare_vol_20000, "\n")
cat("Eroare absolută (N = 20000): ", eroare_abs_20000, "\n")
cat("Eroare relativă (N = 20000): ", eroare_rel_20000, "\n")

cat("Estimare volum (N = 50000): ", estimare_vol_50000, "\n")
cat("Eroare absolută (N = 50000): ", eroare_abs_50000, "\n")
cat("Eroare relativă (N = 50000): ", eroare_rel_50000)




#B2
estimeaza_aria_T = function(N) {
  puncte_in_triunghi = 0
  for (i in 1:N) {
    x = runif(1, 0, 6/5)
    y = runif(1, 0, 12/5)
    if (y <= 2 * x && y <= 6 - 3 * x) {
      puncte_in_triunghi = puncte_in_triunghi + 1
    }
  }
  aria_rectangulara = (6/5) * (12/5)
  raport_arii = puncte_in_triunghi / N
  aria_T = raport_arii * aria_rectangulara
  return(aria_T)
}

aria_estimata = estimeaza_aria_T(20000)
cat("Aria estimata a triunghiului T:", aria_estimata)

#B3
#a
MC_integrala_a <- function(N) {
  sum = 0
  for (i in 1:N) {
    x = runif(1, -1, 1)
    integrand_value = (2 * x - 1) / (x^2 - x - 6)
    sum = sum + integrand_value
  }
  return(sum * 2 / N)
}

MC_integrala_medie_a <- function(k, N) {
  estimari = numeric(k)
  for (i in 1:k) {
    estimari[i] = MC_integrala_a(N)
  }
  medie_estimari = mean(estimari)
  cat("Valoarea estimata (a):", medie_estimari, "\n")
}
MC_integrala_medie_a(50, 10000)





#b
MC_integrala_b = function(N) {
  sum = 0
  for (i in 1:N) {
    x = runif(1, 3, 11)
    sum = sum + (x + 4) / (x - 3)^(1/3)
  }
  return(sum * 8 / N)
}
MC_integrala_medie_b = function(k, N) {
  estimari = vector()
  for (i in 1:k) {
    estimari[i] = MC_integrala_b(N)
  }
  medie_estimari = mean(estimari)
  cat("Valoarea estimata (b):", medie_estimari, "\n")
}
MC_integrala_medie_b(50, 10000)



#c
MC_integrala_c = function(N) {
  sum = 0
  for (i in 1:N) {
    x = rexp(1, 1)
    sum = sum + (x * exp(-x^2))/ exp(-x)
  }
  return(sum / N)
}
MC_integrala_medie_c = function(k, N) {
  estimari = vector()
  for (i in 1:k) {
    estimari[i] = MC_integrala_c(N)
  }
  medie_estimari = mean(estimari)
  cat("Valoarea estimata (c):", medie_estimari, "\n")
}
MC_integrala_medie_c(50, 10000)




#B4
#a
estimeaza_ani_pana_la_15000_utilizatori <- function(N, n, p, q, utilizatori_initiali, utilizatori_tinta) {
  numar_ani = numeric(N)
  
  for (sim in 1:N) {
    utilizatori = utilizatori_initiali
    ani = 0
    
    while (utilizatori < utilizatori_tinta) {
      utilizatori_noi = rbinom(1, n, p)
      utilizatori_ramasi = rbinom(1, utilizatori, 1 - q)
      utilizatori = utilizatori_ramasi + utilizatori_noi
      ani = ani + 1
    }
    
    numar_ani[sim] = ani
  }
  
  return(mean(numar_ani))
}

ani_estimat = estimeaza_ani_pana_la_15000_utilizatori(10000, 1000, 0.25, 0.01, 10000, 15000)
cat("Numarul de ani estimat: ", ani_estimat, "\n")



#b
estimate_prob_40_years_10_months <- function(N, n, p, q, initial_users, target_users, years, months) {
  target_time = years + months / 12
  successes = 0
  
  for (sim in 1:N) {
    users = initial_users
    time = 0
    
    while (time < target_time) {
      new_users = rbinom(1, n, p)
      remaining_users = rbinom(1, users, 1 - q)
      users = remaining_users + new_users
      time = time + 1 / 12
    }
    
    if (users >= target_users) {
      successes = successes + 1
    }
  }
  
  return(successes / N)
}

prob_40_years_10_months = estimate_prob_40_years_10_months(10000, 1000, 0.25, 0.01, 10000, 15000, 40, 10)
cat("Probabilitatea dupa 40 de ani si 10 luni: ", prob_40_years_10_months, "\n")

#c 