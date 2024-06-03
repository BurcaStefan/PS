#D1
interval_incredere_probabilitati = function(alfa, n, medie_esantion, sigma) {
  z_critic = qnorm(1 - alfa / 2, 0, 1)
  a = medie_esantion - z_critic * sigma / sqrt(n)
  b = medie_esantion + z_critic * sigma / sqrt(n)
  interval = c(a, b)
  return(interval)
}

rezultat_probabilitati = function(nume_fisier){
  esantion = read.csv(nume_fisier, header = T)
  medie_esantion = mean(esantion$probabilitati)
  sigma = sqrt(92.16)
  n = length(esantion$probabilitati)
  
  interval_95 = interval_incredere_probabilitati(0.05, n, medie_esantion, sigma)
  cat("Intervalul de incredere de 95% este:", interval_95, "\n")
  
  interval_99 = interval_incredere_probabilitati(0.01, n, medie_esantion, sigma)
  cat("Intervalul de incredere de 99% este:", interval_99, "\n")
}
rezultat_probabilitati("probabilitati.csv")



#D2
interval_incredere_statistica = function(alfa, n, medie_esantion, sigma) {
  z_critic = qnorm(1 - alfa / 2, 0, 1)
  a = medie_esantion - z_critic * sigma / sqrt(n)
  b = medie_esantion + z_critic * sigma / sqrt(n)
  interval = c(a, b)
  return(interval)
}

rezultat_statistica = function(nume_fisier) {
  esantion = read.csv(nume_fisier, header = TRUE)
  medie_esantion = mean(esantion$statistica)
  sigma = sqrt(92.16)
  n = length(esantion$statistica)
  
  interval_95 = interval_incredere_statistica(0.05, n, medie_esantion, sigma)
  cat("Intervalul de incredere de 95% este:", interval_95, "\n")
  
  interval_99 = interval_incredere_statistica(0.01, n, medie_esantion, sigma)
  cat("Intervalul de incredere de 99% este:", interval_99, "\n")
}
rezultat_statistica("statistica.csv")




#D3
test_proportie_modificat = function(alfa, p0, n, succese, tip) {
  p_prim = succese / n
  scor_z = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)
  
  if (tip == 'r')
    z_critic = qnorm(1 - alfa)
  if (tip == 'l')
    z_critic = qnorm(alfa)
  if (tip == 's')
    z_critic = qnorm(1 - alfa / 2)
  
  return(c(scor_z, z_critic))
}

rezultat_5 = test_proportie_modificat(0.05, 0.15, 100, 14, 'l')
scor_z_5 = rezultat_5[1]
z_critic_5 = rezultat_5[2]

cat("Pentru nivelul de semnificatie de 5%:\n")
cat("Scor_z:", scor_z_5, "\n")
cat("Z_critic:", z_critic_5, "\n")

if (scor_z_5 < z_critic_5) {
  cat("Ipoteza nula se respinge la 5%.\n")
} else {
  cat("Ipoteza nula nu se respinge la 5%.\n")
}

rezultat_1 = test_proportie_modificat(0.01, 0.15, 100, 14, 'l')
scor_z_1 = rezultat_1[1]
z_critic_1 = rezultat_1[2]

cat("\nPentru nivelul de semnificatie de 1%:\n")
cat("Scor_z:", scor_z_1, "\n")
cat("Z_critic:", z_critic_1, "\n")

if (scor_z_1 < z_critic_1) {
  cat("Ipoteza nula se respinge la 1%.\n")
} else {
  cat("Ipoteza nula nu se respinge la 1%.\n")
}
