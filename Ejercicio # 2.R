# ================= EJERCICIO # 2 =================== #

salarios <- read.csv("salarios2014.csv")

N <- 38123
# a) *************************************************
e <- 800
z <- qnorm(0.975)

S2 <- var(salarios$BasePay)

n_0 <- (z^2*S2)/e^2
n_0

n <- round(n_0/(1 + n_0/N))
n

set.seed(29)
indices <- sample(1:N,n)

muestra <- salarios[indices,]

# b) *************************************************
 
 ybar <- mean(muestra$BasePay)
 ybar 
 
 that <- N*ybar
 that 
 
 s2 <- var(muestra$BasePay) 
 
 Vhat_that <- N^2*(1-n/N)*(s2/n)
 
 t <- qt(0.975,n-1) 
 
 LI <- that - t*sqrt(Vhat_that)  
 LS <- that + t*sqrt(Vhat_that) 
 
 cat("El IC del 95% para el total del sueldo base es de:\n","[",LI,",",LS,"]")
 
# c) **************************************************
  PT <- ifelse(muestra$Status == "PT",1,0)
  
  muestra <- cbind(muestra,PT) 
  
  phat <- mean(PT)
  phat  
 
  Vhat_phat <- (1-n/N)*((phat*(1-phat))/(n-1))
  Vhat_phat
 
  t <- qt(0.975,n-1)
 
  LI <-phat - t*sqrt(Vhat_phat)  
  LS <- phat + t*sqrt(Vhat_phat)  
  
  cat("El IC del 95% para la proporcion de trabajadores a medio tiempo es de: \n","[",LI,",",LS,"]")  
  