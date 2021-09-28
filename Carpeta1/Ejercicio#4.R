# ================== EJERCICIO 4 ================ #

muestra <- read.csv("bancoMuestra.csv")
x <- ifelse(muestra$y == 0,1,0)
muestra <- cbind(muestra,x)
u <- ifelse(muestra$x == 1,muestra$balance,0)
muestra <- cbind(muestra, u)

ybar_d <- mean(u)/mean(x)
ybar_d

n_d <- sum(x)
n_d
n <- 6493
sy2_d <-tapply(muestra$balance, muestra$x, var)

Vhat.ybar_d <- (n/n_d^2)*((n_d-1)/(n-1))*sy2_d[2]
Vhat.ybar_d

t <- qt(0.975,n_d-1)

LI <- ybar_d - t*sqrt(Vhat.ybar_d)
LS <- ybar_d + t*sqrt(Vhat.ybar_d)

cat("El IC del 95% para el balance promedio de los usuarios que no aceptaron el plan de depositos es de:\n",
    "[",LI,",",LS,"]")
