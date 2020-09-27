# Objetivo de la clase de hoy: Clonar un proyecto, hacer fork, y realizar cambios.

# Determinar una distribucion de la verdadera poblacion
rm(list=ls())
x1 <- rnorm(10000, 4,3) 
plot(density(x1))

plot(ecdf(x1))

x2 <- rgamma(10000,2,1.5)

plot(density(x2))
plot(ecdf(x2))

# tomar muestras de la poblacion
s <- rep(0,500)
for(i in 1:500) {
  s[i] <- mean(sample(x2, 100))
}

plot(density(s))

# regresión, modelo
# instalar paquete MASS (install.packages("MASS"))
library(MASS)
rm(list=ls())

Sigma <- matrix(c(10,3,3,2),2,2)

X <- mvrnorm(n=1000, c(4,6), Sigma)
plot(X[,1],X[,2])

# modelo,(como modelador de los datos, conoces epsilon)
#como dato a considerar se sabe que la media de epsilon es 0
beta <- c(4,-2, 9, -3)
c <- 15
epsilon = rnorm(1000,0,4)
cuadrado <- X[,1]^2
interaccion <- X[,1]*X[,2]

X <- cbind(X, cuadrado, interaccion)

y <- c + X%*%beta + epsilon
fm1 <- y ~  X 
fm2 <- y ~  X[,1]
plot(y, X[,1])


reg1 <- lm(fm1)
summary(reg1)
#se puede aplicar un plot en las dos regresiones para 
#observar de mejor manera la dispersión de datos 
plot(reg1)

reg2 <- lm(fm2)
summary(reg2)
plot(reg2)
