# Objetivo de la clase de hoy: Clonar un proyecto, hacer fork, y realizar cambios.

# Determinar una distribucion de la verdadera poblacion
rm(list=ls())
x1 <- rnorm(10000, 4,3) 
#una distribucion normal que 10000
#aleatorios  con mean=3 y desviaciones estandar 3
#redondeados a 3 lugares despues de la coma.
x1<- round(rnorm(10000,4,3),3)
x1

plot(ecdf(x1))

x2 <- rgamma(10000,2,1.5)
#redondeados a 3 lugares despues de la coma.
x2<- round(rnorm(10000,4,3),3)
x2

plot(density(x2))
plot(ecdf(x2))

# tomar muestras aleatorias de la poblacion
#redondeando a 3 lugares despues de la coma 
s <- rep(0,500)
for(i in 1:500) {
  s[i] <- round(mean(sample(x2, 100)),3)
}
s
plot(density(s))

# regresión, modelo
# instalar paquete MASS (install.packages("MASS"))
library(MASS)
rm(list=ls())

Sigma <- matrix(c(10,3,3,2),2,2)
Sigma
X <- round(mvrnorm(n=1000, c(4,6), Sigma),3)
X
plot(X[,1],X[,2],main = 'Distribucion normal de datos multivariados')


# modelo,(como modelador de los datos, conoces epsilon)

beta <- c(4,-2, 9, -3)
beta
c <- 15
epsilon = rnorm(1000,0,4)
epsilon
cuadrado <- X[,1]^2
cuadrado
mediacuadradox<-mean(cuadrado)
mediacuadradox

interaccion <- X[,1]*X[,2]
# cbind une combina todos los datos en una misma columna
X <- cbind(X, cuadrado, interaccion)
#para q el producto matricial tenga sentido 
#guardo X como una matriz de m filas x4columnas
X<-matrix(X,1000,4)
X
y <- c + X%*%beta + epsilon
y
fm1 <- y ~  X 
fm2 <- y ~  X[,1]
plot(y, X[,1])

#REGRESION DE Y VS 4 COLUMNAS
reg1 <- lm(fm1)
summary(reg1)

#REGRESION DE Y VS PRIMERA COLUMNAS
reg2 <- lm(fm2)
summary(reg2)
plot(reg2)

# Problemas con epsilon (en las notas, es u)

# a) u no es normal
epsilon <- rgamma(1000,2,2)*13  # su media no es cero, re-centrar
epsilon <- epsilon - mean(epsilon)
hist(epsilon)

# volver a obtener la ecuación que genera los datos (desconocida por el investigador)

y <- c + X%*%beta + epsilon

# asumamos que conocemos todos los regresores... cual sería el problema?

reg1 <- lm(fm1)
summary(reg1)

e <- reg1$residuals  # ojo, no errores, residuales

hist(e)   # hereda la distribución madre
          # ¿Cuál fue la distribución de la regresión normal?
  
# Para probar distribuciones, instalar el paquete "normtest"
# install.packages("normtest")

library(normtest)

jb.norm.test(e, nrepl=1000)   # rechaza normalidad... pero.... es un problema???

# Veamos con un problema mas terrenal... no 1000 datos, menos, como 50

# generemos las variables de nuevo, ahora 50
# primero normal
X <- mvrnorm(n=50, c(4,6), Sigma)
beta <- c(4,-2, 9, -3)
c <- 15
epsilon = rnorm(50,0,4)
cuadrado <- X[,1]^2
interaccion <- X[,1]*X[,2]
X <- cbind(X, cuadrado, interaccion)
y <- c + X%*%beta + epsilon
fm1 <- y ~  X 
reg2_normal <- lm(fm1)
summary(reg2_normal)
e <- reg2_normal$residuals
hist(e)
jb.norm.test(e, nrepl=1000)

# todo excelente!, ahora violemos normalidad

epsilon <- rgamma(50,2,2)*13  # su media no es cero, re-centrar
epsilon <- epsilon - mean(epsilon)
y <- c + X%*%beta + epsilon

reg2_gamma <- lm(fm1)
summary(reg2_gamma)
e <- reg2_gamma$residuals
hist(e)
jb.norm.test(e, nrepl=1000)

# que vemos... que pasó con los valores p?????
# moraleja: cuando esta violación es un problema?????

# b) Violación homoscedasticidad
# instalar sandwich lmtest
library(lmtest)
library(sandwich)
# cada observación tiene una desviación estándar distinta... entonces modifiquemos
# epsilon
set.seed(1233)
epsilon = rnorm(50,0,2*abs(X[,1]))
jb.norm.test(epsilon, nrepl=1000)


y <- c + X%*%beta + epsilon

reg3_het <- lm(fm1)
summary(reg3_het)
e <- reg3_het$residuals
hist(e)
jb.norm.test(e, nrepl=1000)

# no es normal, pero por razones diferentes.

bptest(reg3_het)  # bajo H0, es homoscedastico... si rechaza es heteroscedastico

# como corregirlo???

vc_reg3 <- vcovHC(reg3_het, type="HC")
sqrt(diag(vc_reg3))  # nuevos errores estándar... se deben sustituir

# c) Autocorrelación (cuando veamos series de tiempo)

## Problemas con X
# Exclusión de variables relevantes
# Ya lo hicimos, al exluir X2

# Inclusión de variables irrelevantes
# Siempre mejoran la R2, pero no la R2 ajustada
# la teoría normalmente les dirá cuando es irrelevante

# Forma funcional incorrecta
# Ya lo hicimos, al exluir interacción y cuadraticas del modelo al inicio.

# Matriz X no tiene rango completo (collinealidad)
# Recordar la función VIF del semestre pasado

# Variables no estacionales
# se verán en series de tiempo

# Siguiente clase: Consistencia de Datos, cambio estructural y variable dicotómica.
# (estamos atrasados 1 clase)

# Consistencia de datos
# Uno de los criterios mas importantes para estimar una ecuación, es que
# dichos parámetros deben tener relevancia para datos que no se usaron en la estimación.
# Ir a pizarrón para explicación empírica.


