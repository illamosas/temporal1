rm(list=ls())

setwd("/mnt/c/Users/illamosas/OneDrive/Documents/GitHub/temporal1/")
A <- read.csv("psidextract.csv")   # tanto la base como el codigo estarán en Github en ambos
                                        # repositorios.
head(A)
# Base de datos de Baltagi, Khanti-Akom (1990) del Panel Study of Income Dynamics
# PSID

#id     : individuo
#t      : tiempo
# exp   : Años experiencia en trabajo de tiempo completo
# wks   : Semanas trabajadas
# occ   : ocupación (1 es blue-collar)
# ind   : industria (1 si trabaja en manufacturas)
# south : residencia (=1 si radica en el sur)
# smsa  : =1 si vive en zona metropolitana
# ms    : Estado marital
# fem   : =1 si es mujer
# union : si el salario fué establecido por un sindicato
# ed    : años de educación
# blk   : si es afroamericano
# lwage : logaritmo de salario
# tdum  : dummies de tiempo
# exp2  : experiencia al cuadrado

par(mar=c(5,5,3,3))

coplot(lwage ~ t|id, type="l", data=A)
# seleccionamos los primeros 20 individuos para ver como se desempeñan los salarios
plot(lwage~t, type="l", data=A[A$id<20,], xlab="tiempo",ylab="Ln(salario)")
plot(lwage~t, type="l", data=A, xlab="tiempo",ylab="Ln(salario)")

# semanas trabajadas
plot(wks~t, type="l", data=A[A$id<20,], xlab="tiempo",ylab="semanas trabajadas")

# salario y experiencia
library(ggplot2)
p <- ggplot(A, aes(exp, lwage)) +
  geom_point()
# si la regresión es lineal
p + geom_smooth(method = lm)
# un poco mas flexible.
p + geom_smooth(method = "loess")
# es lineal??

library(gplots)
plotmeans(lwage ~ t, main="Heterogeneidad en periodos", data=A)

# regresión lineal
# ols = mco (ordinary least squares = minimos cuadrados ordinarios)
ols <- lm(lwage ~ exp + exp2 + wks + ed, data=A)
summary(ols)

# pico en experiencia laboral = 31 años... ¿por qué?

# LSDV (Least Squares Dummy Variables) (MCO con variables dicotomicas individuales)
A$cat_id <- factor(A$id)
ols <- lm(lwage ~ exp + exp2 + wks + ed + cat_id, data=A)
summary(ols)

# Efectos Fijos
library(plm)
ef <- plm(lwage ~ exp + exp2 + wks + ed, data=A, index=c("id", "t"), model="within")
summary(ef)
# ¿Qué le pasó a educación?!!!!!!!!!!!!!!!!!!!!!

# Efectos Aleatorios
ea <- plm(lwage ~ exp + exp2 + wks + ed, data=A, index=c("id", "t"), model="random")
summary(ea)

# otra forma mas ordenada de hacer los dos comandos anteriores
panel <- pdata.frame(A, index = c("id", "t"))
ef <- plm(lwage ~ exp + exp2 + wks + ed, data=panel, model="within")
summary(ef)

# Efectos fijos o efectos aleatorios
# hipotesis nula, Efectos Aleatorios provee estimadores consistentes.
phtest(ef, ea)
# si sale significativo, se debe usar efectos fijos, si no... efectos aleatorios.

# controlando por heteroscedasticidad
library(lmtest)
coeftest(ef, vcov=vcovHC)

# cluster estandard errors robust.
coeftest(ef, vcov=vcovHC(ef, type="sss", cluster="group"))

