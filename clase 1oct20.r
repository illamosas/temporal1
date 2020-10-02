library(ggplot2)
library(strucchange)

auto<-read.table("http://www.econ.uiuc.edu/~econ536/Data/AUTO2.txt",header=T)

example <- auto[auto$quarter>=1959.1 & auto$quarter<=1973.3,]

# manipulé las constantes para que salieran en la misma escala
p = ggplot() +
    geom_line(data=example, aes(x=quarter, y=gas+3.5), color="black") +
    geom_line(data=example, aes(x=quarter, y=income), color="blue") +
    geom_line(data=example, aes(x=quarter, y=price-9), color="red")
    xlab("Trimestre") +
    ylab("log(variable)")

print(p)

reg1 <- lm(gas ~ income + price, data=example)
summary(reg1)
eet <- sum(reg1$residuals^2)
# lo mismo que t(reg1$residuals)%*%reg1$residuals
# Chow test parameter instability

largo <- length(example$quarter)
A <- matrix(0,40,2)
for(i in 1:40){
    reg <- lm(gas~income + price, data=example[1:(largo-i),], )
    ee1 <- sum(reg$residuals^2)
    A[i,1] <- ((eet - ee1)/i)/(ee1/(largo-i-2))
    A[i,2] <- 1-pf(A[i,1],i,(largo-i-2))
}

plot(A[,2], type="l")  # discutir


# RESET
example$gas2 <- example$gas^2
example$gas3 <- example$gas^3
example$gas4 <- example$gas^4
reg2 <- lm(gas ~ income + price + gas2 + gas3 + gas4, data=example)
summary(reg2)

model <- gas ~ income + price

# CUSUM

cusum <- efp(model, type="OLS-CUSUM", data=example)
plot(cusum)

fs <- Fstats(model, data=example)
plot(fs)

# Cambio Estructural <- teorico, martes, junto con panel data.
