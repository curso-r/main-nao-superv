
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(ggfortify)


# PCA-2d ------------------------------------------------------------------

set.seed(01092022)

Xx <- rnorm(1:1000, sd = 1)

X <- tibble(Xx, Y = Xx+rnorm(length(Xx), sd = 0.5))

PCA <- princomp(X)

autoplot(PCA)

A <- t(PCA$loadings)
Y <- PCA$scores

plot(aux)
lines(
  rbind(
    PCA$center,
    A[,1]
  ), col = 'blue', lwd = 10)
lines(
  rbind(
    PCA$center,
    A[,2]
  ), col = 'red', lwd = 10)


# aproximação -------------------------------------------------------------

X_aprox = as_tibble(Y %*% A + PCA$center) |>
  set_names(c("X", "Y"))

X[1,]
X_aprox[1,]

View(X)
View(Y %*% A + PCA$center)

X_aprox_1 = as.matrix(Y[,1]) %*% A[1,] + PCA$center

X_aprox_1[1,]
X[1,]

plot(aux)
lines(
  rbind(
    PCA$center,
    A[,1]
  ), col = 'blue', lwd = 10)
lines(
  rbind(
    PCA$center,
    A[,2]
  ), col = 'red', lwd = 10)

points(X_aprox_1, col = "darkgreen")

# O PCA minimiza a distância euclidiana dos pontos pretos pros pontos verdes

# Erro --------------------------------------------------------------------

# O PCA minimiza o seguinte erro:

sqrt(sum((X-X_aprox_1)^2))
# mas esse número não diz absolutamente nada.

# um número mais fácil de interpretar e de calcular é a comparação das
# variâncias
var(X_aprox_1)
var(X)

sum(diag(var(X_aprox_1)))/sum(diag(var(X)))
# aproximação boa

sum(diag(var(X_aprox)))/sum(diag(var(X)))
# aproximação ótima


