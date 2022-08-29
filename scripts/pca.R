
# Pacotes -----------------------------------------------------------------

library(tidyverse)


# exemplo -----------------------------------------------------------------

X = iris[,1:4]
mu = colMeans(X)

Xpca = prcomp(X)

nComp = 2
Xhat = Xpca$x[,1:nComp] %*% t(Xpca$rotation[,1:nComp])
Xhat = scale(Xhat, center = -mu, scale = FALSE)

Xhat[1,]
X[1,]

plot(Xpca$x[,1:2])


# interpretando a matriz A ------------------------------------------------

library(factoextra)

fviz(prcomp(X), "ind")
