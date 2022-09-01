# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(factoextra)
library(fpc)

# exemplo -----------------------------------------------------------------

X = iris[,1:4]
mu = colMeans(X)

Xpca = princomp(X)

fviz(Xpca, "ind")

# interpretando a matriz A ------------------------------------------------

fviz(Xpca, "var")

# essa é uma visualização da matriz A truncada nas colunas
# dos componentes principais!

# clusterização e PCA -----------------------------------------------------

fviz_cluster(kmeans(iris[,1:4], centers = 3), data = iris[,1:4])
fviz_cluster(hcut(iris[,1:4], k = 3), data = iris[,1:4])
fviz_cluster(dbscan(iris[,1:4], eps = 1), data = iris[,1:4])

# PCA menos ingenuo -------------------------------------------------------

dados_batedores <- ISLR::Hitters |>
  drop_na() |>
  select(-c("League", "Division", "NewLeague")) |>
  mutate(
    across(
      .fns = function(x){(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}
    )
  )

princomp(dados_batedores) |>
  plot()

princomp(dados_batedores) |>
  fviz("var")

princomp(dados_batedores) |>
  fviz("ind")


fviz_cluster(kmeans(dados_batedores, centers = 3), data = dados_batedores)
fviz_cluster(hcut(dados_batedores, k = 3), data = dados_batedores)

# PCA na matriz de distancias ---------------------------------------------

dados_batedores <- ISLR::Hitters |>
  drop_na() |>
  select(-c("League", "Division", "NewLeague")) |>
  mutate(
    across(
      .fns = function(x){(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}
    )
  )

distancias <- as.matrix(dist(dados_batedores))

princomp(distancias) |>
  fviz("ind")

fviz_cluster(hcut(distancias, k = 3), data = distancias)

# PCA na matriz de distancias  --------------------------------------------

ISLR::College |>
  select(-Private) |>
  princomp() |>
  fviz("ind")

ISLR::College |>
  select(-Private) |>
  princomp() |>
  fviz("var")

ISLR::College |>
  select(-Private) |>
  dist(method = "max") |>
  as.matrix() |>
  princomp() |>
  fviz("ind")

dados_corredores <- factoextra::decathlon2 |>
  select_if(is.numeric) |>
  mutate(
    across(
      .fns = function(x){(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}
    )
  )


dados_corredores |>
  princomp() |>
  plot()

dados_corredores |>
  princomp() |>
  fviz("ind")

dados_corredores |>
  princomp() |>
  fviz("var")

dados_corredores |>
  dist() |>
  as.matrix() |>
  princomp() |>
  fviz("ind")

fviz_cluster(kmeans(dados_corredores, centers = 3),
             data = dados_corredores)
fviz_cluster(hcut(dados_corredores, k = 3), data = dados_corredores)



