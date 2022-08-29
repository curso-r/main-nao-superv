# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(cluster)
library(factoextra)

# kmeans ------------------------------------------------------------------


modelo <- iris |>
  select(-Species) |>
  kmeans(centers = 3)

modelo

dados_aumentados <- augment(modelo, iris)

dados_aumentados |>
  ggplot() +
  aes(
    Petal.Length,
    Petal.Width) +
  geom_point(
    aes(color = .cluster),
    size=4)


# Selecionando numero de grupos -------------------------------------------

data <- iris |>
  select(-Species)

wss <- c(
  kmeans(data, 1, nstart=50,iter.max = 15 )$tot.withinss,
  kmeans(data, 2, nstart=50,iter.max = 15 )$tot.withinss,
  kmeans(data, 3, nstart=50,iter.max = 15 )$tot.withinss,
  kmeans(data, 4, nstart=50,iter.max = 15 )$tot.withinss,
  kmeans(data, 5, nstart=50,iter.max = 15 )$tot.withinss,
  kmeans(data, 6, nstart=50,iter.max = 15 )$tot.withinss
  )

wss

plot(1:6, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# O pacote factoextra -----------------------------------------------------

iris_sem_species <- iris |> select(-Species)

fviz_nbclust(iris |> select(-Species), kmeans, method = "wss")

fviz_nbclust(USArrests, kmeans, method = "wss")

# também funciona com o hclust!

fviz_nbclust(iris |> select(-Species), hcut, method = "gap_stat")

fviz_nbclust(iris |> select(-Species), hcut, method = "wss")

fviz_nbclust(USArrests, hcut, method = "wss")

fviz_nbclust(iris |> select(-Species), kmeans, method = "gap_stat")

fviz_nbclust(iris |> select(-Species), kmeans, method = "silhouette")

fviz_nbclust(iris |> select(-Species), hcut, method = "silhouette")


# PCA ---------------------------------------------------------------------

fviz(prcomp(iris_sem_species), element = "ind")

# PCA + cluster -----------------------------------------------------------

fviz_cluster(kmeans(iris_sem_species, centers = 3), geom = "point", data = iris_sem_species)

fviz_cluster(hcut(iris_sem_species, k = 3), geom = "point", data = iris_sem_species)


