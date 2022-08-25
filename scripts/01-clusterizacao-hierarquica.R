
# Matriz de distâncias ----------------------------------------------------

vetor_de_distancias <- iris |>
  dist()
# quando tem texto no meio complica!


vetor_de_distancias <- iris |>
  select(-Species) |>
  dist()

# o que sai é um vetor, se quisermos visualiar podemos transformar em matriz

as.matrix(vetor_de_distancias) |>
  View()

# é aqui que mudamos as distâncias se quisermos:

distancias_euclidianas <- iris |>
  select(-Species) |>
  dist()
  #padrao

distancias_euclidianas |>
  as.matrix() |>
  View()

distancias_manhattan <- iris |>
  select(-Species) |>
  dist(method = "manhattan")

distancias_manhattan |>
  as.matrix() |>
  View()

distancias_maximum <- iris |>
  select(-Species) |>
  dist(method = "maximum")

distancias_maximum |>
  as.matrix() |>
  View()

# Dendrogramas ------------------------------------------------------------

dendrograma_euclidiano <- hclust(distancias_euclidianas)

plot(dendrograma_euclidiano)
# sem ggplot

rect.hclust(dendrograma_euclidiano, k = 3)
# retângulos visuais

dendrograma_euclidiano_2 <- hclust(distancias_euclidianas, method = "median")

plot(dendrograma_euclidiano_2)
# sem ggplot

rect.hclust(dendrograma_euclidiano_2, k = 3)
# retângulos visuais

dendrograma_euclidiano_2 <- hclust(distancias_manhattan, method = "median")

plot(dendrograma_euclidiano_2)
# sem ggplot

rect.hclust(dendrograma_euclidiano_2, k = 3)

library(ggdendro)

ggdendrogram(dendrograma_euclidiano)
ggdendrogram(dendrograma_euclidiano_2)
# com ggplot

barplot(dendrograma_euclidiano$height)
barplot(dendrograma_euclidiano_2$height)

barplot(dendrograma_euclidiano$height[(nrow(iris)-10):nrow(iris)])
barplot(dendrograma_euclidiano_2$height[(nrow(iris)-10):nrow(iris)])

# Outros dados ------------------------------------------------------------

us_distancia_euclid <- USArrests |>
  dist()

us_distancia_max <- USArrests |>
  dist(method = "maximum")

us_distancia_manhattan <- USArrests |>
  dist(method = "manhattan")

dendro_euclid <- hclust(us_distancia_euclid)

dendro_max <- hclust(us_distancia_max)

dendro_manh <- hclust(us_distancia_manhattan)

barplot(dendro_euclid$height[(nrow(USArrests)-10):nrow(USArrests)])
barplot(dendro_max$height[(nrow(USArrests)-10):nrow(USArrests)])
barplot(dendro_manh$height[(nrow(USArrests)-10):nrow(USArrests)])

dendro_euclid <- hclust(us_distancia_euclid, "median")

dendro_max <- hclust(us_distancia_max, "median")

dendro_manh <- hclust(us_distancia_manhattan, "median")

barplot(dendro_euclid$height[(nrow(USArrests)-10):nrow(USArrests)])
barplot(dendro_max$height[(nrow(USArrests)-10):nrow(USArrests)])
barplot(dendro_manh$height[(nrow(USArrests)-10):nrow(USArrests)])


