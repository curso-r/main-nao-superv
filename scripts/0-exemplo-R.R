library(tidyverse)
library(tidymodels)

modelo <- iris |>
  select(-Species) |>
  kmeans(centers = 3)

dados_aumentados <- augment(modelo, iris)

dados_aumentados |>
  ggplot() +
  aes(
    Petal.Length,
    Petal.Width) +
geom_point(
  aes(color = .cluster),
  size=4)
