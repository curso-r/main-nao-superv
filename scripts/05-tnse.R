
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(tsne)


# Dados -------------------------------------------------------------------


multishapes <- factoextra::multishapes |>
  tibble()

multishapes |>
  ggplot(aes(x = x, y = y, color = factor(shape))) +
  geom_point()

reducao_sne <- multishapes[,-3] |>
  tsne(k = 2, perplexity = 50)

reduzido = reducao_sne |>
  as_tibble() |>
  set_names(c("x", "y")) |>
  mutate(shape = multishapes$shape)

reduzido |>
  ggplot(aes(x = x, y = y, color = factor(shape))) +
  geom_point()
