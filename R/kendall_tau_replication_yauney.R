## Replication of "Yauney et al"
## 
library(dplyr)
library(readr)
library(janitor)
library(pcaPP)

set.seed(1)

aggdf <- read_csv("data/aggdf.csv", show_col_types = FALSE) |> clean_names()

# build bbh test set and attach each model's overall bbh mean ("average")
bbh_data <- aggdf |>
  group_by(model, benchmark) |>
  summarise(average = mean(resp, na.rm = TRUE), .groups = "drop") |>
  group_by(benchmark) |>
  mutate(split = if_else(average <= median(average, na.rm = TRUE), "train", "test")) |>
  ungroup() |>
  filter(benchmark == "bbh", split == "test") |>
  select(model, benchmark, average) |>
  inner_join(aggdf, by = c("model", "benchmark"))

items <- unique(bbh_data$item)

kcors <- replicate(500, {
  randitems <- sample(items, 100, replace = FALSE)
  m <- bbh_data |>
    filter(item %in% randitems) |>
    group_by(model) |>
    summarise(average = mean(average, na.rm = TRUE),
              resp    = mean(resp,    na.rm = TRUE),
              .groups = "drop")
  cor.fk(m$average, m$resp)
})

mean(kcors)