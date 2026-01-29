## Replication of "Yauney et al" to demonstrate differences in predicting 
## with different underlying distributions

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


## alternative (less fast) code
library(dplyr)
library(readr)
library(janitor)
library(pcaPP)
set.seed(1)
aggdf <- read_csv("data/aggdf.csv", show_col_types = F) |> clean_names()
n_items = 100; n_mods = 250
# BBH example: each model's overall mean ("performance on the full dataset")
bbh_data <- aggdf |>filter(benchmark == "bbh") |> drop_na() 
items <- distinct(select(bbh_data,item))
mods <- distinct(select(bbh_data,model))
bbh_data <- aggdf |> group_by(model) |>
  mutate(average = mean(resp, na.rm = T), .groups = "drop") |> ungroup()
# Function to get model-wise mean of a microbenchmark subset and 
# compute kendall corr with full dataset mean (see Figs. 4, 10, 16, and 17)
get_kcor_estimates <- function(niter= 50, n_items=100, n_mods=250){
  kcors <- replicate(niter, {
    randitems <- slice_sample(items, n= n_items, replace = F) |> pull(item)
    randmods <- slice_sample(mods, n=n_mods, replace = F) |> pull(model)
    m <- bbh_data |>filter(item %in% randitems,model %in% randmods) |> group_by(model,average) |>
      summarise(resp = mean(resp, na.rm = T),.groups = "drop")
    cor(m$average, m$resp,method = "kendall")})
  return(tibble(mean_kcor = mean(kcors), se_kcor = sd(kcors)))}
(kcors <- get_kcor_estimates(50, n_items = 100,  n_mods = 250))

testset <- expand_grid(benchmark = unique(bbh_data$benchmark),
                       item = unique(bbh_data$item)) %>%
  mutate(resp = NA_real_)
testset = expand_grid(benchmark = unique(bbh_data$benchmark),
                      item = unique(bbh_data$item)) %>%
  mutate(resp = NA_real_)
