## Code and libraries for interactive analysis
## Including novel nonparametric generalizability theory
## estimates

library(easystats)
library(tidyverse)
library(lme4)
library(lmerTest)
library(afex)
library(janitor)
library(naniar)
library(performance)
library(parameters)
library(modelsummary)
library(insight)
library(here)
library(energy)
library(magrittr)
library(future)
library(furrr)
library(future.apply)
library(foreach)
library(glue)
library(brms)
library(data.table)
library(broom.mixed)
library(mirt)
library(parallel)
library(data.table)
library(glue)
library(tictoc)
library(here)
library(fastverse)
fastverse_extend(Rfast,dtplyr, 
                 coop,Rfast2,cheapr,
                 stringi,stringdist,
                 vctrs, dqrng,
                 parallelDist
)

# fastverse_install(data.table, collapse, kit, only.missing = FALSE, repos = .fastverse_repos)

plan(multisession)


tic("data load")
tempdf = fread(file = here::here("data","tempdf.csv")) |> melt(id.vars = c("benchmark","item"),variable.name="model",value.name = "resp",variable.factor = F) |> 
  lazy_dt() |> group_by(model) |> filter(!any(vec_detect_missing(resp))) |> ungroup()
toc()

bench_list = tempdf |> dtplyr::lazy_dt() |> 
  select(benchmark) |> distinct() |> pull(benchmark)


n_iter = 3
n_i = 100
prop_m =0.1
alpha = 0.5

## each iteration takes 1hr 15 to run
L = list()


tic("all iterations")
for (i in 7:10){
  
  model_list = tempdf |> dtplyr::lazy_dt() |> 
    select(model) |> 
    distinct() |>slice_sample(prop = 0.1) |>  
    pull(model)
  
  for (b in bench_list){
    tic(glue::glue("Iteration {i} - Benchmark: {b}"))
    cat(glue::glue("Iteration {i} - Benchmark: {b}\n"))
    
    set.seed(i*1234)
    
    item_list = tempdf |> dtplyr::lazy_dt() |> 
      filter(benchmark==b) |> 
      select(item) |> 
      distinct() |> slice_sample(n = 100) |>  
      pull(item)
    
    
    
    
    dat = tempdf |> dtplyr::lazy_dt() |> 
      filter(benchmark==b,
             item %in% item_list,
             model %in% model_list) |> 
      drop_na(resp) |>
      mutate(resp = as.numeric(resp),
             item = as.factor(item),
             model = as.factor(model)) 
    
    disco_fit = disco(dat |> select(resp) |> as.data.table() ,
                      factors = dat |> select(-resp,-benchmark) |> 
                        as.data.table(), 
                      distance = F,
                      index = alpha,
                      R=0)
    
    disco_fit_df = disco_fit$stats |> 
      as_tibble() |> 
      select(-any_of("p-value")) |> 
      mutate(facet = disco_fit$factor.names,Withins = Within, Within = disco_fit$within, Total = disco_fit$total, Ep2 = Trt / (Trt + Within), SNR = Ep2/(1-Ep2),benchmark = b,n_items = n_i,prop_models = prop_m) |> select(benchmark,facet,Ep2,SNR,everything())
    
    disco_fit_df = disco_fit_df |> mutate(benchmark = b,
                                          n_items = n_i,prop_models = prop_m,
                                          iteration = i,alpha = alpha)
    
    L[[length(L)+1]] = disco_fit_df
    print(disco_fit_df)
    toc()
  }
}

disco_results = bind_rows(L)

disco_results |> write_csv(file = here::here("results","disco_n100.csv"))
toc()


disco_results |> filter(facet=="model") |> lmer(formula = Ep2 ~ 0 + benchmark + (1|iteration)) |> summary()

