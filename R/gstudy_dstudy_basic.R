# ==============================================================================
# Factorial Mixed Effect Variance Decomposition of LLM Benchmark Performance::
# Generalizability Studies and Design Studies
# Author Mike Hardy
# ==============================================================================
# This script estimates a series of mixed effect models to decompose the variance
# in LLM benchmark performance into components attributable to different model
# characteristics and their interactions.
# ==============================================================================


library(tidyverse)
library(lme4)
library(lmerTest)
library(tictoc)
library(data.table)
library(furrr)
library(janitor)
library(parallel)
library(doParallel)
library(broom.mixed)


# Read in data ------------------------------------------------------------

tic("data load")
tempdf = fread(file = here::here("data","tempdf.csv")) |> melt(id.vars = c("benchmark","item"),variable.name="model",value.name = "resp",variable.factor = F) |> 
  lazy_dt() |> group_by(model) |> filter(!any(vec_detect_missing(resp))) |> ungroup()
toc()

# Load data ---------------------------------------------------------------
# G and D study for item and model counts
# Fit lmer models to estimate variance components separately for bench -------
remodfits = list()
gc()
nb_items_per = 1000
for(b in tempdf$benchmark |> unique()){
  print(b)
  tic(b)
  # remodfits[[b]]
  re_fit = tempdf |> 
    filter(benchmark==b)%>%
    semi_join(tempdf |> filter(benchmark==b) |> 
                select(item) |> distinct() |> sample_n(nb_items_per,replace = F)) |>
    mutate(resp = as.numeric(resp)) |> 
    lmer(formula = resp ~ 1 
          + (1|item)
          + (1|model)
          ,
          # REML = T,
          # nAGQ = 0,
          # data = .,
          # family=binomial,
          na.action = na.exclude,
          control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 1e5),
                                 calc.derivs = F)
    ) 
  re_fit |> summary()

  remodfits[[b]] = re_fit |>
    VarCorr() |>
    janitor::clean_names() |>
    as_tibble() |>
    select(-var2, -var1, -sdcor) |>
    mutate(pct = vcov / sum(vcov) * 100,
           benchmark = b)
  toc()
  gc()
}

remodfits = list_rbind(remodfits,names_to="benchmark")
remodfits |> saveRDS(here::here("data",glue::glue("item_lmer_reliab_{nb_items_per}.rds")))
remodfits = readRDS(here::here("data",glue::glue("item_lmer_reliab_{nb_items_per}.rds")))

n_d_study = 200 ## to get gpqa, need to turn up number of items to 325 to get above 0.7 reliability
remodfits |> select(-pct) |> 
  pivot_wider(names_from = grp, 
              values_from = vcov) |> 
  bind_cols(tibble(num_items = rep(1,6),
                   num_mod = 1)) |>
  mutate(i_rel = item / (item + model + Residual),
         m_dep = model / (item + model + Residual) ,
         m_rel = model / ( model + Residual),
         m_rel_D =  model / ( model + Residual/(num_items*num_mod)),
         m_snr_D = m_rel_D / (1 - m_rel_D),
         m_errtol_D = sqrt((1 - m_rel_D) / m_rel_D)
  )


item_counts = tempdf |> 
  select(benchmark,item) |> distinct() |> group_by(benchmark) |> summarize(n=n())


L = list()
for (n in c(1:20,25,30,40,50,100,150,200,250,300,325,350,400,450,500,600,700,1000)){
  print(n)
  L[[length(L)+1]] = remodfits |> select(-pct) |> 
    pivot_wider(names_from = grp, 
                values_from = vcov) |> 
    bind_cols(tibble(num_items = rep(n,6),
                     num_mod = 1)) |>
    mutate(i_rel = item / (item + model + Residual),
           m_dep = model / (item + model + Residual) ,
           m_rel = model / ( model + Residual),
           m_rel_D =  model / ( model + Residual/(num_items*num_mod)),
           m_snr_D = m_rel_D / (1 - m_rel_D),
           m_errtol_D = sqrt((1 - m_rel_D) / m_rel_D)
    ) |> left_join(item_counts, by = "benchmark") |> 
    mutate(prop_items = num_items/n)
}

dstudydf = list_rbind(L)

dstudydf |> filter(prop_items<=1) |> ggplot(aes(x = prop_items, y = m_rel_D, color=benchmark)) + geom_line() + theme_minimal() 

dstudydf |> filter(prop_items<=1) |> ggplot(aes(x = num_items, y = m_rel_D, color=benchmark)) + geom_line() + theme_minimal() 

# Fit lmer models to estimate variance components across bench -------
# remodfits = list()
gc()
nb_items = 500
tic("all benchmarks")
sampleddf = tempdf |> 
  semi_join(tempdf |> select(benchmark,item) |> distinct() |> group_by(benchmark) |> 
              sample_n(nb_items,replace = F) |> ungroup()) 
fullfit = sampleddf |>
  lmer(formula = resp ~ 1 
       + (1|item)
       + (1|model)
       + (1|benchmark)
       + (1|benchmark:model)
       ,
       REML = T,
       control = lmerControl(optimizer = "bobyqa",
                             # optCtrl = list(maxfun = 1e5),
                             calc.derivs = F)
  ) 


fullfitvar = fullfit |>
  VarCorr() |>
  janitor::clean_names() |>
  as_tibble() |>
  select(-var2, -var1, -sdcor) |>
  mutate(pct = vcov / sum(vcov) * 100)

print(summary(fullfit))

toc()
gc()

fullfit |> saveRDS(here::here("data",glue::glue("item_lmer_reliab_full_{nb_items}.rds")))
fullfit = readRDS(here::here("data",glue::glue("item_lmer_reliab_full_{nb_items}.rds")))

fullfitvar = fullfit |>
  VarCorr() |>
  janitor::clean_names() |>
  as_tibble() |>
  select(-var2, -var1, -sdcor) |>
  mutate(pct = vcov / sum(vcov) * 100)



## model reliability old calcs
## 
mm = 1
ii = 1
bb = 1
(fullfitvar$vcov[1]/(mm*bb)+fullfitvar$vcov[2]/(mm))/(fullfitvar$vcov[1]/(mm*bb)+fullfitvar$vcov[2]/(mm)+fullfitvar$vcov[3]/(bb*ii)+fullfitvar$vcov[4]/bb+fullfitvar$vcov[5]/(bb*ii*mm))
## 
(Ep2 = (fullfitvar$vcov[2]/(mm))/(fullfitvar$vcov[1]/(mm*bb)+fullfitvar$vcov[2]/(mm)+fullfitvar$vcov[5]/(bb*ii*mm)))  
(snr = Ep2/(1-Ep2))
(errtol = sqrt((1-Ep2)/Ep2))


fullfitvar |> select(-pct) |> 
  pivot_wider(names_from = grp, values_from = vcov) |> 
  bind_cols(tibble(num_items = 1:30,
                   num_mod = 1,
                   num_bench = 6)) |> 
  mutate(
    
    i_rel = item / (item + model + Residual),
    m_dep = model / (item + model + Residual) ,
    m_rel = model / ( model + Residual),
    m_rel_D =  (model/num_mod) / ( model/num_mod + benchmark_model/(num_mod*num_bench) + Residual/(num_mod*num_items*num_bench)),
    m_snr_D = m_rel_D / (1 - m_rel_D),
    m_errtol_D = sqrt((1 - m_rel_D) / m_rel_D)
  )
