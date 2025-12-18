
# ==============================================================================
# Factorial Mixed Effect Variance Decomposition of LLM Benchmark Performance::
# Generalizability Studies and Design Studies
# Author Mike Hardy
# ==============================================================================
# This script estimates a series of mixed effect models to decompose the variance
# in LLM benchmark performance into components attributable to different model
# characteristics and their interactions.
# ==============================================================================

library(parallel)
library(data.table)
library(glue)
library(tictoc)
library(here)
library(performance)
library(parameters)
library(modelsummary)
library(modelbased)
library(report)
library(effectsize)
library(insight)
library(see)
library(tidyverse)
library(lme4)
library(lmerTest)
library(fixest)
library(broom.mixed)
library(janitor)
library(broom)
library(furrr)
library(mirt)
library(future)
library(parallel)
library(fastverse)
fastverse_extend(Rfast,dtplyr, 
                 coop,Rfast2,cheapr,
                 stringi,stringdist,
                 vctrs, dqrng,
                 parallelDist
)

future::plan("multicore")







# mixed effect models

metadf = read_csv(here::here("data","openllm_all_model_info_full.csv")) |> 
  janitor::clean_names() |> 
  # filter(number_params_b>0,!flagged) |> 
  mutate(model = make_clean_names(fullname),
         log_parms = if_else(number_params_b>0,log10(number_params_b),NA_real_),
         # generation = generation/10,
         type = as_factor(type),
         author = make_clean_names(str_split_i(fullname,"/",1),allow_dupes = T),
         base_author = make_clean_names(str_split_i(base_model,"/",1),allow_dupes = T),
         removed = as.numeric(base_author == "removed"),
         adapted_weights = weight_type == "Adapter",
         base = str_split_i(architecture,"For|Model",i = 1),
         base = as_factor(make_clean_names(if_else(is.na(base)|(base %in% c("?","Unknown")),"unk",base),allow_dupes = T)),
         chat = as.numeric(t=="💬"),
         anychat = as.numeric((t=="💬") | chat_template),
         mermoe = t=="🤝"|merged,
         multi = as.numeric(t=="🌸"),
         moe = (t=="🤝" & !merged) | mo_e | str_detect(str_to_lower(base),"moe"),
         across(c(merged,mo_e,chat_template,official_providers,adapted_weights,available_on_the_hub,flagged), \(x) as.numeric(x) ),
         fulltune = as.factor(str_c(type,if_else(chat_template==1,"_chat",""),
                                    if_else(mo_e==1,"_moe",""),
                                    if_else(merged==1,"_mg",""))),
         tunetype = as.factor(str_c(type,if_else(chat_template==1,"_chat",""),
                                    if_else(mo_e==1,"_moe",""))),
         not_avail = if_else(available_on_the_hub==0,1,0),
         contrib = as.factor(str_c(author,if_else(removed==1,"_rm",""),
                                   if_else(available_on_the_hub==1,"_hb",""))),
         modelbase = as.factor(str_c(base,as.character(generation),precision,sep=":")),
         modelarch = as.factor(str_c(architecture,generation,precision,sep=":")),
         sft = as.numeric(t=="🔶")) |>
  select(model,everything()) 




# full random effect model ---------
tic()
full_gstudy_fit = metadf |> 
  pivot_longer(c(if_eval,bbh,mmlu_pro,math_lvl_5,gpqa,musr),names_to = "bench",values_to = "val") |> 
  mutate(not_avail = as.numeric(!available_on_the_hub)) |>
  lmer(formula = val ~ 1
       + log_parms
       + (log_parms||bench) # benchmark
       + (log_parms||base:generation:precision) # model base
       + (log_parms||contrib) # contributor
       + (log_parms||fulltune) # model tuning type
       + (log_parms||bench:base:generation:precision)
       + (log_parms||bench:contrib)
       + (log_parms||bench:fulltune)
       + (log_parms||base:generation:precision:contrib)
       + (log_parms||base:generation:precision:fulltune)
       + (log_parms||contrib:fulltune)
       + (log_parms||bench:base:generation:precision:fulltune)
       + (log_parms||contrib:base:generation:precision:fulltune)
       + (log_parms||bench:contrib:fulltune)
       + (log_parms||bench:base:generation:precision:contrib)
       ,
       data = _,
       REML = T,
       na.action = na.exclude,
       control = lmerControl(
         optimizer ="Nelder_Mead", # "Nelder_Mead",bobyqa
         calc.derivs = FALSE)
       
  )
summary(full_gstudy_fit)

gstudy_summary = full_gstudy_fit |> VarCorr() |>
  janitor::clean_names() |>
  as_tibble() |>
  filter(is.na(var2)) |> 
  mutate(pct = round(vcov / sum(vcov) * 100,2),
         bench = str_detect(grp,"bench"),
         base = str_detect(grp,"base|arch"),
         auth = str_detect(grp,"author|contrib"),
         type = str_detect(grp,"type|tune"),
         resid = grp=="Residual",
         npar = var1 == "log_parms",
         npar= replace_na_with(npar,0),
         base_type = base & type, 
         base_type_bench = base_type & bench, 
         across(bench:base_type_bench,\(x) vcov*as.numeric(x))
  )

gstudy_summary|> print(n=253)



toc()


saveRDS(list(mod = full_gstudy_fit,
             sum_tab = gstudy_summary),
        file = here::here("data","results","full_gstudy_openllmbenches.rds"))
        
# saveRDS(list(mod = full_gstudy_fit,
#              sum_tab = gstudy_summary),
#         file = "~/Library/CloudStorage/OneDrive-Stanford/Projects/ai_benchmark/github/benchmark_reliability/benchmark_reliability/data/results/full_gstudy_openllmbenches.rds")




(mod_type_re = (gstudy_summary |> filter(type&base, !bench, !auth, !resid) |> 
                  summarise(vcov = sum(vcov)))$vcov/(gstudy_summary |> filter((type&base)| resid) |> 
                                                       summarise(vcov = sum(vcov)))$vcov)


# full random effect model no abbreviations ---------
tic()
m3 = metadf |> 
  pivot_longer(c(if_eval,bbh,mmlu_pro,math_lvl_5,gpqa,musr),names_to = "bench",values_to = "val") |> 
  mutate(not_avail = as.numeric(!available_on_the_hub)) |>
  lmer(formula = val ~ 1
       + log_parms
       + (log_parms||bench)
       # + (log_parms|base)
       + (log_parms||architecture:generation:merged)
       # + (removed+not_avail|author)
       + (log_parms||author:removed:not_avail)
       + (log_parms||type:chat_template:mo_e)
       + (log_parms||bench:architecture:generation:merged)
       + (log_parms||bench:author:removed:not_avail)
       + (log_parms||bench:type:chat_template:mo_e)
       + (log_parms||architecture:generation:merged:author:removed:not_avail)
       + (log_parms||architecture:generation:merged:type:chat_template:mo_e)
       + (log_parms||author:removed:not_avail:type:chat_template:mo_e)
       + (log_parms||bench:architecture:generation:merged:type:chat_template:mo_e)
       + (log_parms||author:removed:not_avail:architecture:generation:merged:type:chat_template:mo_e)
       + (log_parms||bench:author:removed:not_avail:type:chat_template:mo_e)
       + (log_parms||bench:architecture:generation:merged:author:removed:not_avail)
       
       ,
       data = _,
       na.action = na.exclude,
       control = lmerControl(
         optimizer ="bobyqa", # "Nelder_Mead",bobyqa
         calc.derivs = FALSE)
       
  )
summary(m3)

m3 |> VarCorr() |>
  janitor::clean_names() |>
  as_tibble() |>
  filter(is.na(var2)) |> 
  mutate(pct = round(vcov / sum(vcov) * 100,2)) |> print(n=253)


toc()



gstudy_summary = m3 |> VarCorr() |>
  janitor::clean_names() |>
  as_tibble() |>
  filter(is.na(var2)) |> 
  mutate(pct = round(vcov / sum(vcov) * 100,2),
         bench = str_detect(grp,"bench"),
         base = str_detect(grp,"base|arch"),
         auth = str_detect(grp,"author|contrib"),
         type = str_detect(grp,"type|tune"),
         resid = grp=="Residual",
         npar = var1 == "log_parms",
         npar= replace_na_with(npar,0),
         base_type = base & type, 
         base_type_bench = base_type & bench, 
         across(bench:base_type_bench,\(x) vcov*as.numeric(x))
  )

gstudy_summary|> print(n=253)



toc()


saveRDS(list(mod = m3,
             sum_tab = gstudy_summary),
        file = here::here("data","results","full_gstudy_openllmbenches_best_fit.rds"))
