


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
library(brms)
library(broom.mixed)
library(janitor)
library(naniar)
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



# set up values -------

# data loading ---------

metadf = read_csv(here::here("data","openllm_all_model_info_full.csv")) |> 
  janitor::clean_names() |> 
  filter(number_params_b>0,!flagged) |> 
  select(fullname,number_params_b,merged,mo_e,chat_template,official_providers,type,weight_type,architecture,generation,t,precision ) |> 
  mutate(model = make_clean_names(fullname),
         log_parms = log10(number_params_b),
         # generation = generation/10,
         type = as_factor(type),
         author = str_split_i(fullname,"/",1),
         adapted_weights = weight_type == "Adapted",
         base = as_factor(str_split_i(architecture,"For|Model",i = 1)),
         chat = as.numeric(t=="💬"),
         anychat = as.numeric((t=="💬") | chat_template),
         mermoe = t=="🤝"|merged,
         multi = as.numeric(t=="🌸"),
         moe = (t=="🤝" & !merged) | mo_e | str_detect(str_to_lower(base),"moe"),
         across(c(merged,mo_e,chat_template,official_providers,adapted_weights), \(x) as.numeric(x) ),
         sft = as.numeric(t=="🔶")) |>
  select(model,everything()) 


mmluall_raw = readr::read_csv("/Users/mhardy/Library/CloudStorage/OneDrive-Stanford/Projects/helm/data/resmat_mmlu_all.csv")
# mmluall = mmluall_raw |> dplyr::select(-`...1`) |> as.data.frame()
# 
mmlu_ann = readxl::read_xlsx("/Users/mhardy/Library/CloudStorage/OneDrive-Stanford/Projects/helm/data/res/mmlu_metrics_v1_annotated.xlsx")

bad1 = mmlu_ann |> filter(bad_item == 1) |> pull(item)
bad0 = mmlu_ann |> filter(bad_item > 0) |> pull(item)


wfdf = read_csv(here::here("data","wikifact_resp.csv"))|> janitor::clean_names() |> mutate(item = str_c(scenario,row_number(), sep = "_")) |> pivot_longer(cols = together_t0pp:meta_llama_30b,names_to = "model",values_to = "resp")
(wfdf$item |> n_unique())

bbqdf = read_csv(here::here("data/bbq_resp.csv")) |> janitor::clean_names()|> mutate(item = str_c(scenario,row_number(), sep = "_"))  |> pivot_longer(cols = together_t0pp:ai21_j1_grande_v2_beta,names_to = "model",values_to = "resp")
(bbqdf$item |> n_unique())

airdf = read_csv(here::here("data/airbench_resp.csv")) |> janitor::clean_names()|> mutate(item = str_c(scenario,row_number(), sep = "_"))  |> pivot_longer(cols = databricks_dbrx_instruct:anthropic_claude_3_5_sonnet_20241022,names_to = "model",values_to = "resp")
(airdf$item |> n_unique())

booldf = read_csv(here::here("data/boolq_resp.csv")) |> janitor::clean_names()|> mutate(item = str_c(scenario,row_number(), sep = "_"))  |> pivot_longer(cols = together_t0pp:meta_llama_30b,names_to = "model",values_to = "resp")
(booldf$item |> n_unique())

legdf = read_csv(here::here("data/legalbench_resp.csv")) |> janitor::clean_names()|> mutate(item = str_c(scenario,row_number(), sep = "_"))  |> pivot_longer(cols = allenai_olmo_7b:anthropic_claude_3_5_sonnet_20240620,names_to = "model",values_to = "resp")
(legdf$item |> n_unique())

meddf = read_csv(here::here("data/medqa_resp.csv")) |> janitor::clean_names()|> mutate(item = str_c(scenario,row_number(), sep = "_"))  |> pivot_longer(cols = allenai_olmo_7b:anthropic_claude_3_5_sonnet_20240620,names_to = "model",values_to = "resp")
(meddf$item |> n_unique())

clidf = read_csv(here::here("data/mmluclinical_resp.csv")) |> janitor::clean_names()|> mutate(scenario = "mmluclinic")|> mutate(item = str_c(scenario,row_number(), sep = "_"))  |> pivot_longer(cols = allenai_olmo_7b:anthropic_claude_3_5_sonnet_20241022,names_to = "model",values_to = "resp") 
(clidf$item |> n_unique())

mmeddf = read_csv(here::here("data/mmlumedicine_resp.csv")) |> janitor::clean_names()|> mutate(scenario = "mmlupromed")|> mutate(item = str_c(scenario,row_number(), sep = "_"))  |> pivot_longer(cols = allenai_olmo_7b:anthropic_claude_3_5_sonnet_20241022,names_to = "model",values_to = "resp") 
(mmeddf$item |> n_unique())

mmathdf = read_csv(here::here("data/mmlumath_resp.csv")) |> janitor::clean_names()|> mutate(scenario = "mmlumath")|> mutate(item = str_c(scenario,row_number(), sep = "_"))  |> pivot_longer(cols = allenai_olmo_7b:anthropic_claude_3_5_sonnet_20240620,names_to = "model",values_to = "resp") 
(mmathdf$item |> n_unique())

obdf = read_csv(here::here("data/openbookqa_resp.csv")) |> janitor::clean_names() |> mutate(item = str_c(scenario,row_number(), sep = "_")) |> pivot_longer(cols = allenai_olmo_7b:anthropic_claude_3_5_sonnet_20240620,names_to = "model",values_to = "resp")
(obdf$item |> n_unique())

thaidf = read_csv(here::here("data/thaiexam_resp.csv")) |> janitor::clean_names() |> mutate(item = str_c(scenario,row_number(), sep = "_"))  |> pivot_longer(cols = aisingapore_sea_lion_7b_instruct:anthropic_claude_3_5_sonnet_20240620,names_to = "model",values_to = "resp")
(thaidf$item |> n_unique())


# benchmark aggregation ----------
agg = list(wfdf|> select(model,item,scenario,resp),bbqdf,airdf,booldf,legdf,meddf,clidf,mmeddf,mmathdf,obdf,thaidf) |> reduce(\(x,y) bind_rows(x |> select(model,item,scenario,resp),y|> select(model,item,scenario,resp))) |> rename(benchmark = scenario)

odf = read_csv(here::here("data/official_provider_benchmark.csv")) |> mutate(model=make_clean_names(model_name)) |> select(-model_name) |> pivot_longer(-model, names_to = c("item"),values_to = "resp") |> mutate(scenario = str_split_i(item,pattern = "_q",1) ) |> select(model,item,scenario,resp) |> rename(benchmark = scenario)


## see item counts with no summaries
odf |> filter(!(str_detect(benchmark,"_accuracy|_total_correct"))) |> group_by(benchmark) |> summarize(n=n_unique(item)) |> ungroup()


cdf = odf |> filter(!(str_detect(benchmark,"_accuracy|_total_correct")))

aggdf = agg |> bind_rows(cdf)

rm(wfdf,bbqdf,airdf,booldf,legdf,meddf,clidf,mmeddf,mmathdf,obdf,thaidf)
gc()

# Functions from the package Mokken needed in the function to compute the single-item reliability using Mokken's
# nonparametric approach
# @param X data matrix (persons x items)
# @param P vector with order of the items
# @param N number of persons
# @param J number of items
# @param m number of response categories
compute.PP <- function(X,P,N,J,m=2){
  label <- as.vector(t(outer(paste("P(X",substr(as.character(100+(1:J)),2,3),">=",sep=""),paste(1:(m-1),")",sep=""), paste, sep="")))
  #   substr(as.character(10000+r),2,5)
  PP <- matrix(0,J*(m-1),J*(m-1))
  i <- 0
  j <- 0
  for(i in 1:(J-1)) for(j in (i+1):J)
    PP[((i-1)*(m-1)+1):((i-1)*(m-1)+(m-1)),((j-1)*(m-1)+1):((j-1)*(m-1)+(m-1))] <-
    t(outer(X[,i],0:(m-2),">")) %*% outer(X[,j],0:(m-2),">")/N
  PP <- PP + t(PP) + kronecker(diag(J),matrix(-1,m-1,m-1))
  PP[PP < -.5] <- NA
  dimnames(PP) <- list(label,label)
  PP <- PP[order(P),order(P)]
  return(PP)
}


# Function to estimate the single-item reliability with Pi_ii estimated using Mokken's 
# nonparametric approach

# Function to estimate SIRC using Mokken's nonparametric approach

singlemokken <- function(data) {
  data <- na.omit(data)
  data.0 <- data - min(data)
  J <- ncol(data)
  m <- max(data.0) + 1
  N <- nrow(data)
  
  P <- matrix(t(apply(outer(as.matrix(data.0), 1:(m-1), ">=")*1,c(2,3),mean, na.rm=TRUE)),nrow=(m-1)*J)
  PP <- compute.PP(data.0,P,N,J,m)
  P <- matrix(sort(P))
  off.boundary <- P > 0 & P < 1
  P <- matrix(P[off.boundary,1])
  PP <- PP[off.boundary,off.boundary]
  km <- length(P)
  is.na.PP <- is.na(PP)
  lower.bound.PP <- P %*% t(P)
  upper.bound.PP <- outer(as.numeric(P),as.numeric(P),FUN = "pmin")
  dimnames(P) <- list(dimnames(PP)[[1]],"")
  
  OO <- is.na(PP)
  set.matrix <- outer(as.numeric(P),as.numeric(P),"==")*1
  unique.cells <- which(apply(set.matrix,1,sum)==1)
  Type <- set.matrix
  Type[unique.cells,unique.cells] <- 0
  set.vector <- sign(apply(Type,1,sum))
  Type <- outer(set.vector,set.vector) + outer(rep(1,km),set.vector)*2 + outer(set.vector,rep(1,km))
  
  for (i in 1:km) for (j in 1:km) if(is.na(PP[i,j])){
    if (Type[i,j]==4) PP[i,j] <- mean(PP[set.matrix[i,]==1,set.matrix[,j]==1],na.rm=T)
    else{
      if(Type[i,j]==1){
        RightN <- ifelse(any(!is.na(PP[i,j:km])), j + min(which(!is.na(PP[i,j:km]))) - 1, NA)
        RightPP <- ifelse(is.na(RightN),NA,mean(PP[set.matrix[i,]==1,set.matrix[,RightN]==1],na.rm=T))
        LeftN <- ifelse(any(!is.na(PP[i,1:j])), max(which(!is.na(PP[i,1:j]))),NA)
        LeftPP <- ifelse(is.na(LeftN),NA,mean(PP[set.matrix[i,]==1,set.matrix[,LeftN]==1],na.rm=T))
        LowerN <- UpperN <- i
        LowerPP <- UpperPP <- mean(PP[set.matrix[i,]==1,set.matrix[,j]==1],na.rm=T)
      }
      if(Type[i,j]==2){
        LowerN <- ifelse(any(!is.na(PP[i:km,j])), i + min(which(!is.na(PP[i:km,j]))) - 1, NA)
        LowerPP <- ifelse(is.na(LowerN),NA,mean(PP[set.matrix[LowerN,]==1,set.matrix[,j]==1],na.rm=T))
        UpperN <- ifelse(any(!is.na(PP[1:i,j])), max(which(!is.na(PP[1:i,j]))),NA)
        UpperPP <- ifelse(is.na(UpperN),NA,mean(PP[set.matrix[UpperN,]==1,set.matrix[,j]==1],na.rm=T))
        RightN <- LeftN <- j
        RightPP <- LeftPP <- mean(PP[set.matrix[i,]==1,set.matrix[,j]==1],na.rm=T)
      }
      if(Type[i,j]==0){
        RightN <- ifelse(any(!is.na(PP[i,j:km])), j + min(which(!is.na(PP[i,j:km]))) - 1, NA)
        RightPP <- ifelse(is.na(RightN),NA,mean(PP[set.matrix[i,]==1,set.matrix[,RightN]==1],na.rm=T))
        LeftN <- ifelse(any(!is.na(PP[i,1:j])), max(which(!is.na(PP[i,1:j]))),NA)
        LeftPP <- ifelse(is.na(LeftN),NA,mean(PP[set.matrix[i,]==1,set.matrix[,LeftN]==1],na.rm=T))
        LowerN <- ifelse(any(!is.na(PP[i:km,j])), i + min(which(!is.na(PP[i:km,j]))) - 1, NA)
        LowerPP <- ifelse(is.na(LowerN),NA,mean(PP[set.matrix[LowerN,]==1,set.matrix[,j]==1],na.rm=T))
        UpperN <- ifelse(any(!is.na(PP[1:i,j])), max(which(!is.na(PP[1:i,j]))),NA)
        UpperPP <- ifelse(is.na(UpperN),NA,mean(PP[set.matrix[UpperN,]==1,set.matrix[,j]==1],na.rm=T))
      }
      
      E17a <- ifelse(is.na(LowerN),NA,LowerPP * P[i]/P[LowerN])
      E17b <- ifelse(is.na(RightN),NA,RightPP * P[j]/P[RightN])
      E17c <- ifelse(is.na(UpperN),NA,UpperPP * P[i]/P[UpperN])
      E17d <- ifelse(is.na(LeftN),NA, LeftPP  * P[j]/P[LeftN] )
      E21a <- ifelse(is.na(LowerN),NA,LowerPP  * (1 - P[i])/(1 - P[LowerN]) -
                       P[j] * (P[LowerN] - P[i])/(1 - P[LowerN]))
      E21b <- ifelse(is.na(RightN),NA,RightPP  * (1 - P[j])/(1 - P[RightN]) -
                       P[i] * (P[RightN] - P[j])/(1 - P[RightN]))
      E21c <- ifelse(is.na(UpperN),NA,UpperPP  * (1 - P[i])/(1 - P[UpperN]) +
                       P[j] * (P[i] - P[UpperN])/(1 - P[UpperN]))
      E21d <- ifelse(is.na(LeftN) ,NA,LeftPP   * (1 - P[j])/(1 - P[LeftN] ) +
                       P[i] * (P[j] -  P[LeftN])/(1 - P[LeftN] ))
      
      PP[i,j] <- mean(c(E17a,E17b,E17c,E17d,E21a,E21b,E21c,E21d),na.rm=T)
    }
  }
  PP[is.nan(PP)] <- 0
  PP[PP > upper.bound.PP & OO] <- upper.bound.PP[PP > upper.bound.PP & OO]
  PP[PP < lower.bound.PP & OO] <- lower.bound.PP[PP < lower.bound.PP & OO]
  
  # Sort the data in the original order
  PP <- PP[sort(colnames(PP)),sort(colnames(PP))]
  P <- matrix(P[order(dimnames(P)[[1]])])
  rownames(P) <- dimnames(PP)[[1]]
  
  num <- PP- outer(as.numeric(P),as.numeric(P))
  
  # Determine whether all the categories occur in the data and correct for this.
  # De eerste regel find.cats is een functie die aangeeft welke scores uit een bepaalde range in een bepaalde vector (x) voorkomen
  # De tweede regel past die functie toe op de kolommen van de data. Het resultaat is een list which.cat met J items 
  # De derde regel berekent het aantal ISRFs voor elk item. 
  # De vierde regel geeft aan waar een nieuw item begint in num.
  
  find.cats <- function(x , range = -1000 : 1000) range[range %in% x]
  which.cat <- list()
  for (j in 1:J) {which.cat[[j]] <- find.cats(data[,j])}
  number.ISRFs <- sapply(which.cat, length) - 1
  position <- c(1, cumsum(number.ISRFs)) # c(1, cumsum(number.ISRFs[-1]))
  position <- position[-1]
  
  # Ik denk dat het nu handig is om per item te werken
  # English: I think it is now useful to work per item
  
  item.vars <- apply(data, 2, var)
  singleLCRC <- rep(NA, J)
  for (j in 1 : J) {
    rows <- ((position[j] - (number.ISRFs[j] - 1)) : position[j])
    singleLCRC[j] <- sum(num[rows, rows])/item.vars[j]
  }
  
  return(singleLCRC)
  
}


# Guttmann's lambda 6 reliability estimate for items
# Fit lmer models to estimate variance components separately for bench -------
itemfits = list()
irtfits = list()
modfits = list()
gc()
nb_items_per = 200
nmods = 40
niter = 20
nmodsall = c(40)
irts = c("1PL","2PL","3PL","3PLu","4PL")
nis = c(150)

for(i in 1:niter){
  for(nmods in nmodsall){
    for(nb_items_per in nis){
      print(glue::glue("Iteration of {nmods} models on {nb_items_per}"))
      for(b in aggdf$benchmark |> unique()){
        print(b)
        tic(b)
        tdf = aggdf |> 
          filter(benchmark==b)%>%
          semi_join(aggdf |> filter(benchmark==b) |> 
                      select(item) |> distinct() |> sample_n(nb_items_per,replace = F)) |>
          mutate(resp = as.numeric(resp)) 
        
        itemlist = tdf |> mutate(itemx = item)
        modellist = tdf |> pivot_wider(names_from = item,values_from = resp) |> drop_na() |> select(model) |> distinct() |> slice_sample(n= nmods, replace = F) 
        data_wide = tdf |> select(-any_of("benchmark")) |> filter(model %in% modellist$model) |> pivot_wider(names_from = item, values_from = resp) |> remove_constant()
        
        if(nrow(data_wide) < 3){
          print(glue::glue("Skipping {b} for insufficient data"))
          next
        }
        
        if(ncol(data_wide)-1 < 3){
          print(glue::glue("Skipping {b} for insufficient items"))
          next
        }
        ## for each item, set the data to predict the model response using all other items as covariates
        
        idf = tdf |> filter(model %in% modellist$model) |> left_join(itemlist |> select(-item,-benchmark) |> rename(otheritemscore = resp), by = c("model")) |> # left_join(modellist, by = "model") |> 
          filter(item != itemx)
        
      
        tic("feols fit")
        feols_fit = idf |>  
          feols(fml = resp ~ 1 
                | itemx[[otheritemscore]] 
                + model[[otheritemscore]]
                , 
                data = _,
                na.action = na.exclude,
                split = ~ item,
                cluster = ~ model,
                panel.id = ~ model + itemx,
                nthreads = 8,
                
                
          )
        # print(summary(feols_fit))
        
        reldf = map_df(1:length(feols_fit),
                       \(x) as_tibble_row(fixest::r2(feols_fit[[x]])) |> 
                         mutate(item = feols_fit[[x]]$model_info$sample$value,
                                meanobsr2 = mean((feols_fit[[x]]$scores)^2,na.rm=T),
                                nobs = feols_fit[[x]]$nobs,
                                benchmark = b,
                                n_items = nb_items_per,
                                n_models = nmods) |> 
                         bind_cols(feols_fit[[x]]$coeftable|> 
                                     as_tibble() |> clean_names() |> 
                                     rename(intercept=estimate)),
                       .progress = T) 
          
                       # tibble(item = feols_fit[[x]]$model_info$sample$value, 
                                   # lam6 = feols_fit[[x]]$sq.cor, # lam6 = cor2,r2  1 -var(feols_fit[[x]]$residuals)/var(idf |> filter(item==feols_fit[[x]]$model_info$sample$value) |> pull(resp),na.rm=T), 
                                   # adjr2 = 1-feols_fit[[x]]$sigma2/var(idf |> filter(item==feols_fit[[x]]$model_info$sample$value) |> pull(resp),na.rm=T), lam6sig2 = adjr2, ar2 , difficulty = within r2 (wr2), 
                       #             sqcor = feols_fit[[x]]$sq.cor,
                       #             meanr2 = mean((feols_fit[[x]]$scores)^2,na.rm=T),
                       #             se = feols_fit[[x]]$se,
                       #             n = feols_fit[[x]]$nobs
                       #             ),
                       # .progress = T) |> mutate(benchmark = b,
                       #                          n_items = nb_items_per,
                       #                         n_models = nmods
                       #                         )
        tic("MS")
        reldf = tryCatch({reldf |> 
          left_join(
            tibble(MS = replace_inf(singlemokken(as.matrix(data_wide |> select(-model)|> mutate(across(everything(),as.numeric))))),
                   item = colnames(data_wide |> select(-model))
            ),
            by = "item"
          )},
          error = function(e){
            reldf |> 
              left_join(
                tibble(MS = NA,
                       item = colnames(data_wide |> select(-model))
                ),
                by = "item"
              )
          }
        )
        toc()
        tic("Hi")
        hi = mokken::coefH(data_wide |> select(-model)|> mutate(across(everything(),as.numeric)),se=F,results=F)
        zi = mokken::coefZ(data_wide |> select(-model)|> mutate(across(everything(),as.numeric)))
        
        reldf = reldf |> 
          left_join(
            bind_cols(hi$Hi |> as_tibble(rownames="item")  |> rename(Hi=value),
                      zi$Zi |> as_tibble() |> rename(Zi=value)
            ),
            by = "item"
          ) |> mutate(h = hi$H,
                     z = zi$Z)
        toc()
        
        tic("item rel")
        reldf = reldf |> 
          left_join(
              performance::item_reliability(data_wide |> select(-model)|> mutate(across(everything(),as.numeric))) |> 
                as_tibble() |>
                janitor::clean_names(),
            by = "item"
          ) 
        toc()
        

        
        
        
        
        
        
        print(reldf)
        
        
        relfits[[length(relfits)+1]] = reldf
        toc()    
        
        saveRDS(relfits |> list_rbind(names_to="iter"), here::here("data",glue::glue("item_rel_boot_temp_{today()}_{i}.rds")))
        
        toc()
        gc()
      }
    }
  }
}
  
  
rdf = list_rbind(relfits,names_to="iter") |> group_by(iter) |> mutate(n_items = n_distinct(item),nvali = n_distinct(item,na.rm=T)-sum(is.na(cor2))) |> ungroup()|> mutate(nvalm = round(nobs/n_items),n_models = if_else(is.na(n_models),nvalm,n_models), valid_item = !is.na(cor2)) |> select(iter,benchmark,item,nobs,nvali,nvalm,cor2:wapr2,intercept:discrimination,everything()) |> group_by(item) |> mutate(countvalid = sum(valid_item)) |> ungroup() |> rename(l6 = cor2, d = intercept, a = discrimination,itcor = item_total_correlation, obsr2 = meanobsr2, del_alph = alpha_if_deleted) |> replace_inf()
rdf |> saveRDS(here::here("data",glue::glue("item_reldf_{today()}.rds")))

rdf = readRDS(here::here("data",glue::glue("item_reldf_{today()}.rds"))) |> bind_rows(readRDS(here::here("data",glue::glue("item_reldf_2025-12-29.rds")))) 

# ldf = readRDS(here::here("data",glue::glue("data/item_model_iter_reliab_2025-12-28.rds"))) |> bind_rows(
#   readRDS(here::here("data",glue::glue("item_lam6_reliab_mixed.rds")))
# )

# lam6df = readRDS(here::here("data",glue::glue("item_lam6_reliab_mixed.rds")))

# skimr::skim(rdf)


ldf = readRDS(here::here("data",glue::glue("item_model_iter_reliab_2025-12-28.rds"))) |> bind_rows(
  readRDS(here::here("data",glue::glue("item_lam6_reliab_mixed.rds")))
) |> mutate(Hi = if_else(is.na(Hi),value,Hi)) |> select(-value) |> group_by(iter) |> mutate(n_items = n_distinct(item),nvali = n_distinct(item,na.rm=T)-sum(is.na(lam6))) |> ungroup()|> mutate(nvalm = round(n/n_items),n_models = if_else(is.na(n_models),nvalm,n_models), valid_item = !is.na(lam6)) |> select(iter,benchmark,item,n,nvali,nvalm,lam6,meanr2,MS,Hi,item_total_correlation:difficulty,everything()) |> group_by(item) |> mutate(countvalid = sum(valid_item)) |> ungroup() 


## pct of items appearing more than once
rdf |> filter(valid_item) |> group_by(benchmark,item) |> summarise(n= n()) |> ungroup() |> tabyl(n,benchmark) |> as_tibble() |> summarise(across(air_bench_2024:wikifact,\(x) sum(if_else(n==1,0,x))/sum(x)))
## pct of items appearing more than twice
rdf |>filter(valid_item) |> group_by(benchmark,item) |> summarise(n= n()) |> ungroup() |> tabyl(n,benchmark) |> as_tibble() |> summarise(across(air_bench_2024:wikifact,\(x) sum(if_else(n<3,0,x))/sum(x)))


ni = 150

itemlist = rdf |> 
  filter(valid_item,countvalid>5) |>
  group_by(item) |>
  filter( n_distinct(nvali) > 5) |>
  ungroup() |>
  group_by(benchmark) |>
  slice_sample(n=ni, replace = F) |>
  ungroup() |>
  select(item) |> distinct()

rdf |> semi_join(itemlist, by = "item") |> 
  select(benchmark, item, nvali, l6,ar2,war2,d,MS,Hi,Zi,del_alph,itcor,a,obsr2) |> pivot_longer(cols = l6:obsr2, names_to = "metric", values_to = "est") |>
  ggplot(aes(x = nvali, y = est, group = item)) + 
  # geom_line(alpha=0.2,linewidth = 0.2) + 
  geom_smooth(method = lm, se = F, alpha=0.2,linewidth = 0.2) + 
  theme_minimal() + 
  facet_grid(metric~benchmark, scales="free") +
  labs(title = "Item Reliability Estimates by Number of Items",
       # subtitle = "Using Bootstrap Sampling of Models and Items",
       x = "Number of Items in Sample",
       y = "Item Reliability Estimate")





tic("musr brms fit")
musrmod = rdf |> 
  filter(valid_item,countvalid>5,benchmark=="musr") |> 
  mutate(across(c(l6,ar2,war2,MS,Hi,del_alph,itcor,a,obsr2,d), ~ abs(.)),
         iter = as_factor(iter)) |>
  brm(formula = mvbind(l6,ar2,MS,Hi,del_alph,itcor,obsr2,d) ~ 1 
      + (nvali||item) 
      + (1|iter)
      ,
      data = _,
      cores = 3,
      chains = 3,
      threads = threading(2),
      backend = "cmdstanr",
      save_pars = save_pars(all=TRUE),
      stan_model_args = list(stanc_options = list("O1")),
      iter = 1500, warmup = 750, 
      thin = 5,
      # control = list(adapt_delta = 0.95, max_treedepth = 15),
      file = here::here("data","results","brms_item_reliab_musr_boot_{ni}.rds")
  )

summary(musrmod)
toc()


