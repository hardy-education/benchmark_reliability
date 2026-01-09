## Item Reliability Bootstraps
## 

# Load Libraries ------

library(tictoc)
library(here)
library(easystats)
library(performance)
library(parameters)
library(modelsummary)
library(modelbased)
library(report)
library(effectsize)
library(insight)
library(see)
library(parallel)
library(data.table)
library(glue)
library(psych)
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
library(ggmirt)
library(future)
library(parallel)
library(carrier)
library(fastverse)
fastverse_extend(Rfast,dtplyr, 
                 coop,Rfast2,cheapr,
                 stringi,stringdist,
                 vctrs, dqrng,
                 parallelDist
)

future::plan("multisession")

# Load Data ------

## download this dataset and put in the data folder: https://drive.google.com/file/d/1SL7MNKiynqWhtn5pCN59xp4XpZpK1Qln/view?usp=sharing
aggdf = fread(here("benchmark_reliability", "data","aggdf.csv")) |> 
  pivot_longer(cols = -c(benchmark,item),names_to = "model",values_to = "resp") 

### Summary Counts ------
item_counts = aggdf |> 
  select(benchmark,item) |> distinct() |> group_by(benchmark) |> summarize(n=n())
mod_counts = aggdf |> 
  select(benchmark,model) |> distinct() |> group_by(benchmark) |> summarize(n=n())

# Functions -------
## Utility Functions -------


make_hilo_splits = function(df){
  df |>group_by(model,benchmark) |> 
    summarise(resp = mean(resp, na.rm=T)) |> ungroup() |>  
    group_by(benchmark) |> 
    mutate(med = median(resp,na.rm=T),
           split = if_else(resp<=med,"train","test")) |> 
    ungroup() |> drop_na() |> distinct()
}
  

make_sample_response = function(sampdf,longrespdf,nitems = 100, 
                                samp="train",
                                items = NULL,
                                b = "air_bench_2024") {
  sampdf <- sampdf |> 
    filter(benchmark==b, split==samp) |> 
    select(model,benchmark) |> distinct()
  resplongdf = semi_join(longrespdf, 
                       sampdf,
                       by = join_by(benchmark, model)) |> 
    select(-benchmark) 
  if(is.null(items)&!(samp=="test")){
    items = resplongdf |>
      distinct(item) |>
      slice_sample(n=nitems) |> pull(item)
  }
  resplongdf |> filter(item %in% items) |> long2resp() |> 
    drop_na() |> remove_constant()
}

make_feols_df = function(wide){
  wide |> pivot_longer(-model,names_to = "item",values_to = "resp") |> 
    left_join(wide |> pivot_longer(-model,names_to = "itemx",values_to = "otheritemscore") , 
              by = c("model"),relationship = "many-to-many") |> 
    filter(item != itemx) 
}




long2resp <- function(df, removecols = c("benchmark","split","med","iter")) {
  # Check if the input is a data frame
  if(is.numeric(df$item[1])){
    df |> select(-any_of(removecols)) |> 
      pivot_wider(names_from = item, values_from = resp,names_prefix = "i") |> 
      janitor::remove_constant()
  }
  else{
    df |> select(-any_of(removecols)) |> 
      pivot_wider(names_from = item, values_from = resp) |> 
      janitor::remove_constant()
  }
}
checkmtx = function(df){
  # Check if the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  if((dim(df)[2]==3)&("item" %in% names(df))){
    df = long2resp(df)
  } 
  df
}


create_cortab <- function(corlong) {
  tic()
  res = corlong |>
    group_by(item) |>
    summarize(#p1 = sum(p==1)/n(),
      # psig = sum(p<0.05)/n(),
      # nonpsig = sum(p>=0.05)/n(),
      cneg = sum(rho<0)/n(),
      clow = sum(rho<0.1)/n(),
      c10 = quantile(rho,0.10,na.rm=T),
      c5 = quantile(rho,0.05,na.rm=T),
      c1 = quantile(rho,0.01,na.rm=T),
      c25 = quantile(rho,0.25,na.rm=T),
      c75 = quantile(rho,0.75,na.rm=T),
      c90 = quantile(rho,0.90,na.rm=T),
      c95 = quantile(rho,0.95,na.rm=T),
      c99 = quantile(rho,0.99,na.rm=T),
      cmax = max(rho,na.rm=T),
      cmin = min(rho,na.rm=T),
      cmean = mean(rho,na.rm=T),
      cvar = var(rho,na.rm=T),
      cse = sd(rho,na.rm=T)/sqrt(n()),
      cmedse = mad(rho,na.rm=T)/sqrt(n()),
      cmed = median(rho,na.rm=T)
      
    )
  toc()
  res
}

combine_mtx = function(m1,m2){
  if(is.numeric(m1$item)){m1$item = str_c("i",m1$item)}
  if(is.numeric(m2$item)){m2$item = str_c("i",m2$item)}
  inner_join(m1,m2,by="item")
}

### Plotting Functions -------


testInfoCompare <- function(modlist, # model1, model2, 
                            theta_range = c(-4,4), 
                            title = "Parallel Tests",
                            subtitle = "Test Information Curves") {
  
  theta_range = seq(theta_range[1], theta_range[2], by = .01)
  
  Theta <- matrix(theta_range)
  
  modinfo <- map_dfc(namedList(modlist), function(x) testinfo(x, Theta)) |> 
    mutate(Theta = theta_range)
  # information1 <- testinfo(model1, Theta)
  # information2 <- testinfo(model2, Theta)
  
  
  p <- modinfo %>%
    gather(key, value, -Theta) %>%
    ggplot() +
    geom_line(aes(x = Theta, y = value, color = key), alpha = .75) +
    labs(x = expression(theta), y = expression(I(theta)),
         title = title, subtitle = subtitle) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    theme(legend.title=element_blank())
  
  return(p)
  
}

# itemfitPlot <- function(model,
#                         fit_stats = "infit",
#                         color = "red",
#                         shape = 17,
#                         breaks = c(.5, 1, 1.5), 
#                         limits = c(0, 2),
#                         title = glue("Item Fit Statistics: {fit_stats}"),
#                         ...) {
#   
#   
#   fit <- mirt::itemfit(model, fit_stats = fit_stats, ...)
#   
#   ranges = list(
#     "infit" = list(plot_lower_limit = 0,
#                    plot_upper_limit = ifelse("infit" %in% names(fit),max(fit |> pull(any_of("infit","outfit"))),2),
#                    good_fit = c(.7, 1.3),
#                    borderline = c(1.3,1.5),
#                    poor_fit = c(1.5, Inf)),
#     "outfit" = list(plot_lower_limit =
#                       hline_lower = .5, hline_upper = 1.5),
#     "outfit",
#     "PV_Q1",
#     "X2",
#     "S_X2",
#     "Zh",
#     "X2*",
#     "X2*_df",
#     "PV_Q1*",
#     "G2"
#   )
#   
#   
#   
#   fit %>%
#     select(item, any_of(c("infit","outfit","PV_Q1","X2","S_X2","Zh","X2*","X2*_df","PV_Q1*","G2"))) %>%
#     gather(key, value, -item) %>%
#     ggplot(aes(x = item, y = value)) +
#     geom_point(size = 3, color = color, shape = shape) +
#     geom_line() +
#     geom_hline(yintercept = .5, color = "darkgrey", linetype = "dashed") +
#     geom_hline(yintercept = 1, color = "darkgrey") +
#     geom_hline(yintercept = 1.5, color = "darkgrey", linetype = "dashed") +
#     scale_y_continuous(breaks = breaks, limits = limits) +
#     facet_grid(~key) +
#     coord_flip() +
#     theme_minimal() +
#     labs(y = "", x = "", caption = "Note: Items with values within 0.5 and 1.5 are considered to be productive for measurement.",
#          title = title)
#   
# }


## Metric Functions -------

get_omega <- function(df,replaceval =0,
                      nfactors = 3,
                      cut=0.3,
                      fm = "minres",
                      rotate = "oblimin",
                      poly=T,...
) {
  
  # Calculate tetrachoric correlation
  df = checkmtx(df)
  tic(stringr::str_c("omega_",nfactors))
  om = psych::omegah(df |> select(-any_of(c("model","id"))) |> naniar::replace_na_with(replaceval), 
                     nfactors = nfactors,flip = F,
                     poly = poly,
                     plot=F,
                     digits =8,
                     fm = fm,use = "pairwise.complete.obs",
                     rotate = rotate,...
  ) 
  toc()
  om
}

get_omega_loadings <- function(om, nf
) {
  sl = om$schmid$sl |> 
    as_tibble(rownames="item") |> clean_names() 
  names(sl) = str_c(names(sl),"_",nf)
  sl |> rename("item" = str_c("item_",nf))
}

get_omega_corlong <- function(om
) {
  
  om$R  |> as_tibble(rownames = "idx")  |> pivot_longer(cols = -idx, names_to = "item",values_to = "rho") 
}

get_all_omegadf <- function(df,nf=5,...) {
  # Check if the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  df = checkmtx(df)
  om = get_omega(df,nfactors = nf,...)
  # Calculate tetrachoric correlation
  loads = get_omega_loadings(om,nf=nf)
  cortab = create_cortab(get_omega_corlong(om))
  
  # Combine the results into a single data frame
  combine_mtx(cortab,loads) 
}


get_pc_loading <- function(df,replaceval =0,
                           nfactors = 3,
                           cut=0.3,
                           fm = "minres",
                           rotate = "oblimin",
                           poly=T,...
) {
  
  # Calculate tetrachoric correlation
  df = checkmtx(df)
  tic()
  pa = parameters::principal_components(df |> select(-any_of(c("model","id"))), 
                                        n = nfactors,
                                        use = "pairwise.complete.obs",
                                        rotation = rotate,...
  ) 
  toc()
  pa |> tibble() |> clean_names() |>  rename(item=variable) |> 
    pivot_longer(cols = -item,names_to = "metric",values_to = "val") |> 
    mutate(metric = str_c(metric,"_",nfactors)) |> 
    pivot_wider(names_from = metric,values_from = val)
}


get_alpha <- function(df,replaceval =0) {
  # Check if the input is a data frame
  df = checkmtx(df)
  tic()
  alpha = psych::alpha(df |> select(-any_of(c("model","id"))) |> 
                         naniar::replace_na_with(replaceval), 
                       check.keys = F,
                       warnings = F,
                       use = "pairwise.complete.obs")
  toc()
  inner_join(alpha$item.stats |> as_tibble(rownames = "item"), 
             alpha$alpha.drop |> as_tibble(rownames = "item"), by = "item") |> 
    clean_names() |> 
    rename(r_rest = r_cor,
           alpha_rest = raw_alpha,
           alpha_rest_std = std_alpha,
           g6_rest = g6_smc,
           snr_alpha = s_n,
           valid_n = n)
}

# Modified Functions from the Mokken package for single-item reliability using Mokken's
# nonparametric approach: compute the Positive Positive matrix
# @param X data matrix (persons x items)
# @param P vector with order of the items
# @param N number of persons
# @param J number of items
# @param m number of response categories
compute.PP <- function(X,P,N,J,m=2){
  label <- as.vector(t(outer(paste("P(X",substr(as.character(100+(1:J)),2,3),">=",sep=""),paste(1:(m-1),")",sep=""), paste, sep="")))
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
# Function to estimate MS using Mokken's nonparametric approach

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
  # The first line 'find.cats' is a function that indicates which scores from a certain range occur in a given vector (x).
  # The second line applies that function to the columns of the data. The result is a list 'which.cat' with J items.
  # The third line calculates the number of ISRFs for each item.
  # The fourth line indicates where a new item starts in 'num'.
  
  find.cats <- function(x , range = -1000 : 1000) range[range %in% x]
  which.cat <- list()
  for (j in 1:J) {which.cat[[j]] <- find.cats(data[,j])}
  number.ISRFs <- sapply(which.cat, length) - 1
  position <- c(1, cumsum(number.ISRFs)) # c(1, cumsum(number.ISRFs[-1]))
  position <- position[-1]
  
  # per item
  
  item.vars <- apply(data, 2, var)
  singlemok <- rep(NA, J)
  for (j in 1 : J) {
    rows <- ((position[j] - (number.ISRFs[j] - 1)) : position[j])
    singlemok[j] <- sum(num[rows, rows])/item.vars[j]
  }
  
  return(singlemok)
  
}

make_gstudy_df = function(df,verbose = F){  
    mod = df |> 
      lmer(formula = resp ~ 1 
           + (1|item)
           + (1|model)
           ,
           na.action = na.exclude,
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 1e5),
                                 calc.derivs = F)
      ) 
  
  if(verbose){ 
    print(summary(mod))
  }
  
  modvars = mod |> VarCorr() |>
    janitor::clean_names() |>
    as_tibble() |>
    select(-var2, -var1, -sdcor) |>
    mutate(benchmark = b) |> 
      pivot_wider(names_from = grp, 
                  values_from = vcov) |> 
      mutate(m_rel = model / ( model + Residual),
             i_rel = item / (item + Residual),
             m_dep = model / (item + model + Residual) ,
             i_dep = item / (item + model + Residual) ,
             m_snr = m_rel / (1 - m_rel),
             i_snr = i_rel / (1 - i_rel),
             md_snr = m_dep / (1 - m_dep),
             id_snr = i_dep / (1 - i_dep)
      )
    return(modvars)
  
  
}


feols_estimations = function(idf,get_coef_stats = F){
   tic("feols fit")
  
  suppressMessages(
    {
      feols_fit = idf |>  
        feols(fml = resp ~ 1 
              | itemx[[otheritemscore]] 
              + model[[otheritemscore]]
              , 
              data = _,
              na.action = na.exclude,
              split = ~ item,
              cluster = ~ model,
              warn=F,verbose =0,notes=F,
              panel.id = ~ model + itemx,
              nthreads = 8,
              
              
        )
    }
  )
  fdf = map_df(1:length(feols_fit),
         \(x) as_tibble_row(fixest::r2(feols_fit[[x]])) |> 
           mutate(item = feols_fit[[x]]$model_info$sample$value,
                  meanscores = mean((feols_fit[[x]]$scores),na.rm=T),
                  meanobsr2 = mean((feols_fit[[x]]$scores)^2,na.rm=T),
                  nobs = feols_fit[[x]]$nobs)  |> 
           bind_cols(feols_fit[[x]]$coeftable|> 
                       as_tibble() |> clean_names() |>
                       rename(intercept=estimate)),
         .progress = T) 
  # get_coef_stats not implemented
    if(F){
      fdf = fdf |>  bind_cols(
                     model_performance(feols_fit) |> 
                       bind_rows() |> clean_names() |> 
                       select(aic,bic,rmse,sigma))
    } 
  fdf
  
}


create_sample_metadata = function(df,b = NULL,iter = 1,
                                  col_list = list(
                                    iter = iter,
                                    nmods = n_unique(df$model),
                                    ni = ncol(df)-1),
                                  split = "train"
                                  ){

  if(is.null(b)){
    b = names(df)[3] |> str_remove("_q?[0-9]+$")
  }
  
  col_list$benchmark = b
  
  sampmeta = df |> 
    pivot_longer(-c(model),names_to = "item",values_to = "resp") |>
    group_by(item) |> 
    summarize(n_iobs = n(),
              i_mean = mean(resp,na.rm=T),
              i_sd = sd(resp,na.rm=T),
              i_var = var(resp,na.rm=T)
    ) |> 
    ungroup() |> 
    mutate(#benchmark = b,
           !!!col_list
           # n_items = n_unique(item),
           # n_models = n_unique(df$model),
           # n_obs = n(),
           # iter = col_list$iter,
           # ni = col_list$ni,
           # nmods = col_list$nmods
    ) 
  return(sampmeta)
}

MS_est = function(df){
  df = checkmtx(df)
  singlemokken(df |> select(-any_of(c("model","id"))))
  
  msdf = tryCatch({
        tibble(MS = replace_inf(singlemokken(as.matrix(df |> select(-any_of(c("model")))))),
               item = colnames(df |> select(-any_of(c("model"))))
      )},
      error = function(e){
            tibble(MS = NA,
                   item = colnames(df |> select(-any_of(c("model"))))
          )
      }
  )
  return(msdf)
}


HiZi_est = function(df){
  hi = mokken::coefH(df |> select(-any_of(c("model")))|> 
                       mutate(across(everything(),as.numeric)),se=F,results=F)
  zi = mokken::coefZ(df |> select(-any_of(c("model")))|> 
                       mutate(across(everything(),as.numeric)))
  
  reldf = bind_cols(hi$Hi |> as_tibble(rownames="item")  |> rename(Hi=value),
                zi$Zi |> as_tibble() |> rename(Zi=value)
      ) |> mutate(h = hi$H,z = zi$Z)
  return(reldf)
}


mirt_test_preds = function(rfit,newdat){
  mods = rfit$moddf$model |> unique()
  items = rfit$itdf$item |> unique()
  bench = rfit$irtdf$benchmark[1]
  mods = newdat$model |> unique()
  newthetas = list()
  newitvals = list()
  for(irmod in names(rfit$irt_models)){
    if(is.na(rfit$irt_models[[irmod]]@Fit$AIC)){next}
    sv <- mod2values(rfit$irt_models[[irmod]]) # translate into parameter estimation structure
    mod_new <- mirt(newdat |> select(-model), pars=sv, TOL=NaN) ## create new dummy mod that contains the new responses
    
    preds = fscores(mod_new, response.pattern  = newdat |> select(-model),
                    method = "EAP",
                    full.scores.SE = T,
                    theta_lim = theta_range,
                    max_theta=20, QMC=T)
    newthetas[[irmod]] = personfit(mod_new,Theta = as.matrix(preds[,1]),
                                   method = "EAP",
                                   max_theta=20, QMC=T) |> 
      tibble() |> mutate(split = T,
                         model = newdat$model,
                         irt = irmod,
                         predicted = preds[,1],
                         se = preds[,2],
                         ni_used = n_unique(items),
                         val_test_set = T,
                         rxx = empirical_rxx(preds),
                         n_test_models = n_unique(mods)
      )
    newitvals[[irmod]] = itemfit(mod_new,fit_stats = c("infit","X2","G2"),
                                 Theta = as.matrix(preds[,1]),
                                 # method = "EAP",
                                 max_theta=20, QMC=T) |> 
      tibble() |> 
      select(item,any_of(c("infit","outfit",
                           "PV_Q1","X2",
                           "z.infit","z.outfit",
                           "S_X2","Zh","X2*","X2*_df",
                           "PV_Q1*","G2",starts_with("RMSEA")))) |>
      mutate(split = T,
             n_test_models = n_unique(mods))
  }
  return(list(newth = list_rbind(newthetas),
              newit = list_rbind(newitvals)))
  
}



mirt_fits = function(data_wide,
                     dat_wide_test = NULL,
                     theta_range = c(-9,9),
                     irts = c("2PL"),
                     val_test_set = F,
                     samp = "train",
                     # b = "air_bench_2024",
                     mirtlist = NULL,
                     sampmeta = NULL
                     
){
  
  if(is.null(sampmeta)){
    sampmeta = create_sample_metadata(data_wide)
  }
  b = sampmeta$benchmark[1]
  ni = sampmeta$n_items[1]
  i = sampmeta$iter[1]
  nmods = sampmeta$n_models[1]
  
  
  tic("mirt irt")
  irtdf = list()
  itdfs = list()
  thtdf = list()
  moddfs = list()
  tempirtfits = list()
  
  
  
  if (!is.null(dev.list())) {
    dev.off()
  }
  
  mirtCluster(spec = 5)
  on.exit(mirtCluster(remove = T))
  for(irt in irts){
    print(irt)
    tic(glue("mirt fit {irt}"))
    
    irtmod = tryCatch({
      md = mirt::mirt(data_wide |> select(-model)|> 
                        mutate(across(everything(),as.numeric)), 
                      model = 1, 
                      itemtype = irt, 
                      accelerate = "squarem",
                      technical = list(NCYCLES=2000,
                                       theta_lim = theta_range,
                                       MAXQUAD=100000)
      )
      
      # Person parameter distribution
      p1 <- personDist(md, theta_range = theta_range) +
        labs(title = glue("{b} Item {ni} Person {nmods} Map: {irt}"))
      
      # Item difficulty distribution
      p2 <- itemDist(md, theta_range = theta_range, 
                     shape = 17, color = "red") +
        labs(title=glue("{b} item {ni} diff: {irt}")) 
      
      # Change item labelling if no. items > 10
      if(ni > 10) {
        p2 <- p2 +
          geom_text(aes(label = items), nudge_x = .75, color = "darkgrey", size = 2, check_overlap = T) +
          theme(axis.text.y = element_blank(),
                panel.grid.major.y = element_blank())
      }
      
      # Scale Characteristic Curve
      # p3 <- scaleCharPlot(md, theta_range = theta_range)
      
      # Test information curve
      p4 <- testInfoPlot(md, theta_range = theta_range) +
        labs(title=glue("{b} test i{ni} m{nmods} info: {irt}"))
      
      # Conditional reliability curve
      p5 <- conRelPlot(md, title=glue("{b} rxx: {irt}"),
                       theta_range = theta_range)
      
      # Bind together
      p <- ggpubr::ggarrange(ggpubr::ggarrange(p1, p2, 
                                               ncol = 1, 
                                               align = "hv", 
                                               heights = c(1,2)), 
                             ggpubr::ggarrange(p4, 
                                               # p3, 
                                               p5, 
                                               ncol = 1, 
                                               align = "hv", 
                                               heights = c( 1.5, 1.5)), 
                             ncol = 2)
      
      print(p)
      
      
      preds = fscores(md,
                      method = "EAP",
                      full.scores.SE = T,
                      # quadpts = 241,
                      theta_lim = theta_range,
                      max_theta=20, 
                      QMC=T)
      
      mo_vals = personfit(md,Theta = as.matrix(preds[,1]),
                          method = "EAP",
                          max_theta=20, QMC=T)  |> 
        tibble() |> mutate(model = data_wide$model,
                           th = p1@data$value,
                           predicted = preds[,1],
                           se = preds[,2],
                           irt = irt,benchmark = b,
                           n_items = ni,iter = i,
                           ni_used = ncol(data_wide)-1,
                           val_test_set = val_test_set,
                           n_models = nmods,
                           split = F,
                           rxx = empirical_rxx(preds)
        )
      th_vals = p5@data |> tibble() |> mutate(irt = irt,benchmark = b,
                                              n_items = ni,iter = i,
                                              ni_used = ncol(data_wide)-1,
                                              val_test_set = val_test_set,
                                              n_models = nmods)
      it_vals = p2@data |> 
        tibble() |> 
        mutate(irt = irt,benchmark = b,
               n_items = ni,iter = i,
               ni_used = ncol(data_wide)-1,
               val_test_set = val_test_set,
               n_models = nmods) |> 
        rename(item=items,a=items.a,b =items.b,
               g = items.g,u = items.u) |> 
        left_join(
          itemfit(md,
                  fit_stats = c("infit","X2","G2","Zh"),
                  method = "EAP",Theta = as.matrix(preds[,1]),
                  QMC=T) |> 
            tibble()  %>%
            select(item, any_of(c("infit","outfit",
                                  "PV_Q1","X2",
                                  "z.infit","z.outfit",
                                  "S_X2","Zh","X2*","X2*_df",
                                  "PV_Q1*","G2",starts_with("RMSEA")))),
          by = "item"
        )
      if(val_test_set){
        mirt_test_preds = mirt_test_preds(rfit = list(irt_models = list(!!irt := md),
                                                        irtdf = th_vals,
                                                        itdf = it_vals,
                                                        moddf = mo_vals),
                                        newdat = dat_wide_test)
        th_vals = bind_rows(th_vals, mirt_test_preds$newth)
        it_vals = bind_rows(it_vals, mirt_test_preds$newit)
        
      } 
      
      
      list(
        irtmod = md,
        th_vals=th_vals,
        it_vals=it_vals,
        mo_vals=mo_vals)
    },
    error = function(e){
      th_vals = tibble(value = NA, irt = irt,benchmark = b,
                       n_items = ni,iter = i,
                       ni_used = ncol(data_wide)-1,
                       val_test_set = val_test_set,split = F,
                       n_models = nmods)
      it_vals = tibble(item=colnames(data_wide |> select(-model)),
                       a=NA,b =NA,
                       g =NA,u =NA, irt = irt,benchmark = b,iter = i,
                       val_test_set = val_test_set,
                       ni_used = ncol(data_wide)-1,
                       n_items = ni,
                       n_models = nmods
      )
      mo_vals = tibble(model = data_wide$model,
                       th = NA,
                       irt = irt,benchmark = b,
                       n_items = ni,iter = i,
                       ni_used = ncol(data_wide)-1,
                       val_test_set = val_test_set,
                       n_models = nmods
      )
      return(list(
        irtmod = NULL,
        th_vals=th_vals,
        it_vals=it_vals,
        mo_vals=mo_vals))
      
    })
    
    toc()
    thtdf[[irt]] = irtmod$th_vals
    itdfs[[irt]] = irtmod$it_vals
    moddfs[[irt]] = irtmod$mo_vals
    tempirtfits[[irt]] = irtmod$irtmod
    
  }
  tempirtfits = tempirtfits[!sapply(tempirtfits,is.null)]
  
  irtdf = list()
  for(ii in 1:(length(tempirtfits)-1)){
    for(jj in (ii+1):length(tempirtfits)){
      if(ii != jj){
        irtdf[[length(irtdf)+1]] = tryCatch({
          mirt::anova(tempirtfits[[ii]],tempirtfits[[jj]]) |> 
            tibble() |> mutate(benchmark = b,
                               irt = c(names(tempirtfits)[ii],
                                       names(tempirtfits)[jj]),
                               irt1 = names(tempirtfits)[ii],
                               irt2 = names(tempirtfits)[jj],
                               n_items = ni,
                               iter = i,
                               n_models = nmods)
          
        },
        error = function(e){
          print(glue::glue("Cannot compare {names(tempirtfits)[ii]} and {names(tempirtfits)[jj]}"))
          tibble()
        })
      }
      
    }}
  if(length(irtdf)>0){
    irtdf = irtdf |> bind_rows()
  } else {
    irtdf = tibble()
  }

}




get_bonus_metrics = function(df){
  df |> select(-any_of(c("model"))) |> item_similarity_enhanced()
}


make_gstudy_df = function(benchdf){
  tic("G-D study fits")
  for(b in aggdf$benchmark |> unique()){
    print(b)
    rankrefits[[b]] <- future_map(unique(filter(benchdf,benchmark==b)$metric),
                                  \(m) make_dstudy_df(df = benchdf |>filter(benchmark==b) |>
                                                        distinct() ,verbose = F) |>
                                    mutate(metric = m),
                                  .progress = T,
                                  .options = furrr::furrr_options(seed = TRUE))
    rankrefits[[b]] = list_rbind(rankrefits[[b]])
  }
  toc()
  list_rbind(rankrefits,names_to="bench_metric") 
}




# Bootstrap Looping ----
relfits = list()

n_boots = 100
ni_vals = c(100)

benchmarks = aggdf$benchmark |> unique()

hilo_splits = make_hilo_splits(aggdf)



for(i in 1:n_boots){
  for(ni in ni_vals){
    for(b in benchmarks){
      for(ordered_models in c(T)){
        
        print(glue::glue("Bootstrap {i} - Items {ni} - Benchmark {b}"))
        tic("Total Iteration Time")
        
        # Load Data
        data_wide = make_sample_response(
          hilo_splits,
          aggdf,
          b = b,
          samp = "train",
        ) |> as.data.frame()
        
        reldf = create_sample_metadata(data_wide,iter = length(relfits)+1) |> 
          mutate(boot = i)
        
        # # IRT Modeling
        # mirt_results = mirt_fits(
        #   data_wide = data_wide,
        #   theta_range = c(-9,9),
        #   irts = c("2PL"),
        #   val_test_set = T,
        #   samp = "boot",
        #   sampmeta = sampmeta
        # )

        
        reldf = reldf |> left_join(HiZi_est(data_wide),by="item")
        
        tic("alpha")
        reldf = suppressMessages(
          tryCatch({
            reldf |> left_join(get_alpha(data_wide),by="item")
          },
          error = function(e){
            reldf
          })
        )
        toc()
        # reldf = reldf |> left_join(get_all_omegadf(data_wide,nf=3),by="item")
        
        # reldf = reldf |> left_join(get_pc_loading(data_wide,nfactors=3),by="item")
        
        reldf = reldf |> left_join(feols_estimations(make_feols_df(data_wide)),by="item")
        
        reldf = reldf |> left_join(create_cortab(cor(data_wide |> 
                                                       select(-model)) |> 
                                                   as_tibble(rownames="item") |> 
                                                   pivot_longer(-item, 
                                                                names_to = "itemx",
                                                                values_to = "rho")),
                                   by="item")
        
        reldf = reldf |> left_join(MS_est(data_wide),by="item")
        
        if(exists("item_similarity_enhanced")){
        reldf = reldf |> left_join(get_bonus_metrics(data_wide |> select(-model)),by="item")
        }
        
relfits[[length(relfits)+1]] = list(
  
  reldf = reldf
  # irtdf = irtdf,
  # thtdf = thtdf |> bind_rows(),
  # irt_models = tempirtfits,
  # itdf = itdfs |> bind_rows(),
  # moddf = moddfs |> bind_rows()
)
toc()    


  saveRDS(relfits , here::here("benchmark_reliability", "data","results","boots",glue::glue("itemfit_rel_temp_boot_{today()}.rds"))) 

toc()
gc()


}
}
}
}




irdf = map(relfits,
           \(x) x$reldf, # |> 
             # full_join(
             #   x$itdf |> 
             #     filter(irt%in% c("2PL")) |> #,"3PL")) |>
             #     select(item,irt, any_of(c("a","b",# "g",
             #                               "infit","outfit",
             #                               "z.infit","z.outfit",
             #                               "X2","split","val_test_set",
             #                               "Zh","G2"
             #     )),
             #     ) |> distinct(),
             #   by = c("item")
             # ), 
           .progress = T) |> 
  list_rbind(names_to = "iter_grp") |> 
  mutate(CA = (r_rest)^2/alpha_rest,
         DI_g6 = (r_rest)^2/g6_rest,
         DI_std = (r_rest)^2/alpha_rest_std,
         # split = if_else(!is.na(split),"test","train")
         )  |> 
  rename(L6 = r2, D_L6 = intercept, 
         itcor = raw_r,
         itcor_std = std_r,
         ctt_disc = r_rest, ## corrected item-total correlation
         # ctt_disc = r_rest, 
         obsr2 = meanobsr2) |> 
  replace_inf() |> 
  select(-any_of(c("aic","bic","rmse","sigma","irt"))
  ) |> distinct() |> 
  group_by(iter,iter_grp,benchmark) |> 
  mutate( #n_items = n_distinct(item),
    nvali = n_distinct(item,na.rm=T)-sum(is.na(cor2))
  ) |> ungroup()|> 
  mutate(n_valmods = round(nobs/ni),
         n_models = if_else(is.na(nmods),n_valmods,nmods), 
         valid_item = !is.na(cor2)
  ) |> 
  group_by(item) |> 
  mutate(countvalid = sum(valid_item)) |> ungroup() |>   
  select(iter,benchmark,item,iter_grp,
         n_iobs:ni,any_of(c("nobs","valid_n","n_valmods")),
         nvali:countvalid,
         # split,
         L6,MS,CA,
         # G2,D,itcor,
         everything(),-any_of(c("aic","bic","rmse","sigma","irt"))
  ) |> distinct() |> 
  mutate(iter=glue("{ifelse(ordered_models,'hilo','rand')}_long_{iter}")) |> select(iter, everything())

saveRDS(irdf , here::here("benchmark_reliability", "data","results","boots",glue::glue("itemfit_rel_boot_{today()}.rds")))



tic()
adf = suppressMessages(irdf |> filter(valid_item,split=="train",
                                      valid_item,countvalid>5) |> 
                         mutate(iter_grp = as_factor(iter_grp),
                                item = as_factor(item),
                                benchmark = as_factor(benchmark),
                                itemgroup = interaction(iter,val_test_set,iter_grp)) |> 
                         group_by(benchmark) |>
                         datawizard::adjust(effect = c("ni",
                                                       "n_valmods",
                                                       "nobs",
                                                       "itemgroup"
                         ),
                         select =  "L6:DI_std",
                         # additive = T,
                         multilevel = T,
                         keep_intercept = T,
                         verbose = T
                         ) |> 
                         ungroup() |>
                         pivot_longer(L6:DI_std,names_to = "metric",values_to = "est") |> 
                         group_by(benchmark,item,metric) |> 
                         summarise(est = mean(est, na.rm=T)) |> 
                         ungroup() |> 
                         pivot_wider(names_from = metric, values_from = est)
)
adf

adf = adf |> mutate(rand = runif(nrow(adf)), rand2 = rnorm(nrow(adf)))
gc()
adf |> saveRDS(here::here("benchmark_reliability/data/results/boots", glue::glue("item_agg_adj_{today()}.rds")))
toc()


rankrefits = list()
gc()

nitems_for_gstudy = 50

item_samples = adf  |> 
  group_by(benchmark) |> 
  mutate(comp = (dense_rank(CA) + dense_rank(Hi) + 
                   # dense_rank(a) # switch lines if mirt models estimated
                   dense_rank(a1_approx_cov)
  )/(3*n())) |> ungroup() |> 
  pivot_longer(cols = CA:comp,names_to = "metric",values_to = "est") |> 
  mutate(est = if_else(metric %in% c("outfit","infit","z.outfit","z.infit"
  ), abs(est),est),
  est = if_else(metric %in% c(
    "outfit","infit","z.outfit","z.infit","pr_t",
    "std_error","clow","average_r","cneg","cse",
    "Zh","del_alpha"
  ), -1 * est, est)) |>
  slice_max(order_by = est, by = c(benchmark,metric),n = nitems_for_gstudy) |> 
  select(benchmark,item,metric) |> distinct()

benchdf = aggdf |> semi_join(item_samples)


plan("multisession", workers = 4L)


rankrefits = list()

gc()
dstudydf = make_gstudy_df(benchdf)

dstudydf = list_rbind(rankrefits,names_to="bench_metric") 

dstudydf |> saveRDS(here::here("benchmark_reliability/data/results/boots", glue::glue("item_gdstudydf_{today()}.rds")))
toc()

metrics_to_filter = c("")

dstudydf |> select(bench_metric,benchmark,metric,m_rel,i_rel) |> distinct() |> 
  filter(!(metric %in% metrics_to_filter)) |>
  mutate(isrand = if_else(metric %in% c("rand","rand2"),"rand","normal")) |> 
  ggplot(aes(x= i_snr, y = m_snr, color=metric,shape = metric)) + 
  geom_hline(mapping= aes(yintercept = m_snr),
             data = subset(dstudydf,metric == "rand")) + 
  geom_vline(mapping= aes(xintercept = i_snr),
             data = subset(dstudydf,metric == "rand")) + 
  theme_minimal() + 
  scale_shape_manual(values = c(15:18,1:7,21:24,15:19,1:7,21:24,15:18,1:7,21:24,15:19,1:7,21:24,15:18,1:7,21:24,15:19,1:7,21:24)) +
  geom_point(size=3) + 
  labs(title = "Model SNR vs. Item SNR", x = "Item-diff SNR", y = "Model-score SNR") + 
  guides(shape = guide_legend(ncol=18),color = guide_legend(ncol=18)) +
  theme(legend.position = "bottom") + facet_wrap(~benchmark,scales="free")



## make line plots 
## 


expand_grid(num_items = c(1:20,25,30,40,50,75,100,125,250,500,750,1000),num_mods = c(1:20,25,30,40,50,75,100,125,250,500,750,1000),benchmark = unique(dstudydf$benchmark)) |> right_join(dstudydf_mad, by = "benchmark",multiple = "all",relationship = "many-to-many") |>
  mutate(num_item =1,
         num_mod = 1, i_rel = item / (item + model + Residual),
         m_dep = model / (item + model + Residual) ,
         m_rel = model / ( model + Residual),
         m_snr = m_rel / (1 - m_rel),
         i_snr = i_rel / (1 - i_rel),
         m_rel_D =  model / ( model + Residual/(num_items*num_mod)),
         i_rel_D = item / ( item + Residual/(num_item*num_mods)),
         m_snr_D = m_rel_D / (1 - m_rel_D),
         i_snr_D = i_rel_D / (1 - i_rel_D),
         m_errtol_D = sqrt((1 - m_rel_D) / m_rel_D),
         i_errtol_D = sqrt((1 - i_rel_D) / i_rel_D), metric = if_else(metric == "rand","baseline",metric),metric = if_else(metric == "comp","composite",metric),
  ) |> left_join(item_counts, by = "benchmark"
  ) |> mutate(prop_items = num_items/n
  ) |> filter(metric %in% c("a","CA","Hi","baseline","G2","composite"),num_items <90) |> ggplot(aes(x = num_items, y = m_rel_D, color=metric,linetype=metric)) + geom_hline(yintercept = 0.9, linewidth=0.4, color = "darkred") + geom_line(linewidth=1) + theme_minimal() + ggthemes::scale_color_colorblind() + labs(title = "Model Rank Reliability vs. Number of Items Used", x = "Number of Items Used", y = "Model Rank Reliability (Adjusted for Dependent Items)") + theme(legend.position = "bottom") + facet_wrap(~benchmark)

