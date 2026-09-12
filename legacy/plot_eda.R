### Paper Plots

library(tidyverse)
library(fastverse)
library(data.table)



bsubset <- c(
  "mmlu_pro",
  "bbh",
  "arc_challenge",
  "musr",
  "gpqa"
)

mxsubset <- c(
  "2PL discrimination", "a", "CA",
  "L6", "diff",
  "rand",
  "Corrected Interitem Corr.",
  "Lambda 6",
  "Raw Difficulty",
  "Hi",
  "Random"
)


pldf <- all_dstudydf |> # "comp",
  select(benchmark, metric, samp, n_items, everything()) |>
  fmutate(
    mod_snr_dom = m_snr - i_snr,
    mod_snr_domrat = m_snr / i_snr,
    modl_snr_dom = log(m_snr / i_snr),
    mod_dep_dom = m_dep - i_dep,
    mod_rel_dom = m_rel - i_rel,
    mod_dom = m_rel - i_rel,
    metric = if_else(metric %in% rand_metrics, "rand", metric)
  ) |>
  select(-any_of("kcor")) |>
  inner_join(corrstudydf |>
    filter(samp %in% c("test", "train")) |> # "comp",
    fmutate(metric = if_else(metric %in% rand_metrics, "rand", metric)) |>
    fgroup_by(n_items, benchmark, metric, samp) |>
    fsummarise(across(kcor, fmean)) |>
    ungroup(), by = c("benchmark", "metric", "samp", "n_items")) |>
  inner_join(modcounts, by = "benchmark") |>
  filter(metric %in% c(bonusset, mxsubset, "cralpha_rv", "war2_pgl6_rv", "rand")) |>
  fmutate(
    m_rel_D = model / (model + Residual / (n_items)),
    m_rel_D_snr = m_rel_D / (1 - m_rel_D),
    m_dep_D = model / (model + item / n_items + Residual / (n_items)),
    ## More optional metrics for advanced analysis
    m_rel_Scale = (model / ntest) / (model / ntest + Residual / (n_items * ntest)),
    greiner_kcor = sin(pi / 2 * kcor),
    m_dep_Scale = (model / ntest) / (model / ntest + item / n_items + Residual / (n_items * ntest)),
    i_rel_D = item / (item + Residual / (ntest)),
    i_rel_Scale = (item / n_items) / (item / n_items + Residual / (n_items * ntest)), ## had ntest in denom
    i_dep_D = item / (item + model / ntest + Residual / (ntest * n_items)),
    i_dep_Scale = (item / n_items) / (item / n_items + model / ntest + Residual / (n_items * ntest)),
    i_rel_D_snr = i_rel_D / (1 - i_rel_D),
    i_rel_Scale_snr = i_rel_Scale / (1 - i_rel_Scale),
    i_dep_D_snr = i_dep_D / (1 - i_dep_D),
    i_dep_Scale_snr = i_dep_Scale / (1 - i_dep_Scale),
    m_D_dom = m_rel_D - i_rel_D,
    m_Scale_dom = m_rel_Scale - i_rel_Scale,
    m_dep_D_dom = m_dep_D - i_dep_D,
    m_dep_Scale_dom = m_dep_Scale - i_dep_Scale,
    m_dep_D_snr = m_dep_D / (1 - m_dep_D),
    m_dep_Scale_snr = m_dep_Scale / (1 - m_dep_Scale),
    mtc = metric,
    metric = if_else(metric == "rand", "Random", metric),
    metric = if_else(metric == "a", "2PL discrimination", metric),
    metric = if_else(metric == "MS", "Molenaar-Sijtsma", metric),
    metric = if_else(metric == "L6", "Lambda 6*", metric),
    metric = if_else(metric == "diff", "Raw Difficulty", metric),
    metric = if_else(metric == "CA", "Corrected Interitem Corr.", metric),
    metric = if_else(metric == "cralpha_rv", "ISU metric", metric),
    metric = if_else(metric == "forbes_adj", "Fish Counting", metric),
    metric = if_else(metric == "std_error_rv", "L6 Precision", metric),
    metric = if_else(metric == "war2_pgl6_rv", "L6 Uniqueness", metric),
    metric = if_else(metric == "Zh_rv", "Zh Uniqueness", metric)
  ) |>
  select(benchmark, mtc, metric, samp, n_items, ntrain, ntest, kcor, greiner_kcor, model:m_dep_Scale_snr) |>
  arrange(n_items)



# The challenges of distribution shift ------
## comparisons of ability distributions --------

## all benchmarks
hilo_splits |> ggplot(aes(x = resp, fill = split)) +
  geom_histogram() +
  theme_minimal() +
  facet_wrap(~benchmark, scales = "free", nrow = 3) +
  labs(
    title = "Score Distributions by Benchmark and Split",
    x = "Percent Correct", y = "Count"
  ) +
  theme(text = element_text(family = "times", size = 13)) +
  ggthemes::scale_fill_colorblind()


(psplit <- hilo_splits |>
  filter(benchmark %in% bsubset) |>
  mutate(split = factor(split,
    levels = c("train", "test", "rand"),
    labels = c("Worst Model Sample", "Best Model Sample", "Random Model Sample")
  )) |>
  ggplot(aes(x = resp, fill = split)) +
  geom_histogram() +
  theme_minimal() +
  facet_wrap(~benchmark, scales = "free", nrow = 1) +
  labs(
    title = "Score Distributions by Benchmark and Split",
    x = "Percent Correct", y = "Count"
  ) +
  ggthemes::scale_color_wsj() +
  ggthemes::scale_fill_wsj() +
  ggthemes::scale_color_economist() +
  ggthemes::scale_fill_economist() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  theme(
    text = element_text(family = "times", size = 14),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ))

## comparison of kcor in train and test and rand -----
corrstudydf_rand |>
  mutate(samp = "rand") |>
  bind_rows(
    corrstudydf |>
      filter(samp %in% c("test", "train"))
  ) |>
  filter(metric %in% c(
    "rand",
    "Random"
  ), n_items <= 250) |>
  fmutate(
    samp = factor(samp,
      levels = c("train", "test", "rand"),
      labels = c("Worst Model Sample", "Best Model Sample", "Random Model Sample")
    )
  ) |>
  arrange(n_items) |>
  ggplot(aes(
    x = n_items, y = kcor,
    color = samp, fill = samp,
    linetype = samp
  )) +
  geom_hline(yintercept = 0.8, linewidth = 0.4, color = "darkred") +
  geom_smooth(linewidth = 0.6) +
  facet_wrap(~benchmark, nrow = 2, scales = "free", ) +
  theme_minimal() +
  ggthemes::scale_color_wsj() +
  ggthemes::scale_fill_wsj() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "times", size = 12),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    # Reduce the margin around the whole legend box
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  ) +
  labs(
    title = "Empirical Rank Reliability (Kendall) vs. Number of Items",
    x = "Number of Items",
    y = "Empirical Rank Reliability (Kendall)"
  )

pkcor <- corrstudydf_rand |>
  mutate(samp = "rand") |>
  bind_rows(
    corrstudydf |>
      filter(samp %in% c("test", "train"))
  ) |>
  filter(metric %in% c(
    "rand",
    "Random"
  ), n_items <= 250) |>
  filter(benchmark %in% c(
    "mmlu_pro",
    "bbh",
    "arc_challenge",
    "med_qa",
    "gpqa"
  )) |>
  fmutate(
    samp = factor(samp,
      levels = c("train", "test", "rand"),
      labels = c("Worst Model Sample", "Best Model Sample", "Random Model Sample")
    )
    # metric = factor(if_else(metric == "rand","random baseline",metric), levels = c("random baseline",mtx_to_display)),
  ) |>
  arrange(n_items) |>
  ggplot(aes(
    x = n_items, y = kcor,
    color = samp, fill = samp,
    linetype = samp
  )) +
  geom_hline(yintercept = 0.8, linewidth = 0.4, color = "darkred") +
  # geom_hline(yintercept = 0, linewidth=0.4, color = "darkred") +
  # geom_path(linewidth = 0.6) +
  geom_smooth(linewidth = 0.6) +
  facet_wrap(~benchmark,
    nrow = 1,
    scales = "free",
    # labeller = as_labeller()
  ) +
  theme_minimal() +
  ggthemes::scale_color_economist() +
  ggthemes::scale_fill_economist() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  theme(
    legend.position = "right", # legend.position = "bottom",
    text = element_text(family = "times", size = 14),
    legend.title = element_blank(),
    # legend.key.size = unit(0.1, "cm"),
    legend.text = element_text(size = 12),
    # Adjust horizontal and vertical spacing between items
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    # Reduce the margin around the whole legend box
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  ) +
  labs(
    title = "Empirical Rank Reliability (Kendall) vs. Number of Items, when Sampling from Different Model Pools",
    x = "Number of Items",
    y = "Empirical Rank Reliability (Kendall)"
  )


cowplot::plot_grid(
  psplit, # + theme(legend.position="none"),
  pkcor, # + theme(legend.position="none"),
  nrow = 2,
  labels = c("A", "B"), label_fontfamily = "times", label_size = 16
)


# reliability plots across metrics -------
### kendal tau for everyone --------
pldf |> # "comp",
  fmutate(metric = if_else(metric %in% rand_metrics, "rand", metric)) |>
  fgroup_by(n_items, benchmark, mtc, metric, samp) |>
  fsummarise(across(kcor:m_dep_Scale_snr, fmean)) |>
  filter(samp == "test") |>
  inner_join(modcounts, by = "benchmark") |>
  filter(n_items <= 300) |>
  filter(!str_detect(mtc, "gutt|hard|Zh")) |>
  filter(mtc %in% c("CA", "L6", "rand", "diff", "ISU", "Hi", "war2_pgl6_rv")) |> # c(mxsubset)) |> # bonusset
  # filter(benchmark %in% bsubset) |>
  arrange(n_items) |>
  ggplot(aes(x = n_items, y = kcor, color = metric, fill = metric)) +
  geom_hline(yintercept = 0.8, linewidth = 0.4, color = "darkred") +
  # geom_path(linewidth = 0.6) +
  geom_smooth(linewidth = 0.6) +
  facet_wrap(~benchmark, nrow = 2, scales = "free") +
  theme_minimal() +
  scale_x_log10() +
  guides(
    # color = guide_legend(ncol=12,
    color = guide_legend(
      nrow = 1,
      override.aes = list(linewidth = 1.5)
    )
  ) +
  labs(
    title = "Empirical Rank Reliability (Kendall) vs. Number of Items",
    x = "Number of Items",
    y = expression(tau)
    # y = "Empirical Rank Reliability (Kendall)"
  ) +
  theme( # legend.position = "right",
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "times", size = 14),
    # legend.key.size = unit(0.1, "cm"),
    # legend.text = element_text(size = 7),
    # Adjust horizontal and vertical spacing between items
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    # Reduce the margin around the whole legend box
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  )




# panel plots -------

maxi <- 200
mx_to_use <- c(mxsubset2, "cralpha_rv")
mx_to_use <- c("rand", "L6")
benches_to_use <- bsubset # unique(pldf$benchmark) # bsubset
# mx_to_use = setdiff(c(mxsubset,"cralpha_rv"),"Hi")

## kendall tau -------

pkcorall <- pldf |> # "comp",
  fmutate(metric = if_else(metric %in% rand_metrics, "rand", metric)) |>
  fgroup_by(n_items, benchmark, mtc, metric, samp) |>
  fsummarise(across(kcor:m_dep_Scale_snr, fmean)) |>
  ungroup() |>
  filter(samp == "test") |>
  filter(n_items <= maxi) |>
  filter(Residual > 1e-8) |>
  inner_join(modcounts, by = "benchmark") |>
  filter(mtc %in% mx_to_use) |>
  filter(benchmark %in% benches_to_use) |>
  arrange(n_items) |>
  ggplot(aes(x = n_items, y = kcor, color = metric, fill = metric)) +
  geom_hline(yintercept = 0.8, linewidth = 0.4, color = "darkred") +
  # geom_path(linewidth = 0.6) +
  geom_smooth(linewidth = 0.6) +
  facet_wrap(~benchmark, nrow = 1, scales = "free") +
  theme_minimal() +
  scale_x_log10() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(linewidth = 1.5)
    )
  ) +
  labs(
    title = "Empirical Rank Correlation (Kendall) vs. Number of Items",
    x = "Number of Items",
    y = expression(tau)
  ) +
  theme( # legend.position = "right",
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "times", size = 14),
    # legend.key.size = unit(0.1, "cm"),
    # legend.text = element_text(size = 7),
    # Adjust horizontal and vertical spacing between items
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    # Reduce the margin around the whole legend box
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  )



## generalizabililty estimates D-study ------

pgencoef <- pldf |> # "comp",
  fmutate(metric = if_else(metric %in% rand_metrics, "rand", metric)) |>
  fgroup_by(n_items, benchmark, mtc, metric, samp) |>
  fsummarise(across(kcor:m_dep_Scale_snr, fmean)) |>
  ungroup() |>
  filter(samp == "test") |>
  filter(Residual > 1e-8) |>
  filter(n_items <= maxi) |>
  inner_join(modcounts, by = "benchmark") |>
  # filter(metric %in% mxsubset2) |>
  filter(mtc %in% mx_to_use) |>
  filter(benchmark %in% benches_to_use) |>
  arrange(n_items) |>
  ggplot(aes(x = n_items, y = m_rel_D, color = metric, fill = metric)) +
  geom_hline(yintercept = 0.8, linewidth = 0.4, color = "darkred") +
  # geom_path(linewidth = 0.6) +
  geom_smooth(linewidth = 0.6) +
  facet_wrap(~benchmark, nrow = 1, scales = "free") +
  theme_minimal() +
  scale_x_log10() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  # guides(
  #   color = guide_legend(ncol=25,
  #                        # override.aes = list(linewidth =0.5)
  #   )) +
  guides(
    # color = guide_legend(ncol=12,
    color = guide_legend(
      ncol = 1,
      override.aes = list(linewidth = 1.5)
    )
  ) +
  labs(
    title = "Empirical Generalizability Coefficient (D-study) vs. Number of Items",
    x = "Number of Items",
    y = expression("E" * rho^2)
    # y = "Empirical Generalizability Coefficient"
  ) +
  theme(
    legend.position = "right", # legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "times", size = 14),
    # Adjust horizontal and vertical spacing between items
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    # Reduce the margin around the whole legend box
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  )



pdepcoef <- pldf |> # "comp",
  fmutate(metric = if_else(metric %in% rand_metrics, "rand", metric)) |>
  fgroup_by(n_items, benchmark, mtc, metric, samp) |>
  fsummarise(across(kcor:m_dep_Scale_snr, fmean)) |>
  ungroup() |>
  filter(samp == "test") |>
  filter(n_items <= maxi) |>
  filter(Residual > 1e-8) |>
  inner_join(modcounts, by = "benchmark") |>
  # filter(metric %in% mxsubset2) |>
  filter(mtc %in% mx_to_use) |>
  filter(benchmark %in% benches_to_use) |>
  arrange(n_items) |>
  ggplot(aes(
    x = n_items, y = n_items * md_snr, # m_dep_D,
    color = metric, fill = metric
  )) +
  # geom_path(linewidth = 0.6) +
  geom_smooth(linewidth = 0.6) +
  facet_wrap(~benchmark, nrow = 1, scales = "free") +
  theme_minimal() +
  scale_x_log10() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  guides(
    # color = guide_legend(ncol=12,
    color = guide_legend(
      nrow = 2,
      override.aes = list(linewidth = 1.5)
    )
  ) +
  labs(
    title = "Empirical Dependability SNR vs. Number of Items",
    x = "Number of Items",
    # y = "Empirical Dependability Coefficient"
    y = expression("SNR(" * Phi * ")x " * n[items])
  ) +
  theme(
    legend.position = "right", # legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "times", size = 14),
    # legend.key.size = unit(0.1, "cm"),
    # legend.text = element_text(size = 7),
    # Adjust horizontal and vertical spacing between items
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    # Reduce the margin around the whole legend box
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  )


cowplot::plot_grid(
  pgencoef + theme(legend.position = "none"), # axis.title.y =  element_blank()),
  pdepcoef + theme(legend.position = "none"), # axis.title.y =  element_blank()),
  pkcorall, # + theme(axis.title.y =  element_blank()),
  nrow = 3, ncol = 1,
  rel_heights = c(1, 1, 1.3),
  labels = c("A", "B", "C"), label_fontfamily = "times", label_size = 16
)


## gain in signal -------


pldf |> # "comp",
  fmutate(metric = if_else(metric %in% rand_metrics, "rand", metric)) |>
  fgroup_by(n_items, benchmark, mtc, metric, samp) |>
  fsummarise(across(kcor:m_dep_Scale_snr, fmean)) |>
  ungroup() |>
  filter(samp == "test") |>
  filter(Residual > 1e-8) |>
  filter(n_items <= maxi) |>
  inner_join(modcounts, by = "benchmark") |>
  # filter(metric %in% mxsubset2) |>
  filter(mtc %in% mx_to_use) |>
  filter(benchmark %in% benches_to_use) |>
  group_by(n_items, benchmark, samp) |>
  summarize(across(kcor:m_dep_Scale_snr, \(x) (sum(if_else(mtc == "L6", x, 0)) - sum(if_else(mtc == "rand", x, 0)))), metric = "Improvement over Random") |>
  ungroup() |>
  arrange(n_items) |>
  ggplot(aes(x = n_items, y = (m_rel / (1 - m_rel)), color = metric, fill = metric)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "darkred") +
  # geom_path(linewidth = 0.6) +
  geom_smooth(linewidth = 0.6) +
  facet_wrap(~benchmark, nrow = 1, scales = "free") +
  theme_minimal() +
  scale_x_log10() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  guides(
    # color = guide_legend(ncol=12,
    color = guide_legend(
      ncol = 1,
      override.aes = list(linewidth = 1.5)
    )
  ) +
  labs(
    title = "Expected Increase in Generalizability for a Subsequent Item selected by L* over Random",
    x = "Number of Items",
    y = expression(Delta[lambda^"*" - rand] * "E" * rho^2) # #*"-"*Delta[lambda^rand] *"E" * rho[Rand]^2)
    # y = "Empirical Generalizability Coefficient"
  ) +
  theme( # legend.position = "right",
    legend.position = "none",
    legend.title = element_blank(),
    text = element_text(family = "times", size = 14),
    # legend.key.size = unit(0.1, "cm"),
    # legend.text = element_text(size = 7),
    # Adjust horizontal and vertical spacing between items
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    # Reduce the margin around the whole legend box
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  )





(tadpole <- pldf |>
  filter(mtc %in% c("rand")) |>
  # filter(samp=="test")  |>
  filter(mtc %in% setdiff(mxsubset, c("Hi", "a"))) |>
  filter(n_items <= 250) |>
  fgroup_by(n_items, benchmark, metric, samp) |>
  fsummarise(across(kcor:m_dep_Scale_snr, fmean, na.rm = T)) |>
  ungroup() |>
  arrange(n_items) %>%
  ggplot(aes(
    x = (m_dep_D_snr / (1 + m_dep_D_snr)), # n_items,
    y = kcor^2,
    shape = samp,
    # size = n_items,
    fill = samp,
    color = samp,
    alpha = n_items,
  )) +
  geom_hline(
    yintercept = 0,
    # data = subset(.,metric=="rand") ,
    linewidth = 0.4, color = "darkred"
  ) +
  geom_vline(
    xintercept = 0,
    # data = subset(.,metric=="rand") ,
    linewidth = 0.4, color = "darkred"
  ) +
  geom_vline(
    xintercept = 1,
    # data = subset(.,metric=="rand") ,
    linewidth = 0.4, color = "darkred"
  ) +
  geom_abline(
    slope = 1, intercept = 0,
    linewidth = 0.4, color = "darkred", linetype = "dashed"
  ) +
  geom_path(linewidth = 0.6) +
  # geom_smooth( linewidth = 0.6,se=F) +
  # geom_density_2d_filled(contour_var = "ndensity") +
  geom_point() +
  facet_wrap(~benchmark,
    nrow = 2,
    scales = "free"
  ) +
  theme_minimal() +
  scale_size_continuous(name = expression(tau^2)) +
  scale_alpha_binned(name = "Number of Items", n.breaks = 10, range = c(0.1, 1)) +
  # scale_size_area(max_size = 6) +
  scale_size_binned_area(n.breaks = 10, max_size = 10) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = expression("Benchmark Signal: Attenuation of Empirical Concordance due to Noise via " * R[tau]^2 * " to SNR Conversion on Random Item Draws"),
    x = expression("SNR(" * Phi * ")/(1 +SNR(" * Phi * "))"), # "Cumulative Dominance of the Model Ranking Signal over the Item Ranking Signal as Items are Added",
    y = expression(R^2),
    # y = expression(Phi)
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "times", size = 14),
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  ))


(kc <- pldf |> # "comp",
  fmutate(metric = if_else(metric %in% rand_metrics, "rand", metric)) |>
  fgroup_by(n_items, benchmark, mtc, metric, samp) |>
  fsummarise(across(kcor:m_dep_Scale_snr, fmean)) |>
  ungroup() |>
  filter(samp == "test") |>
  # filter(n_items <= maxi) |>
  filter(Residual > 1e-8) |>
  inner_join(modcounts, by = "benchmark") |>
  # filter(metric %in% mxsubset2) |>
  filter(mtc %in% mx_to_use) |>
  # filter(benchmark %in% benches_to_use) |>
  arrange(n_items) |>
  ggplot(aes(x = n_items, y = kcor, color = metric, fill = metric)) +
  geom_hline(yintercept = 0.8, linewidth = 0.4, color = "darkred") +
  # geom_path(linewidth = 0.6) +
  geom_smooth(linewidth = 0.6) +
  facet_wrap(~benchmark, nrow = 2, scales = "free") +
  theme_minimal() +
  scale_x_log10() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  guides(
    # color = guide_legend(ncol=12,
    color = guide_legend(
      nrow = 2,
      override.aes = list(linewidth = 1.5)
    )
  ) +
  labs(
    title = "Empirical Rank Correlation (Kendall) vs. Number of Items",
    x = "Number of Items",
    y = expression(tau)
    # y = "Empirical Rank Reliability (Kendall)"
  ) +
  theme( # legend.position = "right",
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "times", size = 14),
    # Adjust horizontal and vertical spacing between items
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    # Reduce the margin around the whole legend box
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  ))



## generalizabililty estimates D-study ------




gen_D <- pldf |> # "comp",
  fmutate(metric = if_else(metric %in% rand_metrics, "rand", metric)) |>
  fgroup_by(n_items, benchmark, mtc, metric, samp) |>
  fsummarise(across(kcor:m_dep_Scale_snr, fmean)) |>
  ungroup() |>
  filter(samp == "test") |>
  filter(Residual > 1e-8) |>
  filter(n_items <= maxi) |>
  inner_join(modcounts, by = "benchmark") |>
  # filter(metric %in% mxsubset2) |>
  filter(mtc %in% mx_to_use) |>
  # filter(benchmark %in% benches_to_use) |>
  arrange(n_items) |>
  ggplot(aes(x = n_items, y = m_rel_D, color = metric, fill = metric)) +
  geom_hline(yintercept = 0.8, linewidth = 0.4, color = "darkred") +
  # geom_path(linewidth = 0.6) +
  geom_smooth(linewidth = 0.6) +
  facet_wrap(~benchmark, nrow = 1, scales = "free") +
  theme_minimal() +
  scale_x_log10() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  guides(
    # color = guide_legend(ncol=12,
    color = guide_legend(
      ncol = 1,
      override.aes = list(linewidth = 1.5)
    )
  ) +
  labs(
    title = "Empirical Generalizability Coefficient (D-study) vs. Number of Items",
    x = "Number of Items",
    y = expression("E" * rho^2)
    # y = "Empirical Generalizability Coefficient"
  ) +
  theme(
    legend.position = "right", # legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "times", size = 14),
    # Adjust horizontal and vertical spacing between items
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    # Reduce the margin around the whole legend box
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  )



(depsnr <- pldf |> # "comp",
  fmutate(metric = if_else(metric %in% rand_metrics, "rand", metric)) |>
  fgroup_by(n_items, benchmark, mtc, metric, samp) |>
  fsummarise(across(kcor:m_dep_Scale_snr, fmean)) |>
  ungroup() |>
  filter(samp == "test") |>
  # filter(n_items <= maxi) |>
  filter(Residual > 1e-8) |>
  inner_join(modcounts, by = "benchmark") |>
  # filter(metric %in% mxsubset2) |>
  filter(mtc %in% mx_to_use) |>
  # filter(benchmark %in% benches_to_use) |>
  arrange(n_items) |>
  ggplot(aes(
    x = n_items, y = m_dep_D / (m_dep_D + i_dep_D), # m_dep_D, ## md_snr /(md_snr + id_snr)
    color = metric, fill = metric
  )) +
  # geom_path(linewidth = 0.6) +
  geom_smooth(linewidth = 0.6) +
  facet_wrap(~benchmark, nrow = 2, scales = "free") +
  theme_minimal() +
  scale_x_log10() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  # guides(
  #   color = guide_legend(ncol=25,
  #                        # override.aes = list(linewidth =0.5)
  #   )) +
  guides(
    # color = guide_legend(ncol=12,
    color = guide_legend(
      nrow = 2,
      override.aes = list(linewidth = 1.5)
    )
  ) +
  labs(
    title = "Empirical Dependability SNR vs. Number of Items",
    x = "Number of Items",
    # y = "Empirical Dependability Coefficient"
    y = expression("SNR(" * Phi * ")")
  ) +
  theme(
    legend.position = "right", # legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "times", size = 14),
    # legend.key.size = unit(0.1, "cm"),
    # legend.text = element_text(size = 7),
    # Adjust horizontal and vertical spacing between items
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    # Reduce the margin around the whole legend box
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  ))


cowplot::plot_grid(
  tadpole,
  gen_D + theme(legend.position = "none"), # axis.title.y =  element_blank()),
  depsnr + theme(legend.position = "none"), # axis.title.y =  element_blank()),
  kc, # + theme(axis.title.y =  element_blank()),
  nrow = 4, ncol = 1,
  rel_heights = c(1.3, 1, 1.3),
  labels = c("A", "B", "C", "D"), label_fontfamily = "times", label_size = 16
)

# plots for theory and motivation ---------


maxi <- 200
mx_to_use <- c(mxsubset2, "cralpha_rv")
benches_to_use <- unique(pldf$benchmark) # bsubset
mx_to_use <- setdiff(c(mxsubset, "cralpha_rv"), "Hi")
fontsz <- 13

## distributions -- ------


(psplit <- hilo_splits |>
  filter(benchmark %in% bsubset) |>
  mutate(split = factor(split,
    levels = c("train", "test", "rand"),
    labels = c("Sample of Worst Models", "Sample of Best Models", "Random Model Sample")
  )) |>
  ggplot(aes(x = resp, fill = split)) +
  geom_histogram() +
  theme_minimal() +
  facet_wrap(~benchmark, scales = "free", nrow = 1) +
  labs(
    title = "Score Distributions by Benchmark and Split",
    x = "Percent Correct", y = "Count"
  ) +
  ggthemes::scale_color_wsj() +
  ggthemes::scale_fill_wsj() +
  ggthemes::scale_color_economist() +
  ggthemes::scale_fill_economist() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  theme(
    text = element_text(family = "times", size = 14),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ))



## Kcor vs n_items plot -------

(kcor_ttr <- corrstudydf_rand |> mutate(samp = "rand") |> bind_rows(
  corrstudydf |>
    filter(samp %in% c("test", "train"))
) |>
  filter(benchmark %in% bsubset) |>
  filter(metric %in% c(
    "rand",
    "Random"
  ), n_items <= 250) |>
  fmutate(
    metric = if_else(metric %in% rand_metrics, "rand", metric),
    samp = factor(samp,
      levels = c("train", "test", "rand"),
      labels = c("Sample of Worst Models", "Sample of Best Models", "Random Model Sample")
    )
  ) |>
  arrange(n_items) |>
  ggplot(aes(
    x = n_items, y = kcor,
    # color=metric,fill=metric,
    color = samp, fill = samp,
    linetype = samp
  )) +
  geom_hline(yintercept = 0.8, linewidth = 0.4, color = "darkred") +
  # geom_path(linewidth = 0.6) +
  geom_smooth(linewidth = 0.6) +
  facet_wrap(~benchmark,
    nrow = 1, scales = "free",
    # labeller = as_labeller()
  ) +
  theme_minimal() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  theme(
    text = element_text(family = "times", size = fontsz),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  labs(
    title = "Empirical Rank Reliability (Kendall) vs. Number of Items",
    x = "Number of Items",
    y = expression(tau)
  ))

## tadpole -------


(main_tad <- pldf |>
  filter(mtc %in% c("rand")) |>
  filter(benchmark %in% bsubset) |>
  # filter(samp=="test")  |>
  # filter(mtc %in% mxsubset # c("rand" )#,"cralpha_rv","diff","L6"
  # ) |>
  fgroup_by(n_items, benchmark, metric, samp) |>
  fsummarise(across(kcor:m_dep_Scale_snr, fmean, na.rm = T)) |>
  ungroup() |>
  arrange(n_items) %>%
  ggplot(aes(
    x = (m_dep_D_snr / (1 + m_dep_D_snr)), # n_items,
    y = kcor^2,
    color = samp,
    fill = samp,
    alpha = n_items,
  )) +
  geom_hline(
    yintercept = 0,
    # data = subset(.,metric=="rand") ,
    linewidth = 0.5, color = "darkgray"
  ) +
  geom_vline(
    xintercept = 0,
    # data = subset(.,metric=="rand") ,
    linewidth = 0.5, color = "darkgray"
  ) +
  geom_abline(
    slope = 1, intercept = 0,
    linewidth = 0.4, color = "darkred", linetype = "dashed"
  ) +
  geom_path(linewidth = 0.6) +
  geom_point() +
  facet_wrap(~benchmark,
    nrow = 1,
    # scales="free_x"
  ) +
  theme_minimal() +
  scale_size_continuous(name = expression(tau^2)) +
  scale_alpha_binned(name = "Number of Items", n.breaks = 10, range = c(0.1, 1)) +
  # scale_size_area(max_size = 6) +
  scale_size_binned_area(n.breaks = 10, max_size = 10) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = expression("Benchmark Signal: Attenuation of Empirical Concordance due to Noise via " * R[tau]^2 * " to SNR Conversion on Random Item Draws"),
    x = expression("SNR(" * Phi * ")/(1 +SNR(" * Phi * "))"), # "Cumulative Dominance of the Model Ranking Signal over the Item Ranking Signal as Items are Added",
    y = expression(R^2),
    # y = expression(Phi)
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "times", size = fontsz),
    legend.spacing.x = unit(0.001, "cm"),
    legend.spacing.y = unit(0.001, "cm"),
    legend.margin = margin(t = 0.001, r = 0.001, b = 0.001, l = 0.001, unit = "cm")
  ))