# Paper figures

`analysis/04_paper_figures.R` draws the paper's 16 figures and writes their
LaTeX captions. `analysis/06_shift_summary.R` writes the tables behind the
positive-shift results. Both write to `paper_figures/`, which is git-ignored:
the figures and LaTeX are build outputs.

```bash
make complete-data RAW=/path/raw   # once: the Complete benchmarks (Fig. 3) from the data_collection/ outputs
make paper-inputs WORKERS=8        # item scores, curves and G-studies the figures need (2PL is the slow part)
make paper-figures WORKERS=4       # the 16 PDFs + figures.tex, manifest.csv, preview.tex
make shift-summary                 # tables/*.tex and *.csv
```

`--only=<families>` renders some families (`panel`, `shift`, `1a`, `1c`, `2`,
`3`, `4`, `5`), and `--lines=raw` or `--lines=both` writes `_raw` / `_both`
versions of the curve figures. Compile `paper_figures/preview.tex` with
`pdflatex` to check every figure with its caption at the 5.5 in text width.

## The figures

Benchmark sets: **Main** is ARC-Challenge, BBH, GPQA and MMLU-Pro;
**Appendix** is the 18 benchmarks of `aggdf.csv`; **Complete** adds 109
benchmarks from WABM, OpenEval, Every Eval Ever and LiveBench.

| Section | File | Type | Shows |
|---|---|---|---|
| Main text | `fig1bshift_main_panel_full_grel.pdf` | 1b | G(n) under two positive shifts (rows) on the Main benchmarks (columns), every criterion; 1.92 in tall, legend on the right |
| Main text | `fig1bshift_main_panel_sel4_grel.pdf` | 1b | the same with λ⋆₆, raw difficulty, 2PL discrimination and random only (use one of the two) |
| Main text | `fig1c_main_popsABF.pdf` | 1c | G-theory projection of G(n) and Φ(n) for random draws, populations (A), (B), (F), Main benchmarks |
| Main text | `fig5_main_CtoB.pdf` | 5 | model score distributions, bottom 50% (reference) and top 50% (measured), Main benchmarks |
| Main text | `fig3_complete_snr9.pdf` | 3 | reliability landscape of the Complete set at SNR ≥ 9 |
| Main text | `fig4_main_popsABF.pdf` | 4 | full-benchmark G and Φ by population, Main benchmarks |
| Appendix | `fig1bshift_appendix_<shift>_full_grel.pdf` | 1b | G(n) under one positive shift, every criterion, Appendix benchmarks |
| Appendix | `fig1bshift_appendix_<shift>_full_kendall.pdf` | 1b | the same for Kendall's τ with the full-benchmark ranking |
| Appendix | `fig1a_appendix_popsABF.pdf` | 1a | G(n) and Φ(n) under random item order, populations (A), (B), (F) together, Appendix benchmarks |
| Appendix | `fig2_appendix_popsABF.pdf` | 2 | SNR–R²_τ path under random item order, populations (A), (B), (F), Appendix benchmarks |
| Appendix | `fig4_appendix_popsABF.pdf` | 4 | full-benchmark G and Φ by population, Appendix benchmarks |
| Supplementary | `fig3_complete_snr100.pdf` | 3 | reliability landscape of the Complete set at SNR ≥ 100 |

`<shift>` is one of the three positive capability shifts. Items are scored on
lower-scoring models and every outcome is measured on higher-scoring ones,
because future models are expected to score higher than the ones used to
choose items:

| File code | Pair (`reference->measured`) | In words |
|---|---|---|
| `CtoB` | `bottom_half->top_half` | bottom 50% → top 50% |
| `JtoE` | `not_top_frac:0.1->top_frac:0.1` | bottom 90% → top 10% |
| `KtoF` | `not_top_n:10->top_n:10` | all but the top ten → top ten |

The captions name shifts in words; the codes appear only in file names.
`manifest.csv` records each figure's section, size, label and caption, and
`figures.tex` groups the figures by section.

## Populations

| Code | String | Models |
|---|---|---|
| (A) | `all` | every model |
| (B) | `top_half` | score above the median |
| (C) | `bottom_half` | score at or below the median |
| (E) | `top_frac:0.1` | top 10% |
| (F) | `top_n:10` | the 10 highest-scoring models |
| (J) | `not_top_frac:0.1` | the bottom 90%: every model not in (E) |
| (K) | `not_top_n:10` | the bottom N − 10: every model not in (F) |

Other population strings (`middle_half`, `random_frac:<p>:<seed>`,
`random_comp:<p>:<seed>`, `early_half` / `late_half`, ...) are listed in the
README.

**Eligibility.** A top-Y% or top-n population is used on a benchmark only if
it has more than 5 models and is at most half of the benchmark's models, so
top 10% needs at least 60 models and top n = 10 at least 20. Bottom groups,
including the complements (J) and (K), only need more than 5 models.
`population_eligible()` applies the rule. Ineligible benchmark × population
cells are left out and the caption names them: the bottom 90% → top 10% shift
drops AIR-Bench 2024, BBQ and Thai Exam, whose top 10% has 4 models.

## Design decisions

* **SNR thresholds.** The primary threshold is SNR = 9 (reliability 0.9), the
  secondary SNR = 4 (0.8); `--thresholds` changes them. On reliability axes
  the reference lines sit at G* = SNR / (1 + SNR). On the Kendall τ axis they
  sit at τ* = (2/π)·asin(√G*), about 0.795 and 0.705: the concordance between
  an n-item subset and the full benchmark when G(n) = G*. Reference lines are
  grey, solid for the primary threshold and dotted for the secondary, and are
  drawn beneath the data.
* **Item selection (type 1b).** Items are scored on the reference population
  and outcomes are measured on the measured population. Random is the mean of
  10 seeded orders at each n. The main-text panel marks 10% of each
  benchmark's items with a dotted vertical line, the budget of the shift
  summary.
* **Reliability contrasts (type 1a).** Items are added in random order (mean
  of 10 orders), so the figure shows reliability as a function of budget, not
  item selection. Colour is the population and line type the reliability
  type: relative solid, absolute dashed.
* **SNR–R² path (type 2).** τ is Kendall's τ between the models' subset and
  full-benchmark scores in the measured population. Greiner's identity gives
  ρ = sin(πτ/2), and the subset-vs-full correlation satisfies
  ρ² = G(n)/G(N) ≈ G(n) for a long benchmark. So the y axis is
  R²_τ = sin²(πτ/2), the x axis the empirical G(n) of the same subset, and the
  reference line x = y. For random orders R²_τ tracks G(n)/G(N) to within
  about 0.05 for all models.
* **Smoothing.** Curves are `geom_smooth()` fits at ggplot2's defaults on the
  log-x scale, with standard-error bands. `--lines=raw` draws the item-by-item
  curves.
* **Complete benchmarks.** Replicates, runs and prompt setups are averaged
  within each model × item cell, giving one score per cell, the unit a
  benchmark score averages over. Variance components are exact REML for
  complete matrices and `lmer` on the observed cells otherwise.
* **Format.** Times, 5.5 in wide (the text width), base font 10 pt so text
  prints at about caption size, Okabe–Ito colours each paired with a line type
  or shape, and no titles or captions inside the figures. Each method,
  population and reliability type is drawn the same way in every figure.

## Plot functions

| Type | Function | Input |
|---|---|---|
| 1a reliability contrasts | `plot_reliability_contrast()` | curves (random orders) for one or more populations |
| 1b item-selection contrasts | `plot_selection_curves()` | curves for one or more reference → measured pairs |
| 1c theoretical random draws | `plot_dstudy()` | G-study variance components |
| 2 SNR–R² path | `plot_snr_r2_path()` | curves |
| 3 reliability landscape | `plot_reliability_landscape()` | G-study proportions (Complete) |
| 4 relative–absolute dots | `plot_population_dots()` | G-studies by population |
| 5 population distributions | `plot_population_histograms()` | model scores |

`theme_benchrel()`, `scale_method()`, `scale_population()`,
`scale_reliability_type()` and `snr_reference_lines()` give them their shared
look, and `save_figure()` writes a PDF at paper size. The package also has
`plot_pair_curves()` (one criterion across several pairs) and
`plot_items_needed()` (benchmark length vs. items needed for an SNR target),
which no paper figure uses.

## Shift summary

`analysis/06_shift_summary.R` compares each criterion with random order at
10% of each benchmark's items (`--frac` changes the budget), for the three
shifts on the Appendix benchmarks. For G, Φ and Kendall's τ it reports:

* how often the criterion beats random, and its median gain;
* the cells with a gain on all three outcomes;
* the cells with no material loss (no outcome more than 0.10 below random);
* the "no fit" cells, where every measured model answers the selected items
  identically, which count as not beating random and as a material loss.

It writes `paper_figures/tables/shift_table_main.tex` (main text),
`shift_table_by_shift.tex`, `shift_table_levels.tex` and the matching CSVs.

## Checks

`check_curves()` reports, for each curve of any run, the share of prefixes
with no regular REML fit, the share with σ²_M on the boundary, any value
outside [0, 1] and, optionally, the largest difference from `lmer` on refitted
prefixes. The R interface is in the README under "Cached, modular use".
