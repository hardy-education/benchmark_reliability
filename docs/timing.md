# Run-time estimates (Apple M1 Max, 10 cores, 64 GB, R 4.4)

Every figure below comes from a measured run of this repository's code
(`analysis/05_timing_report.R`). Per-job times were recorded on a single core
while two 2PL processes shared the machine, so they are slightly conservative.
"8 workers" schedules those same per-benchmark jobs longest-first onto 8 cores
(`--workers=8`). Starting R adds about 1–2 s per script.

## 1. Item selection values (full item set of the reference population, no bootstrap)

| Reference population | Benchmarks | Methods | 1 core | 8 workers |
|---|---|---|---|---|
| All models | 18 (`aggdf`) | with 2PL | **19.8 min** | **3.7 min** |
| All models | 18 | without 2PL | 2.9 s | 1.9 s |
| All models | 4 (bbh, arc, gpqa, mmlu_pro) | with 2PL | 3.4 min | 2.4 min |
| All models | 4 | without 2PL | 2.5 s | 1.9 s |
| Top 10% | 18 | with 2PL | 7.8 min | 1.6 min |
| Top 10% | 18 | without 2PL | 1.6 s | 1.1 s |
| Top 10% | 4 | with 2PL | 2.9 min | 1.6 min |
| Top 10% | 4 | without 2PL | 1.4 s | 1.1 s |

Run both reference populations in one call and the cost is the sum of the two
rows.

* **λ⋆₆, difficulty, corrected inter-item correlation, Hᵢ, random:** at most
  1 s per benchmark (MMLU-Pro, 429 × 12,032). These use exact closed forms; see
  `closed_forms.md`.
* **2PL:** the only expensive step. The slowest fits are legalbench (221 s),
  arc_challenge (144 s), and wikifact (127 s) with all models, and gpqa (93 s)
  with the top 10%. Many fits stop at the 2,000-EM-cycle cap rather than
  converging. That happened for 11 of 18 benchmarks with all models and 11 of
  18 with the top 10%; see `results/timing/item_metrics_by_benchmark.csv`. The
  2PL times are therefore bounded by `ncycles`. Raising the cap so those fits
  converge would make them slower, and with 4–9 models in the top 10% of the
  HELM benchmarks the 2PL is not identified anyway.

For comparison, the legacy estimators on the same data:

| Legacy step | Measured |
|---|---|
| λ⋆₆ by `feols`, one regression per item, MMLU-Pro all models | 0.87 s/item → about 2.9 h per benchmark |
| `lmer` G-study per prefix, MMLU-Pro all models | 10.6 s at n = 1,000; 148 s at n = 12,032 |

## 2. Empirical variance decomposition as each item is added

A fresh REML decomposition (G, Φ, and Kendall τ) at every n = 2…N, for each
method's order plus 10 random orders. The time does not depend on the
reference population.

| Measured population | Benchmarks | Orderings | 1 core | 8 workers |
|---|---|---|---|---|
| All models | 18 | 15 (with 2PL) | 33 s | 13 s |
| All models | 18 | 14 (without 2PL) | 31 s | 12 s |
| All models | 4 | 15 | 21 s | 13 s |
| All models | 4 | 14 | 20 s | 12 s |
| Top 10% | 18 | 15 | 19 s | 7 s |
| Top 10% | 18 | 14 | 18 s | 7 s |
| Top 10% | 4 | 15 | 11 s | 7 s |
| Top 10% | 4 | 14 | 10 s | 7 s |

The largest job is MMLU-Pro with all models: 0.7 s per ordering for 12,031
decompositions, and 11 s for the whole job including writing the results. The
legacy approach (one `lmer` per n) would need an estimated ~250 CPU-hours per
ordering on MMLU-Pro alone.

## End to end

All 18 benchmarks; both reference populations, with 2PL; all four
reference × measured combinations; figures included:
**about 30 min on one core, or 6–7 min with `make all WORKERS=8`**. 2PL
accounts for nearly all of it. Without 2PL the whole study takes under
2 minutes.
