# Run from the repository root. Each stage skips work whose output exists;
# add FORCE=--force to recompute. WORKERS sets per-stage parallelism.
WORKERS ?= 1
FORCE ?=
R := Rscript

.PHONY: deps test metrics metrics-fast curves gstudies timing all clean \
        complete-data paper-curves paper-gstudies paper-inputs paper-figures shift-summary paper

deps:
	$(R) -e 'install.packages(c("data.table","matrixStats","pcaPP","lme4","mirt","mokken","psych","fixest","ggplot2","testthat","pkgload"))'

test:
	$(R) -e 'pkgload::load_all(quiet = TRUE); testthat::test_dir("tests/testthat")'

metrics-fast:   ## seconds: every method except 2PL, both reference populations
	$(R) analysis/01_item_metrics.R --reference=all,top_frac:0.1 --methods=no_2pl --tag=fast --workers=$(WORKERS) $(FORCE)

metrics: metrics-fast   ## adds 2PL (the slow part)
	$(R) analysis/01_item_metrics.R --reference=all,top_frac:0.1 --methods=irt_a --tag=irt --workers=$(WORKERS) $(FORCE)

curves:
	$(R) analysis/02_reliability_curves.R --reference=all,top_frac:0.1 --measured=all,top_frac:0.1 --workers=$(WORKERS) $(FORCE)

gstudies:
	$(R) analysis/03_population_gstudies.R --populations=all,top_half,top_frac:0.1,top_n:10

# ---- Paper figures and tables (docs/FIGURES.md); outputs go to paper_figures/ (git-ignored) ----
SHIFT_REFS := bottom_half,not_top_frac:0.1,not_top_n:10
RAW ?= data   ## directory with wabm_item_scores/, openeval_response_scores/, eee_item_level_filtered.csv, livebench.parquet

complete-data:   ## the 100+ Complete benchmarks -> data/complete_cells.csv.gz (Fig. 3)
	$(R) analysis/00_build_complete.R --raw=$(RAW)

paper-curves:   ## every criterion under the three positive shifts; random order in-sample for (A), (B), (F)
	$(R) analysis/01_item_metrics.R --reference=$(SHIFT_REFS) --methods=no_2pl --tag=fast --workers=$(WORKERS) $(FORCE)
	$(R) analysis/01_item_metrics.R --reference=$(SHIFT_REFS) --methods=irt_a --tag=irt --workers=$(WORKERS) $(FORCE)
	$(R) analysis/02_reliability_curves.R --reference=bottom_half --measured=top_half --workers=$(WORKERS) $(FORCE)
	$(R) analysis/02_reliability_curves.R --reference=not_top_frac:0.1 --measured=top_frac:0.1 --workers=$(WORKERS) $(FORCE)
	$(R) analysis/02_reliability_curves.R --reference=not_top_n:10 --measured=top_n:10 --workers=$(WORKERS) $(FORCE)
	$(R) analysis/01_item_metrics.R --reference=all,top_half,top_n:10 --methods=no_2pl --tag=fast --workers=$(WORKERS) $(FORCE)
	for p in all top_half top_n:10; do \
	  $(R) analysis/02_reliability_curves.R --reference=$$p --measured=$$p --methods=random --workers=$(WORKERS) $(FORCE) || exit 1; \
	done

paper-gstudies:   ## full-benchmark G-studies; the Complete set needs `make complete-data` first
	$(R) analysis/03_population_gstudies.R --benchmarks=appendix --populations=all,top_half,top_frac:0.1,top_n:10
	$(R) analysis/03_population_gstudies.R --benchmarks=complete --populations=all,top_half,top_frac:0.1,top_n:10 --workers=$(WORKERS)

paper-inputs: paper-curves paper-gstudies

paper-figures:   ## the 16 paper figures + figures.tex, manifest.csv, preview.tex
	$(R) analysis/04_paper_figures.R --workers=$(WORKERS)

shift-summary:   ## criteria vs random at 10% of items under positive shift -> paper_figures/tables/
	$(R) analysis/06_shift_summary.R

paper: paper-inputs paper-figures shift-summary

timing:
	$(R) analysis/05_timing_report.R --workers=8

all: metrics curves gstudies timing

clean:
	rm -rf results/* figures/* paper_figures/*
