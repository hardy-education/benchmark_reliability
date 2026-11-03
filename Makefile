# Run from the repository root. Each stage skips work whose output exists;
# add FORCE=--force to recompute. WORKERS sets per-stage parallelism.
WORKERS ?= 1
FORCE ?=
R := Rscript

.PHONY: deps test metrics metrics-fast curves gstudies figures paper-figures timing all clean

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

paper-figures:
	$(R) analysis/06_paper_figures.R

figures:
	$(R) analysis/04_figures.R --benchmarks=core4
	$(R) analysis/04_figures.R

timing:
	$(R) analysis/05_timing_report.R --workers=8

all: metrics curves gstudies figures paper-figures timing

clean:
	rm -rf results/* figures/*
