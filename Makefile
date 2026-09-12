# Reproduction pipeline. Each target depends on the outputs of the previous
# stage, so `make all` runs the study end to end and `make figures` reruns only
# the plots. Stages are long: the item-metric bootstrap is the expensive one.

R := Rscript

.PHONY: all deps data item-metrics adjust gd-studies rank-stability facet figures test clean-figures help

help:
	@echo "Targets:"
	@echo "  deps            install the R packages listed in DESCRIPTION"
	@echo "  data            check that the input data are present"
	@echo "  item-metrics    stage 10: per-item reliability metrics (slow: hours)"
	@echo "  adjust          stage 20: remove draw-level fixed effects, aggregate"
	@echo "  gd-studies      stage 30: G/D studies over item budgets (slow)"
	@echo "  rank-stability  stage 40: empirical Kendall tau curves"
	@echo "  facet           stage 50: Open LLM facet G-study"
	@echo "  figures         stage 60: paper figures into figures/"
	@echo "  test            run the unit tests"
	@echo "  all             stages 10 through 60"

deps:
	$(R) -e 'if (!requireNamespace("pak", quietly=TRUE)) install.packages("pak"); pak::local_install_deps(".")'

data:
	$(R) -e 'source("R/load_all.R"); stopifnot(file.exists(file.path(BR_DATA,"aggdf.csv"))); message("Input data present.")'

item-metrics: data
	$(R) scripts/10_item_metrics.R

adjust:
	$(R) scripts/20_adjust_metrics.R

gd-studies:
	$(R) scripts/30_gd_studies.R

rank-stability:
	$(R) scripts/40_rank_stability.R

facet:
	$(R) scripts/50_facet_gstudy.R

figures:
	$(R) scripts/60_figures.R

test:
	LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 $(R) -e 'testthat::test_dir("tests/testthat")'

all: item-metrics adjust gd-studies rank-stability figures

clean-figures:
	rm -f figures/*.pdf figures/*.png
