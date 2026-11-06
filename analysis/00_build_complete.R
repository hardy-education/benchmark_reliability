# Stage 0 (optional): build the cell-level table of the Complete benchmark set
# (the 100+ benchmarks of the reliability landscape, paper Fig. 3).
#
#   Rscript analysis/00_build_complete.R --raw=/path/to/raw
#   Rscript analysis/00_build_complete.R --wabm=... --openeval=... --eee=... --livebench=...
#
# --raw is a directory holding wabm_item_scores/, openeval_response_scores/,
# eee_item_level_filtered.csv and livebench.parquet (rebuilt by the scripts in
# data_collection/). Per-source flags override single paths.
#
# Output: data/complete_cells.csv.gz (git-ignored, like all of data/)

source("analysis/_common.R")
flags <- parse_flags(defaults = list(raw = data_dir(), out = data_path("complete_cells.csv.gz")))
src <- complete_sources(flags$raw)
for (s in names(src)) if (!is.null(flags[[s]])) src[[s]] <- flags[[s]]
cells <- timed(build_complete_cells(src, flags$out))
log_msg("wrote %s in %.0fs", flags$out, attr(cells, "seconds"))
print(cells[, .(n_items = uniqueN(item), n_models = uniqueN(model), mean = round(mean(resp), 3),
                mean_obs_per_cell = round(mean(n_obs), 2)), by = .(dataset, benchmark)][order(dataset, benchmark)], nrows = 200)
