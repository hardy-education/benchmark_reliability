# ==============================================================================
# load_all.R -- source every module, in dependency order.
#
# Every script in scripts/ starts with:
#
#     source(here::here("R", "load_all.R"))
#
# The numeric prefixes encode the dependency order, so the sort below is the
# load order. Nothing in R/ has side effects beyond defining functions and
# constants -- no data loading, no `future::plan()`, no fitting -- so sourcing
# this file is cheap and idempotent. That separation is deliberate: the original
# scripts interleaved definitions with 200-line driver loops, which meant you
# could not obtain the functions without also launching the analysis.
# ==============================================================================

local({
  module_dir <- if (requireNamespace("here", quietly = TRUE)) {
    here::here("R")
  } else {
    "R"
  }
  modules <- sort(list.files(module_dir, pattern = "^[0-9]{2}.*\\.R$",
                             full.names = TRUE))
  for (m in modules) source(m)
  invisible(modules)
})
