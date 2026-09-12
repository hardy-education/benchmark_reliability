#' @title L_{star} Novel and Efficient Estimation Guttman's Lambda 6 Inspired Item-level Components 

library(data.table)
library(fixest)
library(future)
library(future.apply)

#' Efficiently Estimate Guttman's Lambda 6 Components
#'
#' This function performs a series of regressions required for calculating 
#' a new version of Guttman's 
#' Lambda 6. For each item in a wide-format response matrix, it regresses the item 
#' on all other items, using a sophisticated fixed-effect structure which only
#' is activated through random slopes.
#'
#' The implementation is highly optimized for speed and memory efficiency by:
#' 1. Using a data.table backend.
#' 2. Avoiding the creation of a massive intermediate dataset.
#' 3. Iterating through each item and creating a smaller, temporary dataset on-the-fly.
#' 4. Leveraging the 'future' framework for parallel processing of item regressions.
#'
#' @param wide_dt A response matrix data.table (or an object coercible to one) in wide format. 
#'   Must contain a 'model' or 'id' or 'subject' column  and multiple item columns.
#' @param n_workers Integer. The number of parallel workers to use. Defaults to 
#'   `future::availableCores() - 1`.
#'
#' @return A data.table summarizing the regression results for each item, 
#'   including R-squared values, intercept estimates, and observation counts.
#'
#' @examples
#' # --- Generate realistic sample data ---
#' set.seed(123)
#' N_MODELS <- 215
#' N_ITEMS <- 300 # 
#' 
#' # Create a wide data.table
#' item_names <- paste0("item_", 1:N_ITEMS)
#' wide_data <- data.table(model = 1:N_MODELS)
#' wide_data[, (item_names) := lapply(1:N_ITEMS, \(i) rbinom(N_MODELS, 1, runif(1, 0.2, 0.8)))]
#' 
#' # --- Run the estimation ---
#' # The first run might be slower due to package loading in new sessions.
#' item_L6_estimations <- L6_est(wide_data, n_workers = 2)
#' 
#' # --- View results ---
#' print(item_L6_estimations)
#' 
#' # The overall Lambda 6 value is the mean of the R-squareds
#' overall_lambda6 <- mean(item_L6_estimations$L6, na.rm = TRUE)
#' cat("\nEstimated Guttman's Lambda 6:", lambda6, "\n")
#'
L6_est <- function(wide_dt, n_workers = NULL) {
  
  # --- 1. Setup ---
  if (!is.data.table(wide_dt)) {
    setDT(wide_dt)
  }
  
  item_cols <- setdiff(names(wide_dt), c("model","id","subject"))
  n_items <- length(item_cols)
  
  message(sprintf(
    "Starting estimation for %d items across %d models.",
    n_items, nrow(wide_dt)
  ))
  
  # Configure parallel processing
  if (is.null(n_workers)) {
    n_workers <- future::availableCores(omit = 1)
  }
  if (n_workers > 1) {
    message(sprintf("Using %d parallel workers.", n_workers))
    plan(multicore, workers = n_workers)
  } else {
    message("Using a single core (sequential processing).")
    plan(sequential)
  }
  
  # --- 2. Parallel Estimation using future_lapply ---
  # future_lapply will distribute 'item_cols' among the workers.
  
  results_list <- future_lapply(item_cols, function(target_item) {
    
    # --- Per-Item Data Preparation ---
    
    # Identify all other items
    other_items <- setdiff(item_cols, target_item)
    
    # Melt the 'other_items' columns into a long format.
    # data.table with (N_items-1) * N_models rows.
    reg_data <- melt(wide_dt, 
                     id.vars = "model", 
                     measure.vars = other_items,
                     variable.name = "itemx",
                     value.name = "otheritemscore",
                     variable.factor = FALSE) # Important for performance
    
    # Add the dependent variable ('resp') by joining back to the original data.
    reg_data[, resp := wide_dt[reg_data, on = "model", get(target_item)]]
    
    # --- feols Estimation ---
    # You can disable threading within feols since we are parallelizing at a higher level.
    # setFixest_nthreads(1) # uncomment to disable threading if desired
    
    fit <- feols(
      fml = resp ~ 1 | itemx[[otheritemscore]] + model[[otheritemscore]],
      data = reg_data,
      warn = FALSE, 
      notes = FALSE,
      verbose = 0,
      cluster = ~ model,
      panel.id = ~ model + itemx # Panel.id helps convergence
    )
    
    # --- Result Extraction ---
    # Extract coefficients and R-squared values efficiently.
    # Guttman's Lambda 6 is the average R-squared across all items.
    r2_vals <- as.list(fixest::r2(fit, type = "all"))
    ct <- coeftable(fit)
    
    # Return a single-row data.table for this item.
    data.table(
      item = target_item,
      nobs = fit$nobs,
      L6 = r2_vals$r2,

      intercept = ct["(Intercept)", "Estimate"],
      intercept_se = ct["(Intercept)", "Std. Error"]
    )
    
  }, future.seed = TRUE, future.packages = c("data.table", "fixest"))
  
  # --- 3. Aggregation and Cleanup ---
  # Combine the list of data.tables from each worker into one.
  final_results <- rbindlist(results_list)
  
  # Shut down the parallel workers
  plan(sequential) 
  
  message("Estimation complete.")
  return(final_results)
}
