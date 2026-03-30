# Shared utility functions

# Declare NSE variables used in dplyr/ggplot2 pipelines to silence R CMD check NOTEs
utils::globalVariables(c(
  "abandoned", "admin_type", "area_km2", "canal", "city",
  "disused", "geometry", "label", "length_km", "length_m", "name",
  "osm_id", "population", "proposed", "total_length_m"
))

#' @importFrom utils head
NULL

# Suppress only known-benign sf warnings (attribute assumptions),
# letting genuine warnings (invalid geometry, topology errors) propagate
suppress_sf_attr_warnings <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("attribute variables are assumed to be spatially constant", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}
ensure_chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)

ensure_chr_cols <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) df[miss] <- NA_character_
  dplyr::mutate(df, dplyr::across(dplyr::all_of(cols), as.character))
}

clean_admin_name <- function(x) {
  x <- gsub("^City of ", "", x)
  x <- gsub(", City of$", "", x)
  x <- gsub(", County of$", "", x)
  x <- gsub(" City$", "", x)
  x <- gsub(" Borough$", "", x)
  x <- gsub(" Council$", "", x)
  x <- gsub("upon", "on", x, ignore.case = TRUE)
  trimws(x)
}

sf_bind_rows <- function(lst) {
  lst <- purrr::compact(lst)
  if (!length(lst)) return(NULL)
  if (length(lst) == 1) return(lst[[1]])

  # Get all unique column names across all data frames
  all_cols <- unique(unlist(lapply(lst, names)))

  # Ensure all data frames have the same columns
  lst <- lapply(lst, function(df) {
    # Skip empty data frames
    if (nrow(df) == 0) return(df)

    missing_cols <- setdiff(all_cols, names(df))
    if (length(missing_cols) > 0) {
      # Add missing columns as NA, preserving sf geometry column
      for (col in missing_cols) {
        if (col != attr(df, "sf_column")) {
          df[[col]] <- NA_character_
        }
      }
    }
    # Reorder columns to match
    df[all_cols[all_cols %in% names(df)]]
  })

  # Now bind rows
  out <- lst[[1]]
  if (length(lst) > 1) {
    for (i in 2:length(lst)) {
      out <- suppress_sf_attr_warnings(rbind(out, lst[[i]]))
    }
  }
  out
}
