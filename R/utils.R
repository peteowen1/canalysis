# Shared utility functions
ensure_chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)

ensure_chr_cols <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) df[miss] <- NA_character_
  dplyr::mutate(df, dplyr::across(dplyr::all_of(cols), as.character))
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
      out <- suppressWarnings(rbind(out, lst[[i]]))
    }
  }
  out
}

utm_epsg_for <- function(sf_poly) {
  ctd  <- sf::st_centroid(sf::st_union(sf_poly))
  lon  <- as.numeric(sf::st_coordinates(ctd)[1])
  zone <- floor((lon + 180) / 6) + 1
  32600 + zone
}
