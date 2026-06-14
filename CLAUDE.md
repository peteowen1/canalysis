# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`canalysis` is an R package for analyzing UK canal networks using OpenStreetMap data. It ranks UK cities by canal length, area coverage, and canal density within administrative boundaries.

## Key Commands

### Development
- `devtools::load_all()` - Load package for development
- `devtools::document()` - Generate documentation from roxygen comments
- `devtools::build_readme()` - Render README.Rmd to README.md

### Testing
- `devtools::test()` - Run all tests
- `testthat::test_check("canalysis")` - Run tests via testthat

### Package Building
- `devtools::check()` - Run R CMD check
- `devtools::build()` - Build source package
- `devtools::install()` - Install package locally

## Architecture

The package follows a functional pipeline architecture with 5 main steps:

1. **setup_uk_bbox()** - Configure Overpass API and get UK bounding box
2. **fetch_uk_canals()** - Download canal geometries from OSM
3. **fetch_uk_cities()** - Download full administrative boundary polygons (metropolitan districts, unitary authorities, combined authorities, Greater London) from OSM
4. **get_city_admin_polygons()** - Enrich administrative polygons with population data from CSV and calculate areas
5. **calculate_canal_metrics()** - Compute canal length, area, density, and population metrics using spatial intersection

### Main Functions

- `rank_uk_cities_by_canals()` - High-level function that runs the complete analysis pipeline
- `calculate_canal_segment_metrics()` - Per-city segment count, connected components, length distribution
- `plot_city_canals_map()` - Generate static ggplot maps showing canals over city boundaries
- `plot_all_city_polygons()` - Generate UK-wide map of all administrative boundaries colored by area
- `leaflet_city_canals()` - Interactive leaflet map with canal name/length popups (requires leaflet)
- `plot_canal_ranking()` - Horizontal bar chart ranking top cities by any metric

### Key Design Patterns

- **Caching**: Uses `memoise` with persistent disk caching in `tools::R_user_dir("canalysis", "cache")`
- **Vectorized Spatial Operations**: Uses single `st_intersection()` and `st_area()` calls on entire sf objects with UTM 30N (EPSG:32630) for accurate UK-wide calculations
- **Error Handling**: Validates geometries using `sf::st_make_valid()`
- **Population Integration**: Matches OSM administrative boundary names to official population data via `clean_admin_name()` normalization + fuzzy matching (adist ≤ 4, shared 4-char substring guard)
- **Optional Dependencies**: `leaflet` is in Suggests (not Imports) for interactive maps; checked at runtime with `requireNamespace()`
- **Administrative Classification**: Distinguishes between Metropolitan Districts, Unitary Authorities, and Combined Authorities (including Greater London)

### Data Flow

OSM administrative boundaries + Canal lines → Population data from CSV → Spatial intersection → Canal metrics with admin_type classification

**Key Data Sources:**
- **OpenStreetMap**: Administrative boundaries (metro districts, unitary authorities, Greater London) and canal geometries
- **CSV Population Data**: `inst/extdata/uk_metropolitan_districts.csv` containing official population estimates for UK administrative areas

## File Structure

- `R/utils.R` - Shared utilities (suppress_sf_attr_warnings, ensure_chr, ensure_chr_cols, sf_bind_rows, utm_epsg_for)
- `R/fetch.R` - OSM data acquisition (clear_cache, setup_uk_bbox, fetch_uk_canals, fetch_uk_cities, get_city_admin_polygons)
- `R/metrics.R` - Computation and orchestration (calculate_canal_metrics, calculate_canal_segment_metrics, rank_uk_cities_by_canals)
- `R/plot.R` - Visualisation (plot_city_canals_map, plot_all_city_polygons, leaflet_city_canals, plot_canal_ranking)
- `inst/extdata/uk_metropolitan_districts.csv` - Population data for UK administrative areas (151 areas including metropolitan districts, unitary authorities, metropolitan counties, and traditional counties)
- `analysis/step_by_step_analysis.R` - Script for debugging pipeline execution step-by-step
- `analysis/canal-search.R` - Load cached intermediate data and explore results
- `analysis/profile_pipeline.R` - Profile each pipeline step to find bottlenecks
- `analysis/run_check.R` - Run `devtools::check()` for the package
- `tests/testthat/` - Test suite (online tests skip with `skip_if_offline()`, offline tests run everywhere)
- `vignettes/getting-started.Rmd` - Showcase-style walkthrough of the analysis pipeline

## Development Notes

- Tests require internet access and skip on CRAN (`skip_on_cran()`, `skip_if_offline()`)
- Analysis scripts in `analysis/` provide debugging and step-by-step execution capabilities
- Intermediate results can be cached in `analysis/cache/` for development efficiency
- Uses roxygen2 for documentation generation

## Output Data Structure

The main analysis returns a tibble with columns: `city`, `admin_type`, `population`, `area_km2`, `length_km`, `length_mi`, `density_km_per_km2`, `population_density_per_km2`.

## Gotchas

- **CSV dual-path lookup**: `get_city_admin_polygons()` first tries `system.file("extdata", ...)` (for installed package) then falls back to `inst/extdata/` (for development). Both paths now resolve to the same file.

## Analysis Scripts Conventions

- **No function definitions** in `analysis/` scripts — all reusable functions belong in `R/`. Analysis scripts are for interactive workflows only (sourcing, exploring, profiling).
- **Always use `devtools::load_all()`**, never `library(canalysis)`, in analysis scripts.
- **RStudio document outline**: Every analysis script must use `# Section Name ----` headers for navigability in the RStudio outline pane.