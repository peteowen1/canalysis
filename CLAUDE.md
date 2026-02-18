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
3. **fetch_uk_cities()** - Download full administrative boundary polygons (metropolitan districts, unitary authorities, Greater London) from OSM
4. **get_city_admin_polygons()** - Enrich administrative polygons with population data from CSV and calculate areas
5. **calculate_canal_metrics()** - Compute canal length, area, density, and population metrics using spatial intersection

### Main Functions

- `rank_uk_cities_by_canals()` - High-level function that runs the complete analysis pipeline
- `plot_city_canals_map()` - Generate maps showing canals over city boundaries
- `plot_all_city_polygons()` - Generate UK-wide map of all administrative boundaries colored by area

### Key Design Patterns

- **Caching**: Uses `memoise` with disk caching for expensive OSM queries in `tempdir()/canalysis_cache`
- **Parallel Processing**: Optional parallel execution using `furrr` for metric calculation
- **Spatial Operations**: Handles CRS transformations, geometric intersections, and UTM projection for accurate area calculations
- **Error Handling**: Validates geometries using `sf::st_make_valid()`
- **Population Integration**: Matches OSM administrative boundary names to official population data with fuzzy matching for name variations
- **Administrative Classification**: Distinguishes between Metropolitan Districts, Unitary Authorities, and Greater London

### Data Flow

OSM administrative boundaries + Canal lines → Population data from CSV → Spatial intersection → Canal metrics with admin_type classification

**Key Data Sources:**
- **OpenStreetMap**: Administrative boundaries (metro districts, unitary authorities, Greater London) and canal geometries
- **CSV Population Data**: `data/uk_metropolitan_districts.csv` containing official population estimates for UK administrative areas

## File Structure

- `R/utils.R` - Core pipeline functions (setup_uk_bbox, fetch_uk_canals, fetch_uk_cities, get_city_admin_polygons, calculate_canal_metrics) and shared utilities (ensure_chr, sf_bind_rows, utm_epsg_for). Also contains legacy functions (get_admin_for_city, get_admin_for_city_cached) that are no longer used by the main pipeline.
- `R/uk_canals.R` - High-level pipeline orchestrator (rank_uk_cities_by_canals) and plotting functions (plot_city_canals_map, plot_all_city_polygons)
- `data/uk_metropolitan_districts.csv` - Population data for UK administrative areas (122 areas)
- `analysis/step_by_step_analysis.R` - Script for debugging pipeline execution step-by-step
- `analysis/analysis_helpers.R` - Helper functions for analysis scripts
- `analysis/canal-search.R` - Canal search exploration script
- `tests/testthat/` - Test suite (requires internet connectivity)
- `vignettes/` - Package documentation and examples

## Development Notes

- Tests require internet access and skip on CRAN (`skip_on_cran()`, `skip_if_offline()`)
- Analysis scripts in `analysis/` provide debugging and step-by-step execution capabilities
- Intermediate results can be cached in `analysis/cache/` for development efficiency
- Uses roxygen2 for documentation generation
- Function `setup_admin_utilities()` is deprecated - caching is now handled directly in `get_admin_for_city_cached()`

## Output Data Structure

The main analysis returns a tibble with columns: `city`, `admin_type`, `population`, `area_km2`, `length_km`, `length_mi`, `density_km_per_km2`, `population_density_per_km2`.

## Gotchas

- **NAMESPACE out of sync**: Many functions have `@export` roxygen tags but NAMESPACE only lists 2 exports. Run `devtools::document()` to regenerate.
- **CSV dual-path lookup**: `get_city_admin_polygons()` first tries `system.file("extdata", ...)` (for installed package) then falls back to `data/` (for development). The CSV lives at `data/` — it would need to be at `inst/extdata/` for the installed package path to work.
- **Legacy code in utils.R**: `get_admin_for_city()` and `get_admin_for_city_cached()` are superseded by `fetch_uk_cities()` but still present. The top-level `cache_dir` and `memoise` call at module scope may cause issues.

## Tool Permissions

- Claude Code has full read access to all files and folders within this repository directory
- No permission prompts are needed for reading any files in the canalysis project
- **NEVER run R code via Bash tool** - R segfaults frequently in this environment, use Read tool to examine code instead