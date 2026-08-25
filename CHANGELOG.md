## [Unreleased]

### Added
 - Wrappers for API endpoints added to TaxonWorks since the last release: `tw_biological_associations_graphs` (alias `tw_bag`), `tw_cached_maps` (alias `tw_cm`), `tw_conveyances` (alias `tw_conv`), `tw_field_occurrences` (alias `tw_fo`), `tw_leads` (alias `tw_ld`, dichotomous keys), `tw_news`, `tw_sounds` (alias `tw_snd`), `tw_taxon_determinations` (alias `tw_td`), `tw_pingz` (deep health check), `tw_stats` (project/global statistics), `tw_activity` (recent project activity), `tw_count_valid_species` (unauthenticated citation helper)
 - `tw_GET` now falls back to returning the parsed JSON as-is when it cannot be coerced to a tibble (e.g., for `/stats`)

### Breaking
 - `csv = TRUE` is now the default for endpoints whose TaxonWorks API controller implements a CSV response format: `tw_biological_associations`, `tw_biological_relationships`, `tw_common_names`, `tw_dwc_occurrences`, `tw_otus` (list only; single-record fetch stays JSON), `tw_taxon_name_relationships`, and `tw_taxon_names`. The CSV endpoints return all matching rows and ignore `page`/`per`, so bare calls like `tw_taxon_names()` will now download the full table for the current project. Pass `csv = FALSE` to opt back into paginated JSON.
 - The CSV path now returns `list(meta, data)` with the same shape as the JSON path (previously returned a bare tibble). Scripts that treated the CSV result as a tibble directly (e.g. `nrow(res)`) will need `nrow(res$data)`.

### Added
 - A warning is emitted when `page` or `per` is explicitly passed with `csv = TRUE`, with an inline snippet for client-side trimming via `head(res$data, N)` and a note about `csv = FALSE` for paginated subsets.
 - Expand README with per-function usage documentation and parameter tables
 - Add configurable client-side timeout for CSV downloads (default 900 seconds) with a progress notice after 60 seconds; add `curl` to Imports
 - Add `any_value_attribute` parameter to `tw_sources`

### Fixed
 - `tw_collecting_events` now exposes all documented parameters (previously shadowed by an empty stub definition); adds `project_token`
 - Adjust tests to match current sandbox schema and filter behavior

### Changed
 - Bump CI actions (`actions/cache`, `actions/upload-artifact`) to v4; move Ubuntu runner to 24.04
 - Change license from NCSA to MIT

## [0.0.2] - 2024-05-08
 - Add wrappers for the remaining major TaxonWorks API endpoints (sources, citations, common names, people, identifiers, images, depictions, observations, observation matrices, data attributes, notes, tags, contents, controlled vocabulary terms, downloads, and authentication checks)
 - Add `csv = TRUE` option for bulk CSV downloads
 - Return pagination metadata via `$meta`
 - Handle vector query parameters
 - Add short function aliases (e.g., `tw_tn`, `tw_ba`)

## [0.0.1] - 2024-02-12
 - Initial scaffold with wrappers for taxon names, taxon name classifications, and taxon name relationships
 - HTTP layer built on `httr2` with `jsonlite` for response parsing
