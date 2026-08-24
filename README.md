rtaxonworks
====



<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/SpeciesFileGroup/rtaxonworks/workflows/R-CMD-check/badge.svg)](https://github.com/SpeciesFileGroup/rtaxonworks/actions)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/rtaxonworks)](https://github.com/r-hub/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/rtaxonworks)](https://cran.r-project.org/package=rtaxonworks)

`rtaxonworks` is an R client for the [TaxonWorks](https://taxonworks.org) API. Code follow the spirit/approach of the Gem [rcol](https://github.com/sckott/rcol), and indeed much of the wrapping utility is copied 1:1 from that repo, thanks [@sckott](https://github.com/sckott).

* TaxonWorks: https://taxonworks.org
* TaxonWorks GitHub repository: https://github.com/SpeciesFileGroup/taxonworks
* TaxonWorks API docs: https://api.taxonworks.org
* TaxonWorks documentation: https://docs.taxonworks.org

## Installation


```r
install.packages("rtaxonworks")
```

Install the development version from GitHub with:


```r
install.packages("remotes")
remotes::install_github("SpeciesFileGroup/rtaxonworks")
```

Load the package

```r
library("rtaxonworks")
```

## Authentication

Most endpoints require a project token. Set it in the global environment before calling any function:


```r
TW_PROJECT_TOKEN <- "your-project-token"
```

To discover project tokens for open (publicly accessible) projects, call `tw_projects()`:


```r
tw_projects()
```

Some endpoints additionally require a user token, which is set the same way:


```r
TW_USER_TOKEN <- "your-user-token"
```

Tokens can also be passed per call with the `token` and `project_token` arguments.

## Functions

After loading the package, the following functions are added to your environment:

```r
> ls("package:rtaxonworks")
 [1] "tw_ad"                          "tw_asserted_distributions"
 [3] "tw_ba"                          "tw_biological_associations"
 [5] "tw_biological_relationships"    "tw_both_authenticated"
 [7] "tw_br"                          "tw_ce"
 [9] "tw_citations"                   "tw_cn"
[11] "tw_co"                          "tw_collecting_events"
[13] "tw_collection_objects"          "tw_common_names"
[15] "tw_contents"                    "tw_controlled_vocabulary_terms"
[17] "tw_cvt"                         "tw_da"
[19] "tw_data_attributes"             "tw_depictions"
[21] "tw_download"                    "tw_downloads"
[23] "tw_dwc"                         "tw_dwc_occurrences"
[25] "tw_identifiers"                 "tw_images"
[27] "tw_note"                        "tw_notes"
[29] "tw_obs"                         "tw_observation_matrices"
[31] "tw_observations"                "tw_obsm"
[33] "tw_otu"                         "tw_otus"
[35] "tw_people"                      "tw_ping"
[37] "tw_project_authenticated"       "tw_projects"
[39] "tw_sources"                     "tw_tag"
[41] "tw_tags"                        "tw_taxon_name_classifications"
[43] "tw_taxon_name_relationships"    "tw_taxon_names"
[45] "tw_taxon_names_autocomplete"    "tw_tn"
[47] "tw_tn_auto"                     "tw_tnc"
[49] "tw_tnr"                         "tw_up"
[51] "tw_user_authenticated"          "tw_vocab"
```

Many functions have short aliases (e.g., `tw_tn` for `tw_taxon_names`, `tw_ba` for `tw_biological_associations`). Both forms are interchangeable.

## Return values and pagination

By default, endpoints return a list with two elements:

* `data`: a [tibble](https://tibble.tidyverse.org/) of results for the current page
* `meta`: a list of pagination information (`page`, `next_page`, `per`, `total`, `total_pages`)

Results are paginated (default `per = 50`, controlled with the `page` and `per` arguments), so retrieving a full result set requires walking pages. To avoid pagination and download the full result set in one request, pass `csv = TRUE`. This calls the endpoint's CSV variant and returns a single tibble containing all matching records:


```r
tw_taxon_names(name = "Lycorma delicatula", csv = TRUE)
```

Note that CSV downloads can be large and slow for broad queries.

## Usage


```r
library("rtaxonworks")
```

### Shared parameters

The following parameters are accepted by (almost) every function in addition to the function-specific ones documented below. They are omitted from the per-function parameter tables to reduce noise.

| Param | Type | Description |
|---|---|---|
| `csv` | boolean | if `TRUE`, download all matching records via the CSV endpoint in one request (default `FALSE`) |
| `page` | integer | offset in records (default `0`) |
| `per` | integer | maximum records per page (default `50`, maximum `10000`) |
| `token` | string | a user token; can also be set globally as `TW_USER_TOKEN` |
| `project_token` | string | a project token; can also be set globally as `TW_PROJECT_TOKEN` |
| `...` |  | additional options passed to the underlying HTTP call (`httr2`) |

### Ping

Check if the API is reachable:

```r
tw_ping()
```

`tw_ping()` (alias `tw_up()`) takes no arguments.

### Projects

List open projects and their project tokens:

```r
tw_projects()
```

`tw_projects()` takes no arguments.

### Taxon names

Search taxon names:

```r
tw_taxon_names(name = "Lycorma delicatula", validity = TRUE)
```

<details>
<summary>Parameters for <code>tw_taxon_names</code> (alias <code>tw_tn</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `ancestors` | boolean | include ancestors |
| `author` | string | filter by cached_author_year |
| `author_exact` | boolean | filter by exact match on cached_author_year |
| `authors` | boolean | filter by taxon names that have an author string present |
| `citations` | boolean | filter by taxon names with citations |
| `citation_documents` | boolean | filter by taxon names with citation documents |
| `collecting_event_id` | integer | filter by collecting event id |
| `collection_object_id` | integer | filter by collection object id |
| `combination_taxon_name_id` | integer | filter by combination taxon name id |
| `data_attribute_exact_value` | boolean | filter by exact match on data attribute value |
| `data_attribute_predicate_id` | integer | filter by data attribute predicate id |
| `data_attribute_value` | string | filter by data attribute value |
| `data_attributes` | boolean | filter by taxon names that have 1 or more data attributes |
| `descendants` | boolean | include descendants |
| `descendants_max_depth` | integer | maximum depth of descendants to include |
| `etymology` | boolean | filter by taxon names with etymology |
| `exact` | boolean | filter by exact match on name |
| `image_id` | integer | filter by taxon names associated with an image id |
| `images` | boolean | filter by taxon names with images |
| `keyword_id_and` | integer | filter by keyword id with and operator |
| `keyword_id_or` | integer | filter by keyword id with or operator |
| `leaves` | boolean | filter by names having no descendants |
| `name` | string, vector | filter by name |
| `name_exact` | boolean | filter by exact match on name |
| `note_exact` | boolean | filter by exact match on note |
| `note_text` | string | filter by note text |
| `notes` | boolean | filter by taxon names with notes |
| `nomenclature_code` | string | filter by nomenclatural code |
| `nomenclature_group` | string | filter by nomenclature group |
| `not_specified` | boolean | filter by whether the name has NOT SPECIFIED in one of the cached values |
| `origin_citation` | boolean | filter by taxon names with an origin citation |
| `original_combination` | boolean | filter by names with at least one element of the original combination |
| `otus` | boolean | filter by names that have one or more Otus |
| `parent_id` | integer | filter by parent id |
| `rank` | string | filter by rank class (e.g., `NomenclaturalRank::Iczn::SpeciesGroup::Species`) |
| `tags` | boolean | filter by taxon names with tags |
| `taxon_name_author_id_or` | boolean |  |
| `taxon_name_id` | integer | filter by taxon name id |
| `taxon_name_relationship` | string | filter by taxon name relationship |
| `taxon_name_relationship_type` | string | filter by taxon name relationship type (e.g., `TaxonNameRelationship::Iczn::Invalidating`) |
| `taxon_name_type` | string | filter by taxon name type (e.g., `Protonym`, `Combination`, `Hybrid`) |
| `type_metadata` | boolean | filter by taxon names that are linked to type specimen material |
| `validity` | boolean | filter by taxon names that are valid |
| `year` | integer | filter by authority year in the cached_author_year field |
| `year_end` | integer | filter by cached_author_year less than or equal to this value |
| `year_start` | integer | filter by cached_author_year greater than or equal to this value |

</details>

Autocomplete a taxon name:

```r
tw_taxon_names_autocomplete(term = "Lycorma")
```

<details>
<summary>Parameters for <code>tw_taxon_names_autocomplete</code> (alias <code>tw_tn_auto</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `term` | string | filter by search term |
| `exact` | boolean | filter by exact match on name |
| `no_leaves` | boolean | filter by names having no descendants |
| `nomenclature_group` | string, vector | filter by nomenclature group (e.g., `Family`, `Genus`, `Species`) |
| `parent_id` | integer, vector | filter by parent id |
| `project_id` | integer | filter by project id |
| `type` | string, vector | filter by type (e.g., `Protonym`, `Combination`) |
| `valid` | boolean | filter by valid or invalid names |

</details>

### Taxon name classifications


```r
tw_taxon_name_classifications()
```

<details>
<summary>Parameters for <code>tw_taxon_name_classifications</code> (alias <code>tw_tnc</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `taxon_name_id` | integer, vector | filter by taxon name id |
| `taxon_name_classification_type` | string, vector | filter by taxon name classification type (e.g., `TaxonNameClassification::Iczn::Available`) |
| `taxon_name_classification_set` | string, vector | filter by taxon name classification set (`validating`, `invalidating`, `exceptions`) |

</details>

### Taxon name relationships


```r
tw_taxon_name_relationships()
```

<details>
<summary>Parameters for <code>tw_taxon_name_relationships</code> (alias <code>tw_tnr</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `object_taxon_name_id` | integer, vector | filter by object taxon name id |
| `subject_taxon_name_id` | integer, vector | filter by subject taxon name id |
| `taxon_name_id` | integer, vector | filter by taxon name id on both subject and object |
| `taxon_name_relationship_set` | string | filter by relationship set (`synonym`, `classification`, `status`, and (soon) `misspelling`) |
| `taxon_name_relationship_type` | string, vector | filter by relationship type (e.g., `TaxonNameRelationship::Iczn::Invalidating`) |

</details>

### OTUs

Get OTUs:

```r
tw_otus()
```

Get a single OTU by ID:

```r
tw_otus(id = 1)
```

Get an OTU subresource (e.g., Darwin Core inventory):

```r
tw_otus(id = 1, subresource = "inventory/dwc")
```

<details>
<summary>Parameters for <code>tw_otus</code> (alias <code>tw_otu</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `id` | integer | access an OTU by ID |
| `subresource` | string | access an API subresource (e.g., `inventory/dwc`) |
| `asserted_distribution_ids` | string | filter by asserted distribution ids |
| `biological_association_ids` | string | filter by biological association ids |
| `citations` | boolean | filter by otus with citations |
| `citations_documents` | boolean | filter by otus with citation documents |
| `data_attribute_exact_value` | boolean | require exact match on data attribute value |
| `data_attribute_predicate_id` | integer | filter by data attribute predicate id |
| `data_attribute_value` | string | filter by data attribute value |
| `data_attributes` | boolean | filter by otus with data attributes |
| `data_attributes_attributes` | string | filter by data attributes attributes |
| `image_id` | integer | filter by image id |
| `images` | boolean | filter by otus with images |
| `keyword_id_and` | integer | filter by keyword id with and operator |
| `keyword_id_or` | integer | filter by keyword id with or operator |
| `name` | string | filter by name |
| `note_exact` | boolean | require exact match on note text |
| `note_text` | string | filter by note text |
| `notes` | boolean | filter by otus with notes |
| `origin_citations` | boolean | filter by otus with origin citations |
| `otu_id` | integer | filter by otu id |
| `tags` | boolean | filter by otus with tags |
| `taxon_name_classification_id` | string | filter by taxon name classification ids |
| `taxon_name_id` | integer, vector | filter by taxon name id |
| `taxon_name_relationship_id` | string, vector | filter by taxon name relationship ids |

</details>

### Asserted distributions


```r
tw_asserted_distributions()
```

<details>
<summary>Parameters for <code>tw_asserted_distributions</code> (alias <code>tw_ad</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `asserted_distribution_id` | integer, vector | filter by asserted distribution id |
| `descendants` | boolean | include descendants |
| `geo_json` | boolean | filter by geojson |
| `geographic_area_id` | integer, vector | filter by geographic area id |
| `geographic_area_mode` | boolean |  |
| `otu_id` | integer, vector | filter by otu id |
| `presence` | boolean | filter by presence |
| `radius` | integer | filter by meters around shape boundary |
| `taxon_name_id` | integer, vector | filter by taxon name id |
| `wkt` | string | filter by well known text |
| `citations` | boolean | filter by asserted distributions with citations |
| `citation_documents` | boolean | filter by asserted distributions with citation documents |
| `origin_citation` | boolean | filter by asserted distributions with an origin citation |
| `data_attribute_exact_value` | boolean | require exact match on data attribute value |
| `data_attribute_predicate_id` | integer, vector | filter by data attribute predicate id |
| `data_attribute_value` | string, vector | filter by data attribute value |
| `data_attributes` | boolean | filter by asserted distributions with data attributes |
| `identifier` | string | filter by identifier |
| `identifier_end` | string | filter by identifier end |
| `identifier_exact` | boolean | filter by exact match on identifier |
| `identifier_start` | string | filter by identifier start |
| `identifier_type` | string, vector | filter by identifier class type (e.g., `Identifier::Local::CatalogNumber`) |
| `identifiers` | boolean | filter by asserted distributions with identifiers |
| `local_identifiers` | boolean | filter by asserted distributions with local identifiers |
| `match_identifiers_delimiter` | string | the delimiter used to separate identifiers |
| `match_identifiers_type` | string | filter by internal or external identifier type |
| `namespace_id` | integer | filter by namespace id |
| `keyword_id_and` | integer, vector | filter by keyword id with and operator |
| `keyword_id_or` | integer, vector | filter by keyword id with or operator |
| `tags` | boolean | filter by asserted distributions with tags |

</details>

### Biological associations

Get biological associations:

```r
tw_biological_associations()
```

Page through simple biological associations:

```r
page <- 1
per <- 500
res <- tw_ba(subresource = "simple", page = page, per = per)
results <- res$data
while (page < res$meta$total_pages) {
  page <- page + 1
  res <- tw_ba(subresource = "simple", page = page, per = per)
  results <- rbind(results, res$data)
}
```

<details>
<summary>Parameters for <code>tw_biological_associations</code> (alias <code>tw_ba</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `subresource` | string | access API subresource (e.g., `subresource="simple"` for the simple biological associations table) |
| `biological_association_id` | integer | filter by biological association id |
| `biological_associations_graph_id` | integer | filter by biological associations graph id |
| `biological_relationship_id` | integer | filter by biological relationship id |
| `collecting_event_id` | integer | filter by collecting event id |
| `collection_object_id` | integer | filter by collection object id |
| `descendants` | boolean | include descendants |
| `exclude_taxon_name_relationship` | boolean | exclude taxon name relationship |
| `geo_json` | boolean | filter by geojson |
| `geographic_area_id` | integer | filter by geographic area id |
| `geographic_area_mode` | boolean |  |
| `object_biological_property_id` | integer | filter by object biological property id |
| `object_object_global_id` | integer | filter by object object global id |
| `object_taxon_name_id` | integer | filter by object taxon name id |
| `object_type` | string | filter by object type (`Otu`, `CollectionObject`) |
| `otu_id` | integer | filter by otu id |
| `subject_biological_property_id` | integer | filter by subject biological property id |
| `subject_object_global_id` | integer | filter by subject object global id |
| `subject_taxon_name_id` | integer | filter by subject taxon name id |
| `subject_type` | string | filter by subject type (`Otu`, `CollectionObject`) |
| `taxon_name_id` | integer | filter by taxon name id |
| `taxon_name_id_mode` | boolean |  |
| `wkt` | string | filter by well known text shape to search for biological associations |
| `citations` | boolean | filter by biological associations with citations |
| `citation_documents` | boolean | filter by biological associations with citation documents |
| `origin_citation` | boolean | filter by biological associations with an origin citation |
| `identifier` | string | filter by biological association identifier |
| `identifier_end` | string | filter by biological association identifier end |
| `identifier_exact` | boolean | filter by exact match on biological association identifier |
| `identifier_start` | string | filter by biological association identifier start |
| `identifier_type` | string, vector | filter by identifier class type (e.g., `Identifier::Local::CatalogNumber`) |
| `identifiers` | boolean | filter by biological associations with identifiers |
| `local_identifiers` | boolean | filter by biological associations with local identifiers |
| `match_identifiers_delimiter` | string | the delimiter used to separate identifiers |
| `match_identifiers_type` | string | filter by internal or external identifier type |
| `namespace_id` | integer | filter by namespace id |
| `note_exact` | boolean | filter by exact match on note |
| `note_text` | string | filter by note text |
| `notes` | boolean | filter by biological associations with notes |
| `keyword_id_and` | integer | filter by keyword id with and operator |
| `keyword_id_or` | integer | filter by keyword id with or operator |
| `tags` | boolean | filter by biological associations with tags |

</details>

### Biological relationships


```r
tw_biological_relationships()
```

`tw_biological_relationships()` (alias `tw_br`) accepts only the shared parameters.

### Collecting events


```r
tw_collecting_events()
```

<details>
<summary>Parameters for <code>tw_collecting_events</code> (alias <code>tw_ce</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `citation_documents` | boolean | filter by collecting events with citation documents |
| `citations` | boolean | filter by collecting events with citations |
| `collection_objects` | boolean | filter by collecting events with collection objects |
| `collector_id` | integer, vector | filter by collector id |
| `collector_id_or` | integer, vector | filter by collector id (OR) |
| `collectors` | boolean | filter by collecting events with collectors |
| `data_attribute_exact_value` | string | filter by data attribute exact value |
| `data_attribute_predicate_id` | integer, vector | filter by data attribute predicate id |
| `data_attribute_value` | string, vector | filter by data attribute value |
| `data_attributes` | boolean | filter by collecting events with data attributes |
| `document_label` | string |  |
| `elevation_precision` | numeric |  |
| `end_date` | string | filter by end date |
| `end_date_day` | integer |  |
| `end_date_month` | integer |  |
| `end_date_year` | integer |  |
| `field_notes` | string |  |
| `formation` | string |  |
| `geographic_area` | boolean | filter by collecting events with geographic area |
| `geographic_area_id` | integer, vector | filter by geographic area id |
| `geographic_area_mode` | boolean |  |
| `georeferences` | boolean | filter by collecting events with georeferences |
| `group` | string |  |
| `identifier` | string | filter by cached identifier |
| `identifier_end` | string | filter by identifier end |
| `identifier_exact` | boolean | filter by exact match on identifier |
| `identifier_start` | string | filter by identifier start |
| `identifier_type` | string, vector | filter by identifier type |
| `identifiers` | boolean | filter by collecting events with identifiers |
| `image_id` | integer, vector | filter by image id |
| `images` | boolean | filter by collecting events with images |
| `in_labels` | string | wildcard-wrapped search against any label |
| `in_verbatim_locality` | string | wildcard-wrapped search against verbatim locality |
| `keyword_id_and` | integer, vector | filter by keyword id (AND) |
| `keyword_id_or` | integer, vector | filter by keyword id (OR) |
| `lithology` | string |  |
| `local_identifiers` | boolean | filter by collecting events with local identifiers |
| `match_identifiers_delimiter` | string | the delimiter for the `match_identifiers` parameter |
| `match_identifiers_type` | string | the type of match to perform on the identifiers (`internal` or `identifier`) |
| `maximum_elevation` | numeric | filter by maximum elevation in meters |
| `md5_of_verbatim_label` | string |  |
| `md5_verbatim_label` | string | filter by md5 hash of verbatim label |
| `member` | string |  |
| `meta_prioritize_geographic_area` | string |  |
| `min_ma` | numeric |  |
| `minimum_elevation` | numeric | filter by minimum elevation in meters |
| `namespace_id` | integer | filter by namespace id |
| `note_exact` | boolean | filter by exact match on note |
| `note_text` | string | filter by note text |
| `notes` | boolean | filter by collecting events with notes |
| `origin_citation` | boolean | filter by collecting events with origin citations |
| `otu_id` | integer, vector | filter by otu id |
| `print_label` | string |  |
| `radius` | integer | filter by radius in meters |
| `recent` | boolean | filter by recent collecting events |
| `start_date` | string | filter by start date |
| `start_date_day` | integer |  |
| `start_date_month` | integer |  |
| `start_date_year` | integer |  |
| `tags` | boolean | filter by collecting events with tags |
| `time_end_hour` | integer |  |
| `time_end_minute` | integer |  |
| `time_end_second` | integer |  |
| `time_start_hour` | integer |  |
| `time_start_minute` | integer |  |
| `time_start_second` | integer |  |
| `use_max` | boolean |  |
| `use_min` | boolean |  |
| `verbatim_collectors` | string |  |
| `verbatim_datum` | string |  |
| `verbatim_date` | string |  |
| `verbatim_elevation` | string |  |
| `verbatim_geolocation_uncertainty` | numeric |  |
| `verbatim_habitat` | string |  |
| `verbatim_label` | string |  |
| `verbatim_latitude` | numeric |  |
| `verbatim_locality` | string |  |
| `verbatim_longitude` | numeric |  |
| `verbatim_method` | string |  |
| `verbatim_trip_identifier` | string |  |
| `wkt` | string | filter by well-known text describing the search area |

</details>

### Collection objects


```r
tw_collection_objects()
```

<details>
<summary>Parameters for <code>tw_collection_objects</code> (alias <code>tw_co</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `biocuration_class_id` | integer, vector | filter by biocuration class id |
| `biological_association_id` | integer, vector | filter by biological association id |
| `biological_associations` | boolean | filter by collection objects with/without biological associations |
| `biological_relationship_id` | integer, vector | filter by biological relationship id |
| `buffered_collecting_event` | string | filter by buffered collecting event text |
| `buffered_determinations` | string | filter by buffered determinations text |
| `buffered_other_labels` | string | filter by buffered other labels text |
| `citations` | boolean | filter by collection objects with/without citations |
| `citation_documents` | boolean | filter by collection objects with/without citation documents |
| `collecting_event` | string | filter by collection objects with/without collecting events |
| `collecting_event_id` | integer, vector | filter by collecting event id |
| `collection_object_id` | integer, vector | filter by collection object id |
| `collection_object_type` | string | filter by collection object type (`Lot`, `RangedLot`, `Specimen`) |
| `collectors` | boolean | filter by collection objects with/without collectors |
| `current_determinations` | boolean | filter by collection objects with/without current determinations |
| `current_repository` | boolean | filter by collection objects with/without current repository |
| `current_repository_id` | integer, vector | filter by current repository id |
| `descendants` | boolean | include collection objects for OTU descendants |
| `dates` | boolean | filter by collection objects with/without dates |
| `determiner_id` | integer, vector | filter by determiner id (People IDs) |
| `determiner_id_or` | boolean | use an or operator instead of an and operator for determiner_id |
| `determiners` | boolean | filter by collection objects with/without determiners |
| `dwc_indexed` | boolean | include metadata for Darwin Core indexed collection objects |
| `end_date` | string | filter by end date |
| `exact_buffered_collecting_event` | string | filter by exact buffered collecting event text |
| `exact_buffered_determinations` | string | filter by exact buffered determinations text |
| `exact_buffered_other_labels` | string | filter by exact buffered other labels text |
| `extract_id` | integer, vector | filter by extract id |
| `geojson` | string | filter by geojson search area |
| `geographic_area` | boolean | filter by collection objects with/without geographic area |
| `geographic_area_id` | integer, vector | filter by geographic area id |
| `georeferences` | boolean | filter by collection objects with/without georeferences |
| `image_id` | integer, vector | filter by image id |
| `identifier` | string | filter by identifier associated with the collection object |
| `identifier_end` | string | filter by the last part of an identifier associated with the collection object |
| `identifier_exact` | string | filter by the exact identifier associated with the collection object |
| `identifiers` | boolean | filter by collection objects with/without identifiers |
| `in_labels` | string | wildcard search in all related collecting event labels |
| `in_verbatim_locality` | string | search in verbatim locality in related collecting events |
| `is_type` | string, vector | filter by types |
| `keyword_id_and` | string, vector | filter by keyword id with an and operator |
| `keyword_id_or` | string, vector | filter by keyword id with an or operator |
| `loaned` | boolean | filter by collection objects with/without loans |
| `md5_verbatim_label` | string | filter by md5 hash of verbatim label |
| `namespace_id` | integer | include metadata for associated identifier by namespace id |
| `never_loaned` | boolean | filter by collection objects that were never loaned |
| `note_exact` | string | search for the exact text in notes |
| `notes` | boolean | filter by collection objects with/without notes |
| `on_loan` | boolean | filter by collection objects currently on loan |
| `origin_citation` | boolean | filter by collection objects with/without origin citation |
| `otu_id` | integer, vector | filter by otu id |
| `partial_overlap_dates` | boolean | allow date overlaps |
| `preparation_type` | boolean | filter by collection objects with/without preparation type |
| `preparation_type_id` | integer, vector | filter by preparation type id |
| `radius` | integer | radius around location |
| `repository` | boolean | filter by collection objects with/without repository |
| `repository_id` | integer, vector | filter by repository id |
| `sled_image_id` | integer, vector | filter by sled image id |
| `start_date` | string | filter by start date |
| `tags` | boolean | filter by collection objects with/without tags |
| `taxon_determination_id` | integer, vector | filter by taxon determination id |
| `taxon_determinations` | boolean | filter by collection objects with/without taxon determinations |
| `taxon_name_id` | integer, vector | filter by taxon name id |
| `type_material` | boolean | filter by collection objects with/without type material |
| `type_specimen_taxon_name_id` | integer | filter by taxon name id for type specimens |
| `user_date_end` | string | filter by user date end |
| `user_date_start` | string | filter by user date start |
| `user_id` | integer | filter by user id |
| `user_target` | string | target created or updated user date |
| `validity` | boolean | filter by collection objects with only valid ancestors |
| `with_buffered_collecting_event` | boolean | filter by collection objects with/without buffered collecting event |
| `with_buffered_determinations` | boolean | filter by collection objects with/without buffered determinations |
| `with_buffered_other_labels` | boolean | filter by collection objects with/without buffered other labels |
| `wkt` | string | filter by well-known text describing the search area |

</details>

### Darwin Core occurrences


```r
tw_dwc_occurrences()
```

<details>
<summary>Parameters for <code>tw_dwc_occurrences</code> (alias <code>tw_dwc</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `dwc_occurrence_id` | integer, vector | filter by Darwin Core occurrence ID |
| `dwc_occurrence_object_id` | integer, vector | filter by Darwin Core occurrence object ID |
| `dwc_occurrence_object_type` | string, vector | filter by Darwin Core occurrence object type |

</details>

### Sources


```r
tw_sources(author = "Smith")
```

<details>
<summary>Parameters for <code>tw_sources</code></summary>

| Param | Type | Description |
|---|---|---|
| `author` | string | filter by author name |
| `exact_author` | boolean | filter by exact match on the author parameter |
| `author_id` | integer, vector | filter by one or more author IDs |
| `author_id_or` | boolean | filter by author ID with/without OR operator |
| `bibtex_type` | string, vector | filter by BibTeX type (e.g., `article`, `book`, `inproceedings`) |
| `citation_object_type` | string | filter by citation object type (`AssertedDistribution`, `BiologicalAssociation`, `TaxonName`, `TaxonNameRelationship`, `TypeMaterial`) |
| `citations` | boolean | filter by sources with/without citations |
| `citations_on_otus` | boolean | used with `taxon_name_id` to filter by sources with/without citations on the Otu |
| `documents` | boolean | filter by sources with/without documents attached |
| `empty` | string, vector | filter by fields that should be empty (e.g., `c("journal", "year")`) |
| `extend` | string, vector | extend the response with additional information (e.g., `documents`, `roles`) |
| `not_empty` | string, vector | filter by fields that should not be empty |
| `global_identifiers` | boolean | filter by sources with/without global identifiers |
| `identifier` | string | filter by identifier |
| `identifier_exact` | boolean | filter by exact match on the identifier parameter |
| `identifier_start` | integer | filter by identifier range start |
| `identifier_end` | integer | filter by identifier range end |
| `ids` | integer, vector | filter by one or more source IDs (internal TaxonWorks IDs) |
| `in_project` | boolean | filter by sources in project |
| `local_identifiers` | boolean | filter by sources with/without local identifiers |
| `match_identifiers` | string | match identifiers with a delimited string of identifiers |
| `match_identifiers_delimiter` | string | the delimiter for `match_identifiers` |
| `match_identifiers_type` | string | the type of identifiers to match (`internal` or `identifier`) |
| `namespace_id` | integer | filter by namespace ID |
| `nomenclature` | boolean | filter by sources with/without nomenclature |
| `notes` | boolean | filter by sources with/without notes |
| `roles` | boolean | filter by sources with/without roles |
| `title` | string | search on the title |
| `exact_title` | boolean | filter by exact match on the title parameter |
| `query_term` | string | search on the full citation |
| `serial` | boolean | filter by sources with/without a serial |
| `serial_id` | integer, vector | filter by one or more serial IDs |
| `source_type` | string | filter by source type (`Source::Bibtex`, `Source::Human`, or `Source::Verbatim`) |
| `tags` | boolean | filter by sources with/without tags |
| `topic_id` | integer, vector | filter by topic ID |
| `user_id` | integer | filter by user ID |
| `user_target` | string | use with `user_id` to filter on `created` or `updated` records |
| `user_date_start` | string | filter by user date start (e.g., `2019-01-01`) |
| `user_date_end` | string | filter by user date end (e.g., `2019-01-20`) |
| `venn` | string | filter with json or browser URL of a query |
| `venn_mode` | string | the mode of the venn filter: `exclude` (a), `intersect` (ab), `inverse exclude` (b) |
| `with_doi` | boolean | filter by sources with/without a DOI |
| `with_title` | boolean | filter by sources with/without BibTeX title |
| `year_start` | integer | filter by year start |
| `year_end` | integer | filter by year end |
| `any_value_attribute` | string, vector | filter by any wildcard attribute (any of the fields below) |
| `wildcard_attribute` | string, vector | filter by wildcard attribute (any of the fields below) |
| `abstract` | string | filter by wildcard match on abstract |
| `address` | string | filter by wildcard match on address |
| `annote` | string | filter by wildcard match on annote |
| `booktitle` | string | filter by wildcard match on booktitle |
| `chapter` | string | filter by wildcard match on chapter |
| `copyright` | string | filter by wildcard match on copyright |
| `crossref` | string | filter by wildcard match on crossref |
| `doi` | string | filter by wildcard match on DOI |
| `edition` | string | filter by wildcard match on edition |
| `editor` | string | filter by wildcard match on editor |
| `howpublished` | string | filter by wildcard match on howpublished |
| `institution` | string | filter by wildcard match on institution |
| `journal` | string | filter by wildcard match on journal |
| `key` | string | filter by wildcard match on key |
| `language` | string | filter by wildcard match on language |
| `language_id` | integer | filter by language ID |
| `month` | string | filter by wildcard match on month |
| `note` | string | filter by wildcard match on note |
| `number` | string | filter by wildcard match on number |
| `organization` | string | filter by wildcard match on organization |
| `pages` | string | filter by wildcard match on pages |
| `publisher` | string | filter by wildcard match on publisher |
| `school` | string | filter by wildcard match on school |
| `series` | string | filter by wildcard match on series |
| `stated_year` | string | filter by wildcard match on stated_year |
| `translator` | string | filter by wildcard match on translator |
| `type` | string | filter by wildcard match on type |
| `url` | string | filter by wildcard match on URL |
| `verbatim` | string | filter by wildcard match on verbatim |
| `verbatim_contents` | string | filter by wildcard match on verbatim_contents |
| `verbatim_keywords` | string | filter by wildcard match on verbatim_keywords |
| `volume` | string | filter by wildcard match on volume |
| `year_suffix` | string | filter by wildcard match on year_suffix |

</details>

### Citations


```r
tw_citations()
```

<details>
<summary>Parameters for <code>tw_citations</code></summary>

| Param | Type | Description |
|---|---|---|
| `citation_id` | integer, vector | filter by citation ID |
| `citation_object_id` | integer, vector | filter by citation object ID |
| `citation_object_type` | string, vector | filter by citation object type (`AssertedDistribution`, `BiologicalAssociation`, `NomenclatureStatus`, `Otu`, `TaxonName`, `TaxonNameRelationship`, `TypeMaterial`) |
| `is_original` | boolean | filter by original citation |
| `model_id` | string | filter by model ID |
| `polymorphic` | boolean | filter by polymorphic citation |
| `source_id` | integer | filter by source ID |

</details>

### Common names


```r
tw_common_names()
```

<details>
<summary>Parameters for <code>tw_common_names</code> (alias <code>tw_cn</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `common_name_id` | integer, vector | filter by common name id |
| `geographic_area_id` | integer | filter by geographic area id |
| `language_id` | integer | filter by language id |
| `name` | string | filter by name |
| `otu_id` | integer, vector | filter by otu id |

</details>

### People


```r
tw_people(last_name = "Smith")
```

<details>
<summary>Parameters for <code>tw_people</code></summary>

| Param | Type | Description |
|---|---|---|
| `active_after_year` | integer | filter by active after year |
| `active_before_year` | integer | filter by active before year |
| `born_after_year` | integer | filter by born after year |
| `born_before_year` | integer | filter by born before year |
| `died_after_year` | integer | filter by died after year |
| `died_before_year` | integer | filter by died before year |
| `first_name` | string | filter by first name |
| `identifier` | string | filter by identifier |
| `identifier_end` | string | filter by identifier end |
| `identifier_exact` | boolean | filter by exact match on identifier |
| `identifier_start` | string | filter by identifier start |
| `last_name` | string | filter by last name |
| `last_name_starts_with` | string | filter by last name starts with |
| `person_wildcard` | vector | filter by person wildcard |
| `role` | vector, string | filter by role |
| `project_id` | integer | filter by project id |
| `name` | string | filter by name |
| `tags` | boolean | filter by people with tags |
| `user_date_end` | string | filter by user date end |
| `user_date_start` | string | filter by user date start |
| `data_attribute_exact_value` | boolean | require exact match on data attribute value |
| `data_attribute_predicate_id` | integer, vector | filter by data attribute predicate id |
| `data_attribute_value` | string, vector | filter by data attribute value |
| `data_attributes` | boolean | filter by people with data attributes |
| `note_exact` | boolean | require exact match on note text |
| `note_text` | string | filter by note text |
| `notes` | boolean | filter by people with notes |
| `keyword_id_and` | integer, vector | filter by keyword id with and operator |
| `keyword_id_or` | integer, vector | filter by keyword id with or operator |

</details>

### Identifiers


```r
tw_identifiers(identifier_id = c(1375290, 1375299, 1375300))
```

<details>
<summary>Parameters for <code>tw_identifiers</code></summary>

| Param | Type | Description |
|---|---|---|
| `identifier` | string, vector | filter by one or more identifiers |
| `identifier_id` | integer, vector | filter by one or more identifier IDs (internal TaxonWorks IDs) |
| `identifier_object_id` | integer, vector | filter by one or more object IDs |
| `identifier_object_type` | string, vector | filter by one or more object types (e.g., `Otu`, `CollectionObject`, `TaxonName`) |
| `namespace_id` | integer, vector | filter by one or more namespace IDs |
| `namespace_name` | string | filter by a namespace name |
| `namespace_short_name` | string | filter by a namespace short name |
| `object_global_id` | string | filter by an object global ID |
| `query_string` | string | filter by a query string on the cached identifier (wildcards allowed) |
| `type` | string | filter by identifier type (e.g., `Identifier::Local::CatalogNumber`) |
| `polymorphic` | boolean | filter by polymorphic identifiers |

</details>

### Images


```r
tw_images()
```

<details>
<summary>Parameters for <code>tw_images</code></summary>

| Param | Type | Description |
|---|---|---|
| `biocuration_class_id` | integer, vector | filter by one or more biocuration class IDs |
| `collection_object_id` | integer, vector | filter by one or more collection object IDs |
| `collection_object_scope` | string | filter by collection object scope (e.g., `CollectionObject`, `Observations`, `CollectingEvent`) |
| `depictions` | boolean | filter by images that are depictions |
| `depiction_object_type` | string | filter by depiction object type |
| `image_id` | integer, vector | filter by one or more image IDs |
| `otu_id` | integer, vector | filter by one or more OTU IDs |
| `citations` | boolean | filter by images that have citations |
| `citation_documents` | boolean | filter by images that have citation documents |
| `origin_citation` | boolean | filter by images that have origin citations |
| `keyword_id_and` | integer, vector | filter by one or more keyword IDs (AND) |
| `keyword_id_or` | integer, vector | filter by one or more keyword IDs (OR) |
| `tags` | boolean | filter by images that have tags |

</details>

### Depictions


```r
tw_depictions()
```

<details>
<summary>Parameters for <code>tw_depictions</code></summary>

| Param | Type | Description |
|---|---|---|
| `depiction_id` | integer, vector | filter by depiction id |
| `depiction_object_id` | integer, vector | filter by depiction object id |
| `depiction_object_type` | string, vector | filter by depiction object type |
| `image_id` | integer, vector | filter by image id |

</details>

### Observations


```r
tw_observations()
```

<details>
<summary>Parameters for <code>tw_observations</code> (alias <code>tw_obs</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `character_state_id` | integer | filter by character state ID |
| `collection_object_id` | integer, vector | filter by collection object ID |
| `descendants` | boolean | filter by descendants |
| `descriptor_id` | integer, vector | filter by descriptor ID |
| `observation_id` | integer, vector | filter by observation ID |
| `observation_matrix_id` | integer, vector | filter by observation matrix ID |
| `observation_object_global_id` | string | filter by observation object global ID |
| `observation_object_type` | string, vector | filter by observation object type (`Extract`, `CollectionObject`, `Otu`) |
| `observation_type` | string, vector | filter by observation type (`Observation::Qualitative`, `Observation::Presence`, `Observation::Continuous`, `Observation::Sample`, `Observation::Media`, `Observation::FreeText`) |
| `otu_id` | integer, vector | filter by OTU ID |
| `taxon_name_id` | integer, vector | filter by taxon name ID |
| `citations` | boolean | filter by observations that have citations |
| `citation_documents` | boolean | filter by observations that have citation documents |
| `data_depictions` | boolean | filter by observations that have data depictions |
| `depictions` | boolean | filter by observations that have depictions |
| `identifiers` | boolean | filter by observations that have identifiers |
| `notes` | boolean | filter by observations that have notes |
| `note_exact` | boolean | filter by exact match on note |
| `note_text` | string | filter by note text |
| `protocols` | boolean | filter by observations that have protocols |
| `tags` | boolean | filter by observations that have tags |

</details>

### Observation matrices


```r
tw_observation_matrices()
```

<details>
<summary>Parameters for <code>tw_observation_matrices</code> (alias <code>tw_obsm</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `observation_matrix_id` | integer, vector | filter by observation matrix id |

</details>

### Data attributes


```r
tw_data_attributes()
```

<details>
<summary>Parameters for <code>tw_data_attributes</code> (alias <code>tw_da</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `attribute_subject_id` | integer | filter by attribute subject ID |
| `attribute_subject_type` | string | filter by attribute subject type (e.g., `TaxonName`) |
| `controlled_vocabulary_term_id` | integer, vector | filter by controlled vocabulary term ID |
| `data_attribute_id` | integer, vector | filter by data attribute ID |
| `type` | string | filter by data attribute type |
| `import_predicate` | string | filter by import predicate |
| `value` | string | filter by data attribute value |

</details>

### Notes


```r
tw_notes()
```

<details>
<summary>Parameters for <code>tw_notes</code> (alias <code>tw_note</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `note_id` | integer, vector | filter by note id |
| `note_object_id` | integer, vector | filter by note object id |
| `note_object_type` | string, vector | filter by note object type |
| `text` | string | filter by note text |
| `polymorphic` | boolean | filter by polymorphic notes |

</details>

### Tags


```r
tw_tags()
```

<details>
<summary>Parameters for <code>tw_tags</code> (alias <code>tw_tag</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `tag_id` | integer, vector | filter by tag id |
| `tag_object_id` | integer, vector | filter by tag object id |
| `tag_object_type` | string, vector | filter by tag object type |

</details>

### Contents


```r
tw_contents()
```

<details>
<summary>Parameters for <code>tw_contents</code></summary>

| Param | Type | Description |
|---|---|---|
| `citations` | boolean | filter by contents that have citations |
| `citation_documents` | boolean | filter by contents that have citation documents |
| `origin_citation` | boolean | filter by contents that have an origin citation |
| `exact` | boolean | require exact match on search term |
| `image_id` | integer, vector | filter by image id |
| `images` | boolean | filter by contents that have images |
| `otu_id` | integer, vector | filter by otu id |
| `text` | string | filter by text |
| `topic_id` | integer, vector | filter by topic id |

</details>

### Controlled vocabulary terms


```r
tw_controlled_vocabulary_terms(type = "Keyword")
```

<details>
<summary>Parameters for <code>tw_controlled_vocabulary_terms</code> (aliases <code>tw_cvt</code>, <code>tw_vocab</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `type` | string | filter by type (`BiocurationClass`, `BiologicalProperty`, `Keyword`, `Predicate`, `Topic`) |

</details>

### Downloads


```r
tw_downloads()
```

<details>
<summary>Parameters for <code>tw_downloads</code> (alias <code>tw_download</code>)</summary>

| Param | Type | Description |
|---|---|---|
| `download_id` | integer, vector | filter by download id |
| `download_type` | string, vector | filter by download type |

</details>

### Authentication checks


```r
tw_user_authenticated(token = "your-user-token")
tw_project_authenticated(project_token = "your-project-token")
tw_both_authenticated(token = "your-user-token", project_token = "your-project-token")
```

These helpers accept only the shared parameters and return the result of the corresponding authentication check.

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/SpeciesFileGroup/rtaxonworks. This project is intended to be a safe, welcoming space for collaboration, and contributors are expected to adhere to the [code of conduct](https://github.com/SpeciesFileGroup/rtaxonworks/blob/main/CODE_OF_CONDUCT.md).

## Development

Please read the [CONTRIBUTING guide](CONTRIBUTING.md) for instructions on how to install the development version.

## License

The gem is available as open source under the terms of the [MIT license](https://github.com/SpeciesFileGroup/rtaxonworks/blob/main/LICENSE.txt). You can learn more about the MIT license on [Wikipedia](https://en.wikipedia.org/wiki/MIT_License) and compare it with other open source licenses at the [Open Source Initiative](https://opensource.org/license/mit/).

## Code of Conduct

Everyone interacting in the rtaxonworks project's codebases, issue trackers, chat rooms and mailing lists is expected to follow the [code of conduct](https://github.com/SpeciesFileGroup/rtaxonworks/blob/main/CODE_OF_CONDUCT.md).
