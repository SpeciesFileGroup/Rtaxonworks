#' Biological associations graphs
#'
#' A BiologicalAssociationsGraph groups a set of BiologicalAssociations into
#' a named graph with an optional layout, used for visualising interaction
#' networks.
#'
#' @export
#' @template args
#' @param id (integer) access a BiologicalAssociationsGraph by ID
#' @param biological_associations_graph_id (integer, vector) filter by graph id
#' @param biological_association_id (integer, vector) filter by BiologicalAssociation id
#' @param biological_relationship_id (integer, vector) filter by BiologicalRelationship id
#' @param wkt (string) spatial filter in WKT format
#' @param geo_json (string) GeoJSON geometry for spatial filtering
#' @param radius (integer) buffer radius in meters for `geo_json`
#' @param geo_shape_id (integer, vector) shape ids for geographic filtering
#' @param geo_shape_type (string, vector) shape types (`GeographicArea` or `Gazetteer`)
#' @param geo_mode (string) geographic matching mode
#' @param extend (string, vector) extend the response (`biological_associations_biological_associations_graphs`, `notes`)
#' @return list
#' @examples
#' \dontrun{
#' tw_biological_associations_graphs()
#' }
tw_biological_associations_graphs <- function(id = NULL,
  biological_associations_graph_id = NULL, biological_association_id = NULL,
  biological_relationship_id = NULL, wkt = NULL, geo_json = NULL, radius = NULL,
  geo_shape_id = NULL, geo_shape_type = NULL, geo_mode = NULL, extend = NULL,
  csv = FALSE, token = NULL, project_token = NULL, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    biological_associations_graph_id = biological_associations_graph_id,
    biological_association_id = biological_association_id,
    biological_relationship_id = biological_relationship_id,
    wkt = wkt, geo_json = geo_json, radius = radius,
    geo_shape_id = geo_shape_id, geo_shape_type = geo_shape_type,
    geo_mode = geo_mode, extend = extend,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("biological_associations_graph_id", "biological_association_id",
    "biological_relationship_id", "geo_shape_id", "geo_shape_type", "extend")

  path <- if (is.null(id)) "/biological_associations_graphs"
          else sprintf("/biological_associations_graphs/%d", id)

  res <- tw_GET(api_base_url(), path, query = args, csv = csv,
                vector_params = vector_params, ...)
  return(res)
}

#' Biological associations graphs
#' @rdname tw_biological_associations_graphs
#' @export
tw_bag <- tw_biological_associations_graphs
