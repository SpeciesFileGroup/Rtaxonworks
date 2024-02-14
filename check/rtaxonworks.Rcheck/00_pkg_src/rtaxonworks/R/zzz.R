cc <- function(x) Filter(Negate(is.null), x)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

as_log <- function(x) {
  if (is.null(x)) {
    x
  } else {
    if (x) "true" else "false"
  }
}

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!class(x) %in% y) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

tw_meta <- function(x) {
  meta_cols <- c("success", "limit", "total", "last", "empty")
  x$meta <- tibble::as_tibble(x[names(x) %in% meta_cols])
  for (i in seq_along(meta_cols)) x[[ meta_cols[i] ]] <- NULL
  return(x)
}

tw_list_to_df <- function(my_list) {
  df <- data.frame()
  for(i in seq_along(my_list)) {
    row_df <- as.data.frame(t(sapply(my_list[[i]], function(x) if(is.null(x)) NA else x)), stringsAsFactors = FALSE)
    df <- rbind(df, row_df)
  }
  return(df)
}

move_cols <- function(x, cols) {
  colz <- c(cols, names(x)[!names(x) %in% cols])
  x[colz[colz %in% names(x)]]
}

bind <- function(x) {
  (x <- data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE, idcol = "id")))
}

bindtbl <- function(x) tibble::as_tibble(bind(x))

handle_taxon <- function(x) {
  if (!length(x)) return(tibble::tibble())
  rmv <- c("created", "createdBy", "modified", "modifiedBy", "datasetKey",
    "id", "verbatimKey")
  for (i in rmv) x[[i]] <- NULL
  df <- cbind(x$name,
    x[names(x) %in% c("status", "parentId", "synonym", "taxon", "bareName")])
  first_cols <- c("scientificName", "rank", "id", "status")
  df <- move_cols(df, first_cols)
  return(tibble::as_tibble(df))
}

tou <- function(x) if (is.character(x)) toupper(x) else x
tol <- function(x) if (is.character(x)) tolower(x) else x

#' check if api up in examples
#' @export
#' @keywords internal
#' @return boolean. `TRUE` if http request succeeds, `FALSE` if not
tw_up <- function() {
  z <- tryCatch(tw_GET(api_base_url(), "/ping")$pong,
     error = function(e) e)
  if (inherits(z, "error")) FALSE else z
}
