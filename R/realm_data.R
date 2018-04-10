#' Test files for realm
#'
#' @return data frame
#' @export
#' @importFrom tibble tibble
#' @examples
#'  realm_test_files()
#'
realm_test_files <- function() {
  dp <- system.file("extdata", "ice_realm", "partitioning.64", package = "realm")
  tibble::tibble(fullname = list.files(dp, full.names = TRUE))
}


#' Read structure files
#'
#' @param x structure file set
#' @param ... unused
#'
#' @return realm
#' @export
#'
#' @examples
#' library(dplyr)
#' files <- realm_test_files() %>% dplyr::filter(grepl("part\\.11\\.", fullname))
read_realm <- function(x, ...) {
  UseMethod("read_realm")
}
#' @name read_realm
#' @export
#' @importFrom dplyr %>% filter mutate bind_rows
#' @importFrom readr read_table2
read_realm.tbl_df <- function(x, ...) {
  structure(list(elements = read_elements(x %>% dplyr::filter(grepl("elements$", basename(fullname)))),
       boundary = read_boundaries(x %>% dplyr::filter(grepl("boundary", basename(fullname)))),
       nodes = read_nodes(x %>% dplyr::filter(grepl("nodes", basename(fullname))))),
       class = "realm")
}

read_elements <- function(x) {
  dplyr::bind_rows(lapply(x$fullname, read_elements_file))
}
read_elements_file <- function(x) {
  readr::read_table(x, col_names = FALSE) %>%
    #rename(v0 = X4, v1 = X5, v2 = X6) %>%
    dplyr::mutate(filename = basename(x))
}
read_boundaries <- function(x) {
  dplyr::bind_rows(lapply(x$fullname, read_boundary_file))
}
read_boundary_file <- function(x) {
  readr::read_table2(x, col_names = FALSE) %>% dplyr::mutate(filename = basename(x))
}
read_nodes <- function(x) {
 dplyr::bind_rows(lapply(x$fullname, read_node_file) )
}

read_node_file <- function(x) {
 readr::read_table2(x, col_names = c("node", "group", "X", "Y", "i")) %>%
    dplyr::mutate(filename = basename(x))
}
