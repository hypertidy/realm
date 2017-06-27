#' check if we are on a specific machine
we_are_home <- function() {
  set.seed(1)
  paste(sample(unlist(strsplit(Sys.info()["nodename"], "")), 10), collapse = "") == "tottiennnt"

}
