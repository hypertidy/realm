context("basic-read")
skip_if_not(we_are_home())
files <- realm_test_files()
test_that("finding files works", {

  files %>% expect_s3_class("tbl_df") %>% expect_named("fullname")
}

          )
test_that("reading files works", {
  x <- read_realm(files %>% filter(grepl("part\\.11", fullname)))
  library(dplyr)
  xy <- x$nodes %>% select(X, Y, node)
  plot(xy$X, xy$Y, pch = ".")
  m <- as.matrix(xy[, 1:2])
  e <- as.matrix(x$elements %>% select(v0, v1, v2))
  for (i in seq_len(nrow(x$elements))) {
    polypath(m[match(e[i, ], xy$node), ])
  }
})
