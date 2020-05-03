context("select")
require(fql)

test_that("where works", {
  d <- data.frame(x=1:3, y=c("a", "b", "c"))
  expect_equal(nrow(d %>% select(where=alist(x==1))), 1)
})

test_that("computing column works", {
  d <- data.frame(x=1:3, y=c("a", "b", "c"))
  expect_equal(grepl("z", colnames(d %>% select(what=alist(z=x^2)))), TRUE)
})

