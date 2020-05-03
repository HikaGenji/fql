context("update")
require(fql)

test_that("where works", {
  d <- data.frame(x=1:3, y=c("a", "b", "c"))
  expect_equal(nrow(d %>% update(where=alist(x==1))), 3)
})

test_that("computing column works", {
  d <- data.frame(x=1:3, y=c("a", "b", "c"))
  expect_equal(any(grepl("z", colnames(d %>% update(what=alist(z=x^2))))), TRUE)
})

