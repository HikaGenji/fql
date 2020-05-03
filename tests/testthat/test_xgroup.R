context("xgroup")
require(fql)

test_that("group by one column works", {
  x <- data.frame(name=c("a", "b"), value=1:2, row.names=c("1", "2"))
  expect_length(x %>% xgroup("name"), 2)
})


test_that("no column group return a list with one element", {
  x <- data.frame(name=c("a", "b"), value=1:2, row.names=c("1", "2"))
  expect_length(x %>% xgroup(), 1)
})