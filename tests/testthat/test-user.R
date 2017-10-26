context("test-user.R")

test_that("apiAccount", {
  checkmate::expect_class(apiAccount("31726802b8274ac60ae5526b9fb54703"),
                          classes = c("apiAccount"))
})


test_that("create user", {
  checkmate::expect_class(apiUser(
    "dshurick",
    apiAccount("31726802b8274ac60ae5526b9fb54703")
  ),
  classes = c("apiUser"))
})
