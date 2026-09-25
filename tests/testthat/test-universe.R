test_that("email_from_universe() works", {
  vcr::local_cassette("ropensci-universe")

  info <- email_from_universe("Maëlle Salmon", universe = "maelle")

  expect_identical(
    info[["email"]],
    "maelle.salmon@yahoo.se"
  )
  expect_setequal(colnames(info), c("email", "name", "package"))

  expect_identical(
    email_from_universe("Maelle M Salmon", universe = "maelle")[["email"]],
    "maelle.salmon@yahoo.se"
  )
})
