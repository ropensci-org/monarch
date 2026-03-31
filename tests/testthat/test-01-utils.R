test_that("masto2user()", {
  expect <- "@steffilazerte@fosstodon.org"
  expect_equal(masto2user("https://fosstodon.org/@steffilazerte"), expect)
  expect_equal(masto2user("steffi"), "steffi")
  expect_equal(masto2user("@steffilazerte@fosstodon.org"), expect)
  expect_equal(masto2user(NA), NA)
})
