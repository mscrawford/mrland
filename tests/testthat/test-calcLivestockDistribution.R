test_that("arguments are validated before any data is read", {
  expect_error(calcLivestockDistribution(output = "bogus"), "output must be one of")
  expect_error(calcLivestockDistribution(landProxy = "bogus"), "landProxy must be one of")
  expect_error(calcLivestockDistribution(category = "bogus"), "category must be either")
})
