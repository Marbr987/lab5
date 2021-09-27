context("get_coordinates")

test_that("get_coordinates rejects errounous input", {
  expect_error(get_coordinates(43.2))
  expect_error(get_coordinates(c("Berlin", "Stockholm")))
  expect_error(get_coordinates("lksfdj"))
})

test_that("get_coordinates returns correct type", {
  expect_true(length(get_coordinates("Berlin", "Stockholm")) == 2)
  expect_true(typeof(get_coordinates("Linköping", "Norrköping")) == "double")
})

test_that("get_coordinates returns correct values", {
  expect_equal(get_coordinates("Berlin"), c(52.3, 13.2))
  expect_equal(get_coordinates("Linköping"), c(58.41, 15.62 ))
})