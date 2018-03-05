context("divvy")

test_that("divvy method for grouped_dfs", {
  cars_grouped <- dplyr::group_by(mtcars, cyl)
  divvy_list <- divvy(cars_grouped)
  expect_is(divvy_list, "list")
  expect_is(divvy_list[[1]], "tbl_df")
  expect_equal(length(divvy_list), 3)
  expect_identical(divvy_list, divvy(mtcars, cyl))
})

test_that("divvy on an ungrouped data frame produces the right type", {
  expect_equal(divvy(mtcars), list(mtcars))
})

test_that("divvy works on empty dataframe", {
  df <- tibble(x = 1:3, y = c("B", "A", "A"))[0, ]
  expect_equal(divvy(df), list(df))
  expect_is(divvy(df, x), "list")
  expect_equal(length(divvy(df, y)), 0)
})
