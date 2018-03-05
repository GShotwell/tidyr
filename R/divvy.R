#' Split a dataframe into a list of dataframes
#'
#' `divvy`` splits a dataframe into a list of dataframes based on the splitting variables.
#'
#' @param data A dataframe
#' @param ... variables to split by, grouping variables are always included as
#' splitting variables.
#' @return A list of data frames
#' @export
#' @seealso [base::split()]
#' @examples
#' mtcars %>%
#'  dplyr::group_by(cyl) %>%
#'  divvy()
#'
#' mtcars %>%
#'  dplyr::group_by(cyl) %>%
#'  divvy(wt)
#'
#'  divvy(mtcars, cyl)
divvy <- function(data, ...) {
  UseMethod("divvy")
}

#' @export
divvy.data.frame <- function(data, ...){
  data <- dplyr::group_by(data, ..., add = TRUE)
  if (dplyr::is_grouped_df(data)) {
    return(split(data, dplyr::group_indices(data)))
  } else {
    return(list(dplyr::as_tibble(data)))
  }
}
