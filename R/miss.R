#' Deal with missing values
#'
#' Check if there are any missing values.
#' Return total and percentage of missing values per column.
#' Delete missing values and return complete data set.
#'
#' @param x a data frame.
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' Use airquality data
#' library(datasets)
#' miss(airquality)
miss <- function(x) {
    Any_missing <- anyNA(x)
    Total_missing <- sum(is.na(x))
    Missing_per_column <- sapply(x, function(x)sum(is.na(x)))
    Percentage_of_miss <- function(x){sum(is.na(x))/length(x)*100}
    Miss_per_column <- apply(x, 2, Percentage_of_miss)
    Complete_data <- x[complete.cases(x),]
    write.csv(Complete_data, file = "Complete_data.csv")
    list(Any_missing = Any_missing,
         Total_missing = Total_missing, Missing_per_column,
         Percentage_missing = Miss_per_column,
         Complete_data = Complete_data)
}

