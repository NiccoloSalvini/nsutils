#' @title fuzzy_date_inner_join
#' @description join dates by a time buffer
#' @param df1 first candidate to be joined
#' @param df2 second candidate to be joined
#' @param by Date class joining column
#' @param time_buffer_type time buffer i.e. months/ years/ minutes
#' @param time_buffer numeric
#' @param ... PARAM_DESCRIPTION
#' @return inner joined df
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(tibble)
#'   library(lubridate)
#'   library(dplyr)
#'   df_1 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:27:10", 34.8014080, 103.8499800,
#'     "2018-09-30 04:55:51", 43.3367432, 44.158934,
#'     "2018-02-28 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'
#'   df_2 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:57:10", 34.8014080, 103.8999800,
#'     "2018-09-30 04:55:51", 43.3367432, 48.158934,
#'     "2018-02-27 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'   fuzzy_date_inner_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 1
#'   )
#'
#'   fuzzy_date_inner_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 3
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{period}}, \code{\link[lubridate]{character(0)}}
#'  \code{\link[fuzzyjoin]{difference_join}}
#' @rdname fuzzy_date_inner_join
#' @export
#' @importFrom lubridate minutes hours days years
#' @importFrom fuzzyjoin difference_inner_join
fuzzy_date_inner_join <- function(df1, df2, by, time_buffer, ...) {

  # match.arg(time_buffer, choices = c("minutes", "hours","days", "months", "years"))

  fun <- switch(stringr::str_extract(time_buffer, "[a-z]+"),
    minutes  = function(...) lubridate::minutes(x = readr::parse_number(time_buffer)),
    hours  = function(...) lubridate::hours(x = readr::parse_number(time_buffer)),
    days = function(...) lubridate::days(x = readr::parse_number(time_buffer)),
    weeks = function(...) lubridate::weeks(x = readr::parse_(time_buffer)),
    months = function(...) base::months(x = readr::parse_number(time_buffer)),
    years = function(...) lubridate::years(x = readr::parse_number(time_buffer)),
    stop("Unknown time, check sppelling", time_buffer, call. = FALSE)
  )

  fuzzyjoin::difference_inner_join(
    df1, df2,
    by,
    max_dist = fun(time_buffer), ...
  )
}

#' @title fuzzy_date_left_join
#' @description join dates by a time buffer
#' @param df1 first candidate to be joined
#' @param df2 second candidate to be joined
#' @param by Date class joining column
#' @param time_buffer_type time buffer i.e. months/ years/ minutes
#' @param time_buffer numeric
#' @param ... PARAM_DESCRIPTION
#' @return left joined df
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(tibble)
#'   library(lubridate)
#'   df_1 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:27:10", 34.8014080, 103.8499800,
#'     "2018-09-30 04:55:51", 43.3367432, 44.158934,
#'     "2018-02-28 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'
#'   df_2 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:57:10", 34.8014080, 103.8999800,
#'     "2018-09-30 04:55:51", 43.3367432, 48.158934,
#'     "2018-02-27 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'   fuzzy_date_left_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 1
#'   )
#'
#'   fuzzy_date_left_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 3
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{period}}, \code{\link[lubridate]{character(0)}}
#'  \code{\link[fuzzyjoin]{difference_join}}
#' @rdname fuzzy_date_inner_join
#' @export
#' @importFrom lubridate minutes hours days years
#' @importFrom fuzzyjoin difference_left_join
fuzzy_date_left_join <- function(df1, df2, by, time_buffer, ...) {

  # getOption("lubridate.week.start", 7)

  # match.arg(time_buffer_type, choices = c("minutes", "hours","days", "months", "years"))

  fun <- switch(stringr::str_extract(time_buffer, "[a-z]+"),
    minutes  = function(...) lubridate::minutes(x = readr::parse_number(time_buffer)),
    hours  = function(...) lubridate::hours(x = readr::parse_number(time_buffer)),
    days = function(...) lubridate::days(x = readr::parse_number(time_buffer)),
    weeks = function(...) lubridate::weeks(x = readr::parse_(time_buffer)),
    months = function(...) base::months(x = readr::parse_number(time_buffer)),
    years = function(...) lubridate::years(x = readr::parse_number(time_buffer)),
    stop("Unknown time: ", time_buffer, call. = FALSE)
  )
  fuzzyjoin::difference_left_join(
    df1, df2,
    by,
    max_dist = fun(time_buffer), ...
  )
}

#' @title fuzzy_date_right_join
#' @description join dates by a time buffer
#' @param df1 first candidate to be joined
#' @param df2 second candidate to be joined
#' @param by Date class joining column
#' @param time_buffer_type time buffer i.e. months/ years/ minutes
#' @param time_buffer numeric
#' @param ... PARAM_DESCRIPTION
#' @return right joined df
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(tibble)
#'   library(lubridate)
#'   df_1 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:27:10", 34.8014080, 103.8499800,
#'     "2018-09-30 04:55:51", 43.3367432, 44.158934,
#'     "2018-02-28 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'
#'   df_2 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:57:10", 34.8014080, 103.8999800,
#'     "2018-09-30 04:55:51", 43.3367432, 48.158934,
#'     "2018-02-27 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'   fuzzy_date_right_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 1
#'   )
#'
#'   fuzzy_date_right_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 3
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{period}}, \code{\link[lubridate]{character(0)}}
#'  \code{\link[fuzzyjoin]{difference_join}}
#' @rdname fuzzy_date_inner_join
#' @export
#' @importFrom lubridate minutes hours days years
#' @importFrom fuzzyjoin difference_right_join
fuzzy_date_right_join <- function(df1, df2, by, time_buffer, ...) {

  # match.arg(time_buffer_type, choices = c("minutes", "hours","days", "months", "years"))

  fun <- switch(stringr::str_extract(time_buffer, "[a-z]+"),
    minutes  = function(...) lubridate::minutes(x = readr::parse_number(time_buffer)),
    hours  = function(...) lubridate::hours(x = readr::parse_number(time_buffer)),
    days = function(...) lubridate::days(x = readr::parse_number(time_buffer)),
    weeks = function(...) lubridate::weeks(x = readr::parse_(time_buffer)),
    months = function(...) base::months(x = readr::parse_number(time_buffer)),
    years = function(...) lubridate::years(x = readr::parse_number(time_buffer)),
    stop("Unknown time, check sppelling", time_buffer, call. = FALSE)
  )

  fuzzyjoin::difference_right_join(
    df1, df2,
    by,
    max_dist = fun(time_buffer), ...
  )
}


#' @title fuzzy_date_semi_join
#' @description join dates by a time buffer
#' @param df1 first candidate to be joined
#' @param df2 second candidate to be joined
#' @param by Date class joining column
#' @param time_buffer_type time buffer i.e. months/ years/ minutes
#' @param time_buffer numeric
#' @param ... PARAM_DESCRIPTION
#' @return semi joined df
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(tibble)
#'   library(lubridate)
#'   df_1 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:27:10", 34.8014080, 103.8499800,
#'     "2018-09-30 04:55:51", 43.3367432, 44.158934,
#'     "2018-02-28 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'
#'   df_2 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:57:10", 34.8014080, 103.8999800,
#'     "2018-09-30 04:55:51", 43.3367432, 48.158934,
#'     "2018-02-27 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'   fuzzy_date_semi_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 1
#'   )
#'
#'   fuzzy_date_semi_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 3
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{period}}, \code{\link[lubridate]{character(0)}}
#'  \code{\link[fuzzyjoin]{difference_join}}
#' @rdname fuzzy_date_inner_join
#' @export
#' @importFrom lubridate minutes hours days years
#' @importFrom fuzzyjoin difference_semi_join
fuzzy_date_semi_join <- function(df1, df2, by, time_buffer, ...) {

  # match.arg(time_buffer_type, choices = c("minutes", "hours","days", "months", "years"))

  fun <- switch(stringr::str_extract(time_buffer, "[a-z]+"),
    minutes  = function(...) lubridate::minutes(x = readr::parse_number(time_buffer)),
    hours  = function(...) lubridate::hours(x = readr::parse_number(time_buffer)),
    days = function(...) lubridate::days(x = readr::parse_number(time_buffer)),
    weeks = function(...) lubridate::weeks(x = readr::parse_(time_buffer)),
    months = function(...) base::months(x = readr::parse_number(time_buffer)),
    years = function(...) lubridate::years(x = readr::parse_number(time_buffer)),
    stop("Unknown time, check sppelling", time_buffer, call. = FALSE)
  )
  fuzzyjoin::difference_semi_join(
    df1, df2,
    by,
    max_dist = fun(time_buffer), ...
  )
}

#' @title fuzzy_date_anti_join
#' @description join dates by a time buffer
#' @param df1 first candidate to be joined
#' @param df2 second candidate to be joined
#' @param by Date class joining column
#' @param time_buffer_type time buffer i.e. months/ years/ minutes
#' @param time_buffer numeric
#' @param ... PARAM_DESCRIPTION
#' @return anti joined df
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(tibble)
#'   library(lubridate)
#'   df_1 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:27:10", 34.8014080, 103.8499800,
#'     "2018-09-30 04:55:51", 43.3367432, 44.158934,
#'     "2018-02-28 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'
#'   df_2 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:57:10", 34.8014080, 103.8999800,
#'     "2018-09-30 04:55:51", 43.3367432, 48.158934,
#'     "2018-02-27 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'   fuzzy_date_anti_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 1
#'   )
#'
#'   fuzzy_date_anti_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 3
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{period}}, \code{\link[lubridate]{character(0)}}
#'  \code{\link[fuzzyjoin]{difference_join}}
#' @rdname fuzzy_date_inner_join
#' @export
#' @importFrom lubridate minutes hours days years
#' @importFrom fuzzyjoin difference_anti_join
fuzzy_date_anti_join <- function(df1, df2, by, time_buffer, ...) {

  # match.arg(time_buffer_type, choices = c("minutes", "hours","days", "months", "years"))


  fun <- switch(stringr::str_extract(time_buffer, "[a-z]+"),
    minutes  = function(...) lubridate::minutes(x = readr::parse_number(time_buffer)),
    hours  = function(...) lubridate::hours(x = readr::parse_number(time_buffer)),
    days = function(...) lubridate::days(x = readr::parse_number(time_buffer)),
    weeks = function(...) lubridate::weeks(x = readr::parse_(time_buffer)),
    months = function(...) base::months(x = readr::parse_number(time_buffer)),
    years = function(...) lubridate::years(x = readr::parse_number(time_buffer)),
    stop("Unknown time, check sppelling", time_buffer, call. = FALSE)
  )
  fuzzyjoin::difference_anti_join(
    df1, df2,
    by,
    max_dist = fun(time_buffer), ...
  )
}



#' @title fuzzy_date_full_join
#' @description join dates by a time buffer
#' @param df1 first candidate to be joined
#' @param df2 second candidate to be joined
#' @param by Date class joining column
#' @param time_buffer_type time buffer i.e. months/ years/ minutes
#' @param time_buffer numeric
#' @param ... PARAM_DESCRIPTION
#' @return full joined df
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(tibble)
#'   library(lubridate)
#'   df_1 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:27:10", 34.8014080, 103.8499800,
#'     "2018-09-30 04:55:51", 43.3367432, 44.158934,
#'     "2018-02-28 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'
#'   df_2 <- tribble(
#'     ~datetime, ~latitude, ~longitude,
#'     "2018-10-01 08:57:10", 34.8014080, 103.8999800,
#'     "2018-09-30 04:55:51", 43.3367432, 48.158934,
#'     "2018-02-27 17:03:27", 37.0399910, 115.6672080
#'   ) %>%
#'     mutate(datetime = ymd_hms(datetime))
#'
#'   fuzzy_date_full_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 1
#'   )
#'
#'   fuzzy_date_full_join(df_1, df_2,
#'     by = "datetime",
#'     time_buffer_type = "days",
#'     time_buffer = 3
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{period}}, \code{\link[lubridate]{character(0)}}
#'  \code{\link[fuzzyjoin]{difference_join}}
#' @rdname fuzzy_date_inner_join
#' @export
#' @importFrom lubridate minutes hours days years
#' @importFrom fuzzyjoin difference_full_join
fuzzy_date_full_join <- function(df1, df2, by, time_buffer, ...) {

  # match.arg(time_buffer_type, choices = c("minutes", "hours","days", "months", "years"))

  fun <- switch(stringr::str_extract(time_buffer, "[a-z]+"),
    minutes  = function(...) lubridate::minutes(x = readr::parse_number(time_buffer)),
    hours  = function(...) lubridate::hours(x = readr::parse_number(time_buffer)),
    days = function(...) lubridate::days(x = readr::parse_number(time_buffer)),
    weeks = function(...) lubridate::weeks(x = readr::parse_(time_buffer)),
    months = function(...) base::months(x = readr::parse_number(time_buffer)),
    years = function(...) lubridate::years(x = readr::parse_number(time_buffer)),
    stop("Unknown time, check sppelling", time_buffer, call. = FALSE)
  )

  fuzzyjoin::difference_full_join(
    df1, df2,
    by,
    max_dist = fun(time_buffer), ...
  )
}
