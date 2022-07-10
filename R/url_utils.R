#' @title url_exists2
#' @description it checks if an url exists or not
#' @param x str url
#' @param non_2xx_return_value exclude 2xx errors, Default: FALSE
#' @param quiet non verbose out, Default: FALSE
#' @param ... further
#' @return bool
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{HEAD}}, \code{\link[httr]{GET}}, \code{\link[httr]{status_code}}
#' @rdname url_exists2
#' @export
#' @importFrom httr HEAD GET status_code
url_exists2 <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {

  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })

  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.

  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)

        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }

  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }

  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)

  # Try HEAD first since it's lightweight
  res <- sHEAD(x, httr::config(http_version = 0), ...)

  if (is.null(res$result) ||
      ((httr::status_code(res$result) %/% 200) != 1)) {

    res <- sGET(x, httr::config(http_version = 0), ...)

    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors

    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }

    return(TRUE)

  } else {
    return(TRUE)
  }

}




# dates_pattern = "^(?:(?:31(\//|-|\.)(?:0?[13578]|1[02]))\1|(?:(?:29|30)(\//|-|\.)(?:0?[13-9]|1[0-2])\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:29(\//|-|\.)0?2\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\d|2[0-8])(\//|-|\.)(?:(?:0?[1-9])|(?:1[0-2]))\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$"

extract_date_from_url <- function(url) {
  basename(url) %>%
    stringr::str_extract(pattern = "\\d+") %>%
    return()
}

