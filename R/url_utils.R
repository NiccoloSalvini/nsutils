#' @title url_exists
#' @description  it checks if a url exists or not
#' @param url string url
#' @return bool
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{http_status}}, \code{\link[httr]{GET}}
#' @rdname url_exists
#' @export
#' @importFrom httr http_status GET
url_exists <- function(url) {
  res <- tryCatch(
    {
      invisible(httr::http_status(httr::GET(url))) # , config(http_version = 0)
    },
    error = function(e) {
      invisible(FALSE)
    }
  )

  if (!is.logical(res)) {
    if (res$category == "Success") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}


# dates_pattern = "^(?:(?:31(\//|-|\.)(?:0?[13578]|1[02]))\1|(?:(?:29|30)(\//|-|\.)(?:0?[13-9]|1[0-2])\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:29(\//|-|\.)0?2\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\d|2[0-8])(\//|-|\.)(?:(?:0?[1-9])|(?:1[0-2]))\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$"

extract_date_from_url <- function(url) {
  basename(url) %>%
    stringr::str_extract(pattern = "\\d+") %>%
    return()
}

