#' @title possibly2
#' @description a purrr::possibly wrapper to generate dynamic default when failistng
#' @param .f generic function
#' @param otherwise either static out or a function, Default: NULL
#' @return possibly class function
#' @details it consistnetly works well with purrr::insistently
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname possibly2
#' @export
possibly2 <- function(.f, otherwise = NULL) {
  function(...) {
    tryCatch(
      {
        .f(...)
      },
      error = function(e) otherwise(...)
    )
  }
}
