#' @title scale_y_continuous_euro
#' @description scales y var and adds € suffix
#' @param ... inherited
#' @param prefix inherited, Default: ''
#' @param suffix inherited, Default: ''
#' @return scale_y_continuous_euro with € suffix
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   txhousing %>%
#'     filter(city == "El Paso") %>%
#'     group_by(year) %>%
#'     summarise(
#'       mean_volume_by_year = mean(volume)
#'     ) %>%
#'     ggplot(aes(x = year, y = mean_volume_by_year / 10^6)) +
#'     geom_line() +
#'     scale_y_continuous_euro(suffix = "M ") +
#'     labs(
#'       x = "",
#'       y = "mean volume in Mil (€) "
#'     ) +
#'     theme_ipsum_rc()
#' }
#' }
#' @seealso
#'  \code{\link[ggplot2]{scale_continuous}}
#'  \code{\link[scales]{dollar_format}}
#' @rdname scale_y_continuous_euro
#' @export
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales dollar_format
scale_y_continuous_euro <- function(..., prefix = "", suffix = "") {
  ggplot2::scale_y_continuous(..., labels = scales::dollar_format(prefix = prefix, suffix = paste0(suffix, "€")))
}

#' @title scale_x_continuous_euro
#' @description scales x var and adds € suffix
#' @param ... inherited
#' @param prefix inherited, Default: ''
#' @param suffix inherited, Default: ''
#' @return scale_x_continuous_euro with € suffix
#' @seealso
#'  \code{\link[ggplot2]{scale_continuous}}
#'  \code{\link[scales]{dollar_format}}
#' @rdname scale_y_continuous_euro
#' @export
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom scales dollar_format
scale_x_continuous_euro <- function(..., prefix = "", suffix = "") {
  ggplot2::scale_x_continuous(..., labels = scales::dollar_format(prefix = prefix, suffix = paste0(suffix, "€")))
}
