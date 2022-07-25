#' Remove legend(s)
#'
#' With no argument, will remove all legends. Provide the name(s) of
#' specific aesthetic to remove only certain legends.
#'
#'
#' @md
#' @param ... optional name(s) specific aesthetics for which to remove the
#' legend
#'
#' @return either a \code{\link[ggplot2]{theme}} object or a
#' \code{\link[ggplot2]{guides}}object, both of which can be used in
#' \code{\link[ggplot2]{ggplot2}} calls
#' @export
#' @author Alicia Schep
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # Remove all legends
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() + legend_remove()
#'
#' # remove just size legend
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() + legend_remove("size")
#'
#' # can also use:
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() + legend_remove(size)
#'
#' # Remove more than one
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() + legend_remove(size, color)
legend_remove <- function(...) {
  vars <- rlang::exprs(...)
  if (length(vars) == 0) {
    if (teach) {
      message("legend_remove call can be substituted with:")
      message('theme(legend.position = "none")')
    }
    theme(legend.position = "none")
  } else {
    inputs <- lapply(vars, function(x) FALSE)
    names(inputs) <- vars

    do.call(guides, inputs)
  }
}


#' Easily modify legends
#'
#' Change legend position, direction, or justification.
#'
#' Due to limitations of `ggplot2` this will apply to all legends at once
#'
#' @md
#' @param what legend component to modify
#' (`"position"`, `"direction"`, or `"justification"`)
#' @param to to what to set the legend component should be changed
#'
#' @return a \code{\link[ggplot2]{theme}} object
#' @export
#' @author Jonathan Carroll
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # Move legends to bottom
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() + easy_move_legend("bottom")
#'
#' # Make legends horizontal
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() + easy_rotate_legend("horizontal")
#'
#' # Justify legends to the bottom and justify to the right
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() +
#'   easy_move_legend("bottom") +
#'   easy_adjust_legend("right")
easy_change_legend <- function(what = c("position", "direction", "justification"),
                               to) {
  what <- match.arg(what, several.ok = FALSE)

  theme_args <- setNames(to, paste0("legend.", what))
  callingFun <- tryCatch(as.list(sys.call(-1))[[1]], error = function(e) e)
  easy_fun <- if (inherits(callingFun, "simpleError")) {
    paste0("easy_change_legend(", what, ' = "', to, '")')
  } else {
    paste0(callingFun, '("', to, '")')
  }

  do.call(ggplot2::theme, as.list(theme_args))
}

#' @export
#' @rdname legend_move
legend_move <- function(to = c("right", "none", "left", "bottom", "top")) {
  to <- match.arg(to, several.ok = FALSE)
  easy_change_legend(what = "position", to = to)
}

#' @export
#' @rdname legend_rotate
legend_rotate <- function(to = c("vertical", "horizontal")) {
  to <- match.arg(to, several.ok = FALSE)
  easy_change_legend(what = "direction", to = to)
}

#' @export
#' @rdname legend_adjust
legend_adjust <- function(to = c("left", "right", "center")) {
  to <- match.arg(to, several.ok = FALSE)
  easy_change_legend(what = "justification", to = to)
}



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
