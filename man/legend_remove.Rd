% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggutils.R
\name{legend_remove}
\alias{legend_remove}
\title{Remove legend(s)}
\usage{
legend_remove(...)
}
\arguments{
\item{...}{optional name(s) specific aesthetics for which to remove the
legend}
}
\value{
either a \code{\link[ggplot2]{theme}} object or a
\code{\link[ggplot2]{guides}}object, both of which can be used in
\code{\link[ggplot2]{ggplot2}} calls
}
\description{
With no argument, will remove all legends. Provide the name(s) of
specific aesthetic to remove only certain legends.
}
\examples{

library(ggplot2)

# Remove all legends
ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
  geom_point() + legend_remove()

# remove just size legend
ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
  geom_point() + legend_remove("size")

# can also use:
ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
  geom_point() + legend_remove(size)

# Remove more than one
ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
  geom_point() + legend_remove(size, color)
}
\author{
Alicia Schep
}
