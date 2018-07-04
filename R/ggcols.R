#' Generate the ggplot2 color wheel
#'
#' \code{ggcols} Returns a vector of ggplot2 colours. Inspired by an answer by Hadley, Colby J. & zx8754 on Cross Validated.
#'
#' @param n Integer; the number of colours to get.
#' @examples
#' ggcols(1)
#' ## "#F8766D"
#' @export
ggcols <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
