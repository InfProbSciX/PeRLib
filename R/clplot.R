################### Command Line Plotting

#' Make a Command-Line Plot
#'
#' \code{clplot} Makes a plot in the command line. Observations are split linearly into three groups ., x & X by frequency.
#'
#' @param x A numeric vector. NAs are removed.
#' @param y A numeric vector. NAs are removed.
#' @param plot.size Integer; number of bins.
#' @examples
#' clplot(1:100, 1:100)
#'
#'
#' ##     1 12 23 34 45 56 67 78 89 100
#' ## 100                           x
#' ## 89                         x
#' ## 78                      x
#' ## 67                   x
#' ## 56                x
#' ## 45             x
#' ## 34          x
#' ## 23       x
#' ## 12    x
#' ## 1   x
#'
#' @export
clplot <- function (x, y, plot.size = 10) {

  min.x <- min(x)
  max.x <- max(x)
  min.y <- min(y)
  max.y <- max(y)

  x <- as.numeric(x)
  y <- as.numeric(y)

  if (!is.null(is.na(y))) {
    x <- x[!is.na(y)]
    y <- y[!is.na(y)]
  }

  if (!is.null(is.na(x))) {
    y <- y[!is.na(x)]
    x <- x[!is.na(x)]
  }

  x <- (x - min(x)) / (max(x) - min(x))
  x[x == min(x)] <- min(x) + 0.000001
  x[x == max(x)] <- max(x) - 0.000001

  y <- (y - min(y)) / (max(y) - min(y))
  y[y == min(y)] <- min(y) + 0.000001
  y[y == max(y)] <- max(y) - 0.000001

  coords.x <- as.numeric(cut(x, breaks = plot.size))
  coords.y <- as.numeric(cut(y, breaks = plot.size))

  plt <- array(0, dim = c(plot.size, plot.size))

  for (i in 1:min(length(x), length(y))){
    plt[coords.x[i], coords.y[i]] <- plt[coords.x[i], coords.y[i]] + 1
  }

  plt <- as.numeric(plt)
  plt[plt != 0] <- as.numeric(cut(plt[plt != 0], 3))
  plt[plt == 0] <- ""
  plt[plt == 1] <- "."
  plt[plt == 2] <- "x"
  plt[plt == 3] <- "X"
  plt <- array(plt, dim = c(plot.size, plot.size))

  rownames(plt) <- signif(min.x + (max.x - min.x) * seq(0, 1, by = 1/(plot.size - 1)), 2)
  colnames(plt) <- signif(min.y + (max.y - min.y) * seq(0, 1, by = 1/(plot.size - 1)), 2)

  plt <- t(plt)

  print(noquote(plt[plot.size:1, ]))

}
