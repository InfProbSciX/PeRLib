#' Make a pretty Multiple Histogram
#'
#' \code{hist_fancy} Extends the hist() function by using ggplot2 for making a pretty histogram. The function calls ggplot2::qplot to make multiple histograms, so arguments will be the same as the hist or the qplot. A qplot object is returned so it's possible to add geoms to it.
#'
#' @param Named_Vectors Each vector is used to draw a multiple histogram with each label matching the corresponding vector name.
#' @param Unnamed_Vectors are named x{number}.
#' @param fill Optional character vector specifying fill colours, as large as the number of vectors fed into the function.
#' @param ... Other arguments for the qplot, such as xlim, ylim, xlab, ylab, main, etc.
#' @examples
#' hist_fancy(rnorm(1000))
#' hist_fancy(x = rnorm(1000), main = "Histogram", xlab = "Variables")
#' hist_fancy(x = rnorm(1000), y = rnorm(1000, 2), z = rnorm(1000, 4), alpha = 0.9)
#' @export
hist_fancy <- function(...){

  args <- list(...); j = 1

  for(i in 1:length(args))
    if(toString(names(args)[i]) %in% c("", "NA")) { names(args)[i] <- paste0("x", j); j <- j + 1 }

  lengths <- sapply(args, length)
  numerics <- sapply(args, is.numeric)

  vars <- args[(lengths > 1) & numerics]
  args <- args[!((lengths > 1) & numerics)]

  overlaps <- intersect(c("fill", "xlim", "ylim"), names(vars))
  if(length(overlaps) != 0) {args[overlaps] <- vars[overlaps]; vars[overlaps] <- NULL}

  if(length(vars) == 0) stop("No variables found. Make sure to pass numerics of length two or above.")

  if("fill" %in% names(args)){
    factors <- unname(as.vector(unlist(sapply(names(vars),
                                              function(i) rep(args$fill[which(names(vars) == i)], each = lengths[i])))))
    args$fill <- NULL; factors <- I(factors)
  }else{
    factors <- unname(as.vector(unlist(sapply(names(vars),
                                              function(i) rep(i, each = lengths[i])))))
  }

  vars <- unname(unlist(vars))

  histogram <- function(...) ggplot2::qplot(vars, fill = factors, geom = "histogram", pos = "identity", ...)

  return(do.call("histogram", args))

}
