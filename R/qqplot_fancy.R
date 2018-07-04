#' Make a pretty QQ-Plot
#'
#' \code{qqplot_fancy} Extends the qqplot() function by using ggplot2 for making a pretty qqplot and adding a best-fitting distribution functionality.
#'
#' @param sample A numeric vector with the "actual" observations.
#' @param dist Either (1) a string (default "normal") specifying a known distribution Or (2) another vector of theoretical/model observations. The distribution can be one of beta, exponential, gamma, log-normal, lognormal, normal, pareto, t or weibull.
#' @param col (optional) A string specifying point colour. Defaults to a random colour from the ggplots color wheel.
#' @param start (optional) A list of named starting/initial values for fitditr.
#' @param xlab (optional) A string containing the x axis label.
#' @param ylab (optional) A string containing the y axis label.
#' @param title (optional) A string containing the title.
#' @examples
#' qqplot_fancy(rnorm(1000)) ## pretty line
#' qqplot_fancy(rexp(1000), "exponential") ## pretty line
#' qqplot_fancy(rexp(1000), rexp(1000)) ## pretty line
#' qqplot_fancy(rnorm(10), col = "blue") ## pretty blue line
#' @export
qqplot_fancy <- function(sample, dist = "normal", col = sample(ggcols(50)[c(1:5, 17:38, 47:49)], 1), start = NULL, xlab = NULL, ylab = NULL, title = NULL){

  if(length(dist) >= 2){
    if(length(dist) == length(sample)){
      Theo <- dist
    }else{
      Theo <- quantile(dist, ppoints(length(sample)))
    }

    fitr.dist <- "TheoreticalSample"

  }else{

    if(dist == "beta" & is.null(start)){
      start <- list(shape1 = 1, shape2 = 1)
      sample <- (sample/max(sample))
      sample[sample == 1] <- sample[sample == 1] - 0.00001
      sample[sample == 0] <- sample[sample == 0] + 0.00001
    }else if(dist == "pareto" & is.null(start)){
      start <- list(shape = 1, scale = 10)
    }

    if((dist == "gamma" | dist == "exponential" | dist == "weibull" | dist == "log-normal" | dist == "lognormal") & is.null(start)){
      sample[sample == 0] <- sample[sample == 0] + 0.00001
    }

    fitr.dists <- c("beta",
                    "exponential",
                    "gamma",
                    "log-normal",
                    "lognormal",
                    "normal",
                    "t",
                    "weibull")

    eval.dists <- c("beta",
                    "exp",
                    "gamma",
                    "lnorm",
                    "lnorm",
                    "norm",
                    "t",
                    "weibull")

    n <- length(sample)

    if(dist == "pareto"){
      ppareto <- actuar::ppareto
      dpareto <- actuar::dpareto
      qpareto <- actuar::qpareto
      suppressWarnings(params <- fitdistrplus::fitdist(sample, "pareto", start = start)$estimate)
      fitr.dist <- dist
      eval.dist <- dist
    }else if(sum(as.numeric(dist == fitr.dists)) == 0){
      print("Distribution not recognised. Try: beta, exponential, gamma, log-normal, lognormal, normal, t or weibull")
    }else{
      fitr.dist <- fitr.dists[dist == fitr.dists]
      eval.dist <- eval.dists[dist == fitr.dists]
    }

    if(dist != "pareto")
      suppressWarnings(params <- MASS::fitdistr(sample, fitr.dist, start = start)$estimate)

    if(fitr.dist != "t")
      eval(parse(text = paste0("Theo <- q", eval.dist, "(ppoints(n), ", toString(paste(paste0(attr(params, "names"), " = ", params), sep = ", ")), ")")))
    else if(fitr.dist != "pareto"){
      eval(parse(text = paste0("Theo <- params[1]+(params[2] * q", eval.dist, "(ppoints(n), ", toString(paste(paste0(attr(params[3], "names"), " = ", params[3]), sep = ", ")), "))")))
    }else{
      eval(parse(text = paste0("Theo <- q", eval.dist, "(ppoints(n), ", toString(paste(paste0(attr(params, "names"), " = ", params), sep = ", ")), ")")))
    }

  }

  if(is.null(xlab))
    xlab <- paste0("Theoretical Quantiles ", "(", fitr.dist, ")")

  if(is.null(ylab))
    ylab <- "Actual Quantiles (data)"

  if(is.null(title))
    title <- "QQPlot"

  ggplot2::ggplot(data.frame("Actual" = sort(sample), "Theoretical" = sort(Theo)),
                  ggplot2::aes(x = Theoretical, y = Actual)) +
    ggplot2::geom_point(alpha = 0.8, color = col) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::labs(title = title, x = xlab, y = ylab)

}
