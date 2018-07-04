#' Check if a vector of numbers is Even
#'
#' \code{is.even} Returns a vector of TRUE/FALSEs by applying x%2 to a vector x.
#'
#' @param x A numeric vector.
#' @param n An integer specifying if a number of even numbers should be returned.
#' @examples
#' is.even(1:5)
#'
#' ## FALSE  TRUE FALSE  TRUE FALSE
#' @export
is.even <- function(x, n = FALSE){
    if(!n){
      x %% 2 == 0
    }else{
      x[perlib::is.even(x)]
    }
}

#' Check if a vector of numbers is Odd
#'
#' \code{is.odd} Returns a vector of TRUE/FALSEs by applying x%2 to a vector x.
#'
#' @param x A numeric vector.
#' @param n An integer specifying if a number of odd numbers should be returned.
#' @examples
#' is.odd(1:5)
#'
#' ## TRUE FALSE  TRUE FALSE TRUE
#' @export
is.odd <- function(x, n = FALSE){
    if(!n){
      x %% 2 != 0
    }else{
      x[perlib::is.odd(x)]
    }
}
