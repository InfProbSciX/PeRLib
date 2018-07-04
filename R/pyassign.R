#' Assign variables the python/matlab way
#'
#' \code{%=%}
#'
#' @examples
#' "a, b, d, e" %=% "1, 5, 6, 7"
#'
#' @export
`%=%` <- function(variable_str, assign_str){

  # String cleaning

  variable_str <- gsub(pattern = " ", replacement = "", x = variable_str)
  assign_str <- gsub(pattern = " ", replacement = "", x = assign_str)
  variable_str <- gsub(pattern = "^\\,", replacement = "", x = variable_str)
  variable_str <- gsub(pattern = "\\,$", replacement = "", x = variable_str)
  assign_str <- gsub(pattern = "^\\,", replacement = "", x = assign_str)
  assign_str <- gsub(pattern = "\\,$", replacement = "", x = assign_str)

  variable_str <- unlist(strsplit(variable_str, ","))
  assign_str <- unlist(strsplit(assign_str, ","))

  # Check for valid inputs

  if(length(variable_str) != length(assign_str))
    stop("Number of variables doesn't match number of assignments.")

  # Evaluate the strings

  variable_str <- unlist(strsplit(variable_str, ","))
  assign_str <- unlist(strsplit(assign_str, ","))

  n <- length(variable_str)

  evaler <- function(i){
    eval(parse(text = paste0(variable_str[i], " <- ", assign_str[i])), envir = .GlobalEnv)
  }

  invisible(sapply(1:n, evaler))

}
