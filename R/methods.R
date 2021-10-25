#' Methods for cjtools objects
#'
#' Miscellaneous methods for `cjtools` class objects.
#'
#' @name miscmethods.cjtools
#' @aliases print.cjtools
#' @param x is an object of class `cjtools`.
#' @param object is an object of class `cjtools`.
#' @param digits the number of digits for printing, defaults to `3`.
#' @param width the width of the printing.
#' @param ... further arguments.
#'
#' @rdname miscmethods.cjtools
#' @export
print.cjtools <- function (
  x,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  ...
) {
  cat("A list of models estimated with the following sample sizes:\n")
  cat(names(x), "\n")
  invisible(x)
}
