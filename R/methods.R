#' Methods for cjmodels objects
#'
#' Miscellaneous methods for `cjmodels` class objects.
#'
#' @name miscmethods.cjmodels
#' @aliases print.cjmodels
#' @param x is an object of class `cjmodels`.
#' @param digits the number of digits for printing, defaults to `3`.
#' @param width the width of the printing.
#' @param ... further arguments.
#'
#' @rdname miscmethods.cjmodels
#' @export
print.cjmodels <- function (
  x,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  ...
) {
  cat("A list of models estimated with the following sample sizes:\n")
  cat(names(x), "\n")
  invisible(x)
}

#' Methods for cjdesign objects
#'
#' Miscellaneous methods for `cjdesign` class objects.
#'
#' @name miscmethods.cjdesign
#' @aliases print.cjdesign
#' @param x is an object of class `cjdesign`.
#' @param digits the number of digits for printing, defaults to `3`.
#' @param width the width of the printing.
#' @param ... further arguments.
#'
#' @rdname miscmethods.cjdesign
#' @export
eff.cjdesign <- function (object, ...) {
    cat(comment(doe))
}

#' Print design D and A efficiency
#'
#' Prints the D and A efficiencies of a design.
#' @keywords logitr wtp
#'
#' @param object is an object of class `cjdesign`, a data frame returned
#' from the `makeDoe()` function.
#'
#' @return Prints the D and A efficiencies of a design.
#' @export
#' @examples

eff <- function(object) {
  UseMethod("eff")
}
