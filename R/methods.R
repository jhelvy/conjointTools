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

#' Print design efficiency
#'
#' For a given design, prints the D efficiency and whether the design is
#' balanced and / or orthogonal.
#' @keywords logitr wtp
#'
#' @param object is an object of class `cjdesign`, a data frame returned
#' from the `makeDoe()` function.
#'
#' @return For a given design, prints the D efficiency and whether the design
#' is balanced and / or orthogonal.
#' @export
#' @examples
#' # Define the attributes and levels of an experiment
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Make a fractional-factorial design of experiment
#' doe <- makeDoe(levels, type = "D", nTrials = 15)
#'
#' # View the summary information about the design
#' eff(doe)
eff.cjdesign <- function (object, ...) {
    cat(comment(object))
}

#' Print design efficiency
#'
#' For a given design, prints the D efficiency and whether the design is
#' balanced and / or orthogonal.
#' @keywords logitr wtp
#'
#' @param object is an object of class `cjdesign`, a data frame returned
#' from the `makeDoe()` function.
#'
#' @return For a given design, prints the D efficiency and whether the design
#' is balanced and / or orthogonal.
#' @export
#' @examples
#' # Define the attributes and levels of an experiment
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Make a fractional-factorial design of experiment
#' doe <- makeDoe(levels, type = "D", nTrials = 15)
#'
#' # View the summary information about the design
#' eff(doe)
eff <- function(object) {
  UseMethod("eff")
}
