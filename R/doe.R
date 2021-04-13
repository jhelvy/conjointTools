#' Make a design of experiment
#'
#' @param levels A vector of levels for each attribute.
#' @param varNames A vector of the names of each attribute in `levels` (defaults to `NULL`).
#' @param type The type of design. The default value of `"full"` returns a full factorial design. All other options return fractional factorial designs based on different criteria. The options `"D"`, `"A"`, and `"I"` return fractional factorial designs based on `"D"`, `"A"`, and `"I"` optimality metrics.
#' @param nTrials The number of trials to be used in a fractional factorial design. Defaults to `NA`, but must be a number less than the number of alternatives in the full factorial design if the `type` argument is anything other than `"full"`.
#' @return Returns a full factorial or fraction factorial design of experiment based on the number of levels for each attribute.
#' @export
#' @examples
#' \dontrun{
#' # Generate a full factorial design of experiment
#' doe <- makeDoe(levels = c(3, 3, 3))
#'
#' # Generate a full factorial design of experiment about apples
#' doe <- makeDoe(
#'     levels = c(3, 3, 3),
#'     varNames = c("price", "type", "freshness"),
#'     type = "full"
#' )
#'
#' # Generate a "D-optimal" fractional factorial design of experiment about apples
#' doe <- makeDoe(
#'     levels = c(3, 3, 3),
#'     varNames = c("price", "type", "freshness"),
#'     type = "D",
#'     nTrials = 15
#' )
#' }
makeDoe <- function(levels, varNames = NULL, type = "full", nTrials = NA) {
    ff <- AlgDesign::gen.factorial(
        levels = levels, varNames = varNames, factors = "all"
    )
    if (type == "full") {
        return(ff)
    }
    if (is.na(nTrials)) {
        stop('Fractional factorial designs require a numeric input for the "nTrials" argument.')
    }
    if (type %in% c("D", "A", "I")) {
        result <- AlgDesign::optFederov(
            data = ff, nTrials = nTrials, criterion = type, approximate = TRUE
        )
        design <- ff[result$rows,]
        row.names(design) <- NULL
        return(design)
    }
}
