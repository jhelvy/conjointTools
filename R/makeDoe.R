#' Make a design of experiment
#'
#' @param levels A named list of vectors defining each attribute (the names)
#' and each level for each attribute (the vectors). For example, a design
#' with two attributes, "price" and "type", that each had three levels should
#' be defined as `levels = list(price = c(1, 2, 3), type = c(1, 2, 3))`.
#' @param type The type of design. Defaults to `NULL`, which returns a full
#' factorial design. Set to `"D"`, `"A"`, and `"I"` to obtain a fractional
#' factorial design with either D, A, or I optimality criteria.
#' @param nTrials The number of trials to be used in a fractional factorial
#' design. Defaults to `NA`, but must be a number less than the number of
#' alternatives in the full factorial design if the `type` argument is anything
#' other than `NULL`.
#' @return Returns a full factorial or fraction factorial design of experiment.
#' @export
#' @examples
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Make a full-factorial design of experiment
#' doe <- makeDoe(levels)
#'
#' # Make a fraction-factorial design of experiment based on D-efficiency
#' doe <- makeDoe(levels, type = "D", nTrials = 100)
makeDoe <- function(levels, type = NULL, nTrials = NA) {
    vars <- unlist(lapply(levels, length))
    ff <- AlgDesign::gen.factorial(
        levels = vars, varNames = names(vars), factors = "all"
    )
    if (is.null(type)) {
        return(ff)
    }
    if (is.na(nTrials)) {
        stop(
            'Fractional factorial designs require a numeric input for the ',
            '"nTrials" argument.')
    }
    if (nTrials > nrow(ff)) {
        n <- nrow(ff)
        stop(
            'The full factorial design has ', n, ' rows. Please ',
            'specify a number of "nTrials" that is less than ', n, '.')
    }
    if (type %in% c("D", "A", "I")) {
        result <- AlgDesign::optFederov(
            data = ff, nTrials = nTrials, criterion = type,
            approximate = FALSE
        )
        # Print summary
        message(
            "Design summary:\n",
            "D: ", round(result$D, 6), "\n",
            "A: ", round(result$A, 6)
        )
        # Return design
        doe <- ff[result$rows,]
        row.names(doe) <- NULL
        return(doe)
    }
}

#' Re-code the levels in a design of experiment
#'
#' @param doe A design of experiment data frame. Each row is an alternative,
#' each column is an attribute.
#' @param levels A named list of vectors defining each attribute (the names)
#' and each level for each attribute (the vectors). For example, a design
#' with two attributes, "price" and "type", that each had three levels should
#' be defined as `levels = list(price = c(1, 2, 3), type = c(1, 2, 3))`.
#' @return Returns a re-coded design of experiment using the labels in the
#' `levels` argument.
#' @export
#' @examples
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Make a full-factorial design of experiment
#' doe <- makeDoe(levels)
#'
#' # Re-code levels
#' doe <- recodeDesign(doe, levels)
recodeDesign <- function(doe, levels) {
  types <- unlist(lapply(levels, class))
  for (i in seq_len(length(levels))) {
    col <- which(names(doe) == names(levels)[i])
    levels(doe[,col]) <- levels[[i]]
    if (types[i] == "numeric") {
        doe[,col] <- as.numeric(as.character(doe[,col]))
    } else if (types[i] == "character") {
        doe[,col] <- as.character(doe[,col])
    }
  }
  return(doe)
}
