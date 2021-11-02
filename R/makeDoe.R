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
#' other than `NULL`. If `search = TRUE`, then all feasible designs will be
#' calculated up to `nTrials` or until a perfect D-efficiency is found.
#' @param search If `TRUE`, all feasible designs are calculated up to `nTrials`
#' or until a perfect D-efficiency is found, after which a summary of the
#' search results is printed and the top-ranked d-efficient design is returned.
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
makeDoe <- function(levels, type = NULL, nTrials = NA, search = FALSE) {
    vars <- unlist(lapply(levels, length))
    doe <- getFullFactorial(levels, vars)
    if (!is.null(type)) {
        minLevels <- sum(vars) - length(vars)
        checkDoeInputs(doe, type, nTrials, minLevels)
        if (search) {
            minLevels <- minLevels + 3
        } else {
            minLevels <- nTrials
        }
        doe <- searchDesigns(doe, nTrials, type, minLevels)
    }
    return(doe)
}

getFullFactorial <- function(levels, vars) {
    ff <- AlgDesign::gen.factorial(
        levels = vars, varNames = names(vars), factors = "all"
    )
    return(ff)
}

searchDesigns <- function(ff, nTrials, type, minLevels) {
    results <- list()
    for (i in seq(minLevels, nTrials)) {
        result <- computeDesign(ff, nTrials = i, type)
        results[[as.character(i)]] <- result
        if (result$d == 1) { break }
    }
    result <- aggregateDoeSearch(results)
    result <- result[order(-result$d, result$balanced),]
    print(result)
    return(results[[as.character(result$nTrials[1])]]$doe)
}

computeDesign <- function(ff, nTrials, type) {
    result <- AlgDesign::optFederov(
        data = ff, nTrials = nTrials, criterion = type,
        approximate = FALSE
    )
    row.names(result$design) <- NULL
    doe <- result$design
    return(list(
        doe = doe,
        d = result$Dea,
        balanced = isBalanced(doe),
        nTrials = nTrials
    ))
}

aggregateDoeSearch <- function(results) {
    return(data.frame(
        nTrials = unlist(lapply(results, function(x) x$nTrials)),
        d = unlist(lapply(results, function(x) x$d)),
        balanced = unlist(lapply(results, function(x) x$balanced))
    ))
}

evaluateDoe <- function(doe) {
    vars <- apply(doe, 2, function(x) length(unique(x)))
    ff <- getFullFactorial(levels, vars)
    frml <- stats::formula("~-1 + .")
    eff <- AlgDesign::eval.design(frml = frml, design = doe, X = ff)
    return(list(
        d_eff = eff$Deffbound,
        balanced = isBalanced(doe)
    ))
}

isBalanced <- function(doe) {
    counts <- apply(doe, 2, table)
    numCounts <- unlist(lapply(counts, function(x) length(unique(x))))
    if (any(numCounts != 1)) {
        return(FALSE)
    }
    return(TRUE)
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
