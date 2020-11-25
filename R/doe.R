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

#' Make a conjoint survey from a design of experiment
#'
#' @param doe The design of experiment returned from the `makeDoe()` function
#' @param nResp Maximum number of survey respondents
#' @param nAltsPerQ Number of alternatives per question
#' @param nQPerResp Number of questions per respondent
#' @return Returns a coded conjoint survey by sampling from a design of
#' experiment returned from the `makeDoe()` function. The design eliminates the
#' possibility of duplicate alternatives appearing in the same choice question.
#' @export
#' @examples
#' \dontrun{
#' # Generate a full factorial design of experiment about apples
#' doe <- makeDoe(
#'     levels = c(3, 3, 3),
#'     varNames = c("price", "type", "freshness"),
#'     type = "full"
#' )
#'
#' # Make the coded conjoint survey
#' survey <- makeSurvey(
#'     doe       = doe,  # Design of experiment
#'     nResp     = 1000, # Total number of respondents (upper bound)
#'     nAltsPerQ = 3,    # Number of alternatives per question
#'     nQPerResp = 6     # Number of questions per respondent
#' )
#' }
makeSurvey <- function(doe, nResp, nAltsPerQ, nQPerResp) {
    nRows <- nResp*nAltsPerQ*nQPerResp
    # Create unique IDs for doe
    doe$rowID <- seq(nrow(doe))
    # Replicate the doe to create the survey
    survey <- repDf(doe, n = ceiling(nRows / nrow(doe)))
    survey <- survey[1:nRows,]
    # Randomize the rows
    survey <- survey[sample(nrow(survey)),]
    # Add meta data and remove cases with double alternatives
    survey <- addMetaData(survey, nAltsPerQ, nQPerResp)
    survey <- removeDuplicateAlts(survey, nAltsPerQ, nQPerResp)
    # Re-order column names
    survey$rowID <- NULL
    metaNames <- c("respID", "qID", "altID", "obsID")
    varNames <- setdiff(names(survey), metaNames)
    return(survey[c(metaNames, varNames)])
}

repDf <- function(df, n) {
    return(df[rep(seq_len(nrow(df)), n), ])
}

addMetaData <- function(survey, nAltsPerQ, nQPerResp) {
    nRowsPerResp   <- nAltsPerQ*nQPerResp
    nResp          <- nrow(survey) / nRowsPerResp
    survey$respID     <- rep(seq(nResp), each=nRowsPerResp)
    survey$qID        <- rep(rep(seq(nQPerResp), each=nAltsPerQ), nResp)
    survey$altID      <- rep(seq(nAltsPerQ), nResp*nQPerResp)
    survey$obsID      <- rep(seq(nResp * nQPerResp), each=nAltsPerQ)
    row.names(survey) <- NULL
    return(survey)
}

removeDuplicateAlts <- function(survey, nAltsPerQ, nQPerResp) {
    survey <- addAltCounts(survey)
    doubleRows <- which(survey$nUniqueAlts != nAltsPerQ)
    while (length(doubleRows) != 0) {
        # cat('Number repeated: ', length(doubleRows), '\n')
        newRows <- sample(x = seq(nrow(survey)), size = length(doubleRows),
                          replace = F)
        survey[doubleRows,] <- survey[newRows,]
        survey <- addMetaData(survey, nAltsPerQ, nQPerResp)
        survey <- addAltCounts(survey)
        doubleRows <- which(survey$nUniqueAlts != nAltsPerQ)
    }
    survey$nUniqueAlts <- NULL
    return(survey)
}

addAltCounts <- function(survey) {
    temp <- dplyr::group_by(survey, "obsID")
    temp <- dplyr::mutate(temp, nUniqueAlts = length(unique("rowID")))
    return(dplyr::ungroup(temp))
}
