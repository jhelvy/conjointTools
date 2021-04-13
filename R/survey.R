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
    nRowsPerResp      <- nAltsPerQ*nQPerResp
    nResp             <- nrow(survey) / nRowsPerResp
    survey$respID     <- rep(seq(nResp), each=nRowsPerResp)
    survey$qID        <- rep(rep(seq(nQPerResp), each=nAltsPerQ), nResp)
    survey$altID      <- rep(seq(nAltsPerQ), nResp*nQPerResp)
    survey$obsID      <- rep(seq(nResp * nQPerResp), each=nAltsPerQ)
    row.names(survey) <- NULL
    return(survey)
}

removeDuplicateAlts <- function(survey, nAltsPerQ, nQPerResp) {
    duplicateRows <- getDuplicateRows(survey, nAltsPerQ)
    while (length(duplicateRows) > 0) {
        # cat('Number repeated: ', length(duplicateRows), '\n')
        newRows <- sample(
            x = seq(nrow(survey)), size = length(duplicateRows), replace = F)
        survey[duplicateRows,] <- survey[newRows,]
        survey <- addMetaData(survey, nAltsPerQ, nQPerResp)
        duplicateRows <- getDuplicateRows(survey, nAltsPerQ)
    }
    return(survey)
}

getDuplicateRows <- function(survey, nAltsPerQ) {
    counts <- tapply(survey$rowID, survey$obsID,
                     FUN = function(x) length(unique(x)))
    duplicateIDs <- which(counts != nAltsPerQ)
    duplicateRows <- which(survey$obsID %in% duplicateIDs)
    return(duplicateRows)
}
