#' Make a conjoint survey from a design of experiment
#'
#' @param doe A design of experiment data frame. Each row is an alternative,
#' each column is an attribute.
#' @param nResp Maximum number of survey respondents
#' @param nAltsPerQ Number of alternatives per question
#' @param nQPerResp Number of questions per respondent
#' @param group The name of the group column if your doe is designed with
#' groups (e.g. a labeled experiment where each set of alternatives has the
#' same type). Defaults to `NULL`.
#' @return Returns a conjoint survey by randomly sampling from a design of
#' experiment data frame. The sampling is done iteratively to remove the
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
makeSurvey <- function(doe, nResp, nAltsPerQ, nQPerResp, group = NULL) {
    doe <- as.data.frame(doe) # tibbles break things
    doe$rowID <- seq(nrow(doe)) # Have to set these now to remove dupes later
    nRows <- nResp*nAltsPerQ*nQPerResp
    nReps <- ceiling(nRows / nrow(doe))
    survey <- repDf(doe, n = nReps)
    survey <- survey[1:nRows,]
    if (is.null(group)) {
        return(randomizeSurvey(survey, nResp, nAltsPerQ, nQPerResp))
    }
    return(randomizeSurveyByGroup(survey, nResp, nAltsPerQ, nQPerResp, group))
}

repDf <- function(df, n) {
    return(df[rep(seq_len(nrow(df)), n), ])
}

randomizeSurvey <- function(survey, nResp, nAltsPerQ, nQPerResp) {
    n <- nrow(survey)
    sample_ids <- sample(x = seq_len(n), size = n, replace = FALSE)
    survey <- survey[sample_ids,]
    # Add meta data and remove cases with double alternatives
    survey <- addMetaData(survey, nResp, nAltsPerQ, nQPerResp)
    survey <- removeDuplicateAlts(survey, nResp, nAltsPerQ, nQPerResp)
    # Re-order column names
    metaNames <- c("respID", "qID", "altID", "obsID")
    varNames <- setdiff(names(survey), metaNames)
    survey <- survey[,c(metaNames, varNames)]
    survey$rowID <- NULL
    return(survey)
}

randomizeSurveyByGroup <- function(survey, nResp, nAltsPerQ, nQPerResp, group) {
    nObs <- nrow(survey) / nAltsPerQ
    survey$groupID <- rep(seq_len(nObs), each = nAltsPerQ)
    sampleID <- sample(
        x = unique(survey$groupID), size = nObs, replace = FALSE)
    survey$sampleID <- rep(sampleID, each = nAltsPerQ)
    survey <- survey[order(survey$sampleID),]
    # Add meta data and remove cases with double alternatives
    survey <- addMetaData(survey, nResp, nAltsPerQ, nQPerResp)
    # Re-order column names
    metaNames <- c("respID", "qID", "altID", "obsID")
    varNames <- setdiff(names(survey), metaNames)
    survey <- survey[,c(metaNames, varNames)]
    survey$rowID <- NULL
    survey$groupID <- NULL
    survey$sampleID <- NULL
    return(survey)
}

addMetaData <- function(survey, nResp, nAltsPerQ, nQPerResp, group = NULL) {
    nRowsPerResp      <- nAltsPerQ*nQPerResp
    survey$respID     <- rep(seq(nResp), each = nRowsPerResp)
    survey$qID        <- rep(rep(seq(nQPerResp), each = nAltsPerQ), nResp)
    survey$altID      <- rep(seq(nAltsPerQ), nResp*nQPerResp)
    survey$obsID      <- rep(seq(nResp * nQPerResp), each = nAltsPerQ)
    row.names(survey) <- NULL
    return(survey)
}

removeDuplicateAlts <- function(survey, nResp, nAltsPerQ, nQPerResp) {
    duplicateRows <- getDuplicateRows(survey, nAltsPerQ)
    while (length(duplicateRows) > 0) {
        # cat('Number repeated: ', length(duplicateRows), '\n')
        newRows <- sample(
            x = seq(nrow(survey)), size = length(duplicateRows), replace = F)
        survey[duplicateRows,] <- survey[newRows,]
        survey <- addMetaData(survey, nResp, nAltsPerQ, nQPerResp)
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
