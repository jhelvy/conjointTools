#' Make a randomized conjoint survey from a design of experiment
#'
#' @param doe A design of experiment data frame. Each row is an alternative,
#' each column is an attribute.
#' @param nResp Maximum number of survey respondents
#' @param nAltsPerQ Number of alternatives per question
#' @param nQPerResp Number of questions per respondent
#' @param group The name of the group attribute if your doe is designed with a
#' group (i.e. a labeled experiment design where each set of alternatives
#' contains one of each of the levels in the `group` attribute.
#' @return Returns a conjoint survey by randomly sampling from a design of
#' experiment data frame. The sampling is done iteratively to remove the
#' possibility of duplicate alternatives appearing in the same choice question.
#' @export
#' @examples
#' \dontrun{
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
#'
#' # Make the conjoint survey by randomly sampling from the doe
#' survey <- makeSurvey(
#'   doe       = doe,  # Design of experiment
#'   nResp     = 2000, # Total number of respondents (upper bound)
#'   nAltsPerQ = 3,    # Number of alternatives per question
#'   nQPerResp = 6     # Number of questions per respondent
#' )
#' }
makeSurvey <- function(doe, nResp, nAltsPerQ, nQPerResp, group = NULL) {
    doe <- as.data.frame(doe) # tibbles break things
    doe$rowID <- seq(nrow(doe)) # Have to set these now to remove dupes later
    if (is.null(group)) {
        return(randomizeSurvey(doe, nResp, nAltsPerQ, nQPerResp))
    }
    return(randomizeSurveyByGroup(doe, nResp, nAltsPerQ, nQPerResp, group))
}

repDf <- function(df, n) {
    return(df[rep(seq_len(nrow(df)), n), ])
}

randomizeSurvey <- function(doe, nResp, nAltsPerQ, nQPerResp) {
    survey <- initializeSurvey(doe, nResp, nAltsPerQ, nQPerResp)
    # Randomize rows
    n <- nResp*nAltsPerQ*nQPerResp
    survey <- randomizeRows(survey, n)
    # Add meta data and remove cases with double alternatives
    survey <- addMetaData(survey, nResp, nAltsPerQ, nQPerResp)
    survey <- removeDuplicateAlts(survey, nResp, nAltsPerQ, nQPerResp)
    # Re-order column names
    survey <- reorderSurveyCols(survey)
    return(survey)
}

randomizeSurveyByGroup <- function(doe, nResp, nAltsPerQ, nQPerResp, group) {
    survey <- initializeSurvey(doe, nResp, nAltsPerQ, nQPerResp, group)
    # Randomize rows by group
    groups <- split(survey, survey[group])
    for (i in seq_len(length(groups))) {
        n <- nResp*nQPerResp
        survey_group <- randomizeRows(groups[[i]], n)
        survey_group$groupID <- seq(nrow(survey_group))
        groups[[i]] <- survey_group
    }
    survey <- do.call(rbind, groups)
    survey <- survey[order(survey$groupID),]
    survey$groupID <- NULL
    # Add meta data and remove cases with double alternatives
    survey <- addMetaData(survey, nResp, nAltsPerQ, nQPerResp)
    # Re-order column names
    survey <- reorderSurveyCols(survey)
    return(survey)
}

initializeSurvey <- function(doe, nResp, nAltsPerQ, nQPerResp, group = NULL) {
    # Replicate doe if the needed number of observations (n) is larger than
    # the number of observations in the doe (n_doe)
    n <- nResp*nAltsPerQ*nQPerResp
    n_doe <- nrow(doe)
    nReps <- ceiling(n / n_doe)
    if (!is.null(group)) {
        # If there is a group, then need to replicate based on the
        # lowest number of any one group
        n_groups <- min(table(doe[group]))
        n <- nResp*nQPerResp
        nReps <- ceiling(n / n_groups)
    }
    survey <- doe
    if (nReps > 1) {
        survey <- repDf(doe, nReps)
    }
    row.names(survey) <- NULL
    return(survey)
}

randomizeRows <- function(survey, n) {
    sample_ids <- sample(x = seq_len(nrow(survey)), size = n, replace = FALSE)
    return(survey[sample_ids,])
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

reorderSurveyCols <- function(survey) {
    metaNames <- c("respID", "qID", "altID", "obsID")
    varNames <- setdiff(names(survey), metaNames)
    survey <- as.data.frame(survey)[,c(metaNames, varNames)]
    survey$rowID <- NULL
    return(survey)
}
