#' Make a design of experiment
#'
#' @param ff The full factorial design of experiment
#' @param nResp Total number of respondents (upper bound)
#' @param nAltsPerQ Number of alternatives per question
#' @param nQPerResp Number of questions per respondent
#' @return Creates a design of experiment
#' @export
#' @examples
#' \dontrun{
#' # Generate a full factorial design about apples
#' ff <- expand.grid(
#'     price     = seq(1, 4, 0.5), # $ per pound
#'     type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'     freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Make the doe
#' doe <- makeDoe(
#'     ff        = ff,   # Full factorial design
#'     nResp     = 3000, # Total number of respondents (upper bound)
#'     nAltsPerQ = 3,    # Number of alternatives per question
#'     nQPerResp = 6     # Number of questions per respondent
#' )
#' }
makeDoe <- function(ff, nResp, nAltsPerQ, nQPerResp) {
    nRows <- nResp*nAltsPerQ*nQPerResp
    # Create unique IDs for ff
    ff$rowID <- seq(nrow(ff))
    # Replicate the ff to create the doe
    doe <- repDf(ff, n = ceiling(nRows / nrow(ff)))
    doe <- doe[1:nRows,]
    # Randomize the rows
    doe <- doe[sample(nrow(doe)),]
    # Add meta data and remove cases with double alternatives
    doe <- addMetaData(doe, nAltsPerQ, nQPerResp)
    doe <- removeDoubleAlts(doe, nAltsPerQ, nQPerResp)
    return(doe %>%
               select(respID, qID, altID, obsID, everything()) %>%
               ungroup())
}

repDf <- function(df, n) {
    return(df[rep(seq_len(nrow(df)), n), ])
}

addMetaData <- function(doe, nAltsPerQ, nQPerResp) {
    nRowsPerResp   <- nAltsPerQ*nQPerResp
    nResp          <- nrow(doe) / nRowsPerResp
    doe$respID     <- rep(seq(nResp), each=nRowsPerResp)
    doe$qID        <- rep(rep(seq(nQPerResp), each=nAltsPerQ), nResp)
    doe$altID      <- rep(seq(nAltsPerQ), nResp*nQPerResp)
    doe$obsID      <- rep(seq(nResp * nQPerResp), each=nAltsPerQ)
    row.names(doe) <- NULL
    return(doe)
}

removeDoubleAlts <- function(doe, nAltsPerQ, nQPerResp) {
    doe <- addAltCounts(doe)
    doubleRows <- which(doe$nUniqueAlts != nAltsPerQ)
    while (length(doubleRows) != 0) {
        cat('Number repeated: ', length(doubleRows), '\n')
        newRows <- sample(x = seq(nrow(doe)), size = length(doubleRows),
                          replace = F)
        doe[doubleRows,] <- doe[newRows,]
        doe <- addMetaData(doe, nAltsPerQ, nQPerResp)
        doe <- addAltCounts(doe)
        doubleRows <- which(doe$nUniqueAlts != nAltsPerQ)
    }
    return(select(doe, -nUniqueAlts))
}

addAltCounts <- function(doe) {
    temp <- doe %>%
        group_by(obsID) %>%
        mutate(nUniqueAlts = length(unique(rowID)))
    return(temp)
}
