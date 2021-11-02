checkDoeInputs <- function(ff, type, nTrials) {
    if (is.na(nTrials)) {
        stop(
            'Fractional factorial designs require a numeric input for the ',
            '"nTrials" argument.')
    }
    maxTrials <- nrow(ff)
    if (nTrials > maxTrials) {
        stop(
            'There are only ', maxTrials, ' trials in the full factorial ',
            'design. Set nTrials <= ', maxTrials
        )
    }
    vars <- apply(ff, 2, function(x) length(unique(x)))
    minLevels <- sum(vars) - length(vars)
    if (nTrials <= minLevels) {
        stop(
            'Based on the levels in "levels", the fractional design must have ',
            'at least ', minLevels, ' levels. Set nTrials > ', minLevels
        )
    }
    if (! type %in% c("D", "A", "I")) {
        stop('The type argument must be "D", "A", or "I"')
    }
}
