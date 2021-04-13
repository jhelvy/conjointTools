#' Estimate errors from conjoint designs of different sample sizes
#'
#' This function estimates multiple models with increasing sample sizes of a
#' particular survey design. The number of models to run is set by the
#' `nbreaks` argument, which breaks up the survey design into groups of
#' increasing sample size. This function returns estimated models from the
#' {logitr} package. View a summary of the results with the `summary()`
#' function.
#' @keywords logitr, mnl, mxl, logit, sample size
#'
#' @param survey The choice survey data frame exported from the `makeSurvey()` function.
#' @param parNames A vector of the names of the parameters to be estimated in the model. Must be the same as the column names in the `survey` argument.
#' @param parTypes A vector determining the type of each variable: "c" for continuous, or "d" for discrete. Continuous variables will be linearly coded whereas discrete variables will be dummy coded with one level removed for identification. Defaults to `NULL`, in which case all parameters are coded as "d" for discrete.
#' @param interactions A logical value for adding interactions between all parameters in `parNames`. Defaults to `FALSE`.
#' @param nbreaks The number of different sample size groups.
#' @param randPars A named vector whose names are the random parameters and values the distribution: `'n'` for normal or `'ln'` for log-normal. Defaults to `NULL`.
#' @return Returns a data frame of the standard error values for different sample sizes.
#' @export
#' @examples
#' \dontrun{
#' library(conjointTools)
#' }
getPower = function(
    survey,
    parNames = NULL,
    parTypes = NULL,
    interactions = FALSE,
    nbreaks = 10,
    randPars = NULL
) {
    inputs <- setupInputs(survey, parNames, parTypes, interactions)
    survey <- inputs$survey
    parNames <- inputs$parNames
    # Loop through subsets of different sample sizes
    # In each iteration, estimate a model
    maxObs <- max(survey['obsID'])
    nobs <- ceiling(seq(ceiling(maxObs/nbreaks), maxObs, length.out = nbreaks))
    sizes <- round(nobs / max(survey$qID))
    result <- list()
    for (i in 1:nbreaks) {
        tempSurvey <- survey[which(survey['obsID'] < nobs[i]),]
        suppressMessages(
            model <- logitr::logitr(
                data = tempSurvey, parNames = parNames, randPars = randPars,
                choiceName = 'choice', obsIDName = 'obsID', priceName = NULL,
                randPrice = NULL, modelSpace = 'pref', weights = NULL)
        )
        result[[i]] <- getSE(model, sizes[i])
    }
    return(do.call(rbind, result))
}

setupInputs <- function(survey, parNames, parTypes, interactions) {
    # Set parNames
    if (is.null(parNames)) {
        parNames <- names(survey)[!grepl("ID", names(survey))]
    }
    # Recode survey variables as continuous or discrete
    if (!is.null(parTypes)) {
        cpars <- parNames[which(parTypes == "c")]
        dpars <- parNames[which(parTypes == "d")]
        survey[,cpars] <- lapply(survey[cpars], as.numeric)
        survey[,dpars] <- lapply(survey[dpars], as.factor)
    }
    # Add interactions
    if (interactions) {
        ints <- t(utils::combn(parNames, 2))
        ints <- paste(ints[,1], ints[,2], sep = "*")
        parNames <- c(parNames, ints)
    }
    # Add random choices to the survey
    survey$choice <- generateRandomChoices(survey)
    return(list(survey = survey, parNames = parNames))
}

generateRandomChoices <- function(survey) {
    nrows <- table(survey['obsID'])
    choices <- list()
    for (i in seq_len(length(nrows))) {
        n <- nrows[i]
        choice <- rep(0, n)
        choice[sample(seq(n), 1)] <- 1
        choices[[i]] <- choice
    }
    return(unlist(choices))
}

getSE <- function(model, size) {
    se <- data.frame(
        size = size,
        se   = model$standErrs)
    se$coef = row.names(se)
    row.names(se) <- NULL
    return(se)
}
