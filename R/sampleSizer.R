#' Estimate standard errors from conjoint design
#'
#' This function allows you to estimate a model on a design of experiment
#' that you have not yet used to collect data. This allows you to learn
#' about deficiencies in your design of experiment and also assess the sample
#' size needed to achieve parameter precision levels before you go out and
#' use the design to collect data. The function fills out the survey with
#' random choices and estimates a model. It does this multiple times with an
#' increasing number of observations, set by the `nbreaks` argument. While the
#' coefficients in those models are meaningless, the _standard errors_ on the
#' coefficients are informative. The example below estimates 10 separate models
#' and then plots the standard errors against the number of observations. In
#' this example, assume that the yogurt data was not a completed survey but
#' rather a blank design of experiment with no observed choices.
#' @keywords logitr, mnl, mxl, logit, sample size
#'
#' @param survey The choice survey data frame exported from the `makeSurvey()` function.
#' @param parNames A vector of the names of the parameters to be estimated in the model. Must be the same as the column names in the `survey` argument.
#' @param parTypes A vector determining the type of each variable: "c" for continuous, or "d" for discrete. Continuous variables will be linearly coded whereas discrete variables will be dummy coded with one level removed for identification. Defaults to `NULL`, in which case all parameters are coded as discrete.
#' @param nbreaks The number of different sample size groups.
#' @param randPars A named vector whose names are the random parameters and values the distribution: `'n'` for normal or `'ln'` for log-normal. Defaults to `NULL`.
#' @param options A list of options to control the model estimation.
#' @return Returns a data frame of the standard error values for different sample sizes.
#' @export
#' @examples
#' \dontrun{
#' }
sampleSizer = function(survey, parNames = NULL, parTypes = NULL, nbreaks = 10,
                       randPars = NULL, options = list(message = FALSE)) {
    # Add random choices to the survey
    survey$choice <- generateChoices(survey)
    # Set parNames
    if (!is.null(parNames)) {
        parNames <- names(survey)[!grepl("ID", names(survey))]
    }
    # Set continuous or discrete parameter types
    if (!is.null(parTypes)) {
        cpars <- parNames[which(parTypes == "c")]
        survey[,cpars] <- as.numeric(survey[,cpars])
    }
    # Loop through subsets of different sample sizes
    # In each iteration, estimate a model and record the standard errors
    maxObs <- max(survey['obsID'])
    nobs <- ceiling(seq(ceiling(maxObs/nbreaks), maxObs, length.out = nbreaks))
    sizes <- round(nobs / max(survey$qID))
    standardErrors <- list()
    for (i in 1:nbreaks) {
        tempSurvey <- survey[which(survey['obsID'] < nobs[i]),]
        model <- logitr::logitr(
            data = tempSurvey, parNames = parNames, randPars = randPars,
            choiceName = 'choice', obsIDName = 'obsID', priceName = NULL,
            randPrice = NULL, modelSpace = 'pref', weights = NULL,
            options = options)
        standardErrors[[i]] <- getSE(model, sizes[i])
    }
    return(do.call(rbind, standardErrors))
}

generateChoices <- function(survey) {
    nrows <- table(survey['obsID'])
    choices <- list()
    for (i in 1:length(nrows)) {
        choice <- rep(0, nrows[i])
        choice[sample(seq(nrows[i]), 1)] <- 1
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

#' Creates a plot of the sample size results.
#'
#' @param df The data frame of the output from the `sampleSizer()` function.
#' @return Returns a plot of the standard errors for different sample sizes.
#' @importFrom ggplot2 ggplot aes geom_point scale_y_continuous labs theme_bw
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' }
sampleSizePlot <- function(df) {
    plot <- ggplot(df) +
        geom_point(aes(x = .data$size, y = .data$se, color = .data$coef),
                   fill = "white", pch = 21) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = 'Number of observations',
             y = 'Standard Error',
             color = "Variable") +
        theme_bw()
    return(plot)
}
