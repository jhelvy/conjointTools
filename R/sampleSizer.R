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
#' @param data The choice data, formatted as a `data.frame` object.
#' @param obsIDName The name of the column that identifies the `obsID` variable.
#' @param parNames The names of the parameters to be estimated in the model. Must be the same as the column names in the `data` argument. For WTP space models, do not include price in `parNames`.
#' @param nbreaks The number of different sample size groups.
#' @param priceName The name of the column that identifies the `price` variable. Only required for WTP space models. Defaults to `NULL`.
#' @param randPars A named vector whose names are the random parameters and values the distribution: `'n'` for normal or `'ln'` for log-normal. Defaults to `NULL`.
#' @param randPrice The random distribution for the price parameter: `'n'` for normal or `'ln'` for log-normal. Only used for WTP space MXL models. Defaults to `NULL`.
#' @param modelSpace Set to `'wtp'` for WTP space models. Defaults to `"pref"`.
#' @param options A list of options.
#' @return Returns a data frame of the standard error values for different sample sizes.
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
#'
#' # Compute the standard errors for different sample sizes
#' sizeTest <- sampleSizer(
#'    data       = doe,
#'    obsIDName  = 'obsID',
#'    parNames   = c('price', 'type', 'freshness'),
#'    nbreaks    = 10)
#'
#' head(sizeTest)
#' }
sampleSizer = function(data, obsIDName, parNames, nbreaks = 10,
                       priceName = NULL, randPars = NULL, randPrice = NULL,
                       modelSpace = 'pref', options = list()) {
    # Add random choices to data
    data$choice <- generateChoices(data, obsIDName)
    # Loop through subsets of different sized data and get standard errors
    maxObs <- max(data[obsIDName])
    nobs <- ceiling(seq(ceiling(maxObs/nbreaks), maxObs, length.out = nbreaks))
    standardErrors <- list()
    for (i in 1:nbreaks) {
        tempData <- data[which(data[obsIDName] < nobs[i]),]
        model <- logitr::logitr(
            data       = tempData,
            choiceName = 'choice',
            obsIDName  = obsIDName,
            parNames   = parNames,
            priceName  = priceName,
            randPars   = randPars,
            randPrice  = randPrice,
            modelSpace = modelSpace,
            weights    = NULL,
            options    = options)
        standardErrors[[i]] <- getSE(model, nobs[i])
    }
    return(do.call(rbind, standardErrors))
}

generateChoices <- function(data, obsIDName) {
    nrows <- table(data[obsIDName])
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
#' Creates a plot of the sample size results.
#'
#' @param df The data frame of the output from the sampleSizer function.
#' @return Creates a plot of the standard errors for different sample sizes.
#' @export
#' @examples
#' \dontrun{
#' # First run the sampleSizer function
#' sizeTest <- sampleSizer(
#'     data       = yogurt,
#'     obsIDName  = 'obsID',
#'     parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
#'     nbreaks    = 10,
#'     plot       = TRUE)
#'
#' # Plot the results
#' sampleSizePlot(sizeTest)
#' }
sampleSizePlot <- function(df) {
    plot(df$size, df$se, ylab = 'Standard Error',
         xlab = 'Number of observations')
}
