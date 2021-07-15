#' Estimate the same model on different size subsets of a data set
#'
#' This function estimates the same model multiple times using different size
#' subsets of a set of choice data. The number of models to run is set by the
#' `nbreaks` argument, which breaks up the data into groups of increasing
#' sample sizes. All models are estimated models using the {logitr} package.
#' @keywords logitr, mnl, mxl, logit, sample size, power
#'
#' @param nbreaks The number of different sample size groups.
#' @param data A data frame containing choice observations.
#' @param parNames The names of the parameters to be estimated in the model.
#' Must be the same as the column names in the `data` argument. For WTP space
#' models, do not include price in `parNames`.
#' @param choiceName The name of the column that identifies the choice variable.
#' @param obsIDName The name of the column that identifies each choice
#' observation.
#' @param priceName The name of the column that identifies the price variable.
#' Only required for WTP space models. Defaults to `NULL`.
#' @param randPars A named vector whose names are the random parameters and
#' values the distribution: `'n'` for normal or `'ln'` for log-normal.
#' Defaults to `NULL`.
#' @param randPrice The random distribution for the price parameter: `'n'` for
#' normal or `'ln'` for log-normal. Only used for WTP space MXL models.
#' Defaults to `NULL`.
#' @param modelSpace Set to `'wtp'` for WTP space models. Defaults to `"pref"`.
#' @param weightsName The name of the column that identifies the weights to be
#' used in model estimation. Optional. Defaults to `NULL`.
#' @param clusterName The name of the column that identifies the cluster
#' groups to be used in model estimation. Optional. Defaults to `NULL`.
#' @param robust Determines whether or not a robust covariance matrix is
#' estimated. Defaults to `FALSE`. Specification of a clusterName or
#' weightsName will override the user setting and set this to `TRUE' (a
#' warning will be displayed in this case). Replicates the functionality of
#' Stata's cmcmmixlogit.
#' @param options A list of options to control the model estimation. See the
#' `options` argument in the {logitr} package for details.
#' @return Returns a nested data frame with each estimated model object in
#' the `model` column.
#' @importFrom data.table "data.table" ":="
#' @export
#' @examples
#' \dontrun{
#' library(conjointTools)
#'
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Make a full-factorial design of experiment and recode the levels
#' doe <- makeDoe(levels)
#' doe <- recodeDesign(doe, levels)
#'
#' # Make the survey
#' survey <- makeSurvey(
#'     doe       = doe,  # Design of experiment
#'     nResp     = 2000, # Total number of respondents (upper bound)
#'     nAltsPerQ = 3,    # Number of alternatives per question
#'     nQPerResp = 6     # Number of questions per respondent
#' )
#'
#' # Simulate random choices for the survey
#' data <- simulateChoices(
#'     survey    = survey,
#'     altIDName = "altID",
#'     obsIDName = "obsID"
#' )
#'
#' # Estimate models with different sample sizes
#' models <- estimateModels(
#'     nbreaks     = 10,
#'     data        = data,
#'     parNames    = c("price", "type", "freshness"),
#'     choiceName  = "choice",
#'     obsIDName   = "obsID"
#' )
#' }
estimateModels <- function(
    nbreaks = 10,
    data,
    parNames,
    choiceName  = "choice",
    obsIDName   = "obsID",
    priceName   = NULL,
    randPars    = NULL,
    randPrice   = NULL,
    modelSpace  = "pref",
    weightsName = NULL,
    clusterName = NULL,
    robust      = FALSE,
    options     = list()
) {
    # Initiate objects created in data.table so R CMD check won't complain
    model <- NULL
    d <- data.table::data.table(data)
    maxObs <- max(d[, get(obsIDName)])
    nObs <- ceiling(seq(ceiling(maxObs/nbreaks), maxObs, length.out = nbreaks))
    sampleSize <- round(nObs / max(d$qID))
    subsets <- list()
    for (i in 1:nbreaks) {
        subsets[[i]] <- d[get(obsIDName) < nObs[i],]
    }
    d <- data.table::data.table(
      sampleSize = sampleSize,
      data = subsets
    )
    d[, model := lapply(data, function(x) suppressMessages(logitr::logitr(
        data        = x,
        parNames    = parNames,
        randPars    = randPars,
        choiceName  = choiceName,
        obsIDName   = obsIDName,
        priceName   = priceName,
        randPrice   = randPrice,
        modelSpace  = modelSpace,
        weightsName = weightsName,
        clusterName = clusterName,
        robust      = robust,
        options     = options)))
    ]
    return(d)
}
