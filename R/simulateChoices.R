#' Simulate choices for a `survey` data frame
#'
#' Simulate choices for the `survey` data frame, either randomly or according
#' to a utility model defined by user-provided parameters. If providing
#' parameters, only the parameters provided will be used to simulate choices.
#'
#' @param survey The choice survey data frame exported from the `makeSurvey()`
#' function.
#' @param obsIDName The name of the column that identifies each choice
#' observation. Defaults to `"obsID"`.
#' @param ... One or more parameters separated by commas that define
#' the "true" utility model used to simulate choices for the `survey` data
#' frame. If no parameters are included, choices will be randomly assigned.
#' @return Returns the `survey` data frame with an additional `choice` column
#' identifying the simulated choices.
#' @export
#' @examples
#' \dontrun{
#' library(conjointTools)
#'
#' # Make a design of experiment
#' doe <- makeDoe(
#'     levels = c(3, 3, 3),
#'     varNames = c("price", "type", "freshness"),
#' )
#'
#' # Make a survey
#' survey <- makeSurvey(
#'     doe       = doe,  # Design of experiment
#'     nResp     = 1000, # Total number of respondents (upper bound)
#'     nAltsPerQ = 3,    # Number of alternatives per question
#'     nQPerResp = 6     # Number of questions per respondent
#' )
#'
#' # Simulate random choices for the survey
#' data <- simulateChoices()
#'
#' # Simulate choices based on a utility model with a single "price" parameter
#' # and two discrete parameters for both "freshness" and "type"
#' data <- simulateChoices(
#'     survey = survey,
#'     obsIDName = "obsID",
#'     price        = 0.1,
#'     freshness    = c(0.05, 0.1),
#'     type         = c(0, 0.5)
#' )
#'
#' # Simulate choices based on a utility model with a single "price" parameter,
#' # two discrete, fixed parameters for "freshness", two random normal
#' # parameters for "type", and interaction parameters for "price" with "type"
#' data <- simulateChoices(
#'     survey = survey,
#'     obsIDName = "obsID",
#'     price        = 0.1,
#'     freshness    = c(0.05, 0.1),
#'     type         = randN(mu = c(0, 0.5), sigma = c(1, 2)),
#'     `price*type` = c(1, 2)
#' )
#' }
simulateChoices = function(survey, obsIDName = "obsID", ...) {
    pars <- list(...)
    if (length(pars) == 0) {
        return(simulateRandomChoices(survey, obsIDName))
    }
    return(simulateUtilityChoices(survey, obsIDName, pars))
}

simulateRandomChoices <- function(survey, obsIDName) {
    nrows <- table(survey[obsIDName])
    choices <- list()
    for (i in seq_len(length(nrows))) {
        n <- nrows[i]
        choice <- rep(0, n)
        choice[sample(seq(n), 1)] <- 1
        choices[[i]] <- choice
    }
    survey$choice <- unlist(choices)
    return(survey)
}

simulateUtilityChoices <- function(survey, obsIDName, pars) {
    model <- defineTrueModel(survey, pars)
    result <- logitr::predictChoices(model, model$survey, obsIDName)
    result$choice <- result$choice_predict # Rename choice column
    result$choice_predict <- NULL
    return(result)
}

defineTrueModel <- function(survey, pars) {
    parNamesFull <- names(pars)
    parNames <- dropInteractions(names(pars))
    # Separate out random and fixed parameters
    parNamesRand <- names(pars[lapply(pars, class) == "list"])
    parNamesFixed <- parNames[! parNames %in% parNamesRand]
    # Re-code continuous survey vars as numeric
    cNames <- getContinuousParNames(pars, parNamesFixed, parNamesRand)
    if (length(cNames) > 0) {
      survey[,cNames] <- lapply(survey[cNames], as.numeric)
    }
    # Define all other model objects
    randPars <- unlist(lapply(pars[parNamesRand], function(x) x$type))
    codedData <- logitr::recodeData(survey, parNamesFull, randPars)
    parNamesCoded <- codedData$parNames
    randParsCoded <- codedData$randPars
    parSetup <- getParSetup(parNamesCoded, randParsCoded)
    coefs <- getCoefficients(pars, parNamesCoded, randPars, randParsCoded)
    numDraws <- 10^4
    return(structure(list(
      coef       = coefs,
      modelType  = ifelse(length(parNamesRand) > 0, "mxl", "mnl"),
      modelSpace = "pref",
      priceName  = NULL,
      parNames   = parNamesFull,
      randPars   = randPars,
      parSetup   = parSetup,
      survey     = survey,
      options    = list(numDraws = numDraws),
      standardDraws = createStandardDraws(parSetup, numDraws)
    ),
    class = "logitr"
    ))
}

dropInteractions <- function(parNames) {
    ints <- grepl("\\*", parNames)
    if (any(ints)) {
      return(parNames[ints == FALSE])
    }
    return(parNames)
}

getContinuousParNames <- function(pars, parNamesFixed, parNamesRand) {
    nFixed <- unlist(lapply(pars[parNamesFixed], length))
    nRand <- unlist(
        lapply(pars[parNamesRand], function(x) length(x$pars[[1]])))
    nlevels <- c(nRand, nFixed)
    return(names(nlevels[nlevels == 1]))
}

getParSetup <- function(parNames, randPars) {
  parSetup <- rep("f", length(parNames))
  for (i in seq_len(length(parNames))) {
    name <- parNames[i]
    if (name %in% names(randPars)) {
      parSetup[i] <- randPars[name]
    }
  }
  names(parSetup) <- parNames
  return(parSetup)
}

getCoefficients <- function(pars, parNamesCoded, randPars, randParsCoded) {
  # Define random parameter names
  parNamesRand <- names(randPars)
  parNamesRandCoded <- names(randParsCoded)
  # Get all fixed parameters
  parsFixed <- unlist(pars[! names(pars) %in% parNamesRand])
  names(parsFixed) <- parNamesCoded[! parNamesCoded %in% parNamesRandCoded]
  if (length(randPars) == 0) {
    return(parsFixed)
  }
  # Get all the random parameters
  parsRand_mu <- unlist(lapply(pars[parNamesRand], function(x) x$pars$mu))
  names(parsRand_mu) <- parNamesRandCoded
  parsRand_sigma <- unlist(
    lapply(pars[parNamesRand], function(x) x$pars$sigma))
  names(parsRand_sigma) <- paste(parNamesRandCoded, "sigma", sep = "_")
  # Order and rename the coefficients
  coefs <- c(parsFixed, parsRand_mu)
  coefs <- coefs[parNamesCoded]
  newNames <- parNamesCoded
  newNames[which(newNames %in% parNamesRandCoded)] <- paste(
    parNamesRandCoded, "mu", sep = "_")
  names(coefs) <- newNames
  # Add the sigma coefficients
  coefs <- c(coefs, parsRand_sigma)
  return(coefs)
}

#' Define "true" model parameters as normally-distributed.
#'
#' Define "true" model parameters as normally-distributed. Used in the
#' `simulateChoices()` function.
#'
#' @param mu Vector of means, defaults to `0`.
#' @param sigma Vector of standard deviations, defaults to `1`.
#' @return A list defining normally-distributed parameters of the "true"
#' utility model used to simulate choices in the `simulateChoices()` function.
#' @export
#' @examples
#' \dontrun{
#' library(conjointTools)
#' }
randN <- function(mu = 0, sigma = 1) {
    return(list(pars = list(mu = mu, sigma = sigma), type = "n"))
}

#' Define "true" model parameters as normally-distributed.
#'
#' Define "true" model parameters as normally-distributed. Used in the
#' `simulateChoices()` function.
#'
#' @param mu Mean of the distribution on the log scale, defaults to `0`.
#' @param sigma Standard deviation of the distribution on the log scale,
#' defaults to `1`.
#' @return A list defining log-normally-distributed parameters of the "true"
#' utility model used to simulate choices in the `simulateChoices()` function.
#' @export
#' @examples
#' \dontrun{
#' library(conjointTools)
#' }
randLN <- function(mu = 0, sigma = 1) {
    return(list(pars = list(mu = mu, sigma = sigma), type = "ln"))
}

# Functions modified from logitr because they aren't exported
# These are needed for defining the "true" model

# Modified from logitr::getStandardDraws()
createStandardDraws <- function(parSetup, numDraws) {
  draws <- as.matrix(
    randtoolbox::halton(numDraws, length(parSetup), normal = TRUE))
  draws[, which(parSetup == "f")] <- rep(0, numDraws)
  return(draws)
}
