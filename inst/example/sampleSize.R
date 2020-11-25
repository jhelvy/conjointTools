library(conjointTools)

# Compute and compare standard errors for different sample sizes

# Generate a full factorial design of experiment about apples
doe <- makeDoe(
    levels = c(3, 3, 3),
    varNames = c("price", "type", "freshness"),
    type = "full"
)

# Make the coded conjoint survey
survey <- makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 6     # Number of questions per respondent
)

# Compute the standard errors for different sample sizes
sizeTest <- sampleSizer(
    survey   = survey,
    parNames = c('price', 'type', 'freshness'),
    nbreaks  = 10
)

# Preview results
head(sizeTest)

# Plot results
sampleSizePlot(sizeTest)
