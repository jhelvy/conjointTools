library(conjointTools)
library(ggplot2)

# Generate a full factorial design of experiment
doe1 <- makeDoe(levels = c(3, 3, 3))

# Generate a full factorial design of experiment about apples
doe2 <- makeDoe(
    levels = c(3, 3, 3),
    varNames = c("price", "type", "freshness"),
    type = "full"
)

# Generate a "D-optimal" fractional factorial design of experiment about apples
doe3 <- makeDoe(
    levels = c(3, 3, 3),
    varNames = c("price", "type", "freshness"),
    type = "D",
    nTrials = 15
)

# Make the coded conjoint survey
survey <- makeSurvey(
    doe       = doe2, # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 6     # Number of questions per respondent
)

# Compute standard errors for different sample sizes
results <- getPower(
    survey   = survey,
    parNames = c('price', 'type', 'freshness'),
    parTypes = c('c', 'd', 'd'), # Set continuous vs. discrete variables
    nbreaks  = 10
)

# Preview results
head(results)

# Plot results
ggplot(results) +
    geom_point(aes(x = size, y = se, color = coef),
               fill = "white", pch = 21) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = 'Number of observations',
         y = 'Standard Error',
         color = "Variable") +
    theme_bw()
