library(conjointTools)
library(ggplot2)

# Compute and compare standard errors for different sample sizes

# Example 1 -------------------------------------------------------------------

# A simple conjoint design with no specified attribute names

# Make the design of experiment
doe <- makeDoe(levels = c(3, 3, 3))

# Make the survey
survey <- makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 6     # Number of questions per respondent
)

# Compute sample sizes
results <- sampleSizer(survey)

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



# Example 2 -------------------------------------------------------------------

# A simple conjoint experiment about apples, with one attribute (price)
# modeled as continuous

# Make the design of experiment
doe <- makeDoe(
    levels = c(3, 3, 3),
    varNames = c("price", "type", "freshness"),
    type = "full"
)

# Make the survey
survey <- makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 6     # Number of questions per respondent
)

# Compute sample sizes
results <- sampleSizer(
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
