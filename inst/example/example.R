library(conjointTools)
library(ggplot2)

# Compute and compare standard errors for different sample sizes

# Example 1 -------------------------------------------------------------------

# A simple conjoint experiment about apples

# Define the attributes and levels
levels <- list(
  price     = seq(1, 4, 0.5), # $ per pound
  type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
  freshness = c('Excellent', 'Average', 'Poor')
)

# Make a full-factorial design of experiment
doe <- makeDoe(levels)

# Re-code levels
doe <- recodeDesign(doe, levels)

# Make the survey
survey <- makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 6     # Number of questions per respondent
)
