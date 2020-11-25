library(conjointTools)

# Generate a full factorial design of experiment
doe <- makeDoe(levels = c(3, 3, 3))

# Generate a full factorial design of experiment about apples
doe <- makeDoe(
    levels = c(3, 3, 3),
    varNames = c("price", "type", "freshness"),
    type = "full"
)

# Generate a "D-optimal" fractional factorial design of experiment about apples
doe <- makeDoe(
    levels = c(3, 3, 3),
    varNames = c("price", "type", "freshness"),
    type = "D",
    nTrials = 15
)
