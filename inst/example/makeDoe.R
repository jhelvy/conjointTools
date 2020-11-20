# Generate a full factorial design about apples
ff <- expand.grid(
    price     = seq(1, 4, 0.5), # $ per pound
    type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
    freshness = c('Excellent', 'Average', 'Poor')
)

# Make the doe
doe <- makeDoe(
    ff        = ff,   # Full factorial design
    nResp     = 3000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 6     # Number of questions per respondent
)
