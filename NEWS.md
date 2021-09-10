# conjointTools 0.0.6

Improved how the models are estimated in `estimateModels()` for increased speed using a nested approach with {data.table}.

# conjointTools 0.0.5

Updated functions to be consistent with some breaking changes introduced when {logitr} was updated from 0.2.0 to 0.3.0, which is necessary since {logitr} is the underlying package used to estimate models in the power analyses.

# conjointTools 0.0.4

- Added `estimateModels()` and `getModelResults()` functions for conducting a power analysis.
- Made small changes to improve the `simulateChoices()` function.
- Added support for adding an outside good in the `makeSurvey()` function.

# conjointTools 0.0.3

- Major breaking changes made to the user interface.
- Overhaul of the `makeDoe()` and `makeSurvey()` functions to enable grouped designs.
- Added the `recodeDesign()` function.
- Added preliminary functions for simulating choices.

# conjointTools 0.0.2

Interactions!

## Summary of larger updates:

- Added support for creating interactions among variables
- Removed summary plot function to drop ggplot dependencies

## Summary of smaller updates:

- Added a few new examples on how to use the interactions argument.

# conjointTools 0.0.1

- Created new functions for creating different experiment designs:
    - Full factorial
    - "D", "A", and "I" optimal designs
- Modified previous functions for creating a coded survey from a given experiment design
- Changed the output of the sampleSizer function to show the sample size (rather than the number of observations, which can be different since respondents may answer more than one choice question).

# conjointTools 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
