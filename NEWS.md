# conjointTools 0.0.8

- Several updates to the `makeDoe()` function, including an improved print out of the summary of the design (D-efficiency & balanced or not), as well as optional arguments to conduct a search of designs.
- Added a new `evaluateDoe()` function which returns a list of information about a given design, including the D-efficiency and whether or not the design is balanced.
- Changed the name of `recodeDesign()` to `recodeDoe()` and made it more robust to different data types in `levels`.
- Replaced the use of `parallel::mcapply()` in `estimateModels()` that was introduced in 0.0.7 with just `lapply()` as the `parallel` approach is not supported in Windows. Will add more robust parallel implementation in next package update.

# conjointTools 0.0.7

- Updated functions to use new `predict.logitr()` method introduced in {logitr} v0.4.0. 
- Removed dependency on {data.table} and added use of {parallel} to increase speed of the `estimateModels()` function.
- Added a `print.cjtools()` method for a prettier printing of the models object that is returned from the `estimateModels()` function.

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
