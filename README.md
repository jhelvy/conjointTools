
<!-- README.md is generated from README.Rmd. Please edit that file -->

# conjointTools

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/conjointTools)](https://CRAN.R-project.org/package=conjointTools)
<!-- badges: end -->

This package contains tools for designing choice based conjoint survey
experiments.

## Installation

The current version is not yet on CRAN, but you can install it from
Github using the **devtools** library:

``` r
devtools::install_github("jhelvy/conjointTools")
```

Load the library with:

``` r
library(conjointTools)
```

## Make experiment designs

The first step in designing an experiment is to define the attributes
and levels for your experiment. Many of the functions in {conjointTools}
are more convenient to use if you define these as a separate object. For
example, let’s say you’re designing a conjoint experiment about apples.
You might have the following attributes and levels:

``` r
# Define the attributes and levels
levels <- list(
  price     = seq(1, 4, 0.5), # $ per pound
  type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
  freshness = c('Excellent', 'Average', 'Poor')
)
```

With these levels defined, making the design of experiment is
straightforward using the `makeDoe()` function:

``` r
# Make a full-factorial design of experiment
doe <- makeDoe(levels)
head(doe)
#>   price type freshness
#> 1     1    1         1
#> 2     2    1         1
#> 3     3    1         1
#> 4     4    1         1
#> 5     5    1         1
#> 6     6    1         1
```

You can also make an “optimized” fractional-factorial design based on
different criteria. For example, to make a “D-optimal” design, add the
`type = "D"` argument and specify the number of trials you want to use
from the full factorial design:

``` r
# Make a full-factorial design of experiment
doe <- makeDoe(levels, type = "D", nTrials = 50)
head(doe)
#>   price type freshness
#> 1     1    1         1
#> 2     4    1         1
#> 3     7    1         1
#> 4     2    2         1
#> 5     3    2         1
#> 6     5    2         1
```

Once you’ve made your design, you can easily re-code it using the actual
labels in your `levels` object using `recodeDesign()`:

``` r
# Re-code levels
doe <- recodeDesign(doe, levels)
head(doe)
#>   price type freshness
#> 1     1 Fuji Excellent
#> 2     4 Fuji Excellent
#> 3     7 Fuji Excellent
#> 4     2 Gala Excellent
#> 5     3 Gala Excellent
#> 6     5 Gala Excellent
```

## Make conjoint surveys

A survey can be generated by randomly sampling from the design of
experiment. The `makeSurvey()` function also eliminates the possibility
of duplicate alternatives appearing in the same choice question:

``` r
survey <- makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 6     # Number of questions per respondent
)

dim(survey)
#> [1] 18000     7
head(survey)
#>   respID qID altID obsID price          type freshness
#> 1      1   1     1     1     1          Fuji Excellent
#> 2      1   1     2     1     2          Gala Excellent
#> 3      1   1     3     1     3    Honeycrisp      Poor
#> 4      1   2     1     2     4 Red Delicious      Poor
#> 5      1   2     2     2     5     Pink Lady Excellent
#> 6      1   2     3     2     2     Pink Lady      Poor
```

The resulting data frame includes the following additional columns:

-   `respID`: Identifies each survey respondent.
-   `qID`: Identifies the choice question answered by the respondent.
-   `altID`:Identifies the alternative in any one choice observation.
-   `obsID`: Identifies each unique choice observation across all
    respondents.

## Version and License Information

-   Date First Written: *October 23, 2020*
-   Most Recent Update: June 23 2021
-   License:
    [MIT](https://github.com/jhelvy/conjointTools/blob/master/LICENSE.md)
-   [Latest
    Release](https://github.com/jhelvy/conjointTools/releases/latest):
    0.0.2

## Citation Information

If you use this package for in a publication, we would greatly
appreciate it if you cited it - you can get a bibtex citation entry with
`citation("conjointTools")`:

``` r
citation("conjointTools")
#> 
#> To cite conjointTools in publications use:
#> 
#>   John Paul Helveston, Martin Lukac, Alberto Stefanelli (2020).
#>   conjointTools: Tools For Designing Conjoint Survey Experiments.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {conjointTools: Tools For Designing Conjoint Survey Experiments},
#>     author = {John Paul Helveston and Martin Lukac and Alberto Stefanelli},
#>     year = {2020},
#>     note = {R package version 0.0.3},
#>     url = {https://jhelvy.github.io/conjointTools/},
#>   }
```
