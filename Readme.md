Readme Models
================

## Basic SIR Model with demography

Model Assumptions:

  - The infection circulates in a population of size \(N,\) with a per
    capita background death rate, \(\mu\) which is balanced by a birth
    rate \(\mu N\) From the sum of Eqs. (2.1)– (2.3),
    \(\frac{dN}{dt} = 0\) and \(N = S + I + R\) is thus constant.
  - The infection causes acute morbidity (not mortality); That is, in
    this version of the SIR model we assume we can ignore
    disease-induced mortality. This is reasonable for certain infections
    like chickenpox, but certainly not for others like rabies, SARS, or
    ebola.
  - Individuals are recruited directly into the susceptible class at
    birth (so we ignore perinatal maternal immunity).
  - Transmission of infection from infectious to susceptible individuals
    is controlled by a bilinear contact term \(\beta I S\) This stems
    from the assumption that the \(I\) infectious individuals are
    independently and randomly mixing with all other individuals, so the
    fraction \(S/N\) of the encounters is with susceptible individuals;
    \(\beta\) is the contact rate times the probability of transmission
    given a contact between a susceptible and an infectious individual.
  - Chances of recovery or death is assumed not to change during the
    course of infection.
  - Infectiousness is assumed not to change during the course of
    infection.
  - Infected individuals move directly into the the infectious class (as
    opposed to the SEIR model;and remains there for an average
    infectious period of \(1/\gamma\) (assuming \(\mu << \gamma\)).
  - The model assumes that recovered individuals are immune from
    reinfection for life.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](Readme_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
