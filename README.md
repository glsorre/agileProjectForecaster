# Agile Project Forecaster
The following project a shiny calculator of working days needed to complete an agile project basing on historical data.

The project is published on [shinyapps.io]().

## Methodology
The calculator is able to divide your project in one or more legs, up to 5.

For each leg the user is asked to insert:

* a sample of cycle times;
* the number of stories which will be simulated with the given sample.

Then the calculator will:

* bootstrap the samples creating a distribution of possible sample means;
* run a montecarlo simulation picking randomly from the sample means;
* plot a histogram of the simulations.

The user is able to customize:

* the uantiles to be printed in the histogram;
* the number of predictions created in the bootstraps;
* the number of runs of the montecarlo simulation.

## Reference
It is based on the work of Dimitar Bakardzhiev [published on infoq](http://www.infoq.com/articles/noestimates-monte-carlo) on the 1st of December 2014.
