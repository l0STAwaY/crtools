## Here is a general description of the import functions in our code



# Test Document Overview

The Test Document file contains a large example dataset that illustrates how the system is tested. Specificallym the test-interp_chk_select have alot more 
test case avalible if necessary to illustrate the functionality.


## select_ct()

Fits and compares multiple count regression models (Poisson, quasi-Poisson, negative binomial, zero-inflated Poisson, and zero-inflated negative binomial), computes diagnostics, bootstrap confidence intervals, and recommends the best model based on BIC.

## diag_ct()

This function returns a diagnostic that includes AIC, BIC, and McFadden’s R-squared for each model, selecting the best model based on BIC.

It also calculates bootstrap coefficients as well as normal model confidence intervals. `select_ct()` calls this function to graph all intervals together and pick the best model via BIC. Note, that qpoisson return a table with all NA values since it does not have a liklihood and all our
decided metrics are  liklihood based


## interp_ct()

Provides a unified interpretation framework for count regression models fitted using `fit_ct`. The function generates model interpretation text,
interaction summaries, marginal effects tables, and marginal effects diagnostic plots.

This function summarizes:
- Main model interpretation (log-link coefficient interpretation)
- Emmeans-based marginal means for interactions
- Emtrends-based marginal slopes for interactions
- Pairwise contrasts for both emmeans and emtrends
- Diagnostic interaction plots including:
  - Emmeans plots
  - Emtrends plots
  - Johnson–Neyman plots (when available)
 

It supports 

- Factor × Factor:
Only emmeans are shown. ggemmeansplot displays discrete group comparisons.

- Continuous × Factor:
Both emmeans and emtrends are computed.
 ggemmeansplot shows how the response changes across the continuous variable by group.

- Continuous × Continuous:
 Both emmeans and emtrends are computed.
 One continuous variable is evaluated at mean plus or minus 1 SD (or similar representative values),
 while the other is plotted continuously by group.
  When available, a Johnson–Neyman plot is used.


## chk_ct()


Performs a comprehensive diagnostic check for count regression models, including Poisson, quasi-Poisson, negative binomial, and zero-inflated models and mixed effect models. The model prints and does not return anything.

Note that  Zero-inflation tests are performed using simulation-based methods (DHARMa). However, these tests may fail for some model classes (e.g., \code{zeroinfl} from the \code{pscl} package) due to missing \code{simulate()} methods or incompatible model structures. In such cases, the test is automatically skipped.

## summary_overall()

This function computes a comprehensive set of summary statistics for all numeric  variables in a data frame. It is designed for quick exploratory data analysis and
provides both central tendency and dispersion measures, as well as missing data diagnostics and display a ggpair plot if specified


## summary_ct()

Computes basic summary statistics of the count response varaible (5-number summary, variance, variance/ratio, and zero proportion), optionally grouped by a variable.
 Note that count data are technically not continuous summaries need to be interpreted carefully Specifically: The mean of count data can be a non-integer,
 even though actual data values are always integers. For variance in count data it's important to compare it to the mean and consider natural dispersion
#For median and quantiles it is natural to see repeated or integer jumps in values


## plot_ct()

Plots the corresponding count varible


## fit_ct()

Fits a variety of count regression models using a unified interface, including Poisson, quasi-Poisson, negative binomial, zero-inflated models, and mixed-effects versions of poisson and negative binomial.


## jnplot()

plots johnson-neyman plot for only glm's since interaction does not support other models

Computes and visualizes the Johnson-Neyman significance regionfor interactions involving a continuous predictor and moderator.
The function is not implemented for ZeroInflation or effect models because both don't work the interaction package


## ggemmeansplot()

Uses emmeans to plot predicted values from count models.
Automatically handles continuous and categorical predictors and moderators.
Displays confidence intervals when available.

## All the other emmeans and emmtrends tab, contrast, tab 

These are as it's names suggest, helper functions that help us graph out the emmeans emmtrends contrast and table


## Some other notes
Note: that emmtrends and joshnson-neyman are both on the log(response) scale due to  implmentation difficulties but the results are still meaningful
but require careful interpretation

All emmeans functions contrast and table and ggemmeansplot() are both on the response scale


