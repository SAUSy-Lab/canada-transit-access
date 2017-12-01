## canada-transit-access

various scripts for computing, analyzing, and visualizing measures of transit accessibility


#### access_cumulative.R
computes a measure of cumulative accessibility i.e. the number of opportunities reachable within a travel time threshold. Extended to compute for a range of thresholds

#### access_gravity.R
computes a measure of accessibility weighting nearby opportunities more than those further away using an inverse-power decay function

#### access_floating_catchment.R
extension of the gravity measure, to compute a measure of access to the labour force, weighting by the accessibility at location i, and accounting for two travel modes

#### plot_access_v_householdincome.R
simple plots of transit access v. household income for neighbourhoods

#### plot_cumulative_access_by_threshold.R
plots of cumulative accessibility for different travel time thresholds

#### stats_gini.R
computes simple summary stats as well as Gini coefficients for a vector of access scores - also plots Lorenz curves
