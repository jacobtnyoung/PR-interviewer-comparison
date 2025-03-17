# ================================================================== #
# Who’s Asking? Participatory Research, Interviewer Effects, 
# and the Production of Knowledge in a Women’s Prison
# ================================================================== #

# This file works with the demographic data for tables
# and creates appendices for comparison with randomized
# and non-randomized participants.

# Objects created in the manuscript:

# Table 1: Descriptive Statistics
# Appendix 1: Means Comparison\n between Randomized and Non-Randomized\n Interviewees (n = 200)

# ================================================================== #

# ----
# Setup

# clear the workspace
rm( list = ls() )

# load the libraries
library( dplyr )     # for working with the data
library( here )      # to call local directory
library( stargazer ) # for creating the table


# ----
# load the data
dat <- readRDS( file = here( "PR-interviewer-rodeo/PR-interviewer-rodeo-data/trust.rhps.cntrls.vars.data.rds" ) )

# coerce to a data frame
dat <- as.data.frame( dat )


# ----
# load the functions needed that is in the utilities folder
source( here( "PR-interviewer-utils/PR-interviewer-functions.R" ) )


# ----
# create a table of demographics

# variables to use for the table
select_vars_to_use <- dat |> 
  select( -c( id ) ) 

# create the descriptive stats objects

all_desc <- cbind(
  round( apply( select_vars_to_use, 2, function( x ) mean( x, na.rm = TRUE ) ), 2 ),
  round( apply( select_vars_to_use, 2, function( x )   sd( x, na.rm = TRUE ) ), 2 )
)

randomized_desc <- cbind(
  round( apply( select_vars_to_use[select_vars_to_use$Randomize == 1,], 2, function( x ) mean( x, na.rm = TRUE ) ), 2 ),
  round( apply( select_vars_to_use[select_vars_to_use$Randomize == 1,], 2, function( x )   sd( x, na.rm = TRUE ) ), 2 )
)

nonrandomized_desc <- cbind(
  round( apply( select_vars_to_use[select_vars_to_use$Randomize == 0,], 2, function( x ) mean( x, na.rm = TRUE ) ), 2 ),
  round( apply( select_vars_to_use[select_vars_to_use$Randomize == 0,], 2, function( x )   sd( x, na.rm = TRUE ) ), 2 )
)

# ----
# build the table

# this requires some cleanup after rendering

stargazer(
  cbind( all_desc, randomized_desc, nonrandomized_desc ),
  title = "Table 1: Descriptive Statistics",
  column.labels = c(
    "All (Mean)", "All (SD)", 
    "Randomized (Mean)", "Randomized (SD)", 
    "Nonrandomized (Mean)", "Nonrandomized (SD)"
  ),
  out = here( "table.doc" ),
  type = "html",
  digits = 2,
  covariate.labels = c( 
    "Age", 
    "Age (Logged)",
    "Interviewer (Inside = 1)",
    "Randomized (Random = 1)",
    "White", 
    "Black", 
    "Hispanic", 
    "Years of Education", 
    "Single", 
    "Has Children",
    "First Time in Prison", 
    "Time In Prison", 
    "Sentence Length", 
    "Lifer",
    paste("T ",rep( 1:15 ), sep = "" ),
    paste("CRH ",rep( 1:14 ), sep = "" ),
    paste("PS ",rep( 1:7 ), sep = "" )
    ) 
  )


# ----
# check balance in covariates comparing randomized to non-randomized
# variables used for the comparisons
select_vars_to_use <- dat %>% 
  select( -c( id, Interviewer, Randomize ) )

# Create vector of names for the plot
custom_labels <- c(
  "Age", "Age (Logged)", "White", "Black", "Hispanic", "Years of Education", "Single", "Has Children",
  "First Time in Prison", "Time In Prison", "Sentence Length", "Lifer",
  paste("T ",rep( 1:15 ), sep = "" ),
  paste("CRH ",rep( 1:14 ), sep = "" ),
  paste("PS ",rep( 1:7 ), sep = "" )
)

# labels for the legend
legend_labels <- c( 
  "T = Trust Item", 
  "CRH = Community Relational Health Item", 
  "PS =  Psychological Safety Item" 
)

# Create the object of t-test results
randomized_diff_results <- t.test.data(
  dat, select_vars_to_use, dat$Randomize 
)

# set up plot parameters
op <- par( 
  oma = c( 3.5, 0, 0, 0 )
)

# Appendix 1: Means Comparison\n between Randomized and Non-Randomized\n Interviewees (n = 200)
plot.ttests( randomized_diff_results, "" )

# add a legend
legend( "topright" , 
        legend = legend_labels, 
        title = "Legend", cex = 0.5
)

par( op ) 


# ################################################################## #