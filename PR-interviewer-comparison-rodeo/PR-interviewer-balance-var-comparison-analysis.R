# ================================================================== #
# PAPER TITLE
# ================================================================== #

# This file conducts the balance tests and conducts the ttests
# for the trust, community relational health, and psychological 
# safety variables.

# It creates several figures used in the manuscript.
# Figure 1: Balance among Randomized Interviewees (n = 158)
# Figure 2: Difference of Means among Randomized Interviewees (n = 158)

# ================================================================== #

# ----
# Setup

# clear the workspace
rm( list = ls() )

# load the libraries
library( dplyr ) # for working with the data
library( here )  # to call local directory


# ----
# load the data
dat <- readRDS( file = here( "PR-interviewer-comparison-rodeo/trust.rhps.cntrls.vars.data.rds" ) )

# coerce to a data frame
dat <- as.data.frame( dat )


# ----
# load the functions needed that is in the utilities folder
source( here( "PR-interviewer-comparison-utils/PR-interviewer-functions.R" ) )


# ----
# check balance in covariates among those who were randomized

# Get the data you want to compare
balance.dat <- dat %>% 
  select( 
    id, Interviewer, Randomize, 
    Log_Age, White, Black, Hispanic,
    Education, Single, Children, First_Timer,
    Time_In, Sentence_Length, Lifer
    )

# keep 158 cases, table(balance.dat$Randomize == 1)
randomized.dat <- balance.dat[balance.dat$Randomize == 1,]

# table based on who was interviewed
table( randomized.dat$Interviewer )

# Create the variables to perform the tests on
randomized.vars.to.use <- randomized.dat %>% 
  select( -c( id, Interviewer, Randomize ) )

# Create vector of names for the plot
custom_labels <- c(
  "Age\n (Logged)", "White", "Black", "Hispanic", "Years of\n Education", "Single", "Has\n Children",
  "First Time\n in Prison", "Time In\n Prison", "Sentence\n Length", "Lifer"
)

# Create the object of t-test results
randomized.t.results <- t.test.data(
  randomized.dat, randomized.vars.to.use, randomized.dat$Interviewer 
  )

# "Balance among \nRandomized Interviewees (n = 158)"
plot.ttests( randomized.t.results , "" )


# ----
# compare all covariates for those who are in the randomized condition

# Get the data you want to compare
cov.dat <- dat %>% 
  select(
    id, Interviewer, Randomize, 
    trust1:trust15,
    rh1:rh14,
    ps1:ps7
  )

# keep 158 cases
random.cov.dat <- cov.dat[cov.dat$Randomize == 1,]

# Create the variables to perform the tests on
cov.vars.to.use <- random.cov.dat %>% 
  select( -c( id, Interviewer, Randomize ) )

# Create vector of names for the plot
custom_labels <- c(
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
cov.t.results <- t.test.data(
  random.cov.dat, cov.vars.to.use, random.cov.dat$Interviewer 
)

# plot the tests
# "Difference of Means among \nRandomized Interviewees (n = 158)"
plot.ttests( cov.t.results, "" )

# add a legend
legend( "topleft" , 
        legend = legend_labels, 
        title = "Legend", cex = 0.45
)


# ================================================================== #