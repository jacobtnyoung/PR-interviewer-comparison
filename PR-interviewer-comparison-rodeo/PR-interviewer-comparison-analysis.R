# ================================================================== #
# PAPER TITLE
# ================================================================== #

# DOCUMENT!!!


# ================================================================== #
# Setup ----

# Clear the workspace
rm( list = ls() )

# Load the libraries
library( dplyr ) # for working with the data
library( here )  # to call local directory

 
# ================================================================== #
# load the data
dat <- readRDS( file = here( "PR-interviewer-comparison-rodeo/trust.rhps.cntrls.vars.data.rds" ) )

# coerce to a data frame
dat <- as.data.frame( dat )


# ================================================================== #
# Create the function to build the set of t.tests

t.test.data <- function( dat.to.use, vars.to.use, group.var, alpha = 0.05 ){
  
  # Create the objects to assign values
  tests <- as.list( NULL )
  values <- NA
  pval <- NA
  uci <- NA
  lci <- NA
  
  # Perform the tests
  for( i in 1: ncol( vars.to.use ) ){
    tests[[i]] <-
      t.test( vars.to.use[[i]] ~ group.var, data = dat.to.use )
  }
  
  # Extract the values
  for( i in 1: length( tests ) ){
    values[i] <- round( ( tests[[i]]$estimate[1] - tests[[i]]$estimate[2] ), 3 )
    pval[i]   <- round( tests[[i]]$p.value, 3 )
    lci[i]    <- round( tests[[i]]$conf.int[1], 3 )
    uci[i]    <- round( tests[[i]]$conf.int[2], 3 )
  }
  
  # Create the bonferroni corrected pvalues
  bonf.pval <- round( alpha / length( tests ), 3 )
  
  # Create reject/fail to reject
  reject <- NULL
  reject <- pval < bonf.pval
  
  # Build the data object
  results <- cbind( values, pval, bonf.pval, reject, lci, uci )
  colnames( results ) <- c( "tvalue", "pvalue", "Bonf.Pvalue", "Reject?", "LCI", "UCI" )
  rownames( results ) <- names( vars.to.use )
  
  return( as.data.frame( results ) )
}


# ================================================================== #
# Create the function to plot the results

plot.ttests <- function( t.results, the.title ){
  
  point    <- t.results$tvalue
  upper.ci <- t.results$UCI
  lower.ci <- t.results$LCI
  
  x.ax <- seq( 1, dim( t.results )[1], length.out = dim( t.results )[1] )
  y.ax <- seq( 
    min( c( upper.ci,lower.ci ) ), 
    max( c( upper.ci,lower.ci ) ), 
    length.out= dim( t.results )[1] 
    )
  
  plot(x.ax,
       y.ax,
       type = "n",
       ylab = "t-test coefficient w/ 95% CI",
       xlab = "",
       xaxt = "n"
  )
  
  points( x.ax, point )
  segments(x.ax, upper.ci, x.ax, lower.ci)
  abline( h = 0, lty = 2 )
  axis( side = 1, at = x.ax, las = 3, labels = custom_labels )
  title( the.title )
}


# ================================================================== #
# Check balance in covariates among those who were randomized

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
plot.ttests( randomized.t.results, "Figure #: Balance among \nRandomized Interviewees (n = 158)" )


# ================================================================== #
# Compare all covariates for those who are in the randomized condition.

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
plot.ttests( cov.t.results, "Figure #: Difference of Means among \nRandomized Interviewees (n = 158)"  )

# add a legend
legend( "topleft" , 
        legend = legend_labels, 
        title = "Legend", cex = 0.45
)



!!!HERE WITH WORKING THROUGH AND CLEANING THIS UP

















# ================================================================== #
# Check balance in covariates comparing randomized to non-randomized

###!!!!THIS WILL BE AN APPENDIX WHERE YOU INCLUDE ALL THE VARIABLES
### AND DO TTESTS ON THEM AND PRESENT IT



# Create the variables to perform the tests on.
select.vars.to.use <- balance.dat %>% 
  select( -c( id, Interviewer, Randomize ) )

# Create vector of names for the plot
custom_labels <- c(
  "Age\n (Logged)", "White", "Black", "Hispanic", "Years of\n Education", "Single", "Has\n Children",
  "First Time\n in Prison", "Time In\n Prison", "Sentence\n Length", "Lifer"
)

# Create the object of t-test results
select.t.results <- t.test.data(
  balance.dat, select.vars.to.use, balance.dat$Randomize 
)
plot.ttests( select.t.results, "Figure #: Demographic Comparison between Randomized and Non-Randomizedamong \nRandomized Interviewees (n = 158)" )


# ================================================================== #
# Check balance in covariates comparing randomized to non-randomized

# Create the variables to perform the tests on.
select.vars.to.use <- balance.dat %>% 
  select( -c( id, Interviewer, Randomize ) )

# Create the object of t-test results.
select.t.results <- t.test.data(
  balance.dat, select.vars.to.use, balance.dat$Randomize 
)

# # !!!HERE: going through
# # this needs to just compare those who were randomzied with those who were not randomized
# # to see if there was a difference; this will be an appendix

# # # keep 101 cases where the interviewer is from ASU.
# # asu.dat <- balance.dat[balance.dat$InterviewerType == 0,]
# # asu.vars.to.use <- asu.dat %>% 
# #   select( -c( id, InterviewerType, Randomize ) )
# # asu.t.results <- t.test.data(
# #   asu.dat, asu.vars.to.use, asu.dat$Randomize 
# # )
# # plot.ttests( asu.t.results, "Comparing Randomized to Non-Randomized\n for ASU Interviewed (n = 101)" )

# # # keep 97 cases where the interviewer is from Cruz.
# # cruz.dat <- balance.dat[balance.dat$InterviewerType == 1,]
# # cruz.vars.to.use <- cruz.dat %>% 
# #   select( -c( id, InterviewerType, Randomize ) )
# # cruz.t.results <- t.test.data(
# #   cruz.dat, cruz.vars.to.use, cruz.dat$Randomize 
# # )
# # plot.ttests( cruz.t.results, "Comparing Randomized to Non-Randomized\n for Cruz Interviewed (n = 97)" )
# # 
# # t.test( cruz.vars.to.use$timein_yrs ~ cruz.dat$Randomize, data = cruz.dat )








!!!HERE WITH WORKING THROUGH



# keep 42 cases.
norandom.cov.dat <- cov.dat[cov.dat$Randomize == 0,]

# Create the variables to perform the tests on.
norcov.vars.to.use <- norandom.cov.dat %>% 
  select( -c( id, Interviewer, Randomize ) )

# Create the object of t-test results.
norcov.t.results <- t.test.data(
  norandom.cov.dat, norcov.vars.to.use, norandom.cov.dat$Interviewer 
)

plot.ttests( norcov.t.results, "Comparing Means among \nNon-Randomized Interviewees (n = 42)"  )


# All the cases.
vars.to.use <- cov.dat %>% 
  select( -c( id, Interviewer, Randomize ) )

# Create the object of t-test results.
cov.t.results <- t.test.data(
  cov.dat, vars.to.use, cov.dat$Interviewer 
)

plot.ttests( cov.t.results, "Comparing Means among \nAll Interviewees (n = 200)"  )


# ================================================================== #
# Plot all comparisons.

op <- par( mfrow = c( 3,2 ) )
plot.ttests( randomized.t.results, "Balance among \nRandomized Interviewees (n = 158)" )
plot.ttests( asu.t.results, "Comparing Randomized to Non-Randomized\n for ASU Interviewed (n = 101)" )
plot.ttests( cruz.t.results, "Comparing Randomized to Non-Randomized\n for Cruz Interviewed (n = 97)" )
plot.ttests( cov.t.results, "Comparing Means among \nRandomized Interviewees (n = 158)"  )
plot.ttests( cov.t.results, "Comparing Means among \nAll Interviewees (n = 200)"  )
par( op )


# ################################################################## #
# END OF SYNTAX FILE.
# ################################################################## #