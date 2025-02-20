# ================================================================== #
# Perryville PAR Interviewer Comparison.
# ================================================================== #

# This file prepares and analyzes the S4Q12a variable.

# ================================================================== #

# ----
# Clear workspace
rm( list = ls() )

# ----
# load the needed libraries
library( here ) # used for referencing local directory
library( haven ) # used for reading stata files
library( dplyr ) # for filtering data
library( irr ) # for the reliability analysis


# ================================================================== #

# ----
# Read in the files coded by the research team

# ASU interviewer files
dat.asu.A <- read.csv( here( "PR-interviewer-comparison-wrangling/interviewer-asu-to-code-ALEXIS.csv" ) )
dat.asu.K <- read.csv( here( "PR-interviewer-comparison-wrangling/interviewer-asu-to-code-KEVIN.csv" ) )
dat.asu.J <- read.csv( here( "PR-interviewer-comparison-wrangling/interviewer-asu-to-code-JACOB.csv" ) )


# Cruz interviewer files
dat.cruz.A <- read.csv( here( "PR-interviewer-comparison-wrangling/interviewer-cruz-to-code-ALEXIS.csv" ) )
dat.cruz.K <- read.csv( here( "PR-interviewer-comparison-wrangling/interviewer-cruz-to-code-KEVIN.csv" ) )
dat.cruz.J <- read.csv( here( "PR-interviewer-comparison-wrangling/interviewer-cruz-to-code-JACOB.csv" ) )


# Build the file for analysis
dat.asu.r <- data.frame(
  id = dat.asu.A$id,
  diff.A = dat.asu.A$diff, reas.A = dat.asu.A$reas,
  diff.K = dat.asu.K$diff, reas.K = dat.asu.K$reas,
  diff.J = dat.asu.J$diff, reas.J = dat.asu.J$reas
)

dat.cruz.r <- data.frame(
  id = dat.cruz.A$id,
  diff.A = dat.cruz.A$diff, reas.A = dat.cruz.A$reas,
  diff.K = dat.cruz.K$diff, reas.K = dat.cruz.K$reas, 
  diff.J = dat.cruz.J$diff, reas.J = dat.cruz.J$reas
)

dat.for.r <- rbind( dat.asu.r, dat.cruz.r )
dat.for.r <- dat.for.r %>% arrange( id )

dat.for.r.diff <- dat.for.r %>% select( diff.A, diff.K, diff.J )
dat.for.r.reas <- dat.for.r %>% select( reas.A, reas.K, reas.J )

dat.for.r.diff.asu <- dat.asu.r %>% select( diff.A, diff.K, diff.J )
dat.for.r.diff.cruz <- dat.cruz.r %>% select( diff.A, diff.K, diff.J )
dat.for.r.reas.asu <- dat.asu.r %>% select( reas.A, reas.K, reas.J )
dat.for.r.reas.cruz <- dat.cruz.r %>% select( reas.A, reas.K, reas.J )


# ----
# perform reliability analysis

agree.diff <- agree( dat.for.r.diff )
agree.reas <- agree( dat.for.r.reas )

rel.diff <- kappam.light( dat.for.r.diff )
rel.reas <- kappam.light( dat.for.r.reas )


agree.diff.asu <- agree( dat.for.r.diff.asu )
agree.reas.asu <- agree( dat.for.r.reas.asu )
rel.diff.asu <- kappam.light( dat.for.r.diff.asu )
rel.reas.asu <- kappam.light( dat.for.r.reas.asu )


agree.diff.cruz <- agree( dat.for.r.diff.cruz )
agree.reas.cruz <- agree( dat.for.r.reas.cruz )
rel.diff.cruz <- kappam.light( dat.for.r.diff.cruz )
rel.reas.cruz <- kappam.light( dat.for.r.reas.cruz )


# ================================================================== #

# ----
# compare differences for comfort level


# load the data
dat <- readRDS( file = here( "PR-interviewer-comparison-rodeo/trust.rhps.cntrls.vars.data.rds" ) )

# coerce to data frame
dat <- as.data.frame( dat )

# select the variables needed
dat.type <- dat %>% select( id, Interviewer, Randomize )

# select the variables for the comparison
dat.for.comp <- dat.for.r %>% select( diff.A, diff.K, diff.J, reas.A, reas.K, reas.J )

# bind the data
dat.for.comp <- cbind( dat.type, dat.for.comp )

# remove cases that were not randomized
dat.for.comp <- dat.for.comp %>% 
  filter( Randomize == 1)

# create a variable to indicate a match across coders for the difference variable
diff.match <- dat.for.comp %>%
  select( diff.A, diff.K, diff.J ) %>%
  rowwise %>%
  mutate( match.diff = n_distinct( unlist( cur_data() ) ) == 1 ) %>%
  ungroup()

# create a variable to indicate a match across coders for the reason variable
reas.match <- dat.for.comp %>%
  select( reas.A, reas.K, reas.J ) %>%
  rowwise %>%
  mutate( match.reas = n_distinct( unlist( cur_data() ) ) == 1 ) %>%
  ungroup()


# assign the match variables to the data object
dat.for.comp$match.diff <- diff.match$match.diff
dat.for.comp$match.reas <- reas.match$match.reas


# build data files for each coder
dat.for.comp.A <- dat.for.comp %>% 
  select( Interviewer, diff.A, Randomize ) %>% 
  mutate( matters.A = na_if( diff.A, 99 ) ) 
dat.for.comp.K <- dat.for.comp %>% 
  select( Interviewer, diff.K, Randomize ) %>% 
  mutate( matters.K = na_if( diff.K, 99 ) )
dat.for.comp.J <- dat.for.comp %>% 
  select( Interviewer, diff.J, Randomize ) %>% 
  mutate( matters.J = na_if( diff.J, 99 ) )


# create data for only those cases where the three coders match
# and recode missing values
dat.diff.ALL <- dat.for.comp %>% 
  select( Interviewer, match.diff, diff.A ) %>%
  filter( match.diff == TRUE ) %>% 
  mutate( difference = na_if( diff.A, 99 ) )

dat.reas.ALL <- dat.for.comp %>% 
  select( Interviewer, match.reas, reas.A ) %>%
  filter( match.reas == TRUE ) %>% 
  mutate( reason = na_if( reas.A, 99 ) )
  
# show crosstab
table( dat.diff.ALL$difference, dat.diff.ALL$Interviewer )
table( dat.reas.ALL$reason, dat.reas.ALL$Interviewer )

# create the table of proportions
prop.table( table( dat.diff.ALL$difference, dat.diff.ALL$Interviewer ), margin = 2 )
prop.table( table( dat.reas.ALL$reason, dat.reas.ALL$Interviewer ), margin = 2 )

# chi-squared test if there is a difference
chisq.test( dat.diff.ALL$Interviewer, dat.diff.ALL$difference )
chisq.test( dat.reas.ALL$Interviewer, dat.reas.ALL$reason )




  
  
  
  
  




