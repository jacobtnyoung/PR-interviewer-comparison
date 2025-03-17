# ================================================================== #
# Who’s Asking? Participatory Research, Interviewer Effects, 
# and the Production of Knowledge in a Women’s Prison
# ================================================================== #

# This file prepares and analyzes the S4Q12a variable.
# There are two steps:
# First the data are separated into three files for coding.
# Second these files are merged and reliability analysis is performed.


# ================================================================== #
# Setup 

# ----
# Clear workspace
rm( list = ls() )

# ----
# load the needed libraries
library( here )  # used for referencing local directory
library( haven ) # used for reading stata files
library( dplyr ) # for filtering data


# ================================================================== #
# Step 1

# This is commented out because it is not used after it has
# already been executed
# These files were just to create the files referenced
# in step 2 below.

# ----
# Load the data
## dat <- read.csv(
##   here( "PR-interviewer-wrangling/PR-interviewer-wrangling-data/PR-interviewer-vars.csv" ),
##   as.is = TRUE,
## header = TRUE,
## stringsAsFactors = FALSE
## )


# ----
# Restructure the files and write them to a .csv file.

# take those cases with an ASU interviewer
## dat.asu <- dat %>% 
##   select( id, InterviewerType, S4Q12, S4Q12a ) %>% 
## filter( InterviewerType == 0 ) %>% 
## select( id, S4Q12, S4Q12a )

# take those cases with a Cruz interviewer
## dat.cruz <- dat %>% 
## select( id, InterviewerType, S4Q12, S4Q12a ) %>% 
##   filter( InterviewerType == 1 ) %>% 
## select( id, S4Q12, S4Q12a )

# add a column for coding
## dat.asu$diff  <- rep( 99, dim( dat.asu )[1] )
## dat.cruz$diff <- rep( 99, dim( dat.cruz )[1] )

# add a column for the additional variable
## dat.asu$reas  <- rep( 99, dim( dat.asu )[1] )
## dat.cruz$reas <- rep( 99, dim( dat.cruz )[1] )

# write the files out
## write.csv( dat.asu,  "PR-interviewer-wrangling/PR-interviewer-wrangling-data/interviewer-asu-to-code.csv" )
## write.csv( dat.cruz, "PR-interviewer-wrangling/PR-interviewer-wrangling-data/interviewer-cruz-to-code.csv" )


# ================================================================== #
# Step 2

# ----
# Read in the files

# ASU interviewer files
dat.asu.A <- read.csv( here( "PR-interviewer-wrangling/PR-interviewer-wrangling-data/interviewer-asu-to-code-ALEXIS.csv" ) )
dat.asu.K <- read.csv( here( "PR-interviewer-wrangling/PR-interviewer-wrangling-data/interviewer-asu-to-code-KEVIN.csv" ) )
dat.asu.J <- read.csv( here( "PR-interviewer-wrangling/PR-interviewer-wrangling-data/interviewer-asu-to-code-JACOB.csv" ) )

# Cruz interviewer files
dat.cruz.A <- read.csv( here( "PR-interviewer-wrangling/PR-interviewer-wrangling-data/interviewer-cruz-to-code-ALEXIS.csv" ) )
dat.cruz.K <- read.csv( here( "PR-interviewer-wrangling/PR-interviewer-wrangling-data/interviewer-cruz-to-code-KEVIN.csv" ) )
dat.cruz.J <- read.csv( here( "PR-interviewer-wrangling/PR-interviewer-wrangling-data/interviewer-cruz-to-code-JACOB.csv" ) )

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
# compare differences for comfort level

dat <- as.data.frame( 
  read_dta( 
    here( "PR-interviewer-wrangling/PR-interviewer-wrangling-data/PV PAR Interviews Full_CLEANED.dta" ),
    )
  )

dat.type <- dat %>% select( id, InterviewerType, Randomize )

dat.for.comp <- dat.for.r %>% select( diff.A, diff.K, diff.J )

dat.for.comp <- cbind( dat.type, dat.for.comp )

comps <- dat.for.comp %>%
  select( diff.A, diff.K, diff.J ) %>%
  rowwise %>%
  mutate( match = n_distinct( unlist( cur_data() ) ) == 1 ) %>%
  ungroup()

dat.for.comp$match <- comps$match

dat.for.comp.A <- dat.for.comp %>% 
  select( InterviewerType, diff.A, Randomize ) %>% 
  filter( Randomize == 1) %>% 
  mutate( matters.A = na_if( diff.A, 99 ) )
dat.for.comp.K <- dat.for.comp %>% 
  select( InterviewerType, diff.K, Randomize ) %>% 
  filter( Randomize == 1) %>% 
  mutate( matters.K = na_if( diff.K, 99 ) )
dat.for.comp.J <- dat.for.comp %>% 
  select( InterviewerType, diff.J, Randomize ) %>% 
  filter( Randomize == 1) %>% 
  mutate( matters.J = na_if( diff.J, 99 ) )
dat.for.comp.ALL <- dat.for.comp %>% 
  select( InterviewerType, match, diff.A, Randomize ) %>%
  filter( match == TRUE & Randomize == 1 ) %>% 
  mutate( matters = na_if( diff.A, 99 ) )
