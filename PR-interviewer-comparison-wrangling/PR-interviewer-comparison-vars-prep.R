# ################################################################## #
# PAPER TITLE NEEDED!!!!
# ################################################################## #

# This file prepares trust, relational health, psychological safety,
# and demographic variables for analysis

# ================================================================== #
# Setup ----

# Clear the workspace
rm( list = ls() )

# Load the libraries used
library( here )     # for calling local directory
library( dplyr )    # for working with the data
library( psych )    # for reliability analysis


# ================================================================== #
# Load the data ----

dat <- read.csv(
  here( "PR-interviewer-comparison-wrangling/PR-interviewer-comparison-vars.csv" ),
  as.is = TRUE,
  header = TRUE,
  stringsAsFactors = FALSE
)


# ================================================================== #
# assign the variables to matrices ----

# extract the variables variables, S3SS1_1-S3SS1_15
trust.vars <- as.matrix( dat[,
                             which( colnames( dat ) == ( "S3SS1_1" ) ):
                               which( colnames( dat ) == ( "S3SS1_15" ) )
] )


# extract the relational health variables, S3SS2_1-S3SS2_14
rh.vars <- as.matrix( dat[,
                             which( colnames( dat ) == ( "S3SS2_1" ) ):
                               which( colnames( dat ) == ( "S3SS2_14" ) )
] )


# extract the psychological safety variables, S3SS3_1-S3SS3_7
ps.vars <- as.matrix( dat[,
                          which( colnames( dat ) == ( "S3SS3_1" ) ):
                            which( colnames( dat ) == ( "S3SS3_7" ) )
] )


# ================================================================== #
# missing data ----

# function to replace the missing values
NA.replace <- function( input.dat, pdist ){
  set.seed( 12345 )
  for( i in 1:nrow( input.dat ) ){
    for( j in 1:ncol( input.dat ) ){
      if( is.na( input.dat[i,j] == TRUE ) )
        input.dat[i,j] <- pdist
    }
  }
 return( input.dat )
}

### trust items

# show there are 67 missing values over the 15 items
table( is.na( trust.vars ) )

# for the randomized group there are 51
table( is.na( trust.vars[ dat$Randomize == 1 ] ) )

# run the function to replace the missing values
trust.vars.na   <- NA.replace( trust.vars, rbinom( n = 1, size = 1, prob = 0.5 ) )


### relational health

# replace 15 missing values
table( is.na( rh.vars ) )

# for the randomized group there are 51
table( is.na( rh.vars[ dat$Randomize == 1 ] ) )

# execute the function
rh.vars.na   <- NA.replace( rh.vars, round( runif( n = 1, min = 1, max = 5 ), 0 ) )


### psychological safety

# replace 29 missing values
table( is.na( ps.vars ) )

# for the randomized group there are 51
table( is.na( ps.vars[ dat$Randomize == 1 ] ) )

# execute the function
ps.vars.na   <- NA.replace( ps.vars, round( runif( n = 1, min = 1, max = 5 ), 0 ) )


# ================================================================== #
# compile the data ----

comparison.dat <- cbind( 
  dat$id, dat$S4Q1, dat$lage, dat$InterviewerType, dat$Randomize, 
  dat$white, dat$black, dat$hispanic, 
  dat$S4Q3, dat$single, dat$has_children, dat$firsttimer,
  dat$timein_yrs, dat$stretch_yrs, dat$lifer,
  trust.vars.na, rh.vars.na, ps.vars.na 
  )

names.comparison.dat <- c(
  "id", "Age", "Log_Age", "Interviewer", "Randomize", "White", "Black", "Hispanic", "Education", 
  "Single", "Children", "First_Timer", "Time_In", "Sentence_Length", "Lifer",
  paste("trust",rep( 1:dim(trust.vars.na)[2] ), sep = "" ),
  paste("rh",rep( 1:dim(rh.vars.na)[2] ), sep = "" ),
  paste("ps",rep( 1:dim(ps.vars.na)[2] ), sep = "" )
)

colnames( comparison.dat ) <- names.comparison.dat


# ================================================================== #
# save the file for calling in the analysis ----

saveRDS( comparison.dat, paste( here(), "/PR-interviewer-comparison-rodeo/trust.rhps.cntrls.vars.data", ".rds", sep = "" ) )  

