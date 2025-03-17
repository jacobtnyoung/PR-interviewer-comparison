# ================================================================== #
# Who’s Asking? Participatory Research, Interviewer Effects, 
# and the Production of Knowledge in a Women’s Prison
# ================================================================== #

# This file recodes the S4Q12 variable.

# ================================================================== #


# ----
# Clear workspace
rm( list = ls() )

# ----
# load the needed libraries
library( here )  # used for referencing local directory
library( haven ) # used for reading stata files
library( dplyr ) # for filtering data


# ----
# get the data file
dat <- as.data.frame( 
  read_dta( 
    here( "PR-interviewer-wrangling/PR-interviewer-wrangling-data/PV PAR Interviews Full_CLEANED.dta" ),
  )
)


# ----
# recode open-ended responses

# individuals were asked:
# Would you have been more comfortable completing 
# this interview with [someone incarcerated at Cruz OR an ASU researcher]? 

# if you had an outside interviewer, it read "someone incarcerated at CRUZ"
# if you had an inside interviewer, it read "ASU researcher"



# create the vector of responses based on who did the interview with 
# an ASU researcher

dat_ASU_interviewer <- dat |> 
  filter( Randomize  == 1 ) |>       # take those who were randomized
  filter( InterviewerType == 0 ) |>  # take those with the ASU interviewer  
  select( id, S4Q12, S4Q12a )


# code the values
pref_ASU_interviewer <- case_when(
  
  # code those who would not prefer the alternate interviewer as -1
  dat_ASU_interviewer$S4Q12 == "No" ~ -1,
  dat_ASU_interviewer$S4Q12 == "no" ~ -1,
  dat_ASU_interviewer$S4Q12 == "absolutely not" ~ -1,
  dat_ASU_interviewer$S4Q12 == "Probably not" ~ -1,
  dat_ASU_interviewer$S4Q12 == "No" ~ -1,
  dat_ASU_interviewer$S4Q12 == "ASU researcher" ~ -1,
  dat_ASU_interviewer$S4Q12 == "asu researcher" ~ -1,
  dat_ASU_interviewer$S4Q12 == "Absolutely not" ~ -1,
  dat_ASU_interviewer$S4Q12 == "Dependent on who the incarcerated woman is that would be interviewing her" ~ -1,
  dat_ASU_interviewer$S4Q12 == "No I wouldn't be comfortable talking and sharing with a woman incarcerated here" ~ -1,
  
  # code those with no preference as 0
  dat_ASU_interviewer$S4Q12 == "doesn't matter" ~ 0,
  dat_ASU_interviewer$S4Q12 == "Wouldn't care either way" ~ 0,
  dat_ASU_interviewer$S4Q12 == "Either would have been fine" ~ 0,
  dat_ASU_interviewer$S4Q12 == "It would not have made a difference" ~ 0,
  dat_ASU_interviewer$S4Q12 == "No not really" ~ 0,
  
  # code those who would have preferred an alternate interviewer as 1
  dat_ASU_interviewer$S4Q12 == "Yes" ~ 1
  
)


# create the vector of responses based on who did the interview with 
# an incarcerated interviewer

dat_CRUZ_interviewer <- dat |> 
  filter( Randomize  == 1 ) |>      # take those who are randomized
  filter( InterviewerType == 1 ) |> # take those who had the CRUZ interviewer
  select( id, S4Q12, S4Q12a )

# code the values
pref_CRUZ_interviewer <- case_when(
  
  # code those who would not have preferred an alternate interviewer as -1
  dat_CRUZ_interviewer$S4Q12 == "No" ~ -1,
  dat_CRUZ_interviewer$S4Q12 == "No, not at all" ~ -1,
  dat_CRUZ_interviewer$S4Q12 == "I'd interview with you" ~ -1,
  dat_CRUZ_interviewer$S4Q12 == "no" ~ -1,
  dat_CRUZ_interviewer$S4Q12 == "Probably no" ~ -1,
  dat_CRUZ_interviewer$S4Q12 == "cruz" ~ -1,
  dat_CRUZ_interviewer$S4Q12 == "someone at cruz" ~ -1, 
  dat_CRUZ_interviewer$S4Q12 == "I don't think so, you were very nice :) (Jessica)" ~ -1,
  dat_CRUZ_interviewer$S4Q12 == "incarcerated" ~ -1,  
  dat_CRUZ_interviewer$S4Q12 == "Incarcerated person that conducted interview" ~ -1,
  dat_CRUZ_interviewer$S4Q12 == "Someone incarcerated at Cruz" ~ -1,  
  dat_CRUZ_interviewer$S4Q12 == "Cruz inmate" ~ -1,  
  dat_CRUZ_interviewer$S4Q12 == "Cruz" ~ -1,  
  dat_CRUZ_interviewer$S4Q12 == "Someone incarcerated" ~ -1,  
  dat_CRUZ_interviewer$S4Q12 == "Inmate" ~ -1,  
  dat_CRUZ_interviewer$S4Q12 == "Incarcerated person" ~ -1,  
  dat_CRUZ_interviewer$S4Q12 == "cruz" ~ -1,
  
  # code those with no preference as 0
  dat_CRUZ_interviewer$S4Q12 == "either" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "It wouldn't matter" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "no- doesn't matter" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "doesn't matter" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "Me:) \"Winning\"" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "no preference" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "Either is ok" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "Summer" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "It would have been the same" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "I don't care." ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "Either one would be fine" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "Doesn't matter" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "Either would be fine." ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "The same" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "Either" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "It doesn't matter" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "either" ~ 0,
  dat_CRUZ_interviewer$S4Q12 == "either" ~ 0,
  
  # code those who would prefer the alternate interviewer as 1
  dat_CRUZ_interviewer$S4Q12 == "ASU" ~ 1,
  dat_CRUZ_interviewer$S4Q12 == "ASU researcher" ~ 1,
  dat_CRUZ_interviewer$S4Q12 == "Maybe" ~ 1  
  
)


# ----
# append the items back to the main data file

# create the sub data to use
dat_analysis <- dat |> 
  filter( Randomize  == 1 ) |>
  select( id, InterviewerType, S4Q12 )

# match the ids
dat_ASU_interviewer$pref <- pref_ASU_interviewer
dat_CRUZ_interviewer$pref <- pref_CRUZ_interviewer

# drop the S4Q12 var
dat_ASU_interviewer_analysis <- dat_ASU_interviewer |> select( id, pref)
dat_CRUZ_interviewer_analysis <- dat_CRUZ_interviewer |> select( id, pref)

dat_analysis <- full_join( 
  dat_analysis, 
  rbind( dat_ASU_interviewer_analysis, dat_CRUZ_interviewer_analysis ),
  by = "id"
)


# ----
# conduct the chi-square test

table_to_test <- table( dat_analysis$pref, dat_analysis$InterviewerType )

chisq.test( table_to_test ) # this gives a warning b/c of the last row

# conduct the test without the last row
chisq.test( table_to_test[-3,] ) 


# ----
# Content analysis of open-ended responses

dat_ASU_interviewer$S4Q12a[dat_ASU_interviewer$pref == -1 ]
dat_CRUZ_interviewer$S4Q12a[dat_CRUZ_interviewer$pref == -1 ]

dat_ASU_interviewer$S4Q12a[dat_ASU_interviewer$pref == 1 ]
dat_CRUZ_interviewer$S4Q12a[dat_CRUZ_interviewer$pref == 1 ]
