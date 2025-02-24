# ================================================================== #
# TITLE
# ================================================================== #

# This file conducts reliability tests and chi-square tests
# for the S4Q# variables.

# ================================================================== #


# ----
# Clear workspace
rm( list = ls() )

# ----
# load the needed libraries
library( here ) # used for referencing local directory
library( haven ) # used for reading stata files
library( dplyr ) # for filtering data
library( irr )   # for the reliability analysis

# ----
# Run the script to load the objects you need
source( here( "PR-interviewer-wrangling/PR-interviewer-S4Q-reliability-prep.R" ) )


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


# ----
# compare differences for comfort level

chisq.test( dat.for.comp.ALL$InterviewerType, dat.for.comp.ALL$matters )
chisq.test( dat.for.comp.A$InterviewerType, dat.for.comp.A$matters.A )
chisq.test( dat.for.comp.K$InterviewerType, dat.for.comp.K$matters.K )
chisq.test( dat.for.comp.J$InterviewerType, dat.for.comp.J$matters.J )

t.test( matters ~ InterviewerType, data = dat.for.comp.ALL )
t.test( matters.A ~ InterviewerType, data = dat.for.comp.A )
t.test( matters.K ~ InterviewerType, data = dat.for.comp.K )
t.test( matters.J ~ InterviewerType, data = dat.for.comp.J )
