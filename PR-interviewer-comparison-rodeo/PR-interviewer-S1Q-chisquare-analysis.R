# ================================================================== #
# PAPER TITLE
# ================================================================== #

# This file conducts the chi-square tests.

# It creates several figures used in the manuscript.
# "Figure 3: Proportions for Themes and Differences"

# ================================================================== #

# ----
# Setup

# clear the workspace
rm( list = ls() )

# load the libraries
library( dplyr ) # for working with the data
library( here )  # to call local directory


# ----
# load the functions needed that is in the utilities folder
source( here( "PR-interviewer-comparison-utils/PR-interviewer-functions.R" ) )


# ----
# build the dataset of responses

# record the responses for each theme
s1q1Themes <- rbind( 
  c( 58, 40 ), # cell counts for theme 1
  c( 39, 34 ), # cell counts for theme 2...
  c( 21, 27 ),
  c( 20, 22 ),
  c(  5,  6 ),
  c(  1,  1 )
  )

s1q2Themes <- rbind( 
  c( 50, 44 ),
  c( 18, 15 ),
  c( 12, 14 ),
  c( 15, 13 ),
  c( 14,  5 ),
  c( 10,  8 ),
  c(  8,  8 ),
  c( 10,  3 )
)

s1q3Themes <- rbind( 
  c( 60, 33 ),
  c( 30, 34 ),
  c( 30, 18 ),
  c( 17,  8 ),
  c( 32, 32 ),
  c( 16, 12 ),
  c( 13,  4 ),
  c(  7,  7 ),
  c(  6,  4 ),
  c(  5,  2 ),
  c(  4,  4 ),
  c(  1,  6 )
)

s1q4Themes <- rbind( 
  c( 46, 31 ),
  c( 22, 19 ),
  c( 15, 16 ),
  c( 20, 12 ),
  c( 18, 12 ),
  c( 13, 13 ),
  c(  5,  5 ),
  c(  5,  2 ),
  c(  4,  3 )
)

s1q5Themes <- rbind( 
  c( 51, 47 ),
  c( 50, 37 ),
  c( 42, 36 ),
  c( 23, 21 )
)

s1q6Themes <- rbind( 
  c( 59, 41 ),
  c( 50, 32 ),
  c( 29, 29 ),
  c( 21, 21 ),
  c( 21, 23 ),
  c(  2,  2 )
)

s1q7Themes <- rbind( 
  c( 40, 42 ),
  c( 20, 22 ),
  c( 21, 13 ),
  c( 17, 10 ),
  c( 14,  5 ),
  c(  6,  2 ),
  c(  7,  1 ),
  c(  1,  4 )
)
  
# pull the themes together  
datThemes <- rbind( 
  s1q1Themes,
  s1q2Themes,
  s1q3Themes,
  s1q4Themes,
  s1q5Themes,
  s1q6Themes,
  s1q7Themes
  )

# assign column names
colnames( datThemes ) <- c( "ASU.Yes", "INI.Yes" )

# create the object to store the results
results <- matrix( NA, nrow = dim( datThemes )[1], ncol = 2 )

# loop the function over the results object
for( i in 1:dim( datThemes )[1] ){
  results[i,] <- round(
    test.table( 
      n_ASU_y_THEME_y = datThemes[i,1],
      n_INI_y_THEME_y = datThemes[i,2] 
      ),
    3
  )
}

# assign labels to the columns
colnames( results ) <- c( "chivalue", "pvalue" )

# coerce to data frame
results <- as.data.frame( results )

# create reject/fail based on unadjusted
results$reject <- ifelse( results$pvalue < 0.05, "Yes", "No" )

# Create reject/fail to reject based on bonferroni
results$Bonf.reject <- ifelse( results$pvalue < round( 0.05 / dim( datThemes )[1], 3 ), "Yes", "No" )


# ----
# Create the proportions to use for the plot

datThemes <- as.data.frame( datThemes )

# create the proportions
datThemes$ASU.Yes.p <- round( datThemes$ASU.Yes / 80, 3 )
datThemes$INI.Yes.p <- round( datThemes$INI.Yes / 78 , 3 ) 

# create the difference used to sort below
datThemes$diff <- abs( datThemes$ASU.Yes.p - datThemes$INI.Yes.p )

# Create vector of names for the plot
datThemes$custom_labels <- c(
  paste("Q1t",rep( 1:dim( s1q1Themes )[1] ), sep = "" ),
  paste("Q2t",rep( 1:dim( s1q2Themes )[1] ), sep = "" ),
  paste("Q3t",rep( 1:dim( s1q3Themes )[1] ), sep = "" ),
  paste("Q4t",rep( 1:dim( s1q4Themes )[1] ), sep = "" ),
  paste("Q5t",rep( 1:dim( s1q5Themes )[1] ), sep = "" ),
  paste("Q6t",rep( 1:dim( s1q6Themes )[1] ), sep = "" ),
  paste("Q7t",rep( 1:dim( s1q7Themes )[1] ), sep = "" )
)

# add the results
datThemes$reject <- results$reject


# ----
# Create the objects for the plot

# define parts of the plot
y.ax <- seq( 1: dim( datThemes )[1] )
cols <- c( "#eb5636","#a709f7" )

pchs <- c(
  rep( 0, dim( datThemes )[1] - 1 ),
  rep( 15, 1 ),
  rep( 1, dim( datThemes )[1] - 1 ),
  rep( 16, 1 )
  )

# labels for the legend
legend_labels <- c( 
  "Outside Interviewer", 
  "Inside Interviewer"
)


# execute plot
plot(
  seq( 0, 1, length.out = dim( datThemes )[1] ),
  y.ax,
  type = "n",
  xlab = "Proportions with thematic element",
  ylab = "",
  yaxt = "n"
)

# set up points for the plot
points( datThemes$ASU.Yes.p[order(datThemes$diff)], y.ax, col = cols[1], pch = pchs[1:53] )
points( datThemes$INI.Yes.p[order(datThemes$diff)], y.ax, col = cols[2], pch = pchs[54:106] )

# add the segments
segments( datThemes$ASU.Yes.p[order(datThemes$diff)],y.ax,
          datThemes$INI.Yes.p[order(datThemes$diff)],y.ax,
          col = "grey30",
          lwd = 1.2
          )

axis( side = 2, at = y.ax, las = 1, labels = datThemes$custom_labels[order(datThemes$diff)], cex.axis = 0.8 )
# title( "Figure 3: Proportions for Themes and Differences" )

# add a legend
legend( "bottomright" ,
        pch = c( 15,16 ),
        col = cols,
        legend = legend_labels, 
        title = "Legend", cex = 0.7
)

