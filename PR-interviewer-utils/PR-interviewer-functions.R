# ================================================================== #
# PAPER TITLE
# ================================================================== #

# This file is a utilities file that contains functions
# called in other scripts

# ================================================================== #

# ----
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


# ----
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
  
  points( x.ax, point, pch = 19 )
  segments(x.ax, upper.ci, x.ax, lower.ci)
  abline( h = 0, lty = 2 )
  axis( side = 1, at = x.ax, las = 3, labels = custom_labels )
  title( the.title )
}


# ----
# function to conduct chi-square test

test.table <- function(
    # arguments
  n_ASU = 80, n_INI = 78, # total count for each group
  n_ASU_y_THEME_y = 58,   # yes response to theme for ASU
  n_INI_y_THEME_y = 40,   # yes response to theme for incarcerated interviewers
  n_ASU_y_THEME_n = n_ASU - n_ASU_y_THEME_y, # no response to theme for ASU
  n_INI_y_THEME_n = n_INI - n_INI_y_THEME_y  # no response to them for incarcerated interviewers
){
  
  # create the table  
  dat_table <- matrix(
    c( 
      n_ASU_y_THEME_y,
      n_ASU_y_THEME_n,
      n_INI_y_THEME_y,
      n_INI_y_THEME_n ),
    nrow = 2,
    byrow = FALSE
  )
  
  # run the test
  table_test <- chisq.test( dat_table )
  
  # grab what you want to return
  return(
    cbind( table_test$statistic,
           table_test$p.value 
    )
  )
}


# ================================================================== #