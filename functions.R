
library(testthat)
library(scales)
library(rmcorr)




# rmcorr custom functions -------------------------------------------------

# custom rmc model plotting function
plot_rmc <- function(rmc_model, treatment = FALSE) {
  #' rmc plot
  #'
  #' Produces a scatter plot for a repeated measures correlation model
  #' 
  #' @parm rmc_model a rmc model
  #' @parm treatment should the plot be faceted by grouping variable?
  
  df <- rmc_model[["model"]][["model"]]
  x_label <- rmc_model[["vars"]][2]
  y_label <- rmc_model[["vars"]][3]
  l_label <- rmc_model[["vars"]][1]
  
  ggplot(df, aes(x = Measure1, y = Measure2, col = Participant)) +
    geom_point() +
    geom_line(aes(y = rmc_model[["model"]][["fitted.values"]])) +
    xlab(x_label) +
    ylab(y_label) +
    labs(col = l_label) +
    
    {
      if(treatment)list(
        facet_grid(rows = vars(Participant)) 
      )
    } 
}


# Correlation tables using rmcorr
rmc_cor_table <- function(df, vars, group_var, param = "r", stars = F, decimal = 3) {
  
  #'  Correlation table for rmc models
  #'
  #'  Produces a correlation table for repeated measure correlation models. 
  #'  The table output can be one of correlation coefficients, degrees of freedom, p-values, n, confidence intervals. 
  #'  @param df a dataframe 
  #'  @param vars a vector of column names to correlate  
  #'  @param grouping variable ('participant')
  #'  @param param output model parameter, one of "r", "df", "p", "n", "CI".
  #'  @param stars indicate significance using *, **, or *** at alpha < 0.05, < 0.01, or < 0.001
  #'  @param decimal maximum number of decimal places in output 
  
  # Create df to hold output
  n_vars = length(vars)
  out_df <- setNames(data.frame(matrix(
    ncol = n_vars,
    nrow = n_vars)), 
    c(vars))
  row.names(out_df) <- vars
  
  for(col in vars) {  # iterate through variable columns
    for (row in vars) {  # iterate through variable rows
      if(col == row) {  # check if variables are the same
        out_df[row, col] <- " "  # and set r to unity
      }
      else {
        rmc_mod <- rmcorr::rmcorr(get(group_var), get(col), get(row), df)  # create rmc model
        val <- rmc_mod[[param]]
        
        if(param == "CI") {
          val <- paste(round(val[1], decimal), round(val[2], decimal), sep = "-")
        } else val <- round(val, decimal)
        
        if(stars) {  # if stars argument is TRUE
          pval <- rmc_mod$p  # extract p-value
          ast <- ifelse(pval < 0.001, "***", 
                        ifelse(pval < 0.01, "**", 
                               ifelse(pval < 0.05, "*", " ")))  # check significance levels  
          out_df[row, col] <- paste(val, ast)  # assign selected parameter and *s to table
        }
        else out_df[row, col] <- val  # assign selected parameter to table
      }
    }
  } 
  return(out_df)
}




# Inverse hyperbolic sine transformation ------------------------------------------------------

ihs <- function(x) {
  transformed <- log10(x + sqrt(x ^ 2 + 1))
  return(transformed)
}




# Inverse hyperbolic sine transformation for ggplot2 ------------------------------------------

#' Source: Wolfgang Resch, 2013.
#' https://wresch.github.io/2013/03/08/asinh-scales-in-ggplot2.html

asinh_breaks <- function(x) {
  br <- function(r) {
    lmin <- round(log10(r[1]))
    lmax <- round(log10(r[2]))
    lbreaks <- seq(lmin, lmax, by = 1)
    breaks <- 10 ^ lbreaks
  }
  p.rng <- range(x[x > 0], na.rm = TRUE)
  breaks <- br(p.rng)
  if (min(x) <= 0) {breaks <- c(0, breaks)}
  if (sum(x < 0) > 1) { 
    n.rng <- -range(x[x < 0], na.rm = TRUE)
    breaks <- c(breaks, -br(n.rng))
  }
  return(sort(breaks))
}

test_that("asinh_breaks make sense", {
  expect_equal(asinh_breaks(c(-0.05, 0, 1, 101)), c(0, 1, 10, 100))
  expect_equal(asinh_breaks(c(-0.11, -0.05, 0, 1, 101)), c(-0.1, 0, 1, 10, 100))
  expect_equal(asinh_breaks(c(0, 10, 1001)), c(0, 10, 100, 1000))
  expect_equal(asinh_breaks(c(0, 0.05, 0.07, 0.1, 0.2)), c(0, 0.1))
  expect_equal(asinh_breaks(c(0.01, 0.02)), c(0.01))
})

asinh_trans <- function() {
  trans_new("asinh",
            transform = asinh,
            inverse   = sinh,
            breaks = asinh_breaks)
}


