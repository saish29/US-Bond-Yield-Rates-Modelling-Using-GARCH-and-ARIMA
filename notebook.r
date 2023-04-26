
# Load the packages
# .... YOUR CODE FOR TASK 1 ....

library(xts)
library(readr)

# Load the data
yc_raw <- read_csv("datasets/FED-SVENY.csv")

# Convert the data into xts format
yc_all <- as.xts(x = yc_raw[, -1], order.by = yc_raw$Date)

# Show only the tail of the 1st, 5th, 10th, 20th and 30th columns
yc_all_tail <- tail(yc_all[, c(1,5,10,20, 30)])
yc_all_tail

# These packages need to be loaded in the first @tests cell
library(testthat) 
library(IRkernel.testthat)

soln_yc_raw <- read_csv("datasets/FED-SVENY.csv")
soln_yc_all <- as.xts(x = soln_yc_raw[, -1], order.by = soln_yc_raw$Date)

soln_yc_all_tail <- tail(soln_yc_all[, c(1, 5, 10, 20, 30)])
 
run_tests({
    # Packages loaded
    test_that("the correct package is loaded", {
        expect_true("xts" %in% .packages(), 
                    info = "Did you load the xts package?")
        expect_true("readr" %in% .packages(), 
                    info = "Did you load the readr package?")
    })
    
    # Date set loaded
    test_that("the dataset is loaded correctly", {
        expect_is(yc_raw, "tbl_df", info = "Did you read in the data with read_csv() (not read.csv() )?")
        expect_equal(yc_raw, soln_yc_raw, 
                     info = "yc_raw contains the wrong values. Did you import the CSV file correctly?")
    })
    
    test_that("the xts object is created correctly", {
        expect_equal(yc_all, soln_yc_all, 
                     info = "yc_all contains the wrong values. At the conversion, did you filter out the first column? Did you apply the date column as index?")
    })
    
    # Output shown
    test_that("output shown", {
                expect_equal(yc_all_tail, soln_yc_all_tail, 
                    info = "The output shown is not correct. Did you choose the correct columns? Did you use tail()?")
    })
})


library(viridis)

# Define plot arguments
yields  <- yc_all
plot.type  <- "single"
plot.palette <- viridis(30)
asset.names  <- colnames(yc_all)

# Plot the time series
plot.zoo(x = yields, plot.type = plot.type, col = plot.palette)

# Add the legend
legend(x = "topleft", legend = asset.names,
       col = plot.palette, cex = 0.45, lwd = 3)


soln_yields  <- soln_yc_all
soln_plot.type  <- "single"
soln_plot.palette <- viridis(30)
soln_asset.names <- colnames(soln_yc_all)

run_tests({
    # Plot parameters
    test_that("plot parameters are correct", {
        expect_equal(soln_yields, yields, 
                     info = "The data are not correct. Check that you used yc_all.")
    })
    test_that("plot parameters are correct", {
        expect_equal(soln_plot.type, plot.type,
                    info = "Did you set the plot.type correctly? It can be either 'single' or 'mulitple'.")
    })
    test_that("plot parameters are correct", {
        expect_equal(soln_plot.palette, plot.palette,
                    info = "The color palette is not correct. Did you use 30 colors from viridis?")
    })
    test_that("the legend was correctly defined", {
        expect_equal(soln_asset.names, asset.names,
                    info = "The the legend was not correctly defined. It should be the vector of column names of yc_all.")
        })
    })

# Differentiate the time series  
ycc_all <- diff.xts(yc_all)

# Show the tail of the 1st, 5th, 10th, 20th and 30th columns
ycc_all_tail <- tail(ycc_all[, c(1,5,10,20, 30)])
ycc_all_tail

soln_ycc_all <- diff.xts(soln_yc_all)

# Show only the 1st, 5th, 10th, 20th and 30th columns
soln_ycc_all_tail <- tail(soln_ycc_all[, c(1, 5, 10, 20, 30)])


run_tests({
    # Differentiation is correct
    test_that("ycc_all is correct", {
        expect_equal(ycc_all, soln_ycc_all,
                    info = "You did not differentiate the time series correctly. Did you use the diff.xts() function on yc_all?"
                    )
    })
    
     # Output shown
    test_that("output shown", {
                expect_equal(ycc_all_tail, soln_ycc_all_tail, 
                    info = "The output shown is not correct. Did you choose the correct columns? Did you use tail()?" )
    })
})

# Define the plot parameters
yield.changes <- ycc_all
plot.type <- "multiple"


# Plot the differentiated time series
plot.zoo(x = yield.changes, plot.type = plot.type, 
     ylim = c(-0.5, 0.5), cex.axis = 0.7, 
     ylab = 1:30, col = plot.palette)

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

soln_yield.changes <- soln_ycc_all
soln_plot.type <- "multiple"

run_tests({
    # Plot parameters
    test_that("the plotted data are correct", {
        expect_equal(soln_yield.changes, yield.changes, 
                     info = "Did you set the parameter x correctly? It should be the differentiated data.")
    })
    
    test_that("plot.type set correctly", {
        expect_equal(plot.type, soln_plot.type, 
                     info = "Did you set the plot.type correctly? It can be either 'single' or 'mulitple'.")
    })
})


# Filter for changes in and after 2000
ycc <- ycc_all["2000/", ]

# Save the 1-year and 20-year maturity yield changes into separate variables
x_1 <- ycc[, "SVENY01"]
x_20 <- ycc[, "SVENY20"]

# Plot the autocorrelations of the yield changes
par(mfrow=c(2,2))
acf_1 <- acf(x_1)
acf_20 <- acf(x_20)

# Plot the autocorrelations of the absolute changes of yields
acf_abs_1 <- acf(abs(x_1))
acf_abs_20 <- acf(abs(x_20))

# Filter for changes in and after 2000
soln_ycc <- soln_ycc_all["2000/", ]

# Save the 1-year and 20-year maturity yield changes into separate variables
soln_x_1 <- soln_ycc[, "SVENY01"]
soln_x_20 <- soln_ycc[, "SVENY20"]

# Plot the autocorrelations of the changes of yields
par(mfrow=c(2,2))
soln_acf_1 <- acf(soln_x_1, plot = FALSE)
soln_acf_20 <- acf(soln_x_20, plot = FALSE)
# Plot the autocorrelations of the absolute changes of yields, too
soln_acf_abs_1 <- acf(abs(soln_x_1), plot = FALSE)
soln_acf_abs_20 <- acf(abs(soln_x_20), plot = FALSE)



run_tests({
    test_that("ycc is correct", {
        expect_equal(ycc, soln_ycc,
                     info = "ycc is not correct. Did you filter for the time period in and after 2000 correctly?"
                    )        
    })
    test_that("x_1 is correct", {
        expect_equal(x_1, soln_x_1,
                     info = "x_1 is not correct. Did you select the 1st column?"
                    )        
    })
    test_that("x_20 is correct", {
        expect_equal(x_20, soln_x_20,
                     info = "x_20 is correct. Did you select the 20th column?"
                    )        
    })    
    test_that("acf_1 is correct", {
        expect_equal(acf_1$acf, soln_acf_1$acf,
                     info = "Autocorrelation of x_1 is not correct. Did you use the acf() function on x_1?"
                    )        
    })
    
    test_that("acf_20` is correct", {
        expect_equal(acf_20$acf, soln_acf_20$acf,
                     info = "Autocorrelation of x_20 is not correct. Did you use the acf() function on x_20?"
                    )        
    })        
    test_that("acf_abs_1 is correct", {
        expect_equal(acf_abs_1$acf, soln_acf_abs_1$acf, label = "acf_abs_1", expected.label = "acf(abs(x_1))",
                     info = "Autocorrelation of absolute values of x_1 is not correct. Did you use the acf() and abs() functions on x_1?"
                    )        
    })
    test_that("acf_abs_20 is correct", {
        expect_equal(acf_abs_20$acf, soln_acf_abs_20$acf, label = "acf_abs_20", expected.label = "acf(abs(x_20))",
                     info = "Autocorrelation of absolute values of x_20 is not correct. Did you use the acf() and abs() functions on x_20?"
                    )        
    })
})

library(rugarch)

# Specify the GARCH model with the skewed t-distribution
spec <- ....(distribution.model = ....)

# Fit the model
fit_1 <- ....(...., spec = spec)

# Save the volatilities and the rescaled residuals
vol_1 <- ....
res_1 <- scale(....(...., standardize = ....)) * sd(x_1) + mean(x_1)

# Plot the yield changes with the estimated volatilities and residuals
merge_1 <- merge.xts(...., ...., ....)
....(merge_1)

# Specify the GARCH model with the skewed t-distribution
soln_spec <- ugarchspec(distribution.model = "sstd")

# Fit the model
soln_fit_1 <- ugarchfit(soln_x_1, spec = soln_spec)

# Save the volatilities and the rescaled residuals
soln_vol_1 <- sigma(soln_fit_1)
soln_res_1 <- scale(residuals(soln_fit_1, standardize = TRUE)) * sd(soln_x_1) + mean(soln_x_1)

# Plot the yield changes with the estimated volatilities and residuals.
soln_merge_1 <- merge.xts(soln_x_1, vol_1 = soln_vol_1, res_1 = soln_res_1)

run_tests({
    test_that("spec is correct", {
        expect_equal(spec, soln_spec, 
            info = "The ugarch specification is not correct. Did you use the ugarchspec() function and the 'sstd' parameter?"
        )
    })
    
    test_that("fit_1 is correct", {
        expect_equal(fit_1@fit$residuals, soln_fit_1@fit$residuals, 
            info = "The fitted model is not correct. Did you apply the ugarchfit() function on x_1 with specification defined earlier?"
        )
    })
    
    test_that("vol_1 is correct", {
        expect_equal(vol_1, soln_vol_1, 
            info = "The volatility is not correct. Did you use the sigma() function on the fitted model (fit_1?"
        )
    })
    
    test_that("res_1 is correct", {
        expect_equal(res_1, soln_res_1, 
            info = "The residuals are not correct. Did you use the residuals() function on the fitted model (fit_1? The standardize parameter can be either TRUE or FALSE."
        )
    })
    
    test_that("merge_1 is correct", {
        expect_equal(merge_1, soln_merge_1, 
            info = "The merged object is not correct. Did you applied the merge.xts() function on the original series (x_1), the volatilities (vol_1) and the residuals (res_1)?"
        )
    })
})

# Fit the model
fit_20 <- ....(...., spec = ....)

# Save the volatilities and the rescaled residuals
vol_20 <- ....(....)
res_20 <- scale(....(...., .... = ....)) * sd(x_20) + mean(x_20)

# Plot the yield changes with the estimated volatilities and residuals
merge_20 <- merge.xts(...., ...., ....)
....(merge_20)

# Specify the GARCH model with the skewed t-distribution
soln_spec <- ugarchspec(distribution.model = "sstd")

# Fit the model
soln_fit_20 <- ugarchfit(soln_x_20, spec = soln_spec)

# Save the volatilities and the rescaled residuals
soln_vol_20 <- sigma(soln_fit_20)
soln_res_20 <- scale(residuals(soln_fit_20, standardize = TRUE)) * sd(soln_x_20) + mean(soln_x_20)

# Plot the yield changes with the estimated volatilities and residuals.
soln_merge_20 <- merge.xts(soln_x_20, vol_20 = soln_vol_20, res_20 = soln_res_20)


run_tests({
    test_that("spec is correct", {
        expect_equal(spec, soln_spec, 
            info = "The ugarch specification is not correct. Did you use the ugarchspec() function and the 'sstd' parameter?"
        )
    })
    
    test_that("fit_20 is correct", {
        expect_equal(fit_20@fit$residuals, soln_fit_20@fit$residuals, 
            info = "The fitted model is not correct. Did you apply the ugarchfit() function on x_20 with specification defined earlier?"
        )
    })
    
    test_that("vol_20 is correct", {
        expect_equal(vol_20, soln_vol_20, 
            info = "The volatility is not correct. Did you use the sigma() function on the fitted model (fit_20)?"
        )
    })
    
    test_that("res_20 is correct", {
        expect_equal(res_20, soln_res_20, 
            info = "The residuals are not correct. Did you use the residuals() function on the fitted model (fit_20? The standardize parameter can be either TRUE or FALSE."
        )
    })
    
    test_that("merge_20 is correct", {
        expect_equal(merge_20, soln_merge_20, 
            info = "The merged object is not correct. Did you applied the merge.xts() function on the original series (x_20), the volatilities (vol_20) and the residuals (res_20)?"
        )
    })
})

# Calculate the kernel density for the 1-year maturity and residuals
density_x_1 <- ....(....)
density_res_1 <- ....(....)

# Plot the density diagram for the 1-year maturity and residuals
plot(....)
lines(...., col = "red")

# Add the normal distribution to the plot
norm_dist <- ....(seq(-0.4, 0.4, by = .01), mean = ....(x_1), sd = ....(x_1))
lines(seq(-0.4, 0.4, by = .01), 
      norm_dist, 
      col = "darkgreen"
     )

# Add legend
legend <- c(...., ...., ....)
legend("topleft", legend = legend, 
       col = c("black", "red", "darkgreen"), lty=c(1,1))

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors


# Plot the density diagram for 1-year maturity
soln_density_x_1 <- density(soln_x_1)

# Add the density of the residuals
soln_density_res_1 <- density(soln_res_1)


# And finally add the normal distribution to the plot
soln_norm_dist <- dnorm(seq(-0.4, 0.4, by = .01), mean = mean(soln_x_1), sd = sd(soln_x_1))

# Add legend
soln_legend <- c("Before GARCH", "After GARCH", "Normal distribution")


run_tests({
    test_that("density_x_1 is correct", {
        expect_equal(density_x_1$x, soln_density_x_1$x, 
            info = "Density of original time series is not correct. Did you apply the density() function on x_1?"
        )
        expect_equal(density_x_1$y, soln_density_x_1$y, 
            info = "Density of original time series is not correct. Did you apply the density() function on x_1?"
        )        
    })
    
    test_that("soln_res_1 is correct", {
        expect_equal(density_res_1$x, soln_density_res_1$x, 
            info = "Density of residuals is not correct. Did you apply the density() function on res_1?"
        )
        expect_equal(density_res_1$y, soln_density_res_1$y, 
            info = "Density of residuals is not correct. Did you apply the density() function on res_1?"
        )
    })
    
    test_that("normal distribution is correct", {
        expect_equal(norm_dist, soln_norm_dist, 
            info = "The normal distrubution in the plot is not correct. Did you define the mean and sd correctly?"
        )
    })  
    
    test_that("legend is correct", {
        expect_equal(legend, soln_legend, 
            info = "The legend is not correct. Did you listed the labels in the instructions correctly?"
        )
    })        
    
})


# Define the data to plot: the 1-year maturity yield changes and residuals 
data_orig <- ....
data_res <- ....

# Define the benchmark distribution
distribution <- ....

# Make the Q-Q plot of original data with the line of normal distribution
....(data_orig, ylim = c(-0.5, 0.5))
....(data_orig, distribution = distribution, col = "darkgreen")

# Make the Q-Q plot of GARCH residuals with the line of normal distribution
par(new=TRUE)
....(data_res * 0.614256270265139, col = "red", ylim = c(-0.5, 0.5))
....(data_res * 0.614256270265139, distribution = distribution, col = "darkgreen")
legend("topleft", c("Before GARCH", "After GARCH"), col = c("black", "red"), pch=c(1,1))

# Define plot data: the 1-year maturity yield changes and the residuals 
soln_data_orig <- x_1
soln_data_res <- res_1

# Define the benchmark distribution (qnorm)
soln_distribution <- qnorm


run_tests({
    test_that("the data_orig is correct", {
        expect_equal(data_orig, soln_data_orig, 
            info = "The data_orig is not correct. Is it equal to the original series of 1-year yield changes?"
        )
    })
    
    test_that("the distribution is correct", {
        expect_equal(distribution, soln_distribution, 
            info = "The distribution is not correct. Is it based on the normal distribution?"
        )
    })
    
    test_that("the data_res is correct", {
        expect_equal(data_res, soln_data_res, 
            info = "The data_res is not correct. Is it equal to the residuals of the model fitted on 1-year yield changes?"
        )
    })
})
    

# Q1: Did GARCH revealed how volatility changed over time? # Yes or No?
(Q1 <- ....)

# Q2: Did GARCH bring the residuals closer to normal distribution? Yes or No?
(Q2 <- ....)

# Q3: Which time series of yield changes deviates more 
# from a normally distributed white noise process? Choose 1 or 20.
(Q3 <- ....)

run_tests({
    test_that("the Q1 is correct", {
        expect_equal(tolower(Q1), "yes", 
            info = "The 1st answer is not correct. If GARCH could not estimate the volatility over time, its plot would be a constant line."
        )
    })
    
    test_that("the Q2 is correct", {
        expect_equal(tolower(Q2), "yes", 
            info = "The 2nd answer is not correct. The residuals from the examples are still not normal, but they are less erratic than before."
        )
    })
    
    test_that("the Q3 is correct", {
        expect_equal(Q3, 1, 
            info = "The 3rd answer is not correct. Which time series showed rapid changes in behavior?"
        )
    })
})
