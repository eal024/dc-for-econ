
# Seminar 1



# 3) --------------------------------------------------------------------

# a)
a <- 75
class(a)

# b) 
rep(a, 3)

# c)
v <- c(25,17,24,26,15,17,19,25)


# 4 d) Use the plot function --------------------------------------------

plot(v)
plot(table(v))

plot(
    table(v),
    type = "h",
    main = "Title, frekvens",
    xlab = "x text",
    ylab = "value"
)

# export the plot as a script
dev.copy(pdf, "plot/plot_seimar1.pdf")
dev.off()



# 5. Data.frame -----------------------------------------------------------

# Read data
inc_wealth <- vroom::vroom("data/inc_wealth.csv")

# 
log_mean <- 2.5  # Mean of the logarithm
log_sd <- 0.8    # Standard deviation of the logarithm

# Simulate 800 income values using a Gamma distribution
income <- rgamma(800, shape = 2, rate = 0.00004) * 10
age  <-  sample( 800, replace = T, x = c(18:80))

inc_wealth <- data.frame( inc = income, age = age)

# Histogram of the distribution
hist(inc_wealth$inc )

# add variable: Transform income from NOK to euro
inc_wealth$income_euro <- inc_wealth$income/11.57

# 



# Keep females
# Wommen
inc_wealth$w <- as.numeric(runif( n = nrow(inc_wealth), 0,1 ) > 0.49 )

# Keep
inc_wealth[inc_wealth$w == 1,]

# Keep individuals with 10% highest value

# quantile 
quant <- quantile( inc_wealth$inc, c(0.25, 0.5, 0.75, 0.9))


inc_high_wealth <- inc_wealth |> subset( inc_wealth >= quant[4])
inc_not_high_wealth <- inc_wealth |> subset( inc_wealth < quant[4])

inc_high_walth$age |> sort()
inc_not_high_wealth$age |> sort()

# Showing the age distribtion of two samples 
hist(inc_high_walth$age)
hist(inc_not_high_wealth$age)


# Income inequaltiy -------------------------------------------------------

# install.packages("ineq")
library(ineq)
Gini(inc_wealth$inc)
ineq::Lc(inc_wealth$inc, plot = T)






