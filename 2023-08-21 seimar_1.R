


# 3)
# a)
a <- 75
class(a)

# b) 
rep(a, 3)

# c)
v <- c(25,17,24,26,15,17,19,25)

# 4 d) Use the plot function
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

