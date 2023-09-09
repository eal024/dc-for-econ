
library(tidyverse)

# Programming

10%%3 # Gives 3 and a reminder of 1
10%/%3 # integer division

156/3 # Inge rest
156%/%3
156%%3

# Logical vectors

a <- c(1:10)

# Any
if( any(a < 1)){
    cat("noen verdier er under 1")
    }else{ cat("Ingen verdier under 1")} 

# Alternativ
all(a > 11)

# Excerise
# Dice with s sides. Defult is 6
fun_dice <- function( s = 6) sample( x = 1:s, 1) 

#
fun_sum_rolls <- function( nr.rolls, sides = 6){
    col_dice <- vector()
    for( i in 1:nr.rolls){
        col_dice[i] <- fun_dice(s = sides)
    }
    (col_dice)
}

fun_sum_rolls(nr.rolls = 2, sides = 10)


# The ice cream example ---------------------------------------------------




