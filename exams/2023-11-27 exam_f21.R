

# Exercise two

# 90 squares
# start 0 (before 1)


# 1) Simulate a game, that tellers how many rolls needed to end the game

# dice
dice <- function() sample( size  = 1, x = 1:6, replace = T )

# roll <- 10000
# map_dbl( 1:roll, \(x) dice()) |> table()/roll

# How many rolls
fn_sim_roll <- function( ){

    n_squares <- 90
    sum_dice <- 0
    i <- 0
    while( sum_dice < n_squares){
        i <- i+1
        sum_dice <- sum_dice + dice() 

    } 
    
    i 

    }

sim_plays <- 100000
many_plays <- map_dbl(1:sim_plays, \(x)  fn_sim_roll() )

mean(many_plays)

#
tibble( play = many_plays) |> 
    ggplot( aes( x = play) ) +
    geom_histogram( aes(y = stat(density) ), color = "white"
    ) +
    geom_vline( xintercept = mean(many_plays), color = "red", size = 1
    ) 


# Read data
df <- read.csv("data/4170_2021_ladders.csv")


fn_sim_roll <- function( simple = F ){

    if(simple){
        n_squares <- 90
        sum_dice <- 0
        i <- 0
        while( sum_dice < n_squares){
            i <- i+1
            sum_dice <- sum_dice + dice() 

            } 
        i 
    }else if(simple == 0){
        n_squares <- 90
        sum_dice <- 0
        i <- 0
        while( sum_dice < n_squares){

            sum_dice <- sum_dice + dice() 
            if(sum_dice %in% df$start){
                sum_dice = df$end[df$start == sum_dice]
            }
            
            i <- i+1

            } 
        i 
    }   
}

# 
fn_sim_roll( simple = F)

# Simulate 10 000 
sim_plays <- 10000
many_plays_with_ladders <- map_dbl(1:sim_plays, \(x)  fn_sim_roll(simple = F) )
many_plays_simple <- map_dbl(1:sim_plays, \(x)  fn_sim_roll(simple =  T) )


#
mean(many_plays_with_ladders)
mean(many_plays_simple)

hist(many_plays_with_ladders)
hist(many_plays_simple)

# 3) "ladders work in both directions"
fn_sim_roll_extend <- function( ){
    
    n_squares <- 90
    sum_dice <- 0
    i <- 0
    end <- trowed <- FALSE
    while( sum_dice < n_squares){
            trowed <- FALSE
            sum_dice <- sum_dice + dice()             
            if(sum_dice %in% df$start){
                sum_dice = df$end[df$start == sum_dice]
                trowed <- TRUE
            #     
            }else if(trowed == FALSE & sum_dice %in% df$start ){   
                sum_dice = df$start[df$end == sum_dice]
                trowed <- TRUE
                }
            i <- i+1
            }
            i
        } 

df$end[df$start == 1]

fn_sim_roll(simple = T)
fn_sim_roll(simple = F)
fn_sim_roll_extend()

n <- 10000
simple  <- map_dbl(1:n, \(x)  fn_sim_roll( simple = T) )
one_dir <- map_dbl(1:n, \(x)  fn_sim_roll( simple = F) )
two_dir <- map_dbl(1:n, \(x)  fn_sim_roll_extend( ) )

hist(simple)
hist(one_dir) 
hist(two_dir)

mean(simple)
mean(one_dir)
mean(two_dir)

# Giant ladder
g_ladder <- read.csv("data/4170_2021_ladders_giant.csv")

g_ladder

# Construct a function that makes one simulation of the game and return the number of rolls required to finish the game





















game <- R6::R6Class( "ladder game",
    
    public = list(

        initialize = function( sq) {
            private$squares <- sq

            },
    
    dice = function(  ){
        function() sample( size  = 1, x = 1:6, replace = T )
     },
    
    print = function(...){
    
    cat("Antall ruter: ", private$squares,"\n")}
    ),

    private = list(squares = NULL)

)


my_game <- game$new( sq = 90)

my_game
fn_sim_roll <- function( simple = F , game , sq ){

    fdf <- game

    if(simple==T){
        n_squares <- sq
        sum_dice <- 0
        i <- 0
        while( sum_dice < n_squares){
            i <- i+1
            sum_dice <- sum_dice + dice() 

            } 
        i 
    }else if(simple == FALSE){
        
        n_squares <- sq
        sum_dice <- 0
        i <- 0
        while( sum_dice < n_squares){

            sum_dice <- sum_dice + dice() 
            if(sum_dice %in% fdf$start){
                sum_dice = fdf$end[fdf$start == sum_dice]
            }
            
            i <- i+1

            } 
        i 
    }   
}


fn_sim_roll( simple = T, game = g_ladder, sq = 200)
fn_sim_roll( simple = F, game = g_ladder, sq = 200)