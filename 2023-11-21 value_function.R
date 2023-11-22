
# Utility
u <- function(c) {
    
    if(g == 1){
        return(
            ifelse( c > 0, log(c), penelty)
            )
    }else{
        return( 
            ifelse( c > 0, (c^(1-g))/(1-g) ,         penalty ) 
            )
    }
}

# If g = 1 return log c og penelty
# else 

# Set up
g <- 1.5
alpha <- 1/3
delta <- 0.05
beta <- 0.95

# Set up grid
Nval <- 100
Kmax <- 5
k.grid <- seq( 0.01, Kmax, length.out = Nval) # Seq. from 0.01 to 5, by 100 steps

penelty <- 1e12
error <- 1e-6
iter <- 0

TV <- log(k.grid)
V <- rep(0, Nval)

c <- outer(k.grid, k.grid,
           function(k, k.next) k^alpha + (1-delta)*k-k.next
           )

while( abs(max(V-TV)) > error){
    V <- TV
    RHS <- u(c) + beta*outer(rep(1,Nval), V)
    TV <- apply(RHS, 1, max)
    iter <- inter +1
}


plot(k.grid, V)

