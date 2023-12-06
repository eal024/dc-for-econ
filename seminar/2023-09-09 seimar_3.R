
## Seimar 3 Week 37


## 1 Computing PI

# a)
denomin <- function(n) if(n < 2){1}else{2 + denomin(n-1)}

num <- function(n){ (-1)^(n-1)*(4/denomin(n))}

#
fun_sq <- function(n) {
    purrr::map_dbl(1:n,\(x) num(x))
}

# b) Nilakantha appreach
fun_vec_fac <- function(n){
    if( (n-1) == 0){1}else{ seq(from = (n-1)*2, by = 1, length.out = 3) }  
} 

#reduce(fun_vec_fac(2), `*`)

fun_2pi <- function(n){
    if(n == 1){
        3
        }else{
        (-1)^n*( 4/reduce(fun_vec_fac(n), `*`) )
    }
}

fun_2pi_sum <- function(n) sum(
    map_dbl(1:n, ~ fun_2pi(n = .x) ) 
    )

# 3) 

n_sim <- seq( from = 1, to = 1000, length.out = 50 ) |> 
    as.integer()

df_sim2 <- tibble(
    n_sim = n_sim,
    gregory     = map(n_sim, \(x) sum(fun_sq(n = x))),
    Nilakantha  = map(n_sim, \(x) fun_2pi_sum(n = x)) 
)


# 4 Show how the approx work over time:
df_sim2_long <- df_sim2 |> 
    pivot_longer(-n_sim) |> 
    unnest( value) |> 
    #filter( n  >  1) |> 
    mutate( value = value - pi )

# How the diff. approx work:
df_sim2_long |> 
    mutate( name = factor(name)) |> 
    ggplot( aes( y = value, x = n_sim, color = name) ) + 
    geom_line( ) +
    ylim( c(-0.01, 0.01))

# Importing and cleaning data ---------------------------------------------

df <- vroom::vroom("data/2019-09-11_partifordeling_1_fy_2019.csv")


votes <- df |> 
    # 1) Filter
    filter( Fylkenavn != "Oslo") |> 
    # 2) Vites
    mutate( votes = `Antall stemmer totalt`) |> 
    # 3) select
    select( Fylkenavn, Fylkenummer, Partikode, votes) |> 
    # 4) Change som names
    mutate( 
        across( .cols = c("Fylkenavn", "Partikode"),
                .fns = \(x) str_replace_all(x, "\\?\\?", "O")
                )
            ) |> 
    mutate( Fylkenavn = str_to_sentence(Fylkenavn))

# 4) 
votes |> 
    filter( Partikode %in% c("A", "SV", "RODT", "SP", "FRP", "KRF", "MDG", "H", "FNB", "V")
            ) |> 
    pivot_wider( names_from = Partikode, values_from = votes)


