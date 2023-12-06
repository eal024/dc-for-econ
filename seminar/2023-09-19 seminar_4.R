
# Seminar 4
library(tidyverse)


# 1. Reading wrangling data -----------------------------------------------

# Read
pwt <- read.csv("data/pwt.csv")

# subseting
gdp_nor <- pwt[pwt$countrycode == "NOR" & pwt$year %in% 1980:2000, c("year", "gdp")] 

# Annual growth rate
pwt |> 
    filter( country == "China",
            year %in% c(1970,2000)
            ) |> 
    mutate( lag_gdp = lag(gdp),
            growth = exp((log(gdp)-log(lag_gdp))/30)-1
            )


# The solow model: Poor countries grows faster than rich.

pwt_growth <- as_tibble(pwt) |> 
    mutate( lgdp = log(gdp)
            ) |> 
    filter( year %in% c(1970,2000)
    ) |> 
    select(country, year, lgdp) |>
    pivot_wider( names_from = year, values_from =  lgdp 
                 ) |> 
    mutate( growth = exp(log(`2000`)-log(`1970`) )/30
            )



# 2) Growth rate rich vs. poor countries --------------------------------


pwt_growth |> 
    ggplot( aes( y = growth, x = log(`1970`) )
            ) +
    geom_point() +
    geom_smooth( method = "lm", se = F)

# 3) Create a dummy
growht_years <- pwt |>
    as_tibble() |>
    filter(!countrycode %in% c("ABW", "AGO", "AIA", "ALB", "ARE") 
           ) |>
    mutate(increased = ifelse(gdp > lag(gdp), 1, 0),
           indx = 1,
           .by = country
           ) |>
    filter( !is.na(increased) )


# Fraction of growht years
growht_years |> 
    summarise( frac_growth = sum(increased, na.rm = T)/sum(indx)
               )

# Compare growth to fraction growht years
df <- growht_years |> 
    summarise( frac_growth = sum(increased, na.rm = T)/sum(indx),
               .by = country
    ) |> 
    left_join(
    pwt_growth,
    join_by( country)
    ) |> 
    filter( !is.na( growth) )

df |> 
    ggplot( aes(x = growth, y = frac_growth) 
            ) +
    geom_point( ) +
    geom_text(
        data = df |> arrange(growth) |> tail(4),
        aes(x = growth, y = frac_growth, label = country),
        inherit.aes = F
    ) +
    geom_text(
        data = df |> arrange(growth) |> head(4),
        aes(x = growth, y = frac_growth, label = country),
        inherit.aes = F
    ) +
    labs( title = "Growht rate in periode 1970-2000,\nand fraction of years with positiv growth")


# 3 . Proportional elections ----------------------------------------------


# 1) vector number votes each party 

fun_fair_representatives <- function(vec, representative) {
    # return number of representative for each party, based on fraction votes- 
    representative*vec/sum(vec)
}


#
fun_fair_representatives( vec = c(ap = 50, frp = 20, hoyre = 10), representative = 100)

fun_dHondt <- function(stemmer, ant_rep) {
    
    l <- list(  )
    l <- lapply( names(stemmer),\(x) l[[x]] = 0   )
    names(l) <- names(stemmer)

    repr <- ant_rep
    stemmer <- sort(stemmer, decreasing = T) 
    for( i in 1:repr){
        parti = names(stemmer[1])
        l[[parti]] = l[[parti]] + 1 

        stemmer[1]<- stemmer[1]/2
        cat("Partiet ", parti, " tildelt 1 stemmer,\nhar nå", l[[parti]], " antall stemmer.\n" 
        )
        stemmer <- sort(stemmer, decreasing = T) 
    }
    l
}

stemmer_oslo <- list(
    "Høyre" = 117998,
    "Arbeiderpartiet" =  66804,
    "Miljøpartiet De Grønne"  = 36975,
    "SV - Sosialistisk Venstreparti" =  36612,
    "Venstre" = 32954,
    "Fremskrittspartiet" =  21976,
    "Rødt" =  20929,
    "Kristelig Folkeparti" =  6220,
    "Partiet Sentrum"= 4461,
    "Industri- og Næringspartiet" = 4035,
    "Senterpartiet" =  2989,
    "Pensjonistpartiet" = 2054,
    "Folkestyret-listen" = 1995,
    "Folkets Parti" = 1948,
    "Norgesdemokratene" = 982,
    "Partiet Mot Bompenger" =  883,
    "Konservativt" =  775,
    "Liberalistene" = 626,
    "Alliansen - Alternativ for Norge" = 468,
    "Norges Kommunistiske Parti" = 394,
    "Kystpartiet" = 307
    )

stemmer_oslo<- stemmer_oslo |> unlist() 

class(stemmer_oslo)

# calculate the number of repr. for each party.
oslo <- fun_dHondt( stemmer = stemmer_oslo, ant_rep =  59)


## read data 
election <- vroom::vroom( "data/election.csv")
election$Fylkenavn <- sub( "\\?\\?", "ø", election$Fylkenavn)
names(election) <- sub( "\\?\\?", "ø", names(election))

election_long <- election |> pivot_longer( -c(1,2))
 
# Find the number of representatives for each party in each regio

tbl_election_fylke <- election_long |>
    group_by( Fylkenavn) |>
    nest() |>
    mutate( 
        rep =  map(data, \(x) filter(x, name == "repr") |>pull( value ) ),
        stemmer = map(data, \(x) { x |> filter( name != "repr") |> pull( value, name) })
        )

# Fordeling av represntater til fylke
tbl_election_fylke2 <- tbl_election_fylke |>
    mutate( repr = map2(stemmer, rep, \(s, r) {fun_dHondt(stemmer = s, ant_rep = r) }  ) )


a <- tbl_election_fylke2$repr[[1]] |> unlist() 

# 
fun_Sainte_Lague <- function(stemmer, ant_rep) {
    
    l <- list(  )
    l <- lapply( names(stemmer),\(x) l[[x]] = 0   )
    names(l) <- names(stemmer)

    repr <- ant_rep
    stemmer <- sort(stemmer, decreasing = T) 
    for( i in 1:repr){
        parti = names(stemmer[1])
        l[[parti]] = l[[parti]] + 1 

        stemmer[1]<- stemmer[1]/(1 + 2*l[[parti]] )
        cat("Partiet ", parti, " tildelt 1 stemmer,\nhar nå", l[[parti]], " antall stemmer.\n" 
        )
        stemmer <- sort(stemmer, decreasing = T) 
    }
    l
}


stem <- c("AP" = 500, "FRP" = 5, "MDG" = 4)
fun_Sainte_Lague(stemmer = stem, ant_rep = 10)

tbl_election_fylke3 <- tbl_election_fylke2 |> 
     mutate( sainte_lague = map2( rep, stemmer, \(rep, stemmer) fun_Sainte_Lague(stemmer = stemmer, ant_rep = rep) |> unlist() )
     )


# How the differences is by the diff. rules:
tbl_election_fylke3$repr[[1]] |> unlist()
tbl_election_fylke3$sainte_lague[[1]]

