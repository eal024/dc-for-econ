
# Household and inequality (65%)
library(tidyverse)

# 1)
# import data
cps <- readxl::read_excel("data/4170_2021_cps.xlsx")

# mean hourly earning
mean(cps$ahe[cps$female == 1])
mean(cps$ahe[cps$female == 0])

cps |> summarise( mhe = mean(ahe), .by = female)

# 2) Annual income
cps1 <- cps |> 
    #
    mutate( annual_earning = ahe*8*50 ) |> 
    arrange( annual_earning) |> 
    mutate( rank = 1:n() 
    )

#
q <- unique(cps1$annual_earning) |> quantile( probs = seq(from = .1, to = 1, by = 0.1) )

# Decils

decile <- quantile( cps1$annual_earning, probs = seq(from = 0.1, by = .1)) |> floor()


cps2 <- cps1 |> 
    mutate( decile = ntile(rank, 10) )


# Fraction of woman in each decile
freq_w <- cps2 |>
    group_by(decile, female ) |> 
    summarise( count = n()
    )   |> 
    group_by(decile) |> 
    mutate(  frq = count/sum(count)
    )

# Ploting the result
freq_w |> 
    select(-count) |> 
    ggplot( aes(x = factor(decile), y = frq, fill = factor(female))
    ) +
    geom_col( position = position_dodge2()) +
    labs( title = "Fraction female/male in each decile",
        subtitle =  "The fraction of men is increasing as the wage level increases",
        y = "% of female (1) or male(0) in each decile"
    ) +
    scale_y_continuous( labels = scales::percent )


# 4) Lorenz curve for annual income y


cps_lorenz <- cps1 |> 
    mutate( yi = cumsum(annual_earning)/(mean(annual_earning)*nrow(cps1)) )  


# Plot the curve
plot_lorenz <- ggplot(cps_lorenz, aes(x = rank, y = yi) ) +
    geom_line( )  +
    geom_line( data = tibble( 
        rank = 1:nrow(cps2),
        y = seq(from = 0, to = 1, length.out = nrow(cps2) )
    ),
    aes( y = y,x = rank),
    linetype = 2
    )

# 5) Consider a simple tax schedule where a flat tax rate 𝑡𝑡 = 25%
cps2 <- cps1 |> 
    mutate( 
        tax = annual_earning*0.25,
        income_after_tax = annual_earning-tax,
        transfers = mean(tax),
        income_after_transfers = income_after_tax + transfers  
        ) |> 
        # Lorenz curve
        mutate( yi = cumsum(income_after_transfers)/(mean(income_after_transfers)*nrow(cps2)) 
        )  


# Lorenz curve before and after redistribution.
plot_lorenz + geom_line( data = cps2,aes( x =rank, y = yi), inherit.aes = F, color = "red" )


# 6) Annual earnings in Norway
dir("data")

df_income_couples <- vroom::vroom("data/4170_2021_incomes_couples.csv")

# Gini
ineq::Gini(df_income_couples$inc[df_income_couples$female == T])
ineq::Gini(df_income_couples$inc[df_income_couples$female == F])

df_income_couples |> 
    summarise( gini = ineq::Gini(inc), .by = female)

# 7 Houshold income/gini
df_income_couples |>
    arrange( hhid) |> 
    group_by( hhid ) |> 
    mutate( hh_income = sum(inc) 
    ) |> 
    select(hhid, hh_income) |> 
    distinct( ) |>
    ungroup()  |>  
    summarise( gini = ineq::Gini(hh_income) )
 

 # Correaltion 
model_corr <- df_income_couples |> 
    arrange( hhid, female) |> 
    select( hhid, inc) |> 
    group_by( hhid) |> 
    mutate( index = 1:n() ) |> 
    pivot_wider( names_from = index, values_from = inc)  |> 
    rename( male  = `1`, female = `2`) |> 
    ungroup() |> 
    lm( formula = male ~ female)


# Model correlation 
model_corr |> summary()

# Houshold gini 0.351
# Gini from random matching
l_data <- df_income_couples |> 
    relocate( female, .before = id) |> 
    group_split( female) 


men <- l_data[[1]]
female <- l_data[[2]]

len_nrow <- seq( from = 1, to = nrow(men), by = 1)

random_number <- sample( x = len_nrow , size = nrow(men), replace = F  )


random_men <- men |> 
    select( inc)  |> 
    slice_sample( n = nrow(men)
    ) |> 
    mutate( i = random_number)

random_female <- female |> 
    select(inc) |> 
    mutate( i = random_number)


random <- random_men|> 
    left_join(random_female, join_by(i), suffix = c(".men", ".female"))

random |> 
    mutate( hhinc = inc.men +inc.female ) |> 
    summarise( gini = ineq::Gini(hhinc))

# 0.32 After random

# Is the correlation close to 0?
random |> 
    lm( formula = inc.men ~inc.female) |> 
    summary() # t-value 0


# Gini
cps2 |> 
    summarise( 
        gini.before = ineq::Gini(annual_earning),
        gini.after = ineq::Gini(income_after_transfers)
    )




