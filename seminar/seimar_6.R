

# Seminar 6
library(tidyverse)

# Exercise 1 

# a)
df_debt <- haven::read_dta("data/GlobalDebtDatabase.dta")

# How many var and obs
dim(df_debt)

# How many country

unique(df_debt$country) |> length()

df_obs <- tibble( cont = table(df_debt$country)|> names(),
        obs = table(df_debt$country) |> as.numeric()
        ) 

df_debt |> 
    mutate( across( .cols = everything(),
                    .fns = function(x) sum(is.na(x))
                    ),
            .by = country
            )

# b) Subset
df_debt_norway <-  df_debt |> 
    filter( country == "Norway",
            year %in% c(2000:2017)
            ) |>
    select(ifscode, country, year, pvd_ls, gg) |> 
    # Total debt
    rowwise( ) |>     
    mutate( total_dbt = sum(pvd_ls, gg)
            )


# plot
plot( x = df_debt_norway$year, y = df_debt_norway$total_dbt, type = "ll")

# The share private and GG debt of total
df_debt_norway |> 
    pivot_longer( -c(ifscode:year)
                  ) |> 
    filter( name != "total_dbt") |>
    mutate( andel = value/sum(value), .by = year) |> 
    mutate( name = ifelse( name == "gg",
                           "Goverment",
                           "Private"
                           )
            ) |> 
    filter( name == "Goverment") |> 
    ggplot( aes( x = year, y = andel, color = name) ) +
    geom_line() + 
    scale_y_continuous( labels = scales::percent ) +
    theme( legend.position = "bottom")



# c) Compute the share of GG debt for the Nordic countries
df_debt$country |> unique() |> sort()

nordic <- c("Norway", "Sweden", "Finland", "Denmark", "Iceland")

# df nordic    
df_nordic <- df_debt |> 
    filter( 
        country %in% nordic & year %in% c(2000:2017)
            ) 

df_nordic$country |> unique() |> sort()

# 
df_nordic_share_gg <- df_nordic |>
    summarise( across( .cols = c(pvd_ls,gg),
                       .fns = function(x) mean(x)
                       ),
               .by = year
    ) |> 
    rowwise( ) |> 
    mutate( debt_tot = sum(pvd_ls, gg),
            share_gg = gg/debt_tot
            ) 


## 
df_nordic_share_gg |>
    ggplot( aes(x = year, y = share_gg) ) +
    geom_line()


df_share <- df_debt_norway |> 
    mutate( share_gg = gg/total_dbt ) |> 
    select( year, country, share_gg) |> 
    bind_rows( 
        df_nordic_share_gg |> 
            mutate( country = "Noric") |> 
            select(year, country, share_gg)
        )

# Norway compare to the Nordic
df_share |>
    ggplot( aes(x = year, y = share_gg, color = country) ) +
    geom_line()



# d) ----------------------------------------------------------------------












