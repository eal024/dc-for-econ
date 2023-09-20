
# Seminar 4
library(tidyverse)

# 1) Download data mm.
pwt <- read.csv("data/pwt.csv")

# subsetting
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

# 1)
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


# 3 --------------------------------------------------------------




