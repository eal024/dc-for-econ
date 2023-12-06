

#
library(tidyverse)

# Rainfall and electoral turnout 2011
# Read data
rain <- vroom::vroom("data/41701_vedlegg_rainfall.csv")

# How many municipalities
unique(rain$knr) |> length()

distinct(rain, knr) |> count()

# b) knr 30, daily rainfall
min(rain$date)
max(rain$date)

rain |> 
    filter( knr == 301,
            between(date, ym("2011-06"), ymd("2011-08-31"))
        ) |>
    ggplot( 
        aes( x= date, y = rain )
    )  +
    geom_line()

# c) box plot
rain_oslo <- rain |> filter( knr == 301)

rain_oslo |> 
    mutate( month = month(date) ) |> 
    ggplot( aes( x = factor(month), y = rain , fill = factor(month))
    ) +
    geom_boxplot() +
    theme_light() +
    theme(legend.position = "bottom") +
    lims( y = c(0,100))


# The box-plot shows the distribution of the data, in each group (month). 
# The lower line in each box is the 25 th.percentile, the median (the line in the midle of the box) 
# and the 75-th percentil (line at the top of each box).
# dots above, represent outliers.


# d) Total 
rain_total <- rain |> 
    group_by(knr) |> 
    summarise( total.rain = sum(rain)
    ) 

# Min and max
rain_total |> 
    filter( total.rain == max(total.rain) |  total.rain == min(total.rain)
    )


# e) 
rain_total1 <- rain_total |> 
    mutate( knr = ifelse(nchar(knr) == 3,
                paste0("0",knr),
                knr),
            region       = str_sub(knr, 1,2),
            municipality = str_sub(knr, 3,5)
         )  |> 
    relocate( knr, .before = region
    )

# Mean rain fall by region
rain_mean <-  rain_total1 |>
    summarise( mean_total_rain = mean(total.rain), .by = region
    )

# Table
rain_mean

# Presented by plot 
rain_mean |>
    mutate( region = factor(region) |> fct_reorder(mean_total_rain) ) |> 
    ggplot( aes( y = mean_total_rain, x = region, fill = factor(region)) ) + 
    geom_col() +
    coord_flip()

# f) The rainiest day
df_rainiestday <- rain |>
    arrange( knr) |> 
    group_by( year = year(date), knr) |> 
    mutate( rainisest = max(rain) 
        )

# How many have the rainiest day in each month
df_rainiestday  |> 
    filter(rain == rainisest) |>
    group_by( month(date) )  |> 
    count()

# g) 
df_rainiestday |>
    mutate( knr = as.character(knr)
    ) |> 
    left_join( tbl2, join_by(knr)) |>
    View()


# Clening data for joining
tbl3 <- tbl2 |> 
    filter( municipality2 != "") |> 
    mutate( knr = str_c(region, municipality2) |> as.numeric()  )  

tbl4 <- tbl3 |>
     select(knr, municipality,  entited, woted2 ) |> 
     mutate( share = woted2/entited)

rain_joined <- rain |>
    left_join(tbl4 ,
     join_by(knr) 
    )

# h)
rain_election_day_voters <- rain_joined |> 
    filter( date == ymd("2011-09-12")
    )   |> 
    mutate( above = ifelse(rain >= 60, 1, 0))


rain_election_day_voters |>
    ggplot( aes( y = share,
                 x = rain)
                 ) +
    geom_point() +
    geom_smooth( 
        data = rain_election_day_voters |> filter(above == 0),
                se = T
                ) +
    geom_smooth( 
        data = rain_election_day_voters |> 
                filter(above ==1),
                se = T
    )


# Linear regression
lm( data = rain_election_day_voters,share~ rain + above ) |> summary()
