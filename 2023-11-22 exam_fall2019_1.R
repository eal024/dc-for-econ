

# Exam Q1 Global debdt

# Packates
library(tidyverse)

# a) Read the data into R
df <- haven::read_dta("data/4170_vedlegg_globaldebtdatabase (1).dta") |> 
    # Total debt as sum private debt and govemment debt
    rowwise( ) |> 
    mutate( tot_debt = sum(pvd_ls, gg, na.rm =T) 
            ) |> 
    ungroup()


# Number of countries and observations
df |> 
    summarise( countries = n_distinct(country),
               `nr obs` = n()
               )

dim(df)

# This is one country for a given year
df |> 
    distinct(country, year) |> 
    count()


# b) Select a subset of data from Norway 2000 to 2017
df_norway <- df |> 
    filter( country == "Norway",
            year %in% c(2000:2017)
            )


# Total debt as sum private debt and govemment debt
df_norway <- df_norway |> 
    rowwise( ) |> 
    mutate( tot_debt = sum(pvd_ls, gg, na.rm =T) ) |> 
    ungroup()

# Plot  the evolution of total debt over the period
df_norway |>
    select( country,year, pvd_ls, gg, tot_debt) |> 
    pivot_longer(-c(country, year)
                 ) |> 
    ggplot( aes(y = value,
                x = year,
                fill = as.factor(name), 
                color = as.factor(name)
                )
            ) +
    geom_line() +
    theme_light( ) + 
    theme( legend.position = "bottom") +
    labs( y = "debt private, goverment and total")


# c) The average share GG as share of GDP

nordic <- c("Norway", "Denmark", "Sweden", "Iceland", "Finland")

df_nordic <- df |> 
    select(country, year,  gg) |> 
    filter( country %in% nordic,
            year %in% c(2000:2017)
            )  |> 
    arrange(year) |> 
    mutate( g = mean(gg), .by = year
            ) |> 
    select(year, gg = g) |> 
    mutate( country = "nordic") |> 
    distinct()

df_nordi2 <- df |> 
    select(country, year, gg) |> 
    filter( country %in% nordic,
            year %in% c(2000:2017)
            ) |> 
    bind_rows( df_nordic) |> 
    arrange( year)


# Plot the result
df_nordi2 |> 
    filter( country != "Norway") |> 
    ggplot( aes( y = gg, x= year, color = factor(country)) ) +
    geom_line( ) +
    geom_line( data = df_nordi2 |> filter( country == "Norway"),
               aes( y = gg, x= year, color = factor(country)),
               linetype =2
               ) + 
    geom_text( aes( label = ifelse(country == "Norway" & year == 2010,
                                   "Norway", "")
    ))



# d) country name
Sys.setlocale("LC_CTYPE")
data <-
    read.table(
        'data/countrynames.csv',
        header = TRUE,
        sep = '\t',
        fileEncoding = 'UTF-8',
        stringsAsFactors = FALSE
    ) |> 
    separate(local_name.ifscode, into = c("local_name", "ifscode"), sep = "\t"
             ) |> 
    mutate( ifscode = as.numeric(ifscode)
            ) |> 
    as_tibble()


# Merging data
Sys.setlocale("LC_CTYPE")
df2017 <- df |> 
    select(ifscode, country, year, gg, pvd_all) |> 
    filter( year %in% c(2017)
            ) |> 
    left_join( data, join_by(ifscode)
               )

# Make a plot with labels
Sys.setlocale("LC_CTYPE")
df2017 |> 
    ggplot( aes( y = pvd_all, x = gg)
            ) +
    geom_point() +
    # geom_text(aes(label = local_name),
    #           position = position_jitter(width = 0.2, height = 0.2)
    #           )
    ggrepel::geom_text_repel(aes(label = local_name)) +
    labs( y = "private debt", x = "government debt") +
    geom_abline( color = "red", linetype =2) +
    ggplot2::lims( y = c(0,600), x = c(0,400))


# f) Choose the 10 countries with highest rate gg
Sys.setlocale("LC_CTYPE")
top10 <- df2017 |> 
    arrange( desc(gg) ) |> 
    head(10) |> 
    select(country)

top10_90_2017 <- df |> 
    filter( year %in% 1990:2017,
            country %in% top10$country
            )


top10_90_2017 |> 
    ggplot( aes(y = gg, x = year) ) +
    geom_line() +
    facet_wrap(~country, ncol= 5)




