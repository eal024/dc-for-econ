

## packages
library(tidyverse)


# a) Importe the data

forbes <- vroom::vroom("data/forbes_2022_billionaires.csv")

View(forbes)

# highest and lowest age
forbes |>
    summarise(
        lowest.age = min(age, na.rm = T),
        highest.age = max(age,na.rm = T),
        # Fortune
        max.fortune = max(finalWorth, na.rm = T),
        mean.fortune = mean(finalWorth, na.rm = T)  
        )

# Does the age variable show the correct number?
(min(forbes$birthDate, na.rm = T) %--% ymd("2020-06-12"))/years(1)
(max(forbes$birthDate, na.rm = T) %--% ymd("2020-06-12"))/years(1)

 
# b) Numbers of billionarires in each country.

tbl_n_billionaries <- forbes |> 
    group_by(country) |> 
    count( sort = T) |> 
    head( 12)


tbl_n_billionaries |> 
    ggplot( aes( y = fct_reorder(country, n), x = n)
    ) +
    geom_col() + 
    geom_text( aes( label = n), color = "white", hjust = 1.4 , size = 6) +
    theme_minimal( base_size = 16 ) +
    labs( x = "", y = NULL, title = "Numbers of billionars, by country")


# c) The 200 richest against their rank
# The logarithm decrese in fortune
# decreasing, 
forbes |>
  select( finalWorth, rank) |> 
  ggplot( aes( y = finalWorth, x = rank) ) +
  geom_line( alpha = .6) +
  geom_point() +
  scale_x_log10( )


# Rank by gender
forbes |> 
  select( finalWorth, rank, gender) |> 
  filter( !is.na(gender)) |> 
  group_by( gender) |> 
  mutate( rank = 1:n() ) |>  
  ggplot( aes( y = finalWorth, x = rank, color = gender) ) +
  geom_line( alpha = .6) +
  geom_point() +
  scale_x_log10( ) +
  facet_wrap(~gender, scales = "free_x")


# d) Define billionaires as young if their age is below 65.
tbl_old_man_by_country <- forbes |> 
    #
    mutate( young = ifelse( age > 65, T, F)
    )  |> 
    select(  rank, person = personName, age, young, finalWorth, gender, country)  |> 
    group_by( gender, country, old_man = young ) |> 
    count()

tbl_old_man_by_country |> arrange( desc(n)) 

# e) The Prop. of being a billionarie differ by the day of birth

person_by_weekday <- forbes |> 
    mutate( week_number = week(birthDate)) |> 
    select( birthDate,week_number, finalWorth )


forbes |> 
    filter( str_sub(birthDate, 6, 10) != "01-01") |> 
    mutate( week_of_birth = week(birthDate)) |> 
    ggplot( 
        aes( y = finalWorth, x = week_of_birth) 
     ) + 
     geom_point( ) +
     scale_y_log10() +
     geom_smooth( method = "lm")

# Does not seem birth of week is correlated with welth.
forbes |> 
    filter( str_sub(birthDate, 6, 10) != "01-01") |> 
    mutate( week_of_birth = week(birthDate)) |> 
    lm( formula = I(log(finalWorth)) ~ factor(week_of_birth) )  |> 
    summary()

forbes_by_week <-  forbes |> 
    filter( str_sub(birthDate, 6, 10) != "01-01") |> 
    mutate( week_of_birth = week(birthDate)  ) |> 
    group_by( week_of_birth )  |> 
    summarise(
        n = n(),
        fortune = mean(finalWorth, na.rm= T),
        se = sd(finalWorth, na.rm= T)
    ) 

forbes_by_week |> 
    ggplot( aes( x = week_of_birth, y = fortune ) ) +
    geom_point( size = 4) +
    geom_errorbar(aes(ymin = fortune - se, ymax = fortune + se), width = 1) 

# Confirm that the plot
forbes |> 
    filter( str_sub(birthDate, 6, 10) != "01-01") |> 
    mutate( week_of_birth = week(birthDate))  |> 
    filter( week_of_birth == 49)





