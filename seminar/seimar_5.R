

# Seminar 5
library(tidyverse)

# 1) Import data
co2 <- readxl::read_excel("data/co2.xlsx") |> janitor::clean_names()

# Line plot
co2 |>
    filter( code == "USA") |> 

fun_plot <- function(data = co2, vec_code ){
    data |> 
    filter( code %in% vec_code ) |> 
        ggplot(aes(y = co2, x = year, fill = factor(code)) ) +
        geom_line() +
        geom_text( 
            data = data |>
                filter( code %in% vec_code,
                        year == 1950),
            aes(label = factor(code), x = year, y = co2 ),
            inherit.aes = F
        )
}    
    
fun_plot( data = co2, vec_code = c("USA", "CHN"))    




# 2) Annual emmissions
co2_annual <-  co2 |> 
    filter( code %in% c("USA", "CHN") ) |> 
    mutate( annual_co2 =  co2 - lag(co2), .by = code ) 

fun_plot( data = co2_annual |> 
              rename(
                  co2_total = co2,
                  co2 = annual_co2)
              , vec_code = c("USA", "CHN"))    


## New tibble
global <- co2 |>
    summarise( total = sum(co2), .by = year) |> 
    mutate( cum = cumsum(total) )

usa <- co2 |>
    filter( code == "USA") |> 
    summarise( total = sum(co2), .by = year) |> 
    mutate( cum = cumsum(total) )

each <- co2 |> 
    summarise( total = sum(co2), .by = c(year,code) ) |> 
    mutate( cum = cumsum(total) , .by = code)

# The global cumulative emmison. 
global |> 
    rename( co2 = cum) |> 
    mutate( code = "global") |> 
    bind_rows(usa |>
                  mutate( code = "USA") |> 
                  rename(co2 = cum) ) |> 
    fun_plot( vec_code = c("USA", "global") )
    
# Top 10
top_10 <- each |>
    filter( year == max(year) ) |>
    arrange( desc(cum) ) |> 
    head(6)


each |>
    rename( co2 = cum) |> 
    fun_plot( vec_code = top_10$code |> unique() )


# 4) See how big the US part is of the global
glob_vs_usa <- global |> 
    mutate( code = "global") |> 
    select(year, code, cum) |> 
    left_join( usa |> 
                   mutate( code = "usa") |> 
                   select(year, code, cum)
                   , join_by(year),
               suffix = c("_global", "_usa")
               )


# Calculate
glob_vs_usa2 <- glob_vs_usa |> 
    mutate( usa_share = cum_usa/cum_global)

glob_vs_usa2 |> 
    select(year, co2 = usa_share) |> 
    mutate( code = "usa share of co2") |> 
    fun_plot( vec_code =  "usa share of co2")


# 5) Emissions by contient

# Need countrycode
# install.packages("countrycode")

#
co2$continent <- countrycode::countrycode( co2$code, 'iso3c', 'continent')

# Plot continent cumulatives versus time

continent <- co2 |> 
    summarise( total = sum(co2), .by = c(year,continent) ) |> 
    mutate( cum = cumsum(total) , .by = continent) |> 
    rename( code = continent,
            co2 = cum
            )

# plot
fun_plot(continent, vec_code = continent$code) 


# 6) Stacked chart 
continent |> 
    ggplot( aes( x = year, y = co2/10^12, fill = factor(code)) ) +
    geom_area() +
    lims( x = c(1850, 2017)
          ) +
    theme( legend.position = "bottom",
           legend.title = element_blank()
           ) +
    labs( x = "NULL", y = "CO2 cumulative emmisions")










