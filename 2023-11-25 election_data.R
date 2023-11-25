

# web scraping 

library(tidyverse)
library(rvest)

url <- "https://www.ssb.no/a/english/kortnavn/kommvalg_en/tab-2011-11-04-04-en.html"

# HTML
html <-  read_html(url)

html  |>
    html_elements("body bgcolor='#ffffff'") |> 
    html_table()


tbl <- html_table(html)[[2]]

names( tbl) <- tbl[2,1:7] |> pivot_longer(everything()) |> pull(value)

tbl <- tbl[5:nrow(tbl),1:7]

tbl1 <- tbl |> 
    janitor::clean_names() |>
    filter( no_county_municipality != "" ) |>   
    mutate(  across( .cols = matches("person"), 
                     .fns = function(x) str_replace_all(x, "\\s", "")  |> as.numeric() ) 
        ) |> 
    rename( 
        municipality = 1,
        entited = 2,
        woted = 3,
        woted2 = 4,
        woted3 = 5,
        percent_woted_advance = 6,
        percent_voter_tornout = 7
        ) |> 
    mutate( 
        across( .cols = contains("percent"), as.numeric ),
        knr = str_remove_all(municipality,"[A-Za-z]" ) |>
              str_to_lower( ) |> 
              str_remove_all("ø| |-|å|æ|á")
        ) 


tbl2 <- tbl1 |> 
    mutate(
        region =  str_sub(knr, 1,2),
        municipality2 = str_sub(knr, 3,5)
    )  

tbl2 |> slice(20:30)
