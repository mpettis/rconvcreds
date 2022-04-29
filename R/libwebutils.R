#;; libwebutils.R
#;; Commmon library of functions, global variables, etc.

library(rlang)
library(htmltools)
library(DBI)
library(readr)
library(stringr)
library(janitor)
library(glue)
library(dplyr)
library(purrr)
library(dbplyr)


#;; Database connection
db_filename <-
    here::here("db", "conv.db")
con <-
    dbConnect(RSQLite::SQLite(), db_filename)


#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;; Templates and css strings Load
#;; This loop walks through the .tpl files in source and makes variables like
#;; the following pattern:
# page_tpl <- read_lines(here::here("source", "page.tpl")) %>% paste0(collapse="\n")
list.files(here::here("source"), full.names = TRUE) %>%
    walk(~{
        varname <-
            basename(.x) %>%
            str_replace("\\.", "_")
        contents <-
           read_lines(.x) %>%
            paste0(collapse="\n") 
        assign(varname, contents, envir = global_env())
    })



#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;; Functions

#;; Takes a db (usually single row), and returns the <tr class="..."> value
#;; to use with the CSS selector in main.css to highlight the row color
#;; based on if delegate is in/out, and upgraded or not.
dbRowToRowcolorClass <- function(dfr) {
    dfr %>%
        mutate(
            rowcoloring = case_when(
                !is_checkedIn              ~ "out",
                role == "A" & !is_upgraded ~ "in-alt",
                role == "A" & is_upgraded  ~ "up-alt",
                TRUE                       ~ "in-del"
            )
        ) %>%
        pluck("rowcoloring")
}


#;; Create a status for dels alts that are checked in/out
dbRowToFloorStatus <- function(dfr) {
    dfr %>%
        mutate(
            rowcoloring = dbRowToRowcolorClass(.),
            status_disp = case_when(
                rowcoloring == "out"    ~ "OUT",
                rowcoloring == "in-alt" ~ "PEND",
                rowcoloring == "up-alt" ~ "UP",
                rowcoloring == "in-del" ~ "IN",
                TRUE                    ~ "?"
            )
        ) %>%
        pluck("status_disp")
}



#;; Create Action field html to update user status, based on role.
dbRowToActionHtml <- function(df) {
    alternateTpl <-
        '<a href="/changeStatus?id={id}&status=OUT">OUT</a>
    <a href="/changeStatus?id={id}&status=PEND">PEND</a>
    <a href="/changeStatus?id={id}&status=UP">UP</a>'

    delegateTpl <-
        '<a href="/changeStatus?id={id}&status=OUT">OUT</a>
    <a href="/changeStatus?id={id}&status=IN">IN</a>'
    
    dfr <-
        df %>%
        mutate(actionHtml = ifelse(role == "A",
                                   glue(alternateTpl),
                                   glue(delegateTpl)))

    #;; Return the string
    pluck(dfr, "actionHtml")
}
# people_df <-
#     dbReadTable(con, "people")
# dbRowToActionHtml(people_df %>% slice(1:11))










#;; Take row of people table and make a string that represents the table row
#;; html to construct a table row element with.
#;; Works with multipe-rowed dataframe.
dbRowToTrReadOnly <- function(dfr) {
    # env_bind(current_env(), !!!dfr)
    # env_print()
    
    dfr <-
        dfr %>%
        mutate(
            rowcoloring = dbRowToRowcolorClass(.),
            status_disp = dbRowToFloorStatus(.),
            rank_disp = ifelse(is.na(rank), "", rank)
        )

    #;; Return the string
    glue_data(dfr, trReadOnly_tpl) %>%
        paste0(collapse="\n")
}
# people_df <-
#     dbReadTable(con, "people")
# dbRowToTrReadOnly(people_df %>% slice(1))
# dbRowToTrReadOnly(people_df %>% slice(1:2))
# people_df %>% slice(1) %>% glimpse()



#;; Construct html table given a dataframe, read-only version
dbToTableReadOnly <- function(df) {
    th_contents <-
        thReadOnly_tpl
    tr_contents <-
        dbRowToTrReadOnly(df)     
    glue(table_tpl)
}
# people_df <-
#     dbReadTable(con, "people")
# dbToTableReadOnly(people_df %>% slice(1:111))






#;; Take row of people table and make a string that represents the table row
#;; html to construct a table row element with.
#;; Works with multipe-rowed dataframe.
dbRowToTrUpdate <- function(dfr) {
    # env_bind(current_env(), !!!dfr)
    # env_print()
    
    dfr <-
        dfr %>%
        mutate(
            rowcoloring = dbRowToRowcolorClass(.),
            status_disp = dbRowToFloorStatus(.),
            rank_disp = ifelse(is.na(rank), "", rank),
            action_html = dbRowToActionHtml(.)
        )

    #;; Return the string
    glue_data(dfr, trUpdate_tpl) %>%
        paste0(collapse="\n")
}
# people_df <-
#     dbReadTable(con, "people")
# dbRowToTrUpdate(people_df %>% slice(1))
# dbRowToTrUpdate(people_df %>% slice(1:2))
# people_df %>% slice(1) %>% glimpse()



#;; Construct html table given a dataframe, read-only version
dbToTableUpdate <- function(df) {
    th_contents <-
        thUpdate_tpl
    tr_contents <-
        dbRowToTrUpdate(df)     
    glue(table_tpl)
}
# people_df <-
#     dbReadTable(con, "people")
# dbToTableUpdate(people_df %>% slice(1:111))



#;; Calculate stats based on people record set
getStats <- function(df) {
    
}
