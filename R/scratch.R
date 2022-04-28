#;; scratch.R
#;; Test stuff here

#;; Configs
# db_filename <-
#     here::here("db", "conv.db")

csvInput_filename <-
    here::here("dat", "minneapolis dfl convention del and alt 051310 - Minneapolis DFL convention del .csv")



#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;; Setup
library(rlang)
library(DBI)
library(readr)
library(stringr)
library(janitor)
library(glue)
library(dplyr)
library(purrr)
library(dbplyr)

source(here::here("R", "libwebutils.R"))


#;; Create/connect to db
#;; https://staff.washington.edu/phurvitz/r_sql/creating-an-sqlite-database.html
#;; https://db.rstudio.com/databases/sqlite/
# con <-
#     dbConnect(RSQLite::SQLite(), db_filename)



#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

unit = "61A"

#;; Read the people db, show the updated record
people_df <-
    dbReadTable(con, "people") %>%
    filter(unit == .env$unit)

peopleDel_df <-
    people_df %>%
    filter(role == "D") %>%
    arrange(last_name, first_name)
peopleAlt_df <-
    people_df %>%
    filter(role == "A") %>%
    arrange(rank, last_name, first_name)

#;; Branch for DPL
if ("DPL" %in% unique(people_df$role)) {
    peopleDel_df <-
        people_df %>%
        filter(role == "DPL") %>%
        arrange(last_name, first_name)
    
    peopleAlt_df <-
        peopleDel_df %>%
        filter(FALSE)
}


#;; Some computatations
dels_ttl <-
    peopleDel_df %>%
    nrow()
dels_in <-
    peopleDel_df %>%
    filter(is_checkedIn == 1) %>%
    nrow()
alts_up <-
    peopleAlt_df %>%
    filter(is_checkedIn == 1 & is_upgraded == 1) %>%
    nrow()
alts_pend <-
    peopleAlt_df %>%
    filter(is_checkedIn == 1 & is_upgraded == 0) %>%
    nrow()

body_contents <-
    c(
        glue("<h1>Unit: {unit}</h1>"),
        glue("<p>Dels ttl: {dels_ttl}, Dels In: {dels_in}, Alts Upgraded: {alts_up}, Alts Pending: {alts_pend}</p>"),
        glue("<h2>Delegates</h2>"),
        if (nrow(peopleDel_df)) {dbToTableUpdate(peopleDel_df)} else {""},
        glue("<h2>Alternates</h2>"),
        if (nrow(peopleAlt_df)) {dbToTableUpdate(peopleAlt_df)} else {""},
        '<h1><a href="/xkcd">Return to Main Page</a></h1>'
    ) %>%
    paste0(collapse="\n<br/>\n")

glue(page_tpl)