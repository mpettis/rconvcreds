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

df
        dbReadTable(con, "people") %>%


people_df <-
    dbReadTable(con, "people") %>%
    filter(id == .env$id)
