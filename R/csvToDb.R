#;; csvToDb.R
#;; Convert input CSV to appropriate sqlite db tables

#;; Configs
# db_filename <-
#     here::here("db", "conv.db")

csvInput_filename <-
    # here::here("dat", "minneapolis dfl convention del and alt 051310 - Minneapolis DFL convention del .csv")
    here::here("dat", "2022 CD5 Delegates, Alternates, & DPLs as of 4_27_22 - Full.csv")



#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;; Setup
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



#;;=============================================================================
#;; Read in the people file, condition data
csvInput_df <-
    read_csv(csvInput_filename, col_types = cols())

# csvInput_df %>% count(Gender) %>% print(n=Inf)
# csvInput_df %>% clean_names() %>% glimpse()
 
#;;-----------------------------------------------------------------------------
#;; Write people table
if (dbExistsTable(con, "people")) {
    dbRemoveTable(con, "people")
}

#;; Condition for input into database
dbPeople_df <-
    csvInput_df %>%
    clean_names() %>%
    #;; Get the del/alt (and alt rank) into fields
    mutate(
        #;; Clean the gender category
        sex=toupper(gender),
        #;; Note dels and alts
        # role = ifelse(str_detect(tolower(status), "^a"), "A", "D"),
        role = case_when(
            str_detect(tolower(status), "^dpl") ~ "DPL",
            str_detect(tolower(status), "^del") ~ "D",
            str_detect(tolower(status), "^a")   ~ "A",
            TRUE                                ~ "?" ),
        #;; Rank, if alt
        # rank = str_extract(mpls_del_status, "\\d+") %>% as.numeric(),
        rank = alternate_rank,
        #;; Most granular grouping unit
        unit = party_unit,
        #;; Determine superdelegates.
        is_superdelegate = role == "DPL",
        #;; Checked In status
        is_checkedIn = FALSE,
        #;; If an alt, are you upgraded
        is_upgraded = FALSE
    ) %>%
    #;; Make id values
    arrange(last_name, first_name) %>%
    mutate(id=row_number()) %>%
    #;; Select and groom columns
    select(id, last_name, first_name, sex,
           address, city, zip,
           unit, cd, sd=party_unit,
           role, rank, is_superdelegate,
           is_checkedIn, is_upgraded)

# dbPeople_df %>% count(sex)
# dbPeople_df %>% glimpse()

dbWriteTable(con, "people", dbPeople_df)
# dbListTables(con)
# dbReadTable(con, "people")


#;;-----------------------------------------------------------------------------
#;; Calculate max delegates per unit
if (dbExistsTable(con, "unitStats")) {
    dbRemoveTable(con, "unitStats")
}

dfUnitStats_df <-
    dbPeople_df %>%
    filter(role == "D" & !is_superdelegate) %>%
    count(unit, name="max_del") 

dbWriteTable(con, "unitStats", dfUnitStats_df)



#;;-----------------------------------------------------------------------------
#;; Write htmlTableRows
# if (dbExistsTable(con, "htmlTableRows")) {
#     dbRemoveTable(con, "htmlTableRows")
# }
# 
# #;; Condition for input into database
# dbHtmlTableRows_df <-
#     dbReadTable(con, "people") %>%
#     as_tibble() %>%
#         mutate(
#             trReadOnly = dbRowToTrReadOnly(.)
#         ) %>%
#     select(id, trReadOnly)
