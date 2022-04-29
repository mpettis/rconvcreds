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
getStats <- function(df) {
    ret_lst <- list()

    ret_lst$roster_ttl <-
        df %>%
        nrow()
    ret_lst$roster_present <-
        df %>%
        filter(is_checkedIn == 1) %>%
        nrow()
    ret_lst$dels_ttl <-
        df %>%
        filter(role %in% c("D")) %>%
        nrow()
    ret_lst$dpls_ttl <-
        df %>%
        filter(role %in% c("DPL")) %>%
        nrow()
    ret_lst$alts_ttl <-
        df %>%
        filter(role %in% c("A")) %>%
        nrow()
    ret_lst$floor_ttl <-
        df %>%
        filter((role %in% c("D", "DPL") & is_checkedIn == 1)
               | (role %in% c("A") & is_checkedIn == 1 & is_upgraded == 1)) %>%
        nrow()
    ret_lst$dels_in <-
        df %>%
        filter(role %in% c("D") & is_checkedIn == 1) %>%
        nrow()
    ret_lst$dpls_in <-
        df %>%
        filter(role %in% c("DPL") & is_checkedIn == 1) %>%
        nrow()
    ret_lst$alts_up <-
        df %>%
        filter(role %in% c("A") & is_checkedIn == 1 & is_upgraded == 1) %>%
        nrow()
    ret_lst$alts_pend <-
        df %>%
        filter(role %in% c("A") & is_checkedIn == 1 & is_upgraded == 0) %>%
        nrow()

    ret_lst
}
# people_df <-
#     dbReadTable(con, "people")
# getStats(people_df)
# glue_data(getStats(people_df), tableSummary_tpl)
# people_df %>%
#     filter(is_checkedIn == 1 & is_upgraded == 0)
