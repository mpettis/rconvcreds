#;; plumber.R
#;; Main web runner

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;; Configs
# db_filename <-
#     here::here("db", "conv.db")


#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;; Setup
library(htmltools)
library(DBI)
library(readr)
library(stringr)
library(janitor)
library(glue)
library(dplyr)
library(purrr)
library(dbplyr)

source(here::here("R", "libwebutils.R"))

# con <-
#     dbConnect(RSQLite::SQLite(), db_filename)


#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;; API endpoints

#* Credentials Home
#* @get /
#* @serializer html
function(){
    #;; Read the people db
    people_df <-
        dbReadTable(con, "people")
    
    body_contents <-
        dbToTableReadOnly(people_df)
    
    glue(page_tpl)
}



#* Credentials Update
#* @get /xkcd
#* @serializer html
function(){
    #;; Read the people db
    people_df <-
        dbReadTable(con, "people")
    
    body_contents <-
        dbToTableUpdate(people_df)
    
    glue(page_tpl)
}



#* Update Floor Status
#* @get /changeStatus
#* @serializer html
function(id, status){
    #;; Update the person status
    switch(status,
           OUT = dbExecute(con, glue("update people set is_checkedIn = 0, is_upgraded = 0 where id = {id}")),
           IN = dbExecute(con, glue("update people set is_checkedIn = 1, is_upgraded = 0 where id = {id}")),
           PEND = dbExecute(con, glue("update people set is_checkedIn = 1, is_upgraded = 0 where id = {id}")),
           UP = dbExecute(con, glue("update people set is_checkedIn = 1, is_upgraded = 1 where id = {id}")))
    
    #;; Read the people db, show the updated record
    people_df <-
        dbReadTable(con, "people") %>%
        filter(id == .env$id)
    
    body_contents <-
        c(
            "<h1>Edited Person</h1>",
            dbToTableUpdate(people_df),
            '<h1><a href="/xkcd">Return to Main Page</a></h1>'
        ) %>%
        paste0(collapse="\n<br/>\n")
    
    glue(page_tpl)
}




#* Filter by Unit
#* @get /filterUnit
#* @serializer html
function(unit){
    
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
    
    
    #;; Some computatations
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
            if (nrow(peopleDel_df)) {dbToTableReadOnly(peopleDel_df)} else {""},
            glue("<h2>Alternates</h2>"),
            if (nrow(peopleAlt_df)) {dbToTableReadOnly(peopleAlt_df)} else {""},
            '<h1><a href="/xkcd">Return to Main Page</a></h1>'
        ) %>%
        paste0(collapse="\n<br/>\n")
    glue(page_tpl)
}



#* Filter by Unit, Updateable
#* @get /xkcd/filterUnit
#* @serializer html
function(unit){
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
}




#* Edit person
#* @get /xkcd/edit
#* @serializer html
function(id){
    #;; Read the people db, show the updated record
    people_df <-
        dbReadTable(con, "people") %>%
        filter(id == .env$id)
    
    body_contents <-
        c(
            glue_data(people_df, editForm_tpl)
        ) %>%
        paste0(collapse="\n<br/>\n")
    
    glue(page_tpl)
}

#* Edit person, submitted data
#* @get /xkcd/editSubmit
#* @serializer html
function(id, role, rank, unit, last_name, first_name, sex){
    id <- as.integer(id)
    rank <- ifelse(rank == "NA", "null", rank)
    
    dbExecute(con, glue("update people set role = '{role}', rank = {rank}, unit = '{unit}', last_name = '{last_name}', first_name = '{first_name}' where id = {id}"))

    people_df <-
        dbReadTable(con, "people") %>%
        filter(id == .env$id)
    body_contents <-
        c(
            dbToTableUpdate(people_df),
            '<h1><a href="/xkcd">Return to Main Page</a></h1>'
        ) %>%
        paste0(collapse="\n<br/>\n")
    
    glue(page_tpl)
}

