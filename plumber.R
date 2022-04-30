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
        c("<h1>Main Report Page</h1>",
          glue_data(getStats(people_df), tableSummary_tpl),
            dbToTableReadOnly(people_df)
        ) %>%
        paste0(collapse="\n<br/>\n")
    
    glue(page_tpl)
}



#* Credentials Update
#* @get /xkcd
#* @serializer html
function(){
    #;; Read the people db
    people_df <-
        dbReadTable(con, "people")
    
    # body_contents <-
    #     dbToTableUpdate(people_df)
    body_contents <-
        c("<h1>Main Checkin Page</h1>",
          glue_data(getStats(people_df), tableSummary_tpl),
          dbToTableUpdate(people_df)
        ) %>%
        paste0(collapse="\n<br/>\n")
    
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

    body_contents <-
        c(
            glue("<h1>Unit: {unit}</h1>"),
            glue_data(getStats(people_df), tableSummary_tpl),
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
    
    body_contents <-
        c(
            glue("<h1>Unit: {unit}</h1>"),
            glue_data(getStats(people_df), tableSummary_tpl),
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
function(id, role, rank, unit, last_name, first_name, sex, subcaucus){
    id <- as.integer(id)
    rank <- ifelse(rank == "NA", "null", rank)
    if (!exists("subcaucus") || is.null(subcaucus) || subcaucus == "NA") {
        subcaucus <- ""
    }
    
    dbExecute(con, glue("update people set
                        role = '{role}'
                        , rank = {rank}
                        , unit = '{unit}'
                        , last_name = '{last_name}'
                        , first_name = '{first_name}'
                        , subcaucus = '{subcaucus}'
                        where id = {id}"))
    
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


#* Add person
#* @get /xkcd/add
#* @serializer html
function(){
    body_contents <-
        c(
            addForm_tpl
        ) %>%
        paste0(collapse="\n<br/>\n")
    
    glue(page_tpl)
}



#* Add person, submitted data
#* @get /xkcd/addSubmit
#* @serializer html
function(role, rank=NULL, unit, last_name, first_name, sex, subcaucus){
    if (!exists("rank") || is.null(rank) || rank == "NA") {
        rank <- "null"
    }
    if (!exists("sex") || is.null(sex) || sex == "NA") {
        sex <- "other"
    }
    if (!exists("subcaucus") || is.null(subcaucus) || subcaucus == "NA") {
        subcaucus <- ""
    }
    # glue("role: {role}")
    # glue("rank: {rank}")

    maxid <-
        dbReadTable(con, "people") %>%
        pull("id") %>%
        max()
    newid <- maxid + 1
    
    dbExecute(con, glue("insert into people
                        (id
                        , role
                        , rank
                        , unit
                        , last_name
                        , first_name
                        , sex
                        , subcaucus)
                        values
                        ({newid}
                        , '{role}'
                        , {rank}
                        , '{unit}'
                        , '{last_name}'
                        , '{first_name}'
                        , '{sex}'
                        , '{subcaucus}')"))

        people_df <-
            dbReadTable(con, "people") %>%
            filter(id == newid)
        body_contents <-
            c(
                dbToTableUpdate(people_df),
                '<h1><a href="/xkcd">Return to Main Page</a></h1>'
            ) %>%
            paste0(collapse="\n<br/>\n")

        glue(page_tpl)
}
