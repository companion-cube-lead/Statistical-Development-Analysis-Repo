library(httr)
require(httr)
library(jsonlite)
require(jsonlite)
library(stringr)
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(future.apply)
library(rlist)

# What will the function take as an argument?

# 1) A project or a vector of project keys
# 2) dates for the query
# 3) URL for the server


Create_JQL <- function(project_vector, query_date, server_url, endquerydate = 'now()',  indexnumber = 0, maxresults = 50) { #think about adding a password argument to this function ##########
  ## date type and cast as string ****** lubridate package (protect from users)
  ## consider if NULL, endquery date
  ## wrap inside get function (package creation)
  # Should users create JQL on their own?

  server_url <- server_url
  query_date <- query_date # message to pass it as a string
  query_date <- toString(query_date)
  endquerydate <- endquerydate
  indexstartparam <- "startAt="
  # pass this index number to the other function to the get call, input parameter that defaults to 0, parameter to change pages as well
  maxresults_query <- "maxResults=50"
  maxresults <<- maxresults
  maxresults <- paste("maxresults=" , maxresults, sep = "")

  serverurlendpoint <<- paste("https://",server_url,"/rest/api/2/search?jql=", sep = "")

  if (endquerydate == 'now()'){

    if (length(project_vector) > 1) {
      project_query <- capture.output( cat("project in", "(",(paste(project_vector, collapse = ',')), ") AND createdDate >", '"',query_date,'"',"AND createdDate <", endquerydate)) #incorrect query construction   # create final URL)

    } else {
      project_query <- capture.output( cat("project =", project_vector,"AND createdDate >",'"',query_date,'"',"AND createdDate <", endquerydate))
      #create ginal URL
    }

  } else {

    if (length(project_vector) > 1) {
      project_query <- capture.output( cat("project in", "(",(paste(project_vector, collapse = ',')), ") AND createdDate >", '"',query_date,'"',"AND createdDate <","'",endquerydate,"'")) #incorrect query construction   # create final URL)

    } else {
      project_query <- capture.output( cat("project =", project_vector,"AND createdDate >",'"',query_date,'"',"AND createdDate <", "'",endquerydate,"'"))
      #create ginal URL
    }
  }

  final_url <-paste(serverurlendpoint,project_query,"&", indexstartparam, indexnumber,"&", maxresults, sep = "")
  final_url <- URLencode(final_url)
  return(final_url)
}


Get_Tickets <- function(jql, username, pwd) {
  
  username <- username
  pwd <- pwd
  firstcall <- GET(jql, authenticate(username, pwd, type = "basic") )
  firstcall_jira <- content(firstcall, "text")

  switch(toString(firstcall$status_code),
         "200" = {
           message("Good status code, continuing..")
         },
         "400" = {
           stop("Bad request, query may be incorrect")
         },
         "401" = {
           stop("Unable to authorize")
         },
         "403" = {
           stop("Access denied for this query")
         }
  )

  firstcall_json <- fromJSON(firstcall_jira, flatten = TRUE)
  firstcall_df <- as.data.frame(firstcall_json)

  totalresultsavail <- max(firstcall_df$total) #


  #totalresultsavail = totalresultsavail + maxresults
  pages <- seq.int(0, totalresultsavail, maxresults)
  pages[-length(pages)]    #remove the last element of this vector to get the tickets from the index up

  final_url <- URLdecode(jql)
  larger_query <- str_split(final_url, "&", simplify = TRUE)
  results_parameter <- str_split( larger_query[2], "=", simplify = TRUE)
  new_start_param <- paste(results_parameter[,1], '=', sep = '' )

  list_tickets <- lapply(pages, function(x) { #make seperate function? to test
    

    JiraSearchCall <- paste(larger_query[,1], "&", new_start_param,x,'&', larger_query[,3], sep = "") # make sure the url works for different jql
    #compare against original question

    JiraSearchCall <- URLencode(JiraSearchCall) # this works

    GetJiraSearchCall <- GET(JiraSearchCall, authenticate(username, pwd, type = "basic"))

    print(GetJiraSearchCall$url) # Does seperate request here

    JiraSearchCallText <- content(GetJiraSearchCall, "text")
    #append new text to old text (from previous get call)
    JiraJson <- fromJSON(JiraSearchCallText, flatten = TRUE)
    
    
    JiraDF <- return(tryCatch( as.data.frame(JiraJson), error = function(e) NULL))
    JiraDF
    #dataframe_list[[x]] <- JiraDF # will fail here
    #dataframe_list <- c(dataframe_list, list(JiraDF))

    ##stuck here

    #return(dataframe_list)
  })


  final_tickets_df  <- rbindlist(list_tickets, use.names = TRUE, fill = TRUE)

  final_tickets_df


}


Create_Changelog <- function (changelog_lists) {

  #

  non_emptylists_condition <- sapply(changelog_lists, function(x) typeof(x)=="list")

  changelog_lists <- changelog_lists[non_emptylists_condition]


  changelog_datatable <- rbindlist(changelog_lists, fill = TRUE)

  # we cam implement the chekc here to see if the nested list has any observations




  KEYS <- changelog_datatable$issues.key

  changelog_datatable_keys <- mapply(cbind, changelog_datatable$issues.changelog.histories,"issues.key"=KEYS, SIMPLIFY = F)




  if (length(changelog_datatable_keys) >= 1) {

    rerbind_dataframe  <- rbindlist(changelog_datatable_keys, use.names = TRUE, fill = TRUE)

  }

  else {

    stop("These issue(s) don't have changelogs")
  }

  #changelogs are not created at inception, so this will not work when we access the elements of that column
  #make sure to examine changes at inception of transaction to make sure they match dev





  rerbind_dataframe$rowid <- seq.int(nrow(rerbind_dataframe))

  changelog_granular <- rbindlist(rerbind_dataframe$items, idcol = TRUE)

  setkey(rerbind_dataframe, rowid)

  # We need to deal with the lists within a list with a seperate function

  setkey(changelog_granular, .id)

  result <- merge.data.frame(changelog_granular, rerbind_dataframe, by.x = ".id", by.y = "rowid")

  result$items <- NULL

  result


}

Expand_Changelog <- function(ticket_vector, username, pwd, server) {
  ticket_vector <- ticket_vector

  base3 <- paste("https://",server,"/rest/api/latest/", sep = "")
  # API base URL

  endpoint3 <- "search/" #endpoint

  jqlquerychangelog <- "jql=issue="
  #query issue individually

  expand_property <- "&expand=changelog"

  # check status codes for a test CURL request
  test_call <- paste(base3, endpoint3, "?",jqlquerychangelog, ticket_vector[1], expand_property, sep = "")
  test_get <- GET(test_call, authenticate(username,pwd, type = "basic"))

  #

  print(test_get$status_code)

  switch(toString(test_get$status_code),
         "200" = {
           message("Good status code, continuing..")
           changelog_dataframe_list <- lapply(ticket_vector, function (x) { #lapply on th ticket vector to return a dataframe of tickets

             call4 <-  paste(base3, endpoint3, "?",jqlquerychangelog, x, expand_property, sep = "")
             get_changelog2 <- GET(call4, authenticate(username,pwd, type = "basic"))

             if (get_changelog2$status_code == 200) {

               call4 <-  paste(base3, endpoint3, "?",jqlquerychangelog, x, expand_property, sep = "")
               get_changelog2 <- GET(call4, authenticate(username,pwd, type = "basic"))
               print(get_changelog2)
               get_changelog_text2 <- content(get_changelog2, "text") #clean up variable names (no 2s)
               get_changelog_json2 <- fromJSON(get_changelog_text2, flatten = TRUE)
               changelog_dataframe2 <- as.data.frame(get_changelog_json2)
               changelog_dataframe2$issue.key = x # test this
               changelog_dataframe2$x #try this (value from lapply function)
               changelog_dataframe2





               #changelog_dataframe <- Create_Changelog(changelog_dataframe_list)

               #changelog_dataframe

             }


             else {

               message("This ticket does not exist: ", x)

               missing_tickets <- vector()

               missing_tickets <<- append(missing_tickets, x)
               ## check if it continues for the rest of the elements in the vector
             }




           })

           if (exists("changelog_dataframe_list")) {
             final_changelog <- Create_Changelog(changelog_dataframe_list)
             final_changelog

           }


         },

         "400" = {
           stop("Bad request, query may be incorrect")

         },

         "401" = {
           message("Unable to authorize")

         },

         "403" = {
           stop("Access denied for this query")
         },
         "404" = {
           stop("Not found... Tickets may be missing")
         }



  )

}


Get_Jira_Data <- function(jql, username, pwd, server, data){
  jql <- jql
  username <- username
  pwd <- pwd
  server <- server
  data <- data



  if (data == 1){
    #We get the orginal transaction of jql tickets
    tickets_dataframe <- Get_Tickets(jql, username, pwd)
    tickets_dataframe

  }

  else {



    tickets_dataframe <- Get_Tickets(jql, username, pwd)

    ticket_vector  <- tickets_dataframe$issues.key
    ## make them seperate functions
    changelog_global <- Expand_Changelog(ticket_vector, username, pwd, server)
    ##  no intermediarary for issues vector, (both)
    ## ticket vector can be set to null
    # can accept projects (project query version)


    changelog_global

  }

}


Get_Ticket_Data <- function(username, pwd, projects, server, begin_date, end_date="now()", jql){
  
  
  # look into lubridate::format for dates

  if(is.character(begin_date)){

    if (!is.na(parse_date_time(c(begin_date),orders="ymd"))) {
      begin_date <- gsub("-", "/", begin_date)
    }

    else {
      begin_date <- parse_date_time(begin_date, 'mdy' )
      as.character(begin_date)
      begin_date <- gsub("-", "/", begin_date)

    }
  }

  else {
    stop("Please enter the date as a string")
  }



  if(missing(jql)){
    jqlurl <- Create_JQL(projects, begin_date, server, end_date) ## should we adjust the min pages?

    tickets_dataframe <- Get_Tickets(jqlurl, username, pwd)

    tickets_dataframe


    #deal with vector

  } else {
    #deal with projects

    tickets_dataframe <- Get_Tickets(jql, username, pwd)
    tickets_dataframe
  }
}


Get_Ticket_Data_Flat <- function(username, pwd, projects, server, begin_date, complex_cols=c(
                                  "issues.fields.issuelinks",
                                  "issues.fields.subtasks",
                                  "issues.fields.components",
                                  "issues.fields.fixVersions"),
                                 end_date="now()", jql){
  
  tickets_dataframe <- Get_Ticket_Data(username, pwd, projects, server, begin_date, end_date, jql)
  
  tickets_dataframe <- Dump_Empty_Cols(tickets_dataframe)
  Flatten_Cols(tickets_dataframe, complex_cols)
}


Get_Changelog_Data <- function(username, pwd, projects, server, begin_date, end_date="now()", ticket_vector){

  # arguments <- list(...)
  # paste(arguments)
  username <- username
  pwd <- pwd
  projects <-  projects
  server <- server
  begin_date <- begin_date
  end_date <- end_date


  #begin_date <- gsub("/", "-", begin_date)



  if(is.character(begin_date)){

  if (!is.na(parse_date_time(c(begin_date),orders="ymd"))) {
    begin_date <- gsub("-", "/", begin_date)
  }

  else {
    begin_date <- parse_date_time(begin_date, 'mdy' )
    as.character(begin_date)
    begin_date <- gsub("-", "/", begin_date)

    }
  }

  else {
    stop("Please enter the date as a string")
  }



  if(missing(ticket_vector)){

    jqlurl <- Create_JQL(projects, begin_date, server, end_date) ## should we adjust the min pages?

    tickets_dataframe <- Get_Tickets(jqlurl, username, pwd)

    ticket_vector  <- tickets_dataframe$issues.key
    ## make them seperate functions
    changelog_global <- Expand_Changelog(ticket_vector, username, pwd, server)
    ##  no intermediarary for issues vector, (both)
    ## ticket vector can be set to null
    # can accept projects (project query version)


    changelog_global

    #deal with vector

  } else {
    #deal with projects

    changelog <- Expand_Changelog(ticket_vector, username, pwd, server)

    changelog
  }


}


Get_Changelog_Data_Parallel <- function(username, pwd, projects, server, begin_date, end_date="now()", ticket_vector){
  username <- username
  projects <- projects
  server <- server
  begin_date <- begin_date
  end_date <- end_date

  if(is.character(begin_date)){

    if (!is.na(parse_date_time(c(begin_date),orders="ymd"))) {
      begin_date <- gsub("-", "/", begin_date)
    }

    else {
      begin_date <- parse_date_time(begin_date, 'mdy' )
      as.character(begin_date)
      begin_date <- gsub("-", "/", begin_date)

    }
  }

  else {
    stop("Please enter the date as a string")
  }

  cores <- availableCores()

  cores_array <- seq.int(1, cores, 1)

  cores_array <- rev(cores_array)

  # add date stuff

  # begin with the highest number of cores first, until the highest possible core works


  for ( workers in cores_array ) { #consider lapply here?
  message("The function will try the pull changelog-request with the most amount of cores ")

  message(paste("Trying cores:",  workers))

  plan(multiprocess(workers = as.integer(workers)))

  throw_error<- tryCatch(

  {

  if(missing(ticket_vector)){

    jqlurl <- Create_JQL(projects, begin_date, server, end_date) ## should we adjust the min pages?

    tickets_dataframe <- Get_Tickets_Parallel(jqlurl, username, pwd)

    ticket_vector  <- tickets_dataframe$issues.key


    ## make them seperate functions
    changelog <- Expand_Changelog_Parallel(ticket_vector, username, pwd, server)
    ##  no intermediarary for issues vector, (both)
    ## ticket vector can be set to null
    # can accept projects (project query version)


    changelog

    #deal with vector

  } else {
    #deal with projects

    changelog <- Expand_Changelog_Parallel(ticket_vector, username, pwd, server)

    changelog

    }
  }, error = function(e) e
  )

  if(!inherits(throw_error, "error")) {
    message("Parallel API Call Executed Successfully")
    break

  }
  else {
    message(paste("Failed with "), workers )
    next
    message(paste("Trying with the next optimal amount of workers: "), workers)
      }

    }


}


Get_Ticket_Data_Parallel <- function(username, pwd, projects, server, begin_date, end_date="now()", ticket_vector){
  username <- username
  projects <- projects
  server <- server
  begin_date <- begin_date
  end_date <- end_date

  if(is.character(begin_date)){

    if (!is.na(parse_date_time(c(begin_date),orders="ymd"))) {
      begin_date <- gsub("-", "/", begin_date)
    }

    else {
      begin_date <- parse_date_time(begin_date, 'mdy' )
      as.character(begin_date)
      begin_date <- gsub("-", "/", begin_date)

    }
  }

  else {
    stop("Please enter the date as a string")
  }

  cores <- availableCores()

  cores_array <- seq.int(1, cores, 1)

  cores_array <- rev(cores_array)

  # add date stuff

  # begin with the highest number of cores first, until the highest possible core works


  for ( workers in cores_array ) { #consider lapply here?
    message("The function will try the pull request with the most amount of cores ")

    message(paste("Trying cores:",  workers))

    plan(multiprocess(workers = as.integer(workers)))

    throw_error<- tryCatch(

      {

        if(missing(ticket_vector)){

          jqlurl <- Create_JQL(projects, begin_date, server, end_date) ## should we adjust the min pages?

          tickets_dataframe <- Get_Tickets_Parallel(jqlurl, username, pwd)

          tickets_dataframe

          #deal with vector

        } else {
          #deal with projects

          tickets_dataframe <- Get_Tickets_Parallel(jqlurl, username, pwd)

          tickets_dataframe



        }
      }, error = function(e) e
    )

    if(!inherits(throw_error, "error")) {
      message("Parallel API Call Executed Successfully")
      break

    }
    else {
      message(paste("Failed with "), workers )
      next
      message(paste("Trying with the next optimal amount of workers: "), workers)
    }

  }


}


Get_Tickets_Parallel <- function(jql, username, pwd) {

    #plan(multiprocess)  # specifiy another function for different kinds of parallization


    username <- username
    pwd <- pwd
    firstcall <- GET(jql, authenticate(username, pwd, type = "basic") )
    firstcall_jira <- content(firstcall, "text")

    switch(toString(firstcall$status_code),
           "200" = {
             message("Good status code, continuing..")
           },
           "400" = {
             stop("Bad request, query may be incorrect")
           },
           "401" = {
             stop("Unable to authorize")
           },
           "403" = {
             stop("Access denied for this query")
           }
    )

    firstcall_json <- fromJSON(firstcall_jira, flatten = TRUE)
    firstcall_df <- as.data.frame(firstcall_json)

    dataframe_list <- list() #do not need with lapply


    totalresultsavail <- max(firstcall_df$total) #



    #URLdecode()
    #totalresultsavail = totalresultsavail + maxresults
    pages <- seq.int(0, totalresultsavail, maxresults)
    pages[-length(pages)]    #remove the last element of this vector to get the tickets from the index up

    final_url <- URLdecode(jql)
    larger_query <- str_split(final_url, "&", simplify = TRUE)
    results_parameter <- str_split( larger_query[2], "=", simplify = TRUE)
    new_start_param <- paste(results_parameter[,1], '=', sep = '' )

    list_tickets <- future_lapply(pages, function(x) { #make seperate function? to test

      JiraSearchCall <- paste(larger_query[,1], "&", new_start_param,x,'&', larger_query[,3], sep = "") # make sure the url works for different jql
      #compare against original question

      JiraSearchCall <- URLencode(JiraSearchCall) # this works

    GetJiraSearchCall <- GET(JiraSearchCall, authenticate(username, pwd, type = "basic"))

    print(GetJiraSearchCall$url) # Does seperate request here

    JiraSearchCallText <- content(GetJiraSearchCall, "text")
    #append new text to old text (from previous get call)
    JiraJson <- fromJSON(JiraSearchCallText, flatten = TRUE)
    JiraDF <- as.data.frame(JiraJson)
    JiraDF
    #dataframe_list[[x]] <- JiraDF # will fail here
    #dataframe_list <- c(dataframe_list, list(JiraDF))

    ##stuck here

    #return(dataframe_list)
  })


  final_tickets_df  <- rbindlist(list_tickets, use.names = TRUE, fill = TRUE)

  final_tickets_df


}


Expand_Changelog_Parallel <- function(ticket_vector, username, pwd, server) {


  ticket_vector <- ticket_vector

  base3 <- paste("https://",server,"/rest/api/latest/", sep = "")

  endpoint3 <- "search/" #missing slash for some reason?

  jqlquerychangelog <- "jql=issue="

  expand_property <- "&expand=changelog"


  test_call <- paste(base3, endpoint3, "?",jqlquerychangelog, ticket_vector[1], expand_property, sep = "")
  test_get <- GET(test_call, authenticate(username,pwd, type = "basic"))

  #

  print(test_get$status_code)

  switch(toString(test_get$status_code),
         "200" = {
           message("Good status code, continuing..")
           changelog_dataframe_list <- future_lapply(ticket_vector, function (x) {

             call4 <-  paste(base3, endpoint3, "?",jqlquerychangelog, x, expand_property, sep = "")
             get_changelog2 <- GET(call4, authenticate(username,pwd, type = "basic"))

             if (get_changelog2$status_code == 200) {

               call4 <-  paste(base3, endpoint3, "?",jqlquerychangelog, x, expand_property, sep = "")
               get_changelog2 <- GET(call4, authenticate(username,pwd, type = "basic"))
               print(get_changelog2)
               get_changelog_text2 <- content(get_changelog2, "text") #clean up variable names (no 2s)
               get_changelog_json2 <- fromJSON(get_changelog_text2, flatten = TRUE)
               changelog_dataframe2 <- as.data.frame(get_changelog_json2)
               changelog_dataframe2$issue.key = x # test this
               changelog_dataframe2$x #try this (value from lapply function)
               changelog_dataframe2

               #changelog_dataframe_list
               ## check logic her




               #changelog_dataframe <- Create_Changelog(changelog_dataframe_list)

               #changelog_dataframe

             }




             else {

               message("This ticket does not exist: ", x)

               missing_tickets <- vector()

               missing_tickets <<- append(missing_tickets, x)
               ## check if it continues for the rest of the elements in the vector
             }




           })

           if (exists("changelog_dataframe_list")) {
             final_changelog <- Create_Changelog(changelog_dataframe_list)
             final_changelog

           }


         },
         # logic for missing tickets
         "400" = {
           message("Bad request, query may be incorrect")

         },

         "401" = {
           message("Unable to authorize")

         },

         "403" = {
           message("Access denied for this query")
         },
         "404" = {
           message("Not found... Tickets may be missing")
         }



  )

  #



}

# where is the slowness coming from, the query for everything
#system.time
# We may need a seperate job to pull everything

# Only make sure changelog ids are being matched


# This takes a vector of projects as an arguments
# Other arguments must be strings


#dbplyr translates sql into df package


# for getting the indiviudal isse ticket


## system.time for issues pulled (as a function) How long does it take?
## build plots
## will it respond to 10 at a time
#base_dataframe <- Get_Changelog_Data('andersv','@CCESScontrolbroom85', c('DI'), 'jira.usf.edu', '1/12/20' )
# do we need a datafram e that already exists here?

# the API call is displayed on the console, that's a little messy
# we have to make sure we can create a secure way to get password data
# deal with oauth tokens..
# We need to make logic to deal wth an empty dataframe if it pops up


### Automation Functions Below ###

Write_Changelog_Latest <- function(username, pwd, projects, server, days_to_subtract) {
  projects <- projects
  pwd <- pwd
  username <- username
  days_to_subtract <- days_to_subtract

  # this may become the parent function

  if(file.exists("updated_dataframe.csv")==FALSE) {

    # there needs to be a rule in place where the name of this file cannot be modified



    #present_date <- gsub("-", "/", present_date)

    base_dataframe <- Get_Changelog_Latest(username, pwd, projects, server, days_to_subtract) # replace with  latest function..

    fwrite(base_dataframe, "updated_dataframe.csv")



  }

  else {

    latest_changelog_pulled <- Get_Changelog_Latest(username, pwd,  projects , server , days_to_subtract) # we may need to add an asyn option

    #what's more efficient?
    # replace the above function with the new date function


    fwrite(latest_changelog_pulled, "updated_dataframe.csv", sep = ",",append = T )



    Update_CSV(upward_value, "updated_dataframe.csv") # this works as expected





  }

}


Get_Changelog_Latest <- function(username, pwd, projects, server, subtract_parameter) {
  ## some notes:
  ## it might be possible to directly  query the API from the upward value from our latest CSV


  username <- username
  pwd <- pwd
  projects <- projects
  server <- server

  # will the user have to punch in their login information in everytime?
  begin_date_inrange <- Sys.Date()-subtract_parameter


  #subtracts in "days from now"

  # The subtract parameter represents the amount of days the user can try to see if there are any changelogs available from
  # the current day

  try_dates <- as.character(rev(seq(ymd(begin_date_inrange), ymd(Sys.Date()), by ='1 days')))

  for (date in try_dates){

    throw_error <- tryCatch(

      latest_possible_changelog <- Get_Changelog_Data(username, pwd,  projects , server , date )


      #run code which might fail
      , error = function(e) e
    )


    #add if statement for exception toss
    if(!inherits(throw_error, "error")) {
      message(paste("There is data available for from this date: "), date) #show a message with the relevant date
      break

    }
    else {
      message(paste("No data for the range in dates from "), date, " to ", Sys.Date())
      next
      message(paste("Trying with the next date: "), date)
    }








  }

  #numeric_id_key <- gsub("[^0-9.-]", "", latest_possible_changelog$issues.key)     #clean string


  upward_value <<- as.numeric((max(latest_possible_changelog$id)))


  #upward_value <<- max(substr(gsub("[^0-9.-]", "", latest_possible_changelog$issues.key),2, 8))#clean string

  latest_possible_changelog





}



Update_Observations <- function(username, pwd, projects, server, subtract_parameter) {
  # read the old csv 
  oldobservations <- fread("updated_dataframe.csv")
  
  newobservations <- as.data.table( Get_Changelog_Latest(username, pwd, projects, server, subtract_parameter))
  # now we need to combine the new observations together 
  
  mutual_cols <- intersect(colnames(oldobservations), colnames(newobservations))
  
  oldobservations <- oldobservations[, ..mutual_cols]
  
  newobservations <- newobservations[, ..mutual_cols]
  
  newobservations[, created:=ymd_hms(created)]
  
  oldobservations[, created:=ymd_hms(created)]
  
  
  
  overlapping_observations <- rbind(oldobservations, newobservations)
  
  overlapping_observations[, .id:=NULL ]
  
  true_observations <-unique(overlapping_observations, by = "id")
  
  write.csv(true_observations, "updated_dataframe.csv", append = FALSE)
  
}


Update_Observations_Tickets <- function(username, pwd, projects, server, subtract_parameter=7) {
  previous_tickets <- fread("updated_ticket_dataframe.csv")
  
  new_tickets <- as.data.table( Get_Ticket_Data(username, pwd, projects, server, begin_date = as.character( Sys.Date()-subtract_parameter)))
  
  previous_tickets[, issues.fields.created := ymd_hms(issues.fields.created)]
  
  new_tickets[, issues.fields.created := ymd_hms(issues.fields.created)]
  
  mutual_cols <- intersect(colnames(previous_tickets), colnames(new_tickets))
  
  oldobservations <- previous_tickets[, ..mutual_cols]
  
  newobservations <- new_tickets[, ..mutual_cols]
  
  overlapping_observations <- rbind(oldobservations, newobservations)
  
  true_observations <- unique(overlapping_observations, by = "issues.key")
  
  write.csv(true_observations, "updated_ticket_dataframe.csv", append = F)
  
  
  
}


#####seperate functions for different dataframe types
Write_Tickets_Latest <- function(username, pwd, projects, server, days_to_subtract) {
  projects <- projects
  pwd <- pwd
  username <- username
  days_to_subtract <- days_to_subtract

  # this may become the parent function

  if(file.exists("updated_ticket_dataframe.csv")==FALSE) {

    # there needs to be a rule in place where the name of this file cannot be modified



    #present_date <- gsub("-", "/", present_date)

    base_dataframe <- Get_Tickets_Latest(username, pwd, projects, server, days_to_subtract) # replace with  latest function..

    write.csv(base_dataframe, "updated_ticket_dataframe.csv")



  }

  else {

    base_dataframe <- read.csv(file = "updated_ticket_dataframe.csv")

    latest_changelog_pulled <- Get_Tickets_Latest(username, pwd,  projects , server , days_to_subtract) # we may need to add an asyn option

    #what's more efficient?
    # replace the above function with the new date function


    write.table(latest_changelog_pulled, "updated_ticket_dataframe.csv", sep = ",", col.names = !file.exists("updated_dataframe.csv"), append = T )



    Update_CSV(upward_value, "updated_ticket_dataframe.csv") #we should be able to keep this generic function (rename)


  }

}


Write_Tickets_Latest_Flat <- function(username, pwd, projects, server, days_to_subtract, complex_cols=c(
    "issues.fields.issuelinks",
    "issues.fields.subtasks",
    "issues.fields.components",
    "issues.fields.fixVersions")) {
  
   
  
  complex_cols <- complex_cols
  projects <- projects
  pwd <- pwd
  username <- username
  days_to_subtract <- days_to_subtract

  # Get list of flat tables, and their final output names
  base_dataframes <- Get_Tickets_Latest_Flat(username, pwd, projects, server, days_to_subtract, complex_cols)
  flat_names <- substring(complex_cols, 15,30)
  table_names <- c(flat_names, 'sprints')

  # master table
  banned_cols <- c(complex_cols,
                "issues.fields.customfield_10507",
                "issues.fields.customfield_10508",
                "issues.fields.customfield_10509",
                "issues.fields.customfield_10005",
                "issues.fields.labels")
  master_safe <- base_dataframes[[1]][, (banned_cols):=NULL]
  
  # write.csv(master_safe,"ticket_dataframe_flat.csv")
  # # Write rest of tables
  # i <- 1
  # for (table in base_dataframes) {
  #   file_name = paste('updated_ticket',table_names[i],'dataframe.csv', sep="_")
  # 
  #   if(i==i) {
  #     table <- master_safe
  #     write.csv(table, file_name)
  #   }
    
  
    
    sapply(base_dataframes, function(Y){
      counter = 1 
      write.csv(Y, paste0('updated_ticket',counter,"dataframe.csv"))  
      counter = counter +1
    })
    
}
    
    #since we need to define new update logic for the auxillary tables, we will just write over them in the DIR for now 

    # if(file.exists(file_name)==FALSE) {
    #   write.csv(table, file_name)
    # }
    # 
    # else {
    #   base_table <- read.csv(file = file_name)
    #   write.table(table, file_name, sep = ",", col.names = !file.exists("updated_dataframe.csv"), append = T )
    #   Update_CSV(upward_value, file_name)
    # }

  


Get_Tickets_Latest <- function(username, pwd, projects, server, subtract_parameter) {
  username <- username
  pwd <- pwd
  projects <- projects
  server <- server

  # will the user have to punch in their login information in everytime?
  begin_date_inrange <- Sys.Date()-subtract_parameter


  #subtracts in "days from now"

  # The subtract parameter represents the amount of days the user can try to see if there are any changelogs available from
  # the current day

  try_dates <- as.character(  seq(ymd(begin_date_inrange), ymd(Sys.Date()), by ='1 days' ))

  for (date in try_dates){

    throw_error <- tryCatch(

      latest_possible_tickets <- Get_Ticket_Data(username, pwd,  projects , server , date )


      #run code which might fail
      , error = function(e) e
    )


    #add if statement for exception toss
    if(!inherits(throw_error, "error")) {
      message(paste("There is data available from this date: "), date) #show a message with the relevant date
      break

    }
    else {
      message(paste("No data for the range in dates from "), date, " to ", Sys.Date())
      next
      message(paste("Trying with the next date: "), date)
    }








  }

  numeric_id_key <- gsub("[^0-9.-]", "", latest_possible_tickets$issues.key)     #clean string

  upward_value <<- max(substr(gsub("[^0-9.-]", "", latest_possible_tickets$issues.key),2, 8))#clean string

  latest_possible_tickets
}


Get_Tickets_Latest_Flat <- function(username, pwd, projects, server, subtract_parameter, complex_cols=c(
    "issues.fields.issuelinks",
    "issues.fields.subtasks",
    "issues.fields.components",
    "issues.fields.fixVersions")) {
  username <- username
  pwd <- pwd
  projects <- projects
  server <- server
  complex_cols <- complex_cols
  
  
  
  latest_possible_tickets <- Get_Tickets_Latest(username, pwd, projects, server, subtract_parameter)

  latest_possible_tickets <- Dump_Empty_Cols(latest_possible_tickets)
  Flatten_Cols(latest_possible_tickets, complex_cols)

}





# we can create a function which queries upward from a specific ticket id,
# we can also just use the same strategy we use to update the changelog to do that as well

# this function will handle removing overlpaing obsevrations which may appear in consectuive API Calls

# If the values change, we may need to build logic for that as well


#### Auxiliary Functions

Dump_Empty_Cols  <-function(dt) {
  clean_dataframe <- dt[,which(unlist(lapply(dt, function(x)!all(is.na(x))))),with=F]
  clean_dataframe <- clean_dataframe[,which(unlist(lapply(clean_dataframe, function(x)!all(length(unique(x))<=1)))),with=F]
  clean_dataframe
}


Flatten_Cols <- function(dt, col_list) {
  
  
  
  dt <- as.data.table( dt)
  col_list <- col_list
  

  
  
  unnester <- function(X) {
    
    names <- c("issues.key", X)
    
    nested <- dt[, ..names]  
    
    for (position in seq(nrow(nested[, 2]))) {
      
    tryInsert <- tryCatch(  

        
     nested[, 2][[1]][[position]]$issues.key <- as.character( nested[position, 1 ]),
     
     error=function(e) e
     
    )
    
    if(inherits(tryInsert, "error")) next
     
    }
    
    list2 <- rlist::list.clean(nested, function(x) length(x) == 0L, TRUE )
    
    list2 <- list2[[X]] 
    
    flat <- rbindlist(list2, use.names = TRUE, fill = TRUE)
    
    flat
    

  }
  
  
  flat_list <- lapply(col_list, unnester)
  
  
  

  # Add sprint column
  flat_list[[length(col_list) + 2]] <- Get_Sprint_Data(dt)

  # Join list of individual data.tables (can't be used until conflict of repeated column names is solved)
  # flat_complex_cols <- rbindlist(flat_list, use.names = TRUE, fill = TRUE)
  # master_simple <- dt[, c(col_list):=NULL]
  # master_simple[flat_complex_cols, on = "issues.key"]
  flat_list
}


Handle_Difftime <- function(time_first, time_last){

  round(difftime(time_last, time_first, units = "hours"), digits = 2)

}


Get_Sprint_Data <- function(dt) {
  
  

  copy <- dt


  sprint <- copy[, c('issues.key', 'issues.fields.customfield_10005')]
  sprint[, sprint.vals := str_extract(issues.fields.customfield_10005, regex('(?<=\\[).+?(?=\\])' ))]
  sprint <- sprint[, c('issues.key', 'sprint.vals')]
  spr <- sprint[!is.na(sprint.vals)]


  spr[, sprint.split := str_split(sprint.vals, ",")]


  vals_spr <- lapply(spr[, sprint.split], function(x) str_extract(x, regex('(?<=\\=).*$')))
  spr[, sprint.vals.only := vals_spr]

  cols_spr <- str_extract(spr[[3]][[1]], regex('.+?(?=\\=)'))
  cols_spr


  spr_to_unnest <- spr[, c('issues.key', 'sprint.vals.only')]
  spr_to_unnest <- spr_to_unnest[!is.na(sprint.vals.only)]
  spr_flat <- spr_to_unnest[, unlist(sprint.vals.only), by = issues.key]
  spr_flat <- na.omit(spr_flat)

  spr_final <- as.data.table(cbind(spr_flat, cols_spr))
  colnames(spr_final) <- c('issues.key', 'actualval', 'attrtype')

  test <- spr_final

  trs <- test %>%
    pivot_wider(names_from = attrtype, values_from = actualval)


  neat_sprint <- data.table(trs)

  # Format dates
  neat_sprint[, startDate.dt := ymd_hms(startDate)]
  neat_sprint[, endDate.dt := ymd_hms(endDate)]
  neat_sprint[, completeDate.dt := ymd_hms(completeDate)]

  # Calculate time differences
  neat_sprint[, projectedCompletion := Handle_Difftime(startDate.dt, endDate.dt)]
  neat_sprint[, actualCompletion := Handle_Difftime(startDate.dt, completeDate.dt)]


  neat_sprint
}

