library(data.table)
library(lubridate)


# this function is to join data from tickets to the changelog
Tidy_Changelog <- function(changelog, tickets){
  
  # grab columns we need from ticket data frame 
  
  setDT(changelog)[tickets, `:=` (storypoints = i.issues.fields.customfield_10002, # storypoints field in jira
                                  
                                  resolution_date = i.issues.fields.resolutiondate, 
                                  
                                  current_issue_type = i.issues.fields.issuetype.name, 
                                  
                                  ticket_creation_date = i.issues.fields.created), on = c(issues.key = "issues.key")] # join on issue key
  
  
  # grabbing these fields
  filter1 <- changelog[field %in% c("created","status", "Story Points", "issuetype", "Target end", "Comment", "timeestimate","timespent","timeestimate","resolution", "assignee", "Sprint")]
  
  # don't need these
  filter1[, c("author.avatarUrls.48x48","author.self","author.avatarUrls.24x24","author.avatarUrls.16x16","author.avatarUrls.32x32") := NULL] 
  #don't need these columns 
  
  # format time correctly
  filter1[, `:=`(created_UTC = ymd_hms(created), ticket_creation_date = ymd_hms(ticket_creation_date))]
  #format dates 
  
  
  # extract these features using lubridate
  filter1[, `:=`(created_day = as.numeric( day(created)), 
                 created_month = month(created), 
                 created_week=week(created), 
                 resolution_day=day(ymd_hms(resolution_date)), 
                 resolution_week=week(ymd_hms(resolution_date)), 
                 created_year=year(created),
                 day_of_year = yday(created),
                 created_day_short = as.Date(created),
                 week_day = weekdays(created_UTC))
          ]
  
  # transform levels to days here
  
  filter1$week_day <- factor(filter1$week_day, levels= c("Sunday", "Monday", 
                                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  
  #extract date features 
  
  
  
  #df_appian_new[, issues.fields.customfield_10005] this contains the sprint identifier, let's join on this  
  
  filter1
  
  
  
  
}






Model_Ticket_LifeCycle <- function(changelog, tickets_df, components_table, fixversion_table, sprint_table){
  #this function models ticket transitions linearly 
  
  tidier_changelog <-  as.data.table( Tidy_Changelog(changelog, tickets_df)) #tidy up the dataframe for us 
  
  setorderv(tidier_changelog,'issues.key', order = -1)  #order the issue keys sequentially
  
  # these are the changes we're interested in
  fromString <- c('Open','In Progress','Peer Review', 'QA Review', 'PO Review', 'Closed')
  
  expected_flow <- c(1,2,3,4,5,6) #these changes need to be represented in the order needed to understand them sequentially
  
  complete_cycle_dataframe <- data.frame(expected_flow, fromString)
  
  setDT(tidier_changelog)[complete_cycle_dataframe, fromStringFlowint:=i.expected_flow, on = c(fromString = "fromString") ] 
  
  # we represent the expected flow in referance dataframe and join the ints unto our changelog 
  
  setDT(tidier_changelog)[complete_cycle_dataframe, toStringFlowint:=i.expected_flow, on = c(toString = "fromString") ]
  
  Expected_Flow <- tidier_changelog[as.numeric(fromStringFlowint)<as.numeric(toStringFlowint)][field=="status"]
  
  #now we can query the timedifference in accordance to our flow 
  
  
  #this logic gives us the difference between all changes in hours
  Expected_Flow[, diff_hours:=as.numeric(difftime(created_UTC, shift(created_UTC), units = "hours")), by = issues.key]
  
  
  #now we perform the datediff on tickets we know follow a sequential flow
  
  Expected_Flow[as.character(from) == "10034", from:="2.1"][as.character(from) == "10255", from:="2.2"][as.character(from) == "10254", from:="2.3"]
  
  Expected_Flow[as.character(to) == "10034", to:="2.1"][as.character(to) == "10255", to:="2.2"][as.character(to) == "10254", to:="2.3"]
  
  Expected_Flow[, swimlane_distance:=abs(as.numeric(as.character(from))-as.numeric(as.character(to)))]
  
  
  Expected_Flow[, scaled_diffhours:= scale(diff_hours)]
  
  #Expected_Flow[, outlier := ifelse(scaled_diffhours > 3, "YES", "NO")] 
 
  
  
  # join on components using issue key
  components[, issues.key:=as.character(issues.key)]
  
  
  setDT(Expected_Flow)[components, `:=` (component = i.name), on = c(issues.key = "issues.key")]
  
  
  # join on fix versions using issue key
  fixVersions[, issues.key:=as.character(issues.key)]
  
  # try to join in the data is available
  try_join <-  tryCatch({
    
    setDT(Expected_Flow)[fixVersions, `:=` (fix_version = i.name,
                                            release_date = i.releaseDate,
                                            bool_fix_released = i.released
    ), on = c(issues.key = "issues.key")]
    
  }, 
  
  error = function(e) {
    message("A join failed, probably because data hasn't been pushed yet")
  }, 
  finally = {
  }
  
  )
  
  
  # join on sprints on issue key
  sprint_table[, issues.key:=as.character(issues.key)]
  
  setDT(Expected_Flow)[sprint_table, `:=` (sprint_begin = i.startDate, #columns that we need
                                           sprint_end = i.endDate,
                                           sprint_name = i.name,
                                           sprint_goal = i.goal,
                                           sprint_db_sequence = i.sequence, 
                                           projected_sprint_completion = i.projectedCompletion,
                                           actual_sprint_completion_time = i.actualCompletion
  ), on = c(issues.key = "issues.key")]
  
  
  # setDT(Expected_Flow)[issuelinks_table, `:=` (outwardIssue_key = i.outwardIssue.key,
  #                                              outwardIssue_link_type = i.type.outward,
  #                                              outwardIssue_type = i.outwardIssue.fields.issuetype.name,
  #                                              outwardIssuePriority = i.outwardIssue.fields.priority.name
  #                                              
  # ), on = c(issues.key = "issues.key")]
  
  
  Expected_Flow
  
  
  ##### Print some summary metrics below #####
  
  
  print(Expected_Flow[, .(mean_by_lane=(mean(diff_hours, na.rm = TRUE))), by = fromString])
  
  print(Expected_Flow[, .(Observations_Storypoints=(.N)), by = storypoints][order(storypoints)])
  
  print(Expected_Flow[, .(Spread_Diff_Hoours=sd(diff_hours, na.rm = TRUE)), by=storypoints][order(storypoints)])
  print(Expected_Flow[, .(Mean_Diff_Hours=(mean(diff_hours, na.rm = TRUE))), by=storypoints][order(storypoints) ])
  

  
  Expected_Flow
  
  
}



Model_Ticket_Exception_LifeCycle <- function(original_changelog, tickets_df, regularflow_table){
  
  fromString <- c('Open','In Progress','Peer Review', 'QA Review', 'PO Review', 'Closed')
  
  expected_flow <- c(1,2,3,4,5,6)
  
  complete_cycle_dataframe <- data.frame(expected_flow, fromString)
  
  
  
  
  #Which flows are risky? 
  
  # fromString    toString     risk
  # 1:      Closed In Progress 7  
  # 2:      Closed        Open 9
  # 3: Peer Review In Progress 1
  # 4:      Closed Peer Review 8
  # 5: Peer Review        Open 6
  # 6:   PO Review In Progress 4
  # 7:   QA Review In Progress 3
  # 8: In Progress        Open 5 ( "we decided not to work on this right now")
  # 9:   QA Review Peer Review 2 ("QA Review should catch this,  but it's a step away from ")
  # Now we can create switch statements from this risk prioritization based on the StringFlowInt  
  tidier_changelog <- Tidy_Changelog(original_changelog, tickets_df) #tidy up the dataframe for us 
  
  setDT(tidier_changelog)[complete_cycle_dataframe, fromStringFlowint:=i.expected_flow, on = c(fromString = "fromString") ]
  
  setDT(tidier_changelog)[complete_cycle_dataframe, toStringFlowint:=i.expected_flow, on = c(toString = "fromString") ]
  
  setorderv(tidier_changelog,'issues.key', order = -1) 
  
  Unexpected_Flow <- tidier_changelog[as.numeric(fromStringFlowint)>as.numeric(toStringFlowint)]
  
  fromString <- c('Closed','Closed','Peer Review','Closed','Peer Review','PO Review','QA Review','In Progress', 'QA Review')
  toString <- c('In Progress','Open','In Progress','Peer Review','Open','In Progress','In Progress','Open','Peer Review')
  Risk <- c(7,9,1,8,6,4,3,5,2) # fix this
  
  Risk_Table <- data.table(fromString,toString,Risk)
  
  # we'l create a composite key using paste  so we can later join on the specific flow
  
  Risk_Table[, Composite:= paste(fromString, "to", toString)] # create the key in our Risk Table
  
  Unexpected_Flow[, Composite:=paste(fromString,"to" ,toString)] #create the key in the changelog 
  
  
  
  setDT(Unexpected_Flow)[Risk_Table, `:=` (Risk = i.Risk), on = c(Composite = "Composite")]
  
  
  #Unexpected_Flow[, diff_hours:=as.numeric(difftime(created_UTC, shift(created_UTC), units = "hours")), by = issues.key]
  
  
  # Unexpected_Flow[as.character(from) == "10034", from:="2.1"][as.character(from) == "10255", from:="2.2"][as.character(from) == "10254", from:="2.3"]
  # 
  # Unexpected_Flow[as.character(to) == "10034", to:="2.1"][as.character(to) == "10255", to:="2.2"][as.character(to) == "10254", to:="2.3"]
  # 
  # Unexpected_Flow[, swimlane_distance:=abs(as.numeric(as.character(from))-as.numeric(as.character(to)))]
  # 
  #we'll need to flag tickets which go through the exception lifecycle at first, but go through a regular cycle later on
  
  
  # How do we determine whether there was actually work performed on the issue after it was closed? 
  Unexpected_Flow
  #Regular_Flow_Exeptions <- regularflow_table[Unexpected_Flow, on = "issues.key"]
  
  #Regular_Flow_Exeptions 
  
}


Make_IssueLinks_Tidy <- function(original_changelog, issueLinks, ticket_dataframe){
  
  #Time_ToCreate_Link <- AUSF_Changelog[field == 'Link' ] 
  #so this information only tells us when the link is created to the inward issue (not the actually outward issue!)
  
  
  setDT(issueLinks)[ticket_dataframe, `:=` ( parent_ticket_creation_date = i.issues.fields.created,
                                             resolution_status =  i.issues.fields.resolution.name,
                                             parent_resolution_date  = i.issues.fields.resolutiondate
  ), on=c(issues.key = "issues.key")]
  
  
  outward_issues <- na.exclude(issueLinks[, "outwardIssue.key"])
  
  
  dates_outward_issues <- ticket_dataframe[, c('issues.key','issues.fields.created', 'issues.fields.resolutiondate')]
  
  
  # this holds the creation dates for the outward issues, but now we need the creation dates for the parent ticket
  # that value is held by issue links table, where we previously did the join
  outward_issues_creation_date <- merge(dates_outward_issues, outward_issues, by.x ="issues.key", by.y = "outwardIssue.key")
  
  # now, we will just need to join this table using the outward issue key on the issue links table and the issue key on the outward_issues_creation_date table 
  
  
  issuelinks_creation_dates <- merge(issueLinks[, c('issues.key','parent_ticket_creation_date','parent_resolution_date', 'outwardIssue.key','type.outward','outwardIssue.fields.issuetype.name')], outward_issues_creation_date, by.x = "outwardIssue.key", by.y = "issues.key")
  
  setnames(issuelinks_creation_dates, "issues.fields.created","link_creation_date")
  
  issuelinks_creation_dates[, linked_issue_creation_date:=difftime(parent_ticket_creation_date, link_creation_date, units = "hours")]
  
  #this calculation produces negative values, how can a link be created before the issue it is being linked to is created? 
  # that's because this table doesn't represent the time the link is created, it just represents the time the issue (which will be linked is created)
  # when the link is created is in the changelog 
  #grab the vector for outward issues in the previous created table 
  
  outward_issues <- as.vector( issuelinks_creation_dates[, outwardIssue.key])
  
  linked_parentissue_date <- original_changelog[field=="Link"][to %in% outward_issues][, c('to','created','issues.key')]
  
  
  issuelinks_tidy <-  issuelinks_creation_dates[linked_parentissue_date, on = "issues.key", nomatch = 0, allow.cartesian = TRUE]
  
  
  
  issuelinks_tidy <- unique(issuelinks_tidy)
  
  issuelinks_tidy[, linked_issue_link_hours:=as.numeric( difftime(created, parent_ticket_creation_date, units = "hours"))]
  # this join works!
  issuelinks_tidy
  
  
}


Calculate_Development_Time <- function(issue_links, ticket_dataframe, original_changelog, components_table, fixversion_table, sprint_table, issuelinks_table) {
  
  tidy_links <- Make_IssueLinks_Tidy( original_changelog, issue_links, ticket_dataframe)
  
  
  
  # create a regular flow object 
  RegularFlow <- Model_Ticket_LifeCycle(original_changelog, ticket_dataframe, components_table, fixversion_table, sprint_table, issuelinks_table)
  #what issue links do we include
  
  ticket_dataframe <- ticket_dataframe[, c('issues.fields.customfield_10002',
                                           'issues.fields.resolutiondate',
                                           'issues.fields.issuetype.name', 
                                           'issues.key',
                                           'issues.fields.created')]
  
  relevant_links <- tidy_links[type.outward %in%  c('blocks','parent to') | outwardIssue.fields.issuetype.name %in% "sub task" ] #based on business rules
  
  
  
  sum_changehours <- RegularFlow[, sum(diff_hours, na.rm = TRUE), by = c('fromString' ,'issues.key')]
  
  sum_changehours_spread <- spread(sum_changehours, fromString, V1)
  
  
  setDT(sum_changehours_spread)[relevant_links, `:=` (time_create_link = i.linked_issue_link_hours ), on = c(issues.key = "issues.key")]
  ticket_dataframe <- ticket_dataframe[sum_changehours_spread, on = "issues.key", nomatch=0]
  # let's not focus on this for now
  
  ticket_dataframe[, development_time := rowSums(.SD, na.rm = TRUE), .SDcols = c("In Progress","Peer Review", "QA Review")]
  
  
  # the relevant subtasks for a story should be included when we filter on a sprint basis 
  
  ticket_dataframe 
  
  setDT(ticket_dataframe)[sprint_table, `:=` (sprint_begin = i.startDate,
                                              sprint_end = i.endDate,
                                              sprint_name = i.name,
                                              sprint_goal = i.goal,
                                              sprint_db_sequence = i.sequence, 
                                              projected_sprint_completion = i.projectedCompletion,
                                              actual_sprint_completion_time = i.actualCompletion
  ), on = c(issues.key = "issues.key")]
  
  
  Exceptions <- Model_Ticket_Exception_LifeCycle(original_changelog, ticket_dataframe, RegularFlow)
  
  Exceptions_Aggregate <- Exceptions[, .(count_pushback = .N, risk_sum = sum(Risk)) , by = issues.key]
  
  ticket_dataframe <- Exceptions_Aggregate[ticket_dataframe, on = "issues.key"]
  
  ticket_dataframe
}








Generate_Report <- function(dimension, author_name, moving_average_window = 60, exclusion_value = 3, table, begin_date = as.character( ymd(Sys.Date())-years(1)), end_date  = as.character.Date( Sys.Date())) {
  
  
  RegularFlow_Table <- table[ created_UTC %between% c(begin_date,end_date)]
  
  
  #lapply for NAN's "no data availble#
  
  #need to fix moving avg col name 
  
  if(missing(author_name)) { #this will generate a report for all observations in the dataset, we will have to add projects later
    
    switch((dimension), 
           # this switch probably needs to be broken up in another function
           "swimlane" = {
             swimlane <- RegularFlow_Table[
               diff_hours > exclusion_value][, .("Total Average" = mean(diff_hours, na.rm = TRUE)/24, 
                                                 "Number of Changes" = .N, 
                                                 #"Moving Average" = mean(frollmean(diff_hours, n = moving_average_window, fill = NA, align = "center"), na.rm = TRUE)/24, 
                                                 "Spread from the Mean in Days " = round(sd(diff_hours, na.rm = TRUE)/24, 2 ) ), by = fromString] 
             
             swimlane[is.na(swimlane)] <-  "Not enough observations"
             
             setnames(swimlane, c('fromString'), c('Swim Lane'))
             
             swimlane
             
             
           },
           
           "storypoints" = { 
             storypoints <- RegularFlow_Table[
               fromString == "In Progress"][diff_hours > exclusion_value][, .("Total Average of Days" = mean(diff_hours, na.rm = TRUE)/24, 
                                                                              "Number of Changes" = .N, 
                                                                              #"Moving Average" = frollmean(diff_hours, n = moving_average_window, fill = NA, align = "center")/24, 
                                                                              "Spread from the Mean in Days " = round(sd(diff_hours, na.rm = TRUE)/24, 2 ) ), by = storypoints][order(storypoints)][, storypoints := as.factor(storypoints)]  
             # we need to remove Nan's 
             setnames(storypoints, c('storypoints'), c('Story Points'))
             storypoints
             
             
             
           },
           
           "issuetype" = {
             issuetype <- RegularFlow_Table[
               diff_hours > exclusion_value][
                 fromString == "In Progress"][, .("Total Average of Days" = mean(diff_hours, na.rm = TRUE)/24, 
                                                  "Number of Changes" = .N, 
                                                  #"Moving Average" = mean(frollmean(diff_hours, n = moving_average_window, fill = NA,align = "center"), na.rm = TRUE)/24, 
                                                  "Spread from the Mean in Days " = round(sd(diff_hours, na.rm = TRUE)/24, 2 ) ), by = current_issue_type] 
             issuetype[is.na(issuetype)] <-  "Not enough observations"
             
             setnames(issuetype, c('current_issue_type'), c('Issue Type'))
             
             issuetype
           },
           "top_outliers" = { 
             
             outliers <- head(RegularFlow[fromString == "In Progress"][
               current_issue_type %in% c('Story','Bug', 'sub task')][
                 order(-scaled_diffhours)][
                   , c('issues.key','created_UTC', 'current_issue_type', 'diff_hours')][, created_UTC:= as.character.Date(created_UTC)], 10)
             # we may have to think about what issue types to show 
             
             setnames(outliers, c("issues.key"), c("Key"))
             
             setnames(outliers,  c('created_UTC'), c('Created'))
             
             setnames(outliers,  c('current_issue_type'),c('Issue Type'))
             
             
             outliers
             
           }
    )
    
  }
  
  else {
    switch((dimension), # this probably needs to be broken up in another function
           "swimlane" = {
             swimlane <- RegularFlow_Table[author.displayName == author_name][
               diff_hours > exclusion_value][, .("Total Average" = mean(diff_hours, na.rm = TRUE)/24, 
                                                 "Number of Changes" = .N, 
                                                 #"Moving Average" = mean(frollmean(diff_hours, n = moving_average_window, fill = NA, align = "center"), na.rm = TRUE)/24, 
                                                 "Spread from the Mean in Days " = round(sd(diff_hours, na.rm = TRUE)/24, 2 ) ), by = fromString] 
             
             swimlane[is.na(swimlane)] <-  "Not enough observations"
             swimlane
             
             
           },
           
           "storypoints" = { 
             storypoints <- RegularFlow_Table[author.displayName == author_name][fromString %in% c("In Progress", "Peer Review")][diff_hours > exclusion_value][, .("Total Average of Days" = mean(diff_hours, na.rm = TRUE)/24, 
                                                                                                                                                                    "Number of Changes" = .N, 
                                                                                                                                                                    #"Moving Average" = frollmean(diff_hours, n = moving_average_window, fill = NA, align = "center")/24, 
                                                                                                                                                                    "Spread from the Mean in Days " = round(sd(diff_hours, na.rm = TRUE)/24, 2 ) ), by = storypoints][order(storypoints)][, storypoints := as.factor(storypoints)]  
             # we need to remove Nan's 
             setnames(storypoints, c('storypoints'), c('Story Points'))
             storypoints
             
             
             
           },
           
           "issuetype" = {
             issuetype <- RegularFlow_Table[author.displayName == author_name][
               diff_hours > exclusion_value][, .("Total Average of Days" = mean(diff_hours, na.rm = TRUE)/24, 
                                                 "Number of Changes" = .N, 
                                                 #"Moving Average" = mean(frollmean(diff_hours, n = moving_average_window, fill = NA,align = "center"), na.rm = TRUE)/24, 
                                                 "Spread from the Mean in Days " = round(sd(diff_hours, na.rm = TRUE)/24, 2 ) ), by = current_issue_type] 
             issuetype[is.na(issuetype)] <-  "Not enough observations"
             
             setnames(issuetype, c('current_issue_type'), c('Issue Type'))
             
             issuetype
           },
           "top_outliers" = { 
             
             outliers <- head(RegularFlow[author.displayName == author_name][
               fromString == "In Progress"][
                 current_issue_type %in% c('Story','Bug', 'sub task')][
                   order(-scaled_diffhours)][, c('issues.key','created_UTC', 'current_issue_type', 'storypoints', 'diff_hours', 'scaled_diffhours')][
                     , created_UTC:= as.character.Date(created_UTC)][, diff_days:=diff_hours/24][, c('issues.key','created_UTC', 'current_issue_type', 'storypoints', 'diff_days')], 10)
             # we may have to think about what issue types to show 
             
             setnames(outliers, c("issues.key"), c("Key"))
             
             setnames(outliers,  c('created_UTC'), c('Created'))
             
             setnames(outliers,  c('current_issue_type'),c('Issue Type'))
             
             setnames(outliers,  c('storypoints'),c('Story Points'))
             
             setnames(outliers,  c('diff_days'),c('Days to Move out of In Progress '))
             
             
             outliers
             
           }
    )
    
  }  
}




Generate_Report_Irregular <- function(report_type, author_name, moving_average_window = 13, exclusion_value = 3, table, begin_date = as.character( ymd(Sys.Date())-years(1)), end_date  = as.character.Date( Sys.Date())) {
  
  author_name <- author_name  
  report_type <- report_type
  moving_average_window <- moving_average_window
  exclusion_value <- exclusion_value
  reporting_units <- units
  begin_date <- begin_date
  
  IregularFlow_Table <- table[ created_UTC %between% c(begin_date,end_date)]
  
  #lapply for NAN's "no data availble#
  
  #need to fix moving avg col name 
  
  switch((report_type), # this probably needs to be broken up in another function
         "swimlane" = {
           swimlane <- IregularFlow_Table[author.displayName == author_name][
             diff_hours > exclusion_value][, .("Total Average" = mean(diff_hours, na.rm = TRUE)/24, "Number of Changes" = .N, 
                                               "Moving Average" = mean(frollmean(diff_hours, n = moving_average_window, fill = NA, align = "center"), na.rm = TRUE)/24, 
                                               # we have to modify 
                                               "Spread from the Mean in Days " = round(sd(diff_hours, na.rm = TRUE)/24, 2 ) ), by = fromString] 
           
           swimlane[is.na(swimlane)] <- "Not enough observations"
           print(swimlane)
           swimlane
           
           
         },
         
         "storypoints" = { 
           storypoints <- IregularFlow_Table[author.displayName == author_name][
             fromString == "In Progress"][diff_hours > exclusion_value][, .("Total Average of Days" = mean(diff_hours, na.rm = TRUE)/24, 
                                                                            "Number of Changes" = .N, 
                                                                            "Moving Average" = mean(frollmean(diff_hours, n = moving_average_window, fill = NA, align = "center"),                                                            na.rm = TRUE)/24, 
                                                                            "Spread from the Mean in Days " = round(sd(diff_hours, na.rm = TRUE)/24, 2 ) ), by = storypoints][order(storypoints)]  
           # we need to remove Nan's 
           storypoints
           
           
           
         },
         
         "issuetype" = {
           issuetype <- IregularFlow_Table[author.displayName == author_name][
             diff_hours > exclusion_value][, .("Total Average of Days" = mean(diff_hours, na.rm = TRUE)/24, 
                                               "Number of Changes" = .N, 
                                               "Moving Average" = mean(frollmean(diff_hours, n = moving_average_window, fill = NA,align = "center"), na.rm = TRUE)/24, 
                                               "Spread from the Mean in Days " = round(sd(diff_hours, na.rm = TRUE)/24, 2 ) ), by = current_issue_type] 
           issuetype[is.na(issuetype)] <-  "Not enough observations"
           
           issuetype
         },
         
         "composite" = {
           composite <- IregularFlow_Table[author.displayName == author_name][, .( "Mean Days to Move out of Lane" = mean(diff_hours, na.rm = TRUE)/24,
                                                                                   "Amount of tickets" = .N), by = Composite]
           #composite[is.na(composite)] <- "No data"
           composite
           
         }
         
  )
  
}  


Plot_Sprint <- function(RegularFlow, begin_date = as.character( ymd(Sys.Date())-months(3)), end_date  = as.character.Date( Sys.Date()),  selected_component = component ) {
  
  #theme_set(theme_minimal())
  
  RegularFlow_Plot_Sprint <- RegularFlow
  
  #available y axis dimensions
  
  #days_sprint
  
  #sum_storypoints
  
  
  days_storypoints_sprint <- RegularFlow_Plot_Sprint[component == selected_component][created_UTC %between% c(begin_date, end_date)][fromString %in% c('In Progress','Peer Review','QA Review')]
  
  
  
  
  
  days_storypoints_sprint <- days_storypoints_sprint[, .(days_sprint = mean(diff_hours/24, na.rm = TRUE), 
                                                         sum_storypoints = sum(storypoints, na.rm = TRUE)), 
                                                     by = c('sprint_name','sprint_db_sequence',"author.displayName")][ order(sprint_db_sequence)]
  
  base <- ggbarplot(days_storypoints_sprint,
                    x = "sprint_name",
                    y = "days_sprint", # implement this with the functions going in the next release
                    fill = "author.displayName",
                    title = "Days Estimated in Development"
  )+ theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + labs(y="Reported Days in Development", x = "Sprint") + theme(legend.position="right")
  
  
  sprint_by_day <- ggbarplot(days_storypoints_sprint,
                             x = "sprint_name",
                             y = "days_sprint", # implement this with the functions going in the next release
                             fill = "author.displayName",
                             title = "Days Estimated in Development"
  )+ theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + labs(y="Reported Days in Development", x = "Sprint")+ theme(legend.position = "none")
  
  
  sprint_by_storypoints <- ggbarplot(days_storypoints_sprint,
                                     x = "sprint_name",
                                     y = "sum_storypoints", 
                                     title = "Storypoints Estimated by Sprint",
                                     fill = "author.displayName",
  )+ theme_minimal() + theme(axis.text.x = element_text(angle = 90))+ labs(y="Total Storypoints by Sprint", x = "Sprint")+ theme(legend.position = "none")
  
  
  
  sprint_by_complexity <- ggbarplot(RegularFlow_Plot_Sprint[component == selected_component][created_UTC %between% c(begin_date, end_date)][fromString %in% c('In Progress','Peer Review','QA Review')][, sum((diff_hours)/24, na.rm = TRUE)/actual_sprint_completion_time, by = c('sprint_name','sprint_db_sequence',"author.displayName")][ order(sprint_db_sequence)],
                                    x = "sprint_name",
                                    y = "V1",
                                    fill = "author.displayName",
                                    title = "Average Estimated Efficiency by Sprint",
                                    add = "mean"
  )+ theme_minimal() + theme(axis.text.x = element_text(angle = 90) ) + labs(y="Reported Days in Development", x = "Sprint")+ labs(y="Efficiency", x = "Sprint")+ theme(legend.position = "none")
  
  
  legend_author <- get_legend(base)
  
  plot_grid(sprint_by_day, sprint_by_storypoints, sprint_by_complexity, legend_author, nrow = 1)
  
  
  
  
  
}

Plot_Storypoint_Risk_Sprint <- function (RegularFlow, begin_date = as.character( ymd(Sys.Date())-months(3)), end_date  = as.character.Date( Sys.Date()),  selected_component = component, selected_sprint){
  
  storypoints_efficiency <- RegularFlow[component == selected_component & sprint_name == selected_sprint ][created_UTC %between% c(begin_date, end_date)][fromString %in% c('In Progress','Peer Review','QA Review')]
  
  storypoints_efficiency[, storypoints:=as.factor(storypoints)]
  
  
  #do we need to aggregate these in mem? 
  storypoints_efficiency <- storypoints_efficiency[, .(hours_sprint = sum(diff_hours, na.rm = TRUE)), by = c('sprint_name','sprint_db_sequence',"author.displayName","actual_sprint_completion_time","storypoints")]
  
  storypoints_efficiency[, dev_ratio:= hours_sprint/actual_sprint_completion_time][, sprint_efficiency:= dev_ratio/actual_sprint_completion_time] 
  
  
  
  
  ggboxplot(storypoints_efficiency[ order(sprint_db_sequence)],
            title = "Ratio of Development Time to Amount of Time Sprint was open ",        
            x = "storypoints",
            y = "sprint_efficiency",
            fill = "storypoints",
            add = "mean",
  ) + theme_minimal() + scale_y_continuous(trans = 'log10')
  
  
  
}


Plot_Curve_Developer <- function(RegularFlow, author ){
  
  sprint_developers_granular <- RegularFlow[author.displayName == author][fromString %in% c('In Progress','Peer Review','QA Review')][, .(hours_sprint = mean(diff_hours, na.rm = TRUE), sum_storypoints = sum(storypoints, na.rm = TRUE ), observations = .N), by = c('issues.key','sprint_name','sprint_db_sequence',"actual_sprint_completion_time","component", "author.displayName")]  
  
  sprint_developers_granular[, developer_efficiency := hours_sprint/actual_sprint_completion_time]
  
  
  
  ggscatter(sprint_developers_granular, 
            x = "sum_storypoints", 
            y = "developer_efficiency", 
            color = "author.displayName",
            conf.int = TRUE,
            title = "Development Time as a Function of Sum Storypoints by Sprint",
            facet.by = "author.displayName",
            palette = "aaas"
  ) + scale_x_continuous(trans = 'log10') + scale_y_continuous(trans = 'log10') + theme_minimal() + geom_smooth(method = lm, formula = y ~ splines::bs(x,3), se=TRUE)
  
}


Calculate_Optimization_Cap_Developer <- function(RegularFlow, author) {
  
  
  #get storypoints as an aggregate by author and sprint
  
  sprint_developers <- RegularFlow[author.displayName == author][
    fromString %in% c('In Progress','Peer Review','QA Review')][, .(hours_sprint = median(diff_hours, na.rm = TRUE), sum_storypoints = sum(storypoints, na.rm = TRUE), observations = .N), by = c('sprint_name','sprint_db_sequence',"actual_sprint_completion_time","component", "author.displayName")]  
  
  #the aggegation by sprint is throwing stuff off here, the sum(sp) by sprint needs to make sense, and the sums need to be calculated individually as well (not combined ones)
  
  
  sprint_truesum_storypoints <-  RegularFlow[author.displayName == author][
    fromString %in% c('In Progress','Peer Review','QA Review')][, sum(unique(storypoints), na.rm = TRUE), by = c('sprint_name')]
  
  sprint_developers[, developer_efficiency := (hours_sprint/actual_sprint_completion_time)/actual_sprint_completion_time]
  
  #fit the spline model
  
  splines_dev_efficiency <- lm(log_efficiency~splines::bs(log_sp, 3), data =  na.exclude(sprint_developers[sum_storypoints>0 & developer_efficiency >0][, c('log_sp','log_efficiency'):= list(log10(sum_storypoints), log10(developer_efficiency))]) )
  
  summary(splines_dev_efficiency)
  
  #do we fit this by person or for the entire dataset?
  
  predictions_splines_dev <- predict(splines_dev_efficiency, data.table(na.exclude(sprint_developers[sum_storypoints>0][, c('log_sp','log_efficiency'):= list(log10(sum_storypoints), log10(developer_efficiency))]) ))
  
  
  predicted_efficiency <- cbind(predictions_splines_dev, data.table(na.exclude(sprint_developers[sum_storypoints>0][, c('log_sp','log_efficiency'):= list(log10(sum_storypoints), log10(developer_efficiency))][, c('sum_storypoints','sprint_name', 'component')]) ))
  
  #predicted_efficiency[, .( sp_mean = mean(sum_storypoints), mean_eff = mean(predictions_splines_dev)), by = c('sprint_name','component')][order(-mean_eff)]
  #does the sum or the average make more sense here? (it would be the mean across all sprints available)
  
  # we probably need both, a TOTAL cap if the sprint goes on for more time, and a singular cpa for the actual sprint 
  predicted_efficiency <- predicted_efficiency[sprint_truesum_storypoints, on = "sprint_name", nomatch=0]
  #get true storypoint
  #alternative_turning_point: 
  turning_point <- predicted_efficiency[ order(-predictions_splines_dev)][1, V1]
  #turning_point <- predicted_efficiency[, .(avg_ticket_velocity = mean(predictions_splines_dev), avg_sp = mean(sum_storypoints) ), by = sprint_name][order(-avg_ticket_velocity)][1, avg_sp]
  
  turning_point
  
  
  # efficiency_turning_point[, mean(predictions_splines_dev), by = sum_storypoints)]
  
  # efficiency_turning_point[1, ][, V1]
  
  
  
}


Calculate_Optimization_Devs <- function(dev_list, RegularFlow){ 
  
  dev_optimalstorypoints <- lapply(dev_list, function(x) {
    list <- paste0(x,',',Calculate_Optimization_Cap_Developer(RegularFlow, x))
  })
  
  
  dev_optimalstorypoints <-  data.table(dev_optimalstorypoints)
  
  
  dev_optimalstorypoints[, c("author","recommended_sp") :=  tstrsplit(dev_optimalstorypoints, ",", fixed = TRUE)][, c("author","recommended_sp")]
  
  
  
}


Fit_Developer_Model <- function(devlist, RegularFlow){
  
  
  
  best_fitted_models_devs <- lapply(devlist, function(X){
    
    
    observation_sprint <- RegularFlow[fromString %in% c('In Progress','Peer Review','QA Review')][
      , .(hours_sprint = mean(diff_hours, na.rm = TRUE), sum_storypoints = sum(unique(storypoints), na.rm = TRUE ), 
          observations = .N, sum_swimlane = sum(swimlane_distance, na.rm = TRUE)), 
      by = c('sprint_name','sprint_db_sequence',"actual_sprint_completion_time","component", "author.displayName", "current_issue_type", "actual_sprint_completion_time")]
    
    
    fitControl <- trainControl(
      method = "repeatedcv",
      number = 10,
      repeats = 10,
    )
    
    tuning_grid_polynomialfit <- expand.grid(
      degree = 1:4, 
      nprune = seq(2, 100, length.out = 10) %>% floor()
    )
    
    # multi adaptive regression spline
    mars_model <- train(
      log(hours_sprint) ~ log(sum_storypoints), 
      data = na.exclude(observation_sprint[sum_storypoints > 0 & hours_sprint>0][author.displayName == X]), 
      trControl = fitControl,
      tuneGrid = tuning_grid_polynomialfit,
      method = "earth")
    
    
    
  })
  
  
  
  
}













Get_Optimal_Backlog <- function(RegularFlow, begin_date = as.character( ymd(Sys.Date())-months(3)), end_date  = as.character.Date( Sys.Date()),  selected_component = component ){
  
  RegularFlow_V3[created_UTC %between% c(begin_date, end_date)][component == selected_component][
    fromString %in% c('In Progress','Peer Review','QA Review')][
      , .(hours_sprint = mean(diff_hours, na.rm = TRUE), sum_storypoints = sum(storypoints, na.rm = TRUE)), by = c('sprint_name','sprint_db_sequence',"author.displayName","actual_sprint_completion_time")]
  
  
  sprint_dev[, dev_ratio:= hours_sprint/actual_sprint_completion_time][, sprint_efficiency:= dev_ratio/actual_sprint_completion_time] 
  
  
  
  
  ggboxplot(storypoints_efficiency[sprint_db_sequence>4000 ][sprint_name == "Student: Q3 1920 Sprint 1"][ order(sprint_db_sequence)],
            title = "Ratio of Development Time to Amount of Time Sprint was open ",        
            x = "sprint_name",
            y = "sprint_efficiency",
            fill = "storypoints",
            add = "mean",
  ) + theme_minimal() + scale_y_continuous(trans = 'log10')
  
  
}



Plot_Sprint_Efficiency_Blend <- function(RegularFlow_Plot_Sprint, begin_date = as.character( ymd(Sys.Date())-months(3)), end_date  = as.character.Date( Sys.Date()),  selected_component = component){
  
  
  sprint_dev_issues <- RegularFlow_Plot_Sprint[component == selected_component][created_UTC %between% c(begin_date, end_date)][fromString %in% c('In Progress','Peer Review','QA Review')]
  
  sprint_dev_issues <- sprint_dev_issues[, .(hours_sprint = mean(diff_hours, na.rm = TRUE), sum_storypoints = sum(storypoints, na.rm = TRUE)), by = c('sprint_name','sprint_db_sequence',"current_issue_type","actual_sprint_completion_time")]
  
  
  sprint_dev_issues[, dev_ratio:= hours_sprint/actual_sprint_completion_time][, sprint_efficiency:= dev_ratio/actual_sprint_completion_time]
  
  
  ggbarplot(sprint_dev_issues[ order(sprint_db_sequence)],
            title = "Efficiency of Sprint based on Backlog Blend",        
            x = "sprint_name",
            y = "sprint_efficiency",
            fill = "current_issue_type"
  ) + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + labs(y="Sprint Effiency", x = "Sprint")
  
}

Plot_Backlog_Blend <- function(RegularFlow, begin_date = as.character( ymd(Sys.Date())-months(3)), end_date  = as.character.Date( Sys.Date()),  selected_component = component){
  
  
  modata <- RegularFlow[component == selected_component][ order(-sprint_db_sequence)][created_UTC %between% c(begin_date, end_date)][fromString %in% c('In Progress','Peer Review','QA Review')]
  
  modata[, storypoints := as.factor(storypoints) ]
  
  
  mosaic <- ggplot(data =modata)  + geom_mosaic(aes(x = product(author.displayName, storypoints), fill = author.displayName  ), na.rm = TRUE) + theme_light()+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) 
  
  
  mosaic+ ggtitle("Proportion of Backlog Being Worked on by Entire Component")+ labs(x = "Storypoints")
}

Plot_Backlog_Blend_Recent_Sprint <- function(RegularFlow, begin_date = as.character( ymd(Sys.Date())-months(3)), end_date  = as.character.Date( Sys.Date()),  selected_component = component, selected_sprint){
  
  
  #last_sprints <- as.vector(RegularFlow[component == selected_component][ order(-sprint_db_sequence)][fromString %in% c('In Progress','Peer Review','QA Review')][, head(unique( sprint_db_sequence),sprints)])
  
  modata <- RegularFlow[component == selected_component][created_UTC %between% c(begin_date, end_date)][fromString %in% c('In Progress','Peer Review','QA Review')][sprint_name == selected_sprint]
  
  modata[, storypoints := as.factor(storypoints) ]
  
  
  mosaic <- ggplot(data =modata)  + geom_mosaic(aes(x = product(author.displayName, storypoints), fill = author.displayName  ), na.rm = TRUE) + theme_light()+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) 
  
  
  mosaic+ ggtitle("Proportion of Backlog Tackled Currently Tackled by Developers")+ labs(x = "Storypoints")
}







# then we need a function for a corresponding plot (which can be outside)

Plot_Metrics_Bar <- function(author_name, dimension, exclusion_value = 3, table, begin_date = as.character( ymd(Sys.Date())-years(1)), end_date  = as.character.Date( Sys.Date())) {
  author_name <- author_name  
  exclusion_value <- exclusion_value
  reporting_units <- units
  RegularFlow <- table
  date <- date
  begin_date <- begin_date
  
  # this function can and should be simplified 
  a <- RegularFlow[ created_UTC %between% c(begin_date,end_date)]
  
  
  switch((dimension), #needs a function
         "swimlane" = {
           a <- a[author.displayName == author_name][diff_hours > exclusion_value][, diff_days := diff_hours/24] 
           a <- ggbarplot(a,
                          x = "fromString",
                          y = "diff_days", 
                          fill = "fromString",
                          add = "mean_se",
                          legend = "right",
                          rotate = FALSE, 
                          title = "Average Days Issues Stay in a Lane",
                          palette = "npg"
           ) + theme_minimal() + ylab("Mean Days it Takes to Complete a Change") + xlab("Story Points")+ scale_fill_discrete(name="Swim Lane")
           
           a
         }, 
         "storypoints" = {
           
           a <- a[author.displayName == author_name][fromString =="In Progress"][diff_hours > exclusion_value][, diff_days := diff_hours/24]
           a <- ggbarplot(a,
                          x = "storypoints",
                          y = "diff_days",
                          fill = "storypoints",
                          add = "mean",
                          legend = "right",
                          rotate = FALSE,
                          title = "Average Days Spent Issues stays In Progress based on Storypoints",
                          #facet.by = "current_issue_type",
                          palette = "npg"
           ) + theme_minimal() + ylab("Mean Days it Takes to Complete a Change") + xlab("Story Points")+ scale_fill_discrete(name="Story Points") #+ facet_grid(rows = vars(current_issue_type))
           
           a
           
         }, 
         
         "issuetype" = {
           
           a <- a[author.displayName == author_name][fromString == "In Progress"][diff_hours > exclusion_value][, diff_days := diff_hours/24]
           a <- ggbarplot(a,
                          x = "current_issue_type",
                          y = "diff_days",
                          fill = "current_issue_type",
                          add = "mean_se",
                          legend = "right",
                          rotate = FALSE,
                          title = "Average Days Spent in  'In Progress' Based on Issue Type (In Progres)",
                          palette = "npg"
           ) + theme_minimal() + ylab("Mean Days it Takes to Complete a Change") + xlab("Story Points")+ scale_fill_discrete(name="Story Points")
           
           a
           
           
         }
         
         # "outliers" = {
         #   
         #   
         #   
         #   a <- a[author.displayName == author_name][fromString == "In Progress"][diff_hours > exclusion_value][, diff_days := diff_hours/24][ scaled_diffhours > 3 ]
         #   a <- ggbarplot(a,
         #                  x = "current_issue_type",
         #                  y = "diff_days",
         #                  fill = "current_issue_type",
         #                  add = "mean_se",
         #                  legend = "right",
         #                  rotate = FALSE,
         #                  title = "Average Days Spent in  'In Progress' Based on Issue Type (In Progres)",
         #                  palette = "npg"
         #   ) + theme_minimal() + ylab("Mean Days it Takes to Complete a Change") + xlab("Story Points")+ scale_fill_discrete(name="Story Points")
         #   
         #   a
         #   
         #   
         # }
         
  )
  
}


Plot_Metrics_Boxplot <- function( author_name, exclusion_value = 3, table, begin_date = as.character( ymd(Sys.Date())-years(1)), end_date  = as.character.Date( Sys.Date())){
  RegularFlow <- table 
  
  a <- RegularFlow[ created_UTC %between% c(begin_date,end_date)]
  
  if (missing(author_name)) {
    a <- a[fromString %in% c("In Progress", "Peer Review")][diff_hours > exclusion_value][, diff_days := diff_hours/24] # we need to add an in progress metric
    plot_everyone <- ggboxplot(a,
                               x = "storypoints",
                               y = "diff_days",
                               add = "mean",
                               legend = "right",
                               add.params = list(size=0.09),
                               rotate = FALSE,
                               fill = "storypoints",
                               title = "Variance of hour difference for Storypoints in Progress",
                               facet.by = "fromString",
                               bxp.errorbar = TRUE )+ ylab("Mean Days it Takes to Complete a Change") + xlab("Story Points")+ scale_fill_discrete(name="Story Points")
    
    my_comparisions <- list(c('1','2'), c('2','3'), c('3','4'),c('4','5'),c('5','8'), c('8','13'))
    
    
    plot_everyone  <- plot_everyone + theme_minimal() + scale_y_continuous(trans = 'log10') + stat_compare_means(method = "anova", label.y = 7 )+stat_compare_means(comparisons = my_comparisions,
                                                                                                                                                                    ref.group = ".all.", label.y = 5) 
    
    plot_everyone
    
  }
  else {
    a <- a[fromString %in% c("In Progress", "Peer Review")][author.displayName == author_name][fromString == "In Progress"][diff_hours > exclusion_value][, diff_days := diff_hours/24]
    plot_individual <- ggboxplot(a,
                                 x = "storypoints",
                                 y = "diff_days",
                                 add = "mean",
                                 legend = "right",
                                 add.params = list(size=0.09),
                                 rotate = FALSE,
                                 fill = "storypoints",
                                 title = "Variance of hour difference for Storypoints in Progress",
                                 bxp.errorbar = TRUE
    ) + ylab("Mean hours it Takes to Complete a Change") + xlab("Story Points")+ scale_fill_discrete(name="Story Points")
    
    plot_individual
    
  }
  
  
}



Plot_Metrics_Density <- function( author_name, exclusion_value = 3, table, begin_date = as.character( ymd(Sys.Date())-years(1)), end_date  = as.character.Date( Sys.Date())){
  author_name <- author_name  
  exclusion_value <- exclusion_value
  reporting_units <- units
  RegularFlow <- table
  date <- date
  begin_date <- begin_date
  
  
  a <- RegularFlow[author.displayName == author_name][fromString == "In Progress"][created_UTC %between% c(begin_date,end_date)]
  
  general <- ggdensity(a,
                       x = "diff_hours",
                       y = "..density..",
                       #color = "storypoints",
                       fill = "storypoints",
                       title = "Distribution of Hours Between Status Changes",
                       alpha = 0.8,
                       facet.by = "storypoints",
                       add = "median")+ scale_x_continuous(trans = 'log10') + theme_minimal() + theme( axis.text.x=element_blank() )
  
  general
}


Plot_Metrics_Density_Sum <- function( author_name, exclusion_value = 3, table, begin_date = as.character( ymd(Sys.Date())-years(1)), end_date  = as.character.Date( Sys.Date())){
  author_name <- author_name  
  exclusion_value <- exclusion_value
  reporting_units <- units
  RegularFlow <- table
  date <- date
  begin_date <- begin_date
  
  
  a <- RegularFlow[author_name == author.displayName][fromString == "In Progress"][created_UTC %between% c(begin_date,end_date)][, sum(diff_hours, na.rm = TRUE), by= c('issues.key','storypoints')
                                                                                                                                 ][, .("Sum_Days" = V1/24, "Story_Points" = storypoints, "Issues_Key" = issues.key)
                                                                                                                                   ]
  
  general <- ggdensity(a,
                       x = "Sum_Days",
                       y = "..density..",
                       #color = "storypoints",
                       fill = "Story_Points",
                       title = "Distribution of Sum Hours to Move out of In Progress",
                       alpha = 1,
                       facet.by = "Story_Points",
                       add = "median")+ scale_x_continuous(trans = 'log10') + theme_minimal() + theme( axis.text.x=element_blank() )
  
  general
}


Plot_Metrics_Density_Outliers<- function( author_name, exclusion_value = 3, table, begin_date = as.character( ymd(Sys.Date())-years(1)), end_date  = as.character.Date( Sys.Date())){
  author_name <- author_name  
  exclusion_value <- exclusion_value
  reporting_units <- units
  RegularFlow <- table
  date <- date
  begin_date <- begin_date
  
  
  a <- RegularFlow[author_name == author.displayName][fromString == "In Progress"][created_UTC %between% c(begin_date,end_date)]
  
  outliers <- ggdensity(a[ scaled_diffhours > 3 ][, diff_days:=diff_hours/24],
                        x = "diff_days",
                        y = "..density..",
                        title = "Outlier Hours",
                        fill = "lightgray",
                        legend = "right",
                        alpha = 1,
                        #facet.by = "current_issue_type",
                        add = "median")+ scale_x_continuous(trans = 'log10') + theme_minimal() 
  
  outliers
}


Plot_Density_Individual <- function(individuals_vector, swimlanes_vector, table, begin_date = as.character( ymd(Sys.Date())-years(1)), end_date  = as.character.Date( Sys.Date())) {
  
  table <- table[created_UTC %between% c(begin_date, end_date)]
  
  filtered_developers <- table[author.displayName %in% individuals_vector ]
  
  filtered_developers <- filtered_developers[fromString %in% swimlanes_vector]
  
  final <- filtered_developers[, diff_days:=diff_hours/24 ]
  
  density_plots <- ggplot(final, aes(x = diff_days, y = author.displayName, fill = ..x.. )) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01 ) + scale_x_continuous(trans = 'log10') + theme_ridges(font_size = 13, grid = TRUE)  + facet_grid(cols = vars(fromString))  + theme_minimal() 
  
  density_plots + theme( axis.text.x=element_blank() ) + ylab("Developer") + xlab("Days to move out of Swimlane")
  
  
}




Analyze_Sprint <- function(original_changelog, tickets_df) {
  
  tidier_changelog <- Tidy_Changelog(original_changelog, tickets_df)
  
  tidier_changelog <- tidier_changelog[field=='Sprint']  
  
  setorderv(tidier_changelog,'issues.key', order = -1)   
  
  flagged_sprint <- tidier_changelog[, .N, by=issues.key][, Sprint_Flag_Rollover:=ifelse(N>1, "YES", "NO")][tickets_df, on = "issues.key"]
  # need to check the logic of this query
  #think about removing 'N' column
  #tells us if sprint has roled over 
  #add flag to original ticket dataframe?
  
  # we need to find a way to report the frequency of our observations 
  flagged_sprint  
  # do all tickets have a sprint attached to them? :S 
  #Analyze_Sprints   
  
  #we can analyze sprints through their ID's generated as well
  
}




Analyze_Timeseries <- function(original_changelog, tickets_df){
  
  tidier_changelog <- Model_Ticket_LifeCycle(original_changelog, tickets_df) #tidy up the dataframe for us 
  
  time_movement <- tidier_changelog[, c('diff_hours','storypoints', 'created_day','resolution_date', 'issues.key')]
  
  changelog_ts <- ts(time_movement[, c('diff_hours')], start = c(2019, time_movement[1, created_day]), frequency =365)
  
  changelog_ts 
}


# Compare_Timlogged_Diffhours <- function(original_changelog, tickets_df){
#   
#   new_df_appian_changelog[field == "timespent"]
#   
# } only 25 observvations for this field in my dataset 


