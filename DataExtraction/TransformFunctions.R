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





