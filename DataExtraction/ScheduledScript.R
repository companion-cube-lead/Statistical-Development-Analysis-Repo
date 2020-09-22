source('JiraRApiFunctions.R')

source('TransformFunctions.R')

# need to focus on development time, so we will strictly filter on developers

developers <- c('Alexander Strauss','Vimal Kumar Sekar','Walker Carr','Asha Jaianand','Pamela Bulu','Jose Jarquin','Pedro Ortiz Rodriguez','Jon Schipani','Carson Ferraro','Juan Almonte', 'Thapani Sawaengsri', 'Alden Sharp', 'Sudharshian Ramanathan', "Vinith Abishek")

# this will only work with a flat file in directory, use Get Changelog to get observations up to specified data. The file name must be 'updated_dataframe.csv'
# projects are passed as vectors, but this can be extracted from a database in the future
Update_Observations('username', 'password', c('DI','SEO','AUSF','SITECORE'), 'jira.usf.edu', 5)

# this will only work with a flat file in directory, use Get Ticket Data to get observations up to specified data. The file name must be 'updated_ticket_dataframe.csv'

Update_Observations_Tickets('username', 'password', c('DI','SEO','AUSF','SITECORE'), 'jira.usf.edu', 5)

# get ticket many to one tables
flat_tables_list <- Get_Tickets_Latest_Flat('username','password', c('DI','SEO','AUSF','SITECORE'), 'jira.usf.edu', 5)

# these tables also need to exist to be appended. Use Get Tickets Flat to do so
fwrite(flat_tables_list[[6]], "sprint_table.csv", append = T)

fwrite(as.data.table(flat_tables_list[[4]]), "fixversions_table.csv", append = T)

fwrite(as.data.table(flat_tables_list[[3]]), "components_table.csv", append = T)

changelog <- fread("updated_dataframe.csv") # read into memory newly created files
ticket_base <- fread("updated_ticket_dataframe.csv")

sprint <- unique(fread("sprint_table.csv"), by = "issues.key") # get unique observations
fixVersions <- unique(fread("fixversions_table.csv"), by = "issues.key" )
components <- unique(fread("components_table.csv"), by = "issues.key")


RegularFlow <- Model_Ticket_LifeCycle(changelog, ticket_base, components, fixVersions, sprint) #tidy the dataframe

RegularFlow <- unique(RegularFlow, by = "id")
#RegularFlow <- unique(RegularFlow[, .SD, .SDcols = !( c("V1"))])


RegularFlow_Aggregated <- RegularFlow[current_issue_type %in% c('Story', 'sub task', 'Bug', 'Review Result')
                                          ][fromString %in% c("In Progress", "Peer Review", "QA Review") & author.displayName %in% developers][
                                            , .( sum_days = sum(diff_hours)/24, 
                                                 sum_swim = sum(swimlane_distance), 
                                                 observations = .N),
                                            by= .(issues.key, 
                                                  sprint_name, 
                                                  storypoints, 
                                                  current_issue_type, 
                                                  projected_sprint_completion, 
                                                  actual_sprint_completion_time, 
                                                  created_year, 
                                                  sprint_end, 
                                                  author.displayName, 
                                                  resolution_date, 
                                                  ticket_creation_date, 
                                                  sprint_begin,
                                                  component)] 
# this is the view needed to complete the linear model portion of the workflow

write.csv(RegularFlow_Aggregated, "RegularFlowAggregated_View.csv", append = FALSE) # write back as a csv in the dir

write.csv(RegularFlow, "RegularFlow.csv", append = FALSE) # write back as a csv in the dir
