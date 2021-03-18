##This is just reduced version of sample code (won't need to change to merge, just use code block you have)
SALT <- salt_raw %>% #
  select(Report.Date,Jurisdiction,Tests.Performed,Positive.Test.Results,Percent.Positive.Test.Results) %>%
  rename(tests_performed=Tests.Performed,
         positive_tests=Positive.Test.Results,
         percent_positive=Percent.Positive.Test.Results) %>%
  mutate(Date = as.Date(str_sub(Report.Date, 1, 10)),
         positive_tests = ifelse (!is.na(positive_tests), positive_tests, round(tests_performed*(percent_positive/100))),  #some PTs (AB, ON) only report % positive
         percent_positive = ifelse (!is.na(percent_positive), percent_positive, round((positive_tests/tests_performed)*100, digits = 3))) %>%
  mutate(percent_positive = ifelse(tests_performed<0|positive_tests<0, 0, percent_positive),
         tests_performed = ifelse(tests_performed<0, 0, tests_performed),
         positive_tests = ifelse(positive_tests<0, 0, positive_tests))

#Temp dataset that serves as placeholder; every jurisdiction has response for every date in data 
#(IE/creating rows for PTs that didn't submit data for most recent dates)
temp_exp <- SALT %>% expand(Jurisdiction, Date)

#Expanding the dataset using the temp dataset
DailySALT <- SALT %>% dplyr::right_join(temp_exp) %>%
  arrange(Jurisdiction, Date) %>%
  group_by(Jurisdiction) %>% 
  mutate(cum_tests_performed=cumsum(tests_performed),
         cum_positive_tests=cumsum(positive_tests)) %>%
  ungroup() %>%
  tidyr::fill(cum_tests_performed, cum_positive_tests) %>%
  mutate(value_carried_forward = ifelse(is.na(tests_performed), 1, NA)) %>%
  mutate(across(c(tests_performed, positive_tests, percent_positive), ~ifelse(is.na(value_carried_forward), ., 0))) %>%
  add_row(Jurisdiction="Repatriated", tests_performed=76, positive_tests=13, percent_positive=17.105,
          Date=as.Date("2020-01-01"), cum_tests_performed=76, cum_positive_tests=13, value_carried_forward=NA)


#Repatriated after Jan 1, 2020 will have no incremental values and just cumulative carried forward, doing this in 
#temp dataset
temp_exp2 <- DailySALT %>% expand(Jurisdiction, Date) %>% 
  filter(Jurisdiction=="Repatriated" & Date!="2020-01-01") %>%
  mutate(tests_performed=0, positive_tests=0, percent_positive=0, cum_tests_performed=76, cum_positive_tests=13, value_carried_forward=1)

#Appending repatriated dataset (all but Jan 1 2020 observation) to the original dataset
DailySALT <- DailySALT %>% 
  bind_rows(temp_exp2)

#Aggregating daily values for all of Canada numbers
Canada <- DailySALT %>%
  group_by(Date) %>%
  summarise(across(c(tests_performed, positive_tests, cum_tests_performed, cum_positive_tests),sum),
            .groups="drop_last") %>%
  mutate(Jurisdiction="Canada",
         percent_positive = round((positive_tests/tests_performed)*100, digits=3)) %>%
  ungroup()

DailySALT <- DailySALT %>% 
  bind_rows(Canada) %>%
  mutate(SortVar=factor(Jurisdiction, 
                        levels=c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario",
                                 "Quebec", "Newfoundland and Labrador", "New Brunswick", "Nova Scotia",
                                 "Prince Edward Island", "Yukon", "Northwest Territories", "Nunavut",
                                 "Repatriated", "Canada"), ordered=TRUE)) %>%
  arrange(Date, SortVar)

#Only keeping last 30 days of data for the exports; eliminates concerns with arbitrarily assigned repatriated 
#numbers earlier in pandemic
DailySALT_Incremental <- DailySALT %>% filter(Date>=(max(Date)-30))%>% select(Jurisdiction, tests_performed, positive_tests, percent_positive, Date)
DailySALT_Cumulative <- DailySALT %>% filter(Date>=(max(Date)-30)) %>% select(Jurisdiction, cum_tests_performed, cum_positive_tests, Date, value_carried_forward)

############################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################## PCO TESTING DATA ############################################################################################################################################

Today<-toupper(format(Sys.Date(),"%d%b%Y"))
report_date<-Sys.Date() #this returns today's date

SALT <- salt_raw %>%
  clean_names() %>%
  select(report_date,jurisdiction,tests_performed,positive_test_results,percent_positive_test_results) %>%
  rename(Jurisdiction= jurisdiction,
         positive_tests=positive_test_results,
         percent_positive=percent_positive_test_results,
         test_report_date=report_date) %>%
  mutate(Date = as.Date(str_sub(test_report_date, 1, 10)),
         Time = as_hms(str_sub(test_report_date, 13, 20)),
         datetime = strptime(paste(Date, Time), "%Y-%m-%d%H:%M:%S"),
         positive_tests = ifelse (!is.na(positive_tests), positive_tests, round(tests_performed*(percent_positive/100))),  #some PTs (AB, ON) only report % positive
         percent_positive = ifelse (!is.na(percent_positive), percent_positive, round((positive_tests/tests_performed)*100, digits = 3)))

#because SK sometimes submits same-day lab testing data, can't rely on using max(Date), instead will do based on report_date, which is Sys.Date()
SALT2 <- SALT %>%
  filter(Date>=report_date-8 & Date<report_date-1) %>%
  arrange(Jurisdiction,datetime)

Provincial <- SALT2 %>%
  mutate(confirmed_negative_tests = tests_performed - positive_tests) %>%
  left_join(latest_can_pop, by="Jurisdiction") #left join will return all rows in SALT2 and all columns from both SALT2 and latest_can_pop.

PT_cab_table <- Provincial %>%
  group_by(Jurisdiction, Population) %>%
  summarise(total_days_reported=n(),
            Total_tests_week = sum(tests_performed),
            Total_pos_week = sum(positive_tests),
            Total_neg_week = sum(confirmed_negative_tests),
            .groups="drop_last") %>%
  mutate(Percent_pos_week=round(Total_pos_week/Total_tests_week,digits = 4),
         Avg_tests_per_day=round(Total_tests_week/total_days_reported,digits = 4),
         Num_neg_per_pos=ifelse(Total_pos_week %in% c(NA,0),NA,round(Total_neg_week/Total_pos_week,digits = 4)),
         Avg_tests_per_1000pop = round(Avg_tests_per_day/Population*1000,digits = 4)) %>%
  ungroup()

#setting national pop to actual value, as issues when summing all PTs when some don't report up to latest date
canada_pop<-latest_can_pop$Population[latest_can_pop$Jurisdiction=="Canada"]

#get Canadian totals by summing all the provinces
National <- PT_cab_table %>%
  select(Jurisdiction, Total_tests_week, Total_pos_week, Total_neg_week, Avg_tests_per_day) %>%
  summarise(across(where(is.numeric),sum), #calculate the sum of numeric variables listed in the SELECT statement
            .groups="drop_last") %>%
  mutate(Population=canada_pop,
         Jurisdiction="Canada",
         Percent_pos_week = Total_pos_week/Total_tests_week,
         Avg_tests_per_1000pop = Avg_tests_per_day/Population*1000,
         Num_neg_per_pos = Total_neg_week/Total_pos_week ) %>%
  select(Jurisdiction, Population,Total_tests_week, Total_pos_week, Percent_pos_week, Num_neg_per_pos, Avg_tests_per_day, Avg_tests_per_1000pop)


#format PT_cab_table dataset (containing PT lab testing info) to be same format as national dataset so that it can be combined
PT <- PT_cab_table %>%
  select(Jurisdiction, Population,Total_tests_week, Total_pos_week, Percent_pos_week, Num_neg_per_pos, Avg_tests_per_day, Avg_tests_per_1000pop)

juriorder <- c("British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec","Newfoundland and Labrador","New Brunswick","Nova Scotia","Prince Edward Island","Yukon","Northwest Territories","Nunavut","Canada")

Cabinet_table_unformatted <- rbind(PT,National) %>%
  group_by(Jurisdiction)

Cabinet_table_formatted_old<-Cabinet_table_unformatted%>%
  mutate(Avg_tests_per_day=Total_tests_week/7,
         Avg_tests_per_1000pop=Avg_tests_per_day/Population*1000) %>%
  mutate(Percent_pos_week=ifelse(round(Percent_pos_week,digits=4) < 0.001, percent(Percent_pos_week,accuracy = 0.01), percent(Percent_pos_week,accuracy = 0.1)),
         Num_neg_per_pos = number(Num_neg_per_pos,big.mark = " " ,accuracy = 0.1),
         Avg_tests_per_day = number(Avg_tests_per_day,big.mark = " " ,accuracy = 1),
         Avg_tests_per_1000pop = number(Avg_tests_per_1000pop,big.mark = " " ,accuracy = 0.01),
         Population = number(Population,big.mark = " "),
         Interpretation=ifelse(Total_pos_week %in% c(NA,0), "No new cases in past 7 days", paste("One positive for every ", Num_neg_per_pos, " tests conducted in the past 7 days", sep="")),
         Total_tests_week = number(Total_tests_week,big.mark = " "),
         Total_pos_week = number(Total_pos_week,big.mark = " ") ,
         Jurisdiction = factor(Jurisdiction,levels = juriorder)) %>%
  arrange(Jurisdiction) %>%
  select(Jurisdiction, Population,Total_tests_week, Total_pos_week, Percent_pos_week, Num_neg_per_pos, Interpretation,Avg_tests_per_day, Avg_tests_per_1000pop) %>%
  rename(`Total tests performed in the past 7 days`=Total_tests_week,
         `Total Positive CASES reported in the past 7 days`=Total_pos_week,
         `Percent test positives in the past 7 days`=Percent_pos_week,
         `Number of NEGATIVE tests for each POSITIVE test`=Num_neg_per_pos,
         `Average number of tests performed per day (past 7 days)`= Avg_tests_per_day,
         `Average number of tests performed per day per 1,000 population (past 7 days)` = Avg_tests_per_1000pop)

############################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################## WEEKLY TESTING DATA #########################################################################################################################################

SALT_weekly <- salt_raw %>%
  select(Report.Date,Jurisdiction,Tests.Performed,Positive.Test.Results,Percent.Positive.Test.Results, Latest.Update.Date) %>%
  rename(tests_performed=Tests.Performed,
         positive_tests=Positive.Test.Results,
         percent_positive=Percent.Positive.Test.Results) %>%
  mutate(update_date = as.Date(str_sub(Latest.Update.Date, 1, 10)),
         Date = as.Date(str_sub(Report.Date, 1, 10)),
         Time = as_hms(str_sub(Report.Date, 13, 20)),
         datetime = strptime(paste(Date, Time), "%Y-%m-%d%H:%M:%S"),
         positive_tests = ifelse (!is.na(positive_tests), positive_tests, round(tests_performed*(percent_positive/100))),  #some PTs (AB, ON) only report % positive
         percent_positive = ifelse (!is.na(percent_positive), percent_positive, round((positive_tests/tests_performed)*100, digits = 3)))

SALT_weekly2 <- SALT_weekly %>%
  select(-Latest.Update.Date,-update_date)%>%
  mutate(Start_of_week=floor_date(Date, "week"),
         End_of_week=date(Start_of_week)+6,
         Week=paste(str_sub(months(Start_of_week),1,3),"-",day(Start_of_week), " to ", str_sub(months(End_of_week),1,3),"-",day(End_of_week)),
         Week_before=paste(str_sub(months(date(Start_of_week)-7),1,3),"-",day(date(Start_of_week)-7), " to ", str_sub(months(date(End_of_week)-7),1,3),"-",day(date(End_of_week)-7))) %>%
  filter(Date <= floor_date(max(Date), "week")-1) %>%
  mutate(Current_week=ifelse(date(Date)+7 <= max(Date),"No","Yes")) %>%
  filter(Date>="2021-01-23") %>% #Issues with historical data missing for some PTs - only taking last two weeks data for now.
  arrange(Jurisdiction,datetime)

SALT_weekly3 <- SALT_weekly2 %>%
  group_by(Jurisdiction,Week, Start_of_week) %>%
  summarise(days_reported=n(),
            week_tests_performed=sum(tests_performed),
            week_positive_tests=sum(positive_tests),
            .groups="drop_last") %>%
  mutate(week_negative_tests=week_tests_performed-week_positive_tests,
         week_percent_positive=round(week_positive_tests/week_tests_performed,digits = 4),
         avg_tests_per_day=round(week_tests_performed/days_reported))

National_weekly <- SALT_weekly3 %>%
  select(Week, Jurisdiction, Start_of_week, week_tests_performed, week_positive_tests, week_negative_tests, days_reported, avg_tests_per_day) %>%
  group_by(Week, Start_of_week) %>%
  summarise(across(where(is.numeric),sum),
            .groups="drop_last") %>%
  mutate(Jurisdiction="Canada",
         week_percent_positive = week_positive_tests/week_tests_performed) %>%
  arrange(Start_of_week) %>%
  select(Week, Jurisdiction, week_tests_performed, week_positive_tests, week_negative_tests, week_percent_positive, avg_tests_per_day, days_reported)

Provincial_weekly <- SALT_weekly3 %>%
  arrange(Start_of_week) %>%
  select(Week, Jurisdiction, week_tests_performed, week_positive_tests, week_negative_tests, week_percent_positive, avg_tests_per_day, days_reported)

Testing_weekly <- rbind(National_weekly,Provincial_weekly) %>%
  tidyr::drop_na() %>%
  mutate(Week=gsub(" -","",Week)) %>%
  mutate(Week=gsub("  to  Jan ","-",Week)) %>%
  mutate(Week=gsub("  to  Feb ","-",Week)) %>%
  mutate(Week=gsub("  to  Mar ","-",Week)) %>%
  mutate(Week=gsub("  to  Apr ","-",Week)) %>%
  mutate(Week=gsub("  to  May ","-",Week)) %>%
  mutate(Week=gsub("  to  Jun ","-",Week)) %>%
  mutate(Week=gsub("  to  Jul ","-",Week)) %>%
  mutate(Week=gsub("  to  Aug ","-",Week)) %>%
  mutate(Week=gsub("  to  Sep ","-",Week)) %>%
  mutate(Week=gsub("  to  Oct ","-",Week)) %>%
  mutate(Week=gsub("  to  Nov ","-",Week)) %>%
  mutate(Week=gsub("  to  Dec ","-",Week)) %>%
  group_by(Jurisdiction) %>%
  mutate(Week_no = 1:n()) %>%
  slice(tail(row_number(),12)) %>%
  select(Week_no, Week, Jurisdiction, week_tests_performed,week_positive_tests,week_negative_tests,week_percent_positive,avg_tests_per_day, days_reported)

Testing_weekly2 <- Testing_weekly %>%
  group_by(Jurisdiction) %>%
  slice(tail(row_number(),2)) %>%
  mutate(this_week=max(Week_no),
         week_label=ifelse(Week_no==this_week, "thisweek","lastweek")) %>%
  select(Jurisdiction,week_label,avg_tests_per_day,week_percent_positive) %>%
  pivot_longer(cols = c("avg_tests_per_day","week_percent_positive"),
               names_to="type",
               values_to="value") %>%
  pivot_wider(names_from = c(week_label, type),
              names_glue= "{type}_{week_label}" ,
              values_from=value) %>%
  mutate(change_in_tests=(avg_tests_per_day_thisweek-avg_tests_per_day_lastweek)/avg_tests_per_day_lastweek,
         change_in_positivity=(week_percent_positive_thisweek-week_percent_positive_lastweek)/week_percent_positive_lastweek) %>%
  select(Jurisdiction,week_percent_positive_thisweek)

this_week_num<-max(Testing_weekly$Week_no)
this_week_label<-unique(Testing_weekly$Week[Testing_weekly$Week_no==this_week_num])
last_week_num<-max(Testing_weekly$Week_no)-1
last_week_label<-unique(Testing_weekly$Week[Testing_weekly$Week_no==last_week_num])

Testing_weekly3 <- Testing_weekly2 %>%
  mutate(week_percent_positive_thisweek=percent(week_percent_positive_thisweek,accuracy=0.1)) %>%
  rename(!!paste0("Percent Positivity (",this_week_label,")") := week_percent_positive_thisweek)