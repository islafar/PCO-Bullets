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
  filter(Date>=report_date-7 & Date<report_date) %>%
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

#write_xlsx(list(DailySALT_Incr=DailySALT_Incremental, DailySALT_Cum=DailySALT_Cumulative, PCO_Data=Cabinet_table_formatted_old), paste0("//Ncr-a_irbv2s/irbv2/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/DATA AND ANALYSIS/NML TESTING DATA/SALT/Output/", Today, "TestingNum.xlsx"), format_headers = FALSE )

#rm(list = setdiff(ls(), c("Cabinet_table_unformatted","Cabinet_table_formatted_old","DailySALT_Cumulative",
#                          "DailySALT_Incremental")))