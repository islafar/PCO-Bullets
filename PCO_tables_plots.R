#Table 1
table1a <- daily_numbers %>%
  select(prname,date,numtotal,numtoday,numactive,numdeaths,numdeathstoday) %>%
  left_join(latest_can_pop, by = c("prname"="Jurisdiction")) %>%
  arrange(prname,date) %>%
  mutate(caseper = round((numtoday/Population)*100000,digits=1)) %>%
  filter(date==max(date)) #%>%
#  mutate(numtotal=comma(numtotal,accuracy=1),
#         numtoday=comma(numtoday,accuracy=1),
#         numactive=comma(numactive,accuracy=1),
#         numdeaths=comma(numdeaths,accuracy=1),
#         numdeathstoday=comma(numdeathstoday,accuracy=1))

table1 <- table1a %>%
  left_join(DailySALT_Cumulative %>% filter(Date==max(Date)) %>% select(-Date,-value_carried_forward), 
            by=c("prname"="Jurisdiction")) %>%
  mutate(percentpos=cum_positive_tests/cum_tests_performed) %>%
  mutate(testrateper=round(cum_tests_performed/Population*1000)) %>%
  left_join(Cabinet_table_unformatted %>% select(Jurisdiction,Num_neg_per_pos,Avg_tests_per_day,Avg_tests_per_1000pop),
            by=c("prname"="Jurisdiction")) %>%
  select(-date,-Population) %>%
  #subset(prname!="Canada") %>%
 # adorn_totals("row") %>%
  mutate(numtotal=comma(numtotal,accuracy=1),
         numtoday=comma(numtoday,accuracy=1),
         numactive=comma(numactive,accuracy=1),
         numdeaths=comma(numdeaths,accuracy=1),
         numdeathstoday=comma(numdeathstoday,accuracy=1)) %>%
  mutate(cum_tests_performed=comma(cum_tests_performed)) %>%
  mutate(Num_neg_per_pos=comma(Num_neg_per_pos,accuracy = 0.1)) %>%
  mutate(Avg_tests_per_day=comma(round(Avg_tests_per_day))) %>%
  mutate(Avg_tests_per_1000pop=round(Avg_tests_per_1000pop,digits=2)) %>%
  mutate(percentpos=percent(percentpos,accuracy=0.1)) %>%
  rename(Location=prname,"Total cases"=numtotal,"New cases reported in the last 24 hours"=numtoday,
         "Rate of new daily infections per 100,000"=caseper,"Active cases"=numactive,
         "Total deaths"=numdeaths,"Total tests performed"=cum_tests_performed,
         "Cumulative percent of test positives"=percentpos,
         "Rate of tests performed per thousand"=testrateper,
         "Number of negative tests per positive (last 7 days)"=Num_neg_per_pos,
         "Average number of tests performed per day for last 7 days"=Avg_tests_per_day,
         "Average number of tests performed per day for last 7 days per thousand"=Avg_tests_per_1000pop) %>%
  select(-numdeathstoday,-cum_positive_tests) 

table1 <- table1[c(1,2,3,6,4,5,7,8,9,10,11,12)]
table1 <- table1[order(factor(table1$Location,levels=c(prorder1))),]
table1$Location[table1$Location == "Canada"] <- "Total"

#Table 2
table2 <- daily_numbers %>%
  select(prname,date,numtotal,numtoday) %>%
  mutate(numtotal=comma(numtotal,accuracy=1)) %>%
  mutate(numtoday=comma(numtoday,accuracy=1)) %>%
  arrange(prname,date) %>%
  group_by(prname) %>%
  slice(tail(row_number(),7)) %>%
  #subset(.,prname!="Canada") %>%
  rename(Location=prname,"Total cases"=numtotal,"Total new cases"=numtoday) %>%
  reshape(.,direction="long",varying=c("Total cases","Total new cases"),v.names="Val",
          idvar=c("Location","date"),times=c("Total cases","Total new cases")) %>%
  arrange(Location,date) %>%
  mutate(date=lapply(date, function(x) format(as.Date(x),"%B %d, %Y"))) %>%
  mutate(newdate=paste0(time,", ",date)) %>%
  select(Location,newdate,Val) %>%
  pivot_wider(names_from=newdate,values_from=Val)

table2 <- table2[order(factor(table2$Location,levels=c(prorder1))),]
table2$Location[table2$Location == "Canada"] <- "Total"

#Table 3
table3 <- daily_numbers %>%
  filter(date==max(date)) %>%
  select(prname,numdeaths) %>%
  left_join(latest_can_pop, by=c("prname"="Jurisdiction")) %>%
  mutate(prname=recode(prname,"British Columbia"="BC",
                        "Alberta" = "AB",
                        "Saskatchewan"="SK",
                        "Manitoba"="MB",
                        "Ontario"="ON",
                        "Quebec"="QC",
                        "Newfoundland and Labrador"="NL",
                        "New Brunswick"="NB",
                        "Nova Scotia"="NS",
                        "Prince Edward Island"="PE",
                        "Yukon"="YT",
                        "Northwest Territories"="NT",
                        "Nunavut"="NU",
                        "Canada"="CA",
                        "Repatriated travellers"="Repatriated travellers")) %>%
  left_join(LTC %>% select(`P/T`,`# Deaths in LTC`), by=c("prname"="P/T")) %>%
  mutate(numdeaths=comma(numdeaths,accuracy=1)) %>%
  mutate("# Deaths in LTC"=comma(`# Deaths in LTC`,accuracy=1)) %>%
  mutate(Population=comma(Population,accuracy=1)) %>%
  rename(Province=prname,"Total Deaths"=numdeaths,"Total Deaths in LTC Facilities"="# Deaths in LTC") %>%
  subset(., Province != "Repatriated travellers") %>%
  mutate(Province=factor(Province,levels=c(prorder2)))

table3 <- table3[c(1,3,2,4)]

# Create graphs
plot1 <- ggplot(daily_numbers %>% filter(prname=="Canada") %>% slice(tail(row_number(),14)),
         aes(x=date,y=numtoday)) +
  geom_line(colour="blue",size=2) +
  geom_point(colour="blue",size=4) +
  labs(title = "Daily number of reported new COVID-19 cases in Canada",subtitle="last 14 days",
       x="Report Date",y="Number of reported new cases") +
  scale_x_date(breaks="1 day", labels = date_format("%b-%d")) +
  scale_y_continuous(label=comma,limits=c(0,round(max(daily_numbers$numtoday))), 
                     breaks=trans_breaks(identity,identity,n=10)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45,vjust=.5))
  
plot2 <- ggplot(daily_numbers %>% filter(prname=="Canada") %>% slice(tail(row_number(),14)), 
       aes(x=date,y=numdeathstoday)) +
  geom_line(colour="red",size=2) +
  geom_point(colour="red",size=4) +
  labs(label=comma,title = "Daily number of reported new COVID-19 deaths in Canada",subtitle="last 14 days",
       x="Report Date",y="Number of reported new deaths") +
  scale_x_date(breaks="1 day", labels = date_format("%b-%d")) +
  scale_y_continuous(limits=c(0,round(max(daily_numbers$numdeathstoday))), 
                     breaks=trans_breaks(identity,identity,n=6)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45,vjust=.5))

plot3 <- ggplot(daily_numbers %>% filter(prname=="Canada") %>% slice(tail(row_number(),14)), aes(x=date,y=numactive)) +
  geom_line(colour="darkgreen",size=2) +
  geom_point(colour="darkgreen",size=4) +
  labs(title = "Daily total count of reported COVID-19 active cases in Canada",subtitle="last 14 days",
       x="Report Date",y="Number of reported active cases") +
  scale_x_date(breaks="1 day", labels = date_format("%b-%d")) +
  scale_y_continuous(label=comma,limits=c(0,round(max(daily_numbers$numactive))), 
                     breaks=trans_breaks(identity,identity,n=5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45,vjust=.5))