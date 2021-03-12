gs4_deauth()
daily_numbers <- read_sheet("https://docs.google.com/spreadsheets/d/1ad7-09_Jn6AxsdkVPE33T-iLfGpPRmd3piXQqFiVeas/edit#gid=0",
                            sheet = "PT Data", col_names = TRUE) %>%
  select(pruid, prname, date, update, numtotal, numrecover, numdeaths, numtoday, numdeathstoday, numactive) %>%
  mutate(date=as.Date(date),"EST")

LTC <- read_sheet("https://docs.google.com/spreadsheets/d/1pI6EdBBilNO6Ykq7HNMr_gKKx02SGiXB6pTiJKn5VD4/edit#gid=0",
                  sheet = "LTC", range = "b3:m17", col_names = TRUE)

salt_raw <- read.csv("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/SALT/Submitted+Reports.csv")

#load('Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/PROCEDURES/Templates/12. 8 pm PCO Bullets/latest_can_pop.rda')
load('GitHub/PCO-Bullets/latest_can_pop.rda')

prorder1 <- c("British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec","Newfoundland and Labrador","New Brunswick","Nova Scotia","Prince Edward Island","Yukon","Northwest Territories","Nunavut","Repatriated travellers")
prorder2 <- c("BC","AB","SK","MB","ON","QC","NL","NB","NS","PE","YT","NT","NU","CA")