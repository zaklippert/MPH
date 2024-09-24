library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(reshape2)
library(xlsx)
library(ggplot2)
library(descr)
library(janitor)
library(purrr)
library(stringr)

setwd('C:/Users/Zak_laptop/Documents/UNMC/APEX')

fields_old <- c('first','last','city','state','zipcode','cell or other contact #','donation','date')
fields_new <- c('first','last','city','state','zipcode','in honor/memory','donation','date')



d21_checks <- read_excel('2021 Donations.xlsx', sheet = 'Checks')

d21_paypal <- read_excel('2021 Donations.xlsx', sheet = 'Paypal')
d21_website <- read_excel('2021 Donations.xlsx', sheet = 'Website')
d21_network <- read_excel('2021 Donations.xlsx', sheet = 'NetworkForGood')
d21_amazon <- read_excel('2021 Donations.xlsx', sheet = 'AmazonSmile')
d21_frontdoor <- read_excel('2021 Donations.xlsx', sheet = 'FrontDoor')
d21_frontdoordetails <- read_excel('2021 Donations.xlsx', sheet = 'FrontDoor details')
d21_yourcause <- read_excel('2021 Donations.xlsx', sheet = 'YourCause')
d21_benevity <- read_excel('2021 Donations.xlsx', sheet = 'Benevity')
d21_runsignup <- read_excel('2021 Donations.xlsx', sheet = 'RunSignUpDtn')
d21_runsignupreg <- read_excel('2021 Donations.xlsx', sheet = 'RunSignUpReg')
d21_other <- read_excel('2021 Donations.xlsx', sheet = 'Other')
d21_5k <- read_excel('2021 Donations.xlsx', sheet = '5K+')
d21_bialon <- read_excel('2021 Donations.xlsx', sheet = 'Bialon Memorial')


donations_2011 <- read_excel('Donations 2011-2020.xlsx', sheet = '2011')
colnames(donations_2011) <- tolower(colnames(donations_2011))
#donations_2011$date[is.na(donations_2011$date)] <- 0
#donations_2011$date <- as.Date(as.numeric(donations_2011$date), origin = '1900-01-01')-2
donations_2011 <- donations_2011[,fields_old]


donations_2012 <- read_excel('Donations 2011-2020.xlsx', sheet = '2012')
colnames(donations_2012) <- tolower(colnames(donations_2012))
#donations_2012$date[is.na(donations_2012$date)] <- 0
#donations_2012$date <- as.Date(as.numeric(donations_2012$date), origin = '1900-01-01')-2
donations_2012 <- donations_2012[,fields_old]

donations_2013 <- read_excel('Donations 2011-2020.xlsx', sheet = '2013')
colnames(donations_2013) <- tolower(colnames(donations_2013))
donations_2013$date[is.na(donations_2013$date)] <- 0
donations_2013$date <- as.Date(as.numeric(donations_2013$date), origin = '1900-01-01')-2
donations_2013 <- donations_2013[,fields_new]

donations_2013new <- read_excel('Donations 2011-2020.xlsx', sheet = '2013_New')
colnames(donations_2013new) <- tolower(colnames(donations_2013new))
donations_2013new <- donations_2013new[,fields_new]

donations_2014 <- read_excel('Donations 2011-2020.xlsx', sheet = '2014')
colnames(donations_2014) <- tolower(colnames(donations_2014))
donations_2014 <- donations_2014[,fields_new]

donations_2015 <- read_excel('Donations 2011-2020.xlsx', sheet = '2015')
colnames(donations_2015) <- tolower(colnames(donations_2015))
donations_2015 <- donations_2015[,fields_new]

donations_2016 <- read_excel('Donations 2011-2020.xlsx', sheet = '2016')
colnames(donations_2016) <- tolower(colnames(donations_2016))
donations_2016$date[is.na(donations_2016$date)] <- 0
donations_2016$date <- as.Date(as.numeric(donations_2016$date), origin = '1900-01-01')-2
donations_2016 <- donations_2016[,fields_new]

donations_2017 <- read_excel('Donations 2011-2020.xlsx', sheet = '2017')
colnames(donations_2017) <- tolower(colnames(donations_2017))
donations_2017 <- donations_2017[,fields_new]

donations_2018 <- read_excel('Donations 2011-2020.xlsx', sheet = '2018')
colnames(donations_2018) <- tolower(colnames(donations_2018))
donations_2018 <- donations_2018[,fields_new]

donations_2019 <- read_excel('Donations 2011-2020.xlsx', sheet = '2019')
colnames(donations_2019) <- tolower(colnames(donations_2019))
donations_2019 <- donations_2019[,fields_new]

donations_2020 <- read_excel('Donations 2011-2020.xlsx', sheet = '2020')
colnames(donations_2020) <- tolower(colnames(donations_2020))
donations_2020$date[is.na(donations_2020$date)] <- 0
donations_2020$date <- as.Date(as.numeric(donations_2020$date), origin = '1900-01-01')-2
donations_2020 <- donations_2020[,fields_new]

donation_list_old <- list(donations_2011,donations_2012)

donation_list <- list(donations_2013, donations_2013new,donations_2014,donations_2015, donations_2016,
                      donations_2017,donations_2018, donations_2019, donations_2020)


donations_old <- do.call("rbind", donation_list_old)
names(donations_old)[6] <- 'in honor/memory'

donations_new <- do.call("rbind", donation_list)

don_all <- rbind(donations_old, donations_new)

don_all$year  <- year(don_all$date)
don_all$month <- month(don_all$date)
don_all$qtr <- ceiling(don_all$month/3)

don_all <- don_all[don_all$year>=2000,]
don_all$donation[is.na(don_all$donation)] <- 0
don_all$donation <- as.numeric(don_all$donation)

donations_by_date <- don_all %>%
  group_by(year, month) %>%
  summarise(total = sum(donation))

write.csv(donations_by_date, 'donationsbydate.csv')

dropwords <- c('memory', 'honor', 'for' , 'in')

don_all$`in honor/memory` <- tolower(don_all$`in honor/memory`)
don_all$memory <- ifelse(grepl("memory of ", don_all$`in honor/memory`, fixed = TRUE), sub(".*memory of ", "", don_all$`in honor/memory`),"")
don_all$honor <- ifelse(grepl("honor of ", don_all$`in honor/memory`, fixed = TRUE), sub(".*honor of ", "", don_all$`in honor/memory`),"")
don_all$fundraiser <- ifelse(grepl("fundraiser for ", don_all$`in honor/memory`, fixed = TRUE), sub(".*fundraiser for ", "", don_all$`in honor/memory`),"")
don_all$gift <- ifelse(grepl("giving - ", don_all$`in honor/memory`, fixed = TRUE), sub(".*giving - ", "", don_all$`in honor/memory`),"")
don_all$other <- trimws(gsub(paste(dropwords, collapse = "|"), "", don_all$`in honor/memory`))

don_all$honoree <- trimws(paste(don_all$memory, don_all$honor, don_all$fundraiser, don_all$gift) )
don_all$mem_len <- nchar(don_all$memory)
don_all$honoree <- ifelse(don_all$mem_len == 0, don_all$other, don_all$honoree)

don_all$donor <- paste0(don_all$first," ", don_all$last)
don_all$index <- 1

don_all$type <- ifelse(don_all$mem_len > 0, "memory", ifelse(nchar(don_all$honor) > 0, "honor",'other'))

first_event <- don_all %>%
  group_by(donor) %>%
  summarise(
  first_donation = min(date, na.rm = T),
  total_donations = sum(index, na.rm = T)
  )

events <- merge(don_all, first_event, by = ('donor'))

events$base_donation <- ifelse(events$date == events$first_donation,1,0)

events2 <- events %>% 
  arrange(donor, date) %>%
  group_by(donor) %>%
  mutate(diff = date - lag(date))

events2$diff <- events2$diff/86400
events2$diff[is.na(events2$diff)] <- 0

write.csv(events2, "first_event.csv")





