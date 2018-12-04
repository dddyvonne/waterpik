library(shiny)
library(shinyjs)
library(shinyWidgets)
library(leaflet)
library(data.table)
library(htmltools)
library(highcharter)
library(dplyr)
library(tools)
library(DT)
library(tidyr)
library(openintro)
library(stringr)

rm(list = ls())

idn_df <- fread('data/quva_idn_dashboard.csv', data.table = F)
hospitals_df <- fread('data/clean_up_offices.csv', data.table = F)
rating <- fread('data/office_rating.csv', data.table = F) 
hospitals_df <- merge(x = hospitals_df, y = rating, by = "id", all.x = TRUE)
hospitals_df[is.na(hospitals_df)] <- "D"
hospitals_df <- rename(hospitals_df, "Score" = "Tier")
hospitals_df <- hospitals_df[with(hospitals_df, order(hospitals_df$Score,hospitals_df$name)),]
dentist_df <- fread('data/dentist.csv', data.table = F)
dentist_df <- rename(dentist_df, "dentist name" = "name")
dentist_df <- rename(dentist_df,'Office Name' = 'officeName')
dentist_df <- dentist_df[with(dentist_df, order(dentist_df$`dentist name`,dentist_df$`Office Name`)),]
dentist_df$Address <- apply(dentist_df[ , c(6:8)] , 1 , paste , collapse = "-" )
# Contact Information
hospital_contacts <- fread('data/Hospitals_Contacts.csv', data.table = F)
idn_contacts <- fread('data/IDN_Contacts.csv')

# Separate Information
hospital_contacts <- separate_rows(hospital_contacts, Executive_Name, Email, Title, sep = '\\|')
idn_contacts <- separate_rows(idn_contacts, Executive_Name, Email, Title, sep = '\\|')

idn_df$`QuVA Customer` <- if_else(idn_df$`QuVA Customer`, 'Yes', 'No')
dentist_df <- rename(dentist_df,
                 `Dentist Name` = `dentist name`,
                 `Registration` = `registration`,
                 `Membership Status` = `isMember`,
                 `Phone` = `phone`)#clinics_df$`QuVa Customer` <- if_else(clinics_df$`QuVa Customer`, 'Yes', 'No')

dentist_df <- dentist_df %>%
              select(`Dentist Name`, `Office Name`, Address, Phone)





idn_df <- rename(idn_df,
                 `Sales Rep` = `Sales Rep. Assignment`,
                 `Total Beds` = `IDN Total Beds`)

# Join Sales Person by IDN ID
#hospitals_df <- left_join(hospitals_df, idn_df[, c('Definitive IDN ID', 'Sales Rep')])
#hospitals_df$`Sales Rep`[is.na(hospitals_df$`Sales Rep`)] <- ''

#states <- sort(unique(hospitals_df$State))[-1]
#states <- abbr2state(states)

#sales_reps <- sort(unique(idn_df$`Sales Rep`))[c(-2:-3, -9)]
#sales_reps <- c(sales_reps, sort(unique(idn_df$`Sales Rep`))[c(9, 2:3)])

# Order Dfs correctly for table
hospitals_df <- hospitals_df %>%
  #mutate(City = toTitleCase(City)) %>%
  rename(Name = name) %>%
  rename(Address = address) %>%
  rename(Phone = phone) %>%
  rename(Categories = categories) %>%
  select(Score,Name, Address,Phone,Categories,longitude,latitude)

# Rename Columns
names(hospital_contacts) <- gsub('_', ' ', names(hospital_contacts))
names(idn_contacts) <- gsub('_', ' ', names(idn_contacts))

idn_contacts <- rename(idn_contacts, IDN = `Hospital Name`, Name = `Executive Name`)
hospital_contacts <- rename(hospital_contacts, Name = `Executive Name`)
