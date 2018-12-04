library(dplyr)
hospitals_df <- fread('data/clean_up_offices.csv', data.table = F)
dentist_df <- fread('data/dentist.csv', data.table = F)
dentist_df <- rename.vars(dentist_df, from = "", to = "newname")
dentist_df <- rename(dentist_df, "dentist name" = "name")
dentist_df <- rename(dentist_df,'name' = 'officeName')
complete_df <- merge(hospitals_df, dentist_df,by = "name")

dentist_df %>% 
  rename(
    name = dentist,
    OfficeName = name
  )
