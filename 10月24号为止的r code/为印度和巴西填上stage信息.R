library(readxl)
india <- read_excel('/Users/qidiwang1/Desktop/India/India_data.xlsx',sheet = 'Sheet1')
stage <- read_excel('/Users/qidiwang1/Desktop/India/India_stage.xlsx')
stage$Response <- as.Date(stage$Response)
stage$Trough <- as.Date(stage$Trough)
stage$Recovery <- as.Date(stage$Recovery)
india$Date <- as.Date(india$Date)

for (i in c(1:nrow(stage))){
  india$PET_Phase[india$State==stage$State[i] & india$Date<stage$Response[i]] <- 'Preparation'
  india$PET_Phase[india$State==stage$State[i] & india$Date>=stage$Response[i]] <- 'Response'
  india$PET_Phase[india$State==stage$State[i] & india$Date>=stage$Trough[i]] <- 'Trough'
  india$PET_Phase[india$State==stage$State[i] & india$Date>=stage$Recovery[i]] <- 'Recovery'
}
india_data <- data.frame(Date = india$Date, State = india$State, Total_cases = india$Total_cases, Total_deaths = india$Total_deaths,
                         Total_recoveries = india$Total_recoveries, New_cases = india$New_cases, New_deaths = india$New_deaths, New_recoveries = india$New_recoveries,
                         Population = india$Population, ISO = india$ISO, Mobility = india$Mobility, Economic_Activity = india$Economic_Activity,
                         Doubling_Days = india$Doubling_Days, PET_Phase = india$PET_Phase)
library(writexl)
write_xlsx(india_data,paste('/Users/qidiwang1/Desktop/PET_data_India_',as.character(max(india$index_dt)),'.xlsx'))

brazil <- read_excel('/Users/qidiwang1/Desktop/Brazil/brazil_data.xlsx')
stage <- read_excel('/Users/qidiwang1/Desktop/Brazil/brazil_stage.xlsx')
brazil$Date <- as.Date(brazil$Date,'%m/%d/%y')
stage$Response <- as.Date(stage$Response)
stage$Trough <- as.Date(stage$Trough)
stage$Recovery <- as.Date(stage$Recovery)

for (i in c(1:nrow(stage))){
  brazil$Stage[brazil$ISO==stage$ISO[i] & brazil$Date<stage$Response[i]] <- 'Preparation'
  brazil$Stage[brazil$ISO==stage$ISO[i] & brazil$Date>=stage$Response[i]] <- 'Response'
  brazil$Stage[brazil$ISO==stage$ISO[i] & brazil$Date>=stage$Trough[i]] <- 'Trough'
  brazil$Stage[brazil$ISO==stage$ISO[i] & brazil$Date>=stage$Recovery[i]] <- 'Recovery'
}
brazil_data <- data.frame(Date = brazil$Date, State = brazil$State, Region = brazil$Region, Total_cases = brazil$Total_cases,
                     Total_deaths = brazil$Total_deaths, New_cases = brazil$New_cases, New_deaths = brazil$New_deaths,
                     Population = brazil$Population, ISO = brazil$ISO, Mobility = brazil$Mobility, Economic_Activiey = brazil$Economic_Activity,
                     Doubling_Days = brazil$Doubling_Days, PET_Phase = brazil$Stage)
write_xlsx(brazil_data,paste('/Users/qidiwang1/Desktop/PET_data_Brazil_',as.character(max(brazil$index_dt)),'.xlsx'))


library(readxl)
mexico <- read_excel('/Users/qidiwang1/Desktop/Mexico/墨西哥数据.xlsx')
stage <- read_excel('/Users/qidiwang1/Desktop/Mexico/Mexico_stage.xlsx')
stage$Response <- as.Date(stage$Response)
stage$Trough <- as.Date(stage$Trough)
stage$Recovery <- as.Date(stage$Recovery)
mexico$Date <- as.Date(mexico$Date)

for (i in c(1:nrow(stage))){
  mexico$PET_Phase[mexico$State==stage$State[i] & mexico$Date<stage$Response[i]] <- 'Preparation'
  mexico$PET_Phase[mexico$State==stage$State[i] & mexico$Date>=stage$Response[i]] <- 'Response'
  mexico$PET_Phase[mexico$State==stage$State[i] & mexico$Date>=stage$Trough[i]] <- 'Trough'
  mexico$PET_Phase[mexico$State==stage$State[i] & mexico$Date>=stage$Recovery[i]] <- 'Recovery'
}

mexico_data <- data.frame(Date = mexico$Date, State = mexico$State, Total_cases = mexico$Total_cases,
                          Total_deaths = mexico$Total_deaths, New_cases = mexico$New_cases, New_deaths = mexico$New_deaths,
                          Population = mexico$Population, ISO = mexico$ISO, Mobility = mexico$ISO,
                          Mobility = mexico$Mobiity, Economic_Activity = mexico$Economic_Activity, Doubling_Days = mexico$Doubling_Days,
                          PET_Phase = mexico$PET_Phase)
library(writexl)
write_xlsx(mexico_data,paste('/Users/qidiwang1/Desktop/Mexico/PET_data_Mexico_',as.character(max(mexico$index_dt)),'.xlsx'))
