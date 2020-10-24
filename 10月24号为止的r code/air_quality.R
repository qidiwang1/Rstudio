gc();rm(list = ls())
setwd("/Users/Yunzki/Dropbox (Personal)/Research/Virus")
source("Codes/functions/helper.R")
library(lubridate)
setwd("/Users/Yunzki/Dropbox (Personal)/Research/Alibaba/LHA/")

flist <- function(x){if(length(x[[1]]) == 0){return(NA)}else{return(x[[1]][1])}}

## load data 
air <- read.csv("RawData/waqi-covid19-airqualitydata.csv", stringsAsFactors = F,
                skip = 6)
policy <- read.csv("RawData/OxCGRT_Download_090420_072357_Full.csv", stringsAsFactors = F)
isocode <- read.csv("RawData/isocode.csv", stringsAsFactors = F)
isocode <- isocode[,c("alpha.2", "alpha.3")]
colnames(isocode) <- c("iso2", "iso3")

## clean data 
policy_clean <- policy[,!grepl("Notes", colnames(policy))] %>%
  dplyr::select(-X) %>%
  dplyr::select(country_name = CountryName, iso3 = CountryCode, date = Date,
                cum_confirm = ConfirmedCases, cum_death = ConfirmedDeaths,
                pol_index = StringencyIndex, pub_transport = S4_Close.public.transport,
                internal_move = S6_Restrictions.on.internal.movement,
                close_work = S2_Workplace.closing,
                close_school= S1_School.closing) %>%
  mutate(date = ymd(as.character(date))) %>%
  arrange(country_name, date) %>%
  mutate_at(c("cum_confirm", "cum_death"), zoo::na.locf) %>%
  filter(!is.na(pol_index))

air <- air %>%
  mutate(Specie = ifelse(Specie %in% c("wind-speed", "wind speed"), "wind_speed",
                         ifelse(Specie %in% c("wind gust", "wind-gust"), "wind_gust", Specie))) %>%
  group_by(Specie) %>%
  add_count() %>%
  filter(n >= 3e4) %>%
  dplyr::select(-n)

airw <- air %>%
  dplyr::select(Date, Country, City, Specie, median) %>%
  pivot_wider(id_cols = c("Date", "Country", "City"),
              names_from = Specie, values_from = median) %>%
  arrange(Country, City, Date) %>%
  mutate_at(unique(air$Specie), function(x){return(unlist(lapply(x, flist)))}) %>%
  mutate(Date = ymd(Date)) %>%
  rename(date = Date, city = City, iso2 = Country) %>%
  left_join(isocode, by = "iso2") %>%
  filter(!is.na(iso3))

dt.all <- airw %>%
  left_join(policy_clean, by = c("date", "iso3"))


summary(felm(co ~ pub_transport + internal_move + close_school +
               close_work + wind_speed + humidity + temperature | 
               city + date | 0 | city,
             data = dt.all))


pred.dt <- dt.all %>%
  mutate(ws2 = wind_speed^2,
         weekday = lubridate::wday(date)) %>%
  fastDummies::dummy_cols(select_columns = c("weekday"), 
                                remove_first_dummy = F)
pred.dt <- pred.dt[,sort(colnames(pred.dt))]


pred_pm25 <- function(loc, outvar){
  pred.dt$outvar <- unlist(pred.dt[,outvar])
  m <- lm(outvar ~ wind_speed + ws2 + humidity + temperature + pressure + as.factor(weekday),
          data = filter(pred.dt, city == loc & pub_transport == 0 & internal_move == 0))
  coef <- m$coefficients
  
  d_idpt <- as.matrix(pred.dt %>%
                        filter(city == loc) %>%
                           dplyr::select(wind_speed, ws2, humidity,
                                         temperature, pressure, weekday_2, weekday_3,
                                         weekday_4, weekday_5, weekday_6, weekday_7))
  
  pred_val <- unlist(d_idpt %*% coef[2:length(coef)] + coef[1])
  return(pred_val)
}

pred.dt$pm25_pred <- NA
for(loc in unique(pred.dt$city)){
  print(loc)
  try(pred.dt$pm25_pred[pred.dt$city == loc] <- pred_pm25(loc, "pm25"))
}

pred.dt$pm10_pred <- NA
pred.dt$co_pred <- NA
pred.dt$so2_pred <- NA
for(loc in unique(pred.dt$city)){
  print(loc)
  try(pred.dt$pm10_pred[pred.dt$city == loc] <- pred_pm25(loc, "pm10"))
  try(pred.dt$co_pred[pred.dt$city == loc] <- pred_pm25(loc, "co"))
  try(pred.dt$so2_pred[pred.dt$city == loc] <- pred_pm25(loc, "so2"))
}

cities <- c("Manhattan", "London", "Delhi", "Jakarta", "Paris", "Milan")
first_start <- filter(pred.dt, pub_transport > 0 & city %in% cities) %>%
  group_by(city) %>%
  dplyr::summarize(start = min(date))

ggplot(data = filter(pred.dt, city %in% cities & date >= ymd("20200301"))) +
  geom_line(aes(x = date, y = pm10)) +
  geom_line(aes(x = date, y = pm10_pred), color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = start), first_start, color = "red") +
  theme_minimal() +
  facet_wrap(~city)
ggsave("Graphs/air_pm25_sample.pdf", device = "pdf", width = 11, height = 7)

