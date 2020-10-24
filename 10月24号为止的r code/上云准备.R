data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
#data$Date <- as.Date(data$Date,'%m/%d/%y')

data$Date <- as.character(data$Date)
for (i in c(c:nrow(data))) {
  data$Date[i] <- paste(substr(data$Date[i],1,4),substr(data$Date[i],6,7),substr(data$Date[i],9,10),sep = '')
  
}

      
recover <- cbind(data[,c('area_id','Date','Recover')],'EXT.COV.REC.0050')
names(recover)[2] <- 'index_dt'
names(recover)[3] <- 'index_value'
names(recover)[4] <- 'index_id'
names(data)
death <- cbind(data[,c('area_id','Date','death')],'EXT.COV.DED.0060')
names(death)[2] <- 'index_dt'
names(death)[3] <- 'index_value'
names(death)[4] <- 'index_id'

confirm <- cbind(data[,c('area_id','Date','confirm')],'EXT.COV.CFM.0070')
names(confirm)[2] <- 'index_dt'
names(confirm)[3] <- 'index_value'
names(confirm)[4] <- 'index_id'

S1 <- cbind(data[,c('area_id','Date','S1_School.closing')],'GOV.GPP.LDP.0010')
names(S1)[2] <- 'index_dt'
names(S1)[3] <- 'index_value'
names(S1)[4] <- 'index_id'

S1_GENERAL <- cbind(data[,c('area_id','Date','S1_IsGeneral')],'GOV.GPP.LDP.001A')
names(S1_GENERAL)[2] <- 'index_dt'
names(S1_GENERAL)[3] <- 'index_value'
names(S1_GENERAL)[4] <- 'index_id'

S2 <- cbind(data[,c('area_id','Date','S2_Workplace.closing')],'GOV.GPP.LDP.0020')
names(S2)[2] <- 'index_dt'
names(S2)[3] <- 'index_value'
names(S2)[4] <- 'index_id'

S2_GENERAL <- cbind(data[,c('area_id','Date','S2_IsGeneral')],'GOV.GPP.LDP.002A')
names(S2_GENERAL)[2] <- 'index_dt'
names(S2_GENERAL)[3] <- 'index_value'
names(S2_GENERAL)[4] <- 'index_id'

S3 <- cbind(data[,c('area_id','Date','S3_Cancel.public.events')],'GOV.GPP.LDP.0030')
names(S3)[2] <- 'index_dt'
names(S3)[3] <- 'index_value'
names(S3)[4] <- 'index_id'

S3_GENERAL <- cbind(data[,c('area_id','Date','S3_IsGeneral')],'GOV.GPP.LDP.003A')
names(S3_GENERAL)[2] <- 'index_dt'
names(S3_GENERAL)[3] <- 'index_value'
names(S3_GENERAL)[4] <- 'index_id'

S4 <- cbind(data[,c('area_id','Date','S4_Close.public.transport')],'GOV.GPP.LDP.0040')
names(S4)[2] <- 'index_dt'
names(S4)[3] <- 'index_value'
names(S4)[4] <- 'index_id'

S4_GENERAL <- cbind(data[,c('area_id','Date','S4_IsGeneral')],'GOV.GPP.LDP.004A')
names(S4_GENERAL)[2] <- 'index_dt'
names(S4_GENERAL)[3] <- 'index_value'
names(S4_GENERAL)[4] <- 'index_id'

S5 <- cbind(data[,c('area_id','Date','S5_Public.information.campaigns')],'GOV.GPP.INF.0050')
names(S5)[2] <- 'index_dt'
names(S5)[3] <- 'index_value'
names(S5)[4] <- 'index_id'

S5_GENERAL <- cbind(data[,c('area_id','Date','S5_IsGeneral')],'GOV.GPP.LDP.005A')
names(S5_GENERAL)[2] <- 'index_dt'
names(S5_GENERAL)[3] <- 'index_value'
names(S5_GENERAL)[4] <- 'index_id'

S6 <- cbind(data[,c('area_id','Date','S6_Restrictions.on.internal.movement')],'GOV.GPP.LDP.0060')
names(S6)[2] <- 'index_dt'
names(S6)[3] <- 'index_value'
names(S6)[4] <- 'index_id'

S6_GENERAL <- cbind(data[,c('area_id','Date','S6_IsGeneral')],'GOV.GPP.LDP.006A')
names(S6_GENERAL)[2] <- 'index_dt'
names(S6_GENERAL)[3] <- 'index_value'
names(S6_GENERAL)[4] <- 'index_id'

S7 <- cbind(data[,c('area_id','Date','S7_International.travel.controls')],'GOV.GPP.LDP.0070')
names(S7)[2] <- 'index_dt'
names(S7)[3] <- 'index_value'
names(S7)[4] <- 'index_id'

population <- cbind(data[,c('area_id','Date','Population')],'GOV.MCR.POP.0080')
names(population)[2] <- 'index_dt'
names(population)[3] <- 'index_value'
names(population)[4] <- 'index_id'

newcases <- cbind(data[,c('area_id','Date','New.cases')],'EXT.COV.CFM.007A')
names(newcases)[2] <- 'index_dt'
names(newcases)[3] <- 'index_value'
names(newcases)[4] <- 'index_id'

newdeath <- cbind(data[,c('area_id','Date','New.death')],'EXT.COV.DED.006A')
names(newdeath)[2] <- 'index_dt'
names(newdeath)[3] <- 'index_value'
names(newdeath)[4] <- 'index_id'

google_retail <- cbind(data[,c('area_id','Date','retail_and_recreation')],'EXT.TRF.GOG.008A')
names(google_retail)[2] <- 'index_dt'
names(google_retail)[3] <- 'index_value'
names(google_retail)[4] <- 'index_id'

google_grocery <- cbind(data[,c('area_id','Date','grocery_and_pharmacy')],'EXT.TRF.GOG.008B')
names(google_grocery)[2] <- 'index_dt'
names(google_grocery)[3] <- 'index_value'
names(google_grocery)[4] <- 'index_id'

google_parks <- cbind(data[,c('area_id','Date','parks')],'EXT.TRF.GOG.008C')
names(google_parks)[2] <- 'index_dt'
names(google_parks)[3] <- 'index_value'
names(google_parks)[4] <- 'index_id'

google_transit <- cbind(data[,c('area_id','Date','transit_stations')],'EXT.TRF.GOG.008D')
names(google_transit)[2] <- 'index_dt'
names(google_transit)[3] <- 'index_value'
names(google_transit)[4] <- 'index_id'

google_workplace <- cbind(data[,c('area_id','Date','workplaces_percent')],'EXT.TRF.GOG.008E')
names(google_workplace)[2] <- 'index_dt'
names(google_workplace)[3] <- 'index_value'
names(google_workplace)[4] <- 'index_id'

google_resident <- cbind(data[,c('area_id','Date','residential')],'EXT.TRF.GOG.008F')
names(google_resident)[2] <- 'index_dt'
names(google_resident)[3] <- 'index_value'
names(google_resident)[4] <- 'index_id'

#google_consumption_index <- cbind(data[,c('area_id','Date','google_consumption_index')],'GOL.GCO.DAY.0031')
#names(google_consumption_index)[2] <- 'index_dt'
#names(google_consumption_index)[3] <- 'index_value'
#names(google_consumption_index)[4] <- 'index_id'

tomtom <- cbind(data[,c('area_id','Date','TrafficIndexLive')],'EXT.TRF.TOM.0030')
names(tomtom)[2] <- 'index_dt'
names(tomtom)[3] <- 'index_value'
names(tomtom)[4] <- 'index_id'

citymap <- cbind(data[,c('area_id','Date','X.mean..Citymap')],'EXT.TRF.CMP.0040')
names(citymap)[2] <- 'index_dt'
names(citymap)[3] <- 'index_value'
names(citymap)[4] <- 'index_id'

ICBU_byr <- cbind(data[,c('area_id','Date','ICBU_byr_cnt_norm')],'SPL.B2B.INL.002A')
names(ICBU_byr)[2] <- 'index_dt'
names(ICBU_byr)[3] <- 'index_value'
names(ICBU_byr)[4] <- 'index_id'

ICBU_pay <- cbind(data[,c('area_id','Date','ICBU_pay_amt_norm')],'SPL.B2B.INL.001A')
names(ICBU_pay)[2] <- 'index_dt'
names(ICBU_pay)[3] <- 'index_value'
names(ICBU_pay)[4] <- 'index_id'

flypig_avg_cabin <- cbind(data[,c('area_id','Date','ave_cabin_seats')],'EXT.TRF.FLP.010A')
names(flypig_avg_cabin)[2] <- 'index_dt'
names(flypig_avg_cabin)[3] <- 'index_value'
names(flypig_avg_cabin)[4] <- 'index_id'

flypig_avg_flight <- cbind(data[,c('area_id','Date','ave_flight')],'EXT.TRF.FLP.010B')
names(flypig_avg_flight)[2] <- 'index_dt'
names(flypig_avg_flight)[3] <- 'index_value'
names(flypig_avg_flight)[4] <- 'index_id'

flypig_cabin <- cbind(data[,c('area_id','Date','cabin_seats')],'EXT.TRF.FLP.010C')
names(flypig_cabin)[2] <- 'index_dt'
names(flypig_cabin)[3] <- 'index_value'
names(flypig_cabin)[4] <- 'index_id'

flypig_flight <- cbind(data[,c('area_id','Date','flight_num_new_abroad')],'EXT.TRF.FLP.0120')
names(flypig_flight)[2] <- 'index_dt'
names(flypig_flight)[3] <- 'index_value'
names(flypig_flight)[4] <- 'index_id'

flypig_home_flight <- cbind(data[,c('area_id','Date','FLIGHT_NUM_homeflight')],'EXT.TRF.FLP.0110')
names(flypig_home_flight)[2] <- 'index_dt'
names(flypig_home_flight)[3] <- 'index_value'
names(flypig_home_flight)[4] <- 'index_id'

apple_walking <- cbind(data[,c('area_id','Date','Apple_walking')],'EXT.TRF.APL.009A')
names(apple_walking)[2] <- 'index_dt'
names(apple_walking)[3] <- 'index_value'
names(apple_walking)[4] <- 'index_id'

apple_driving <- cbind(data[,c('area_id','Date','Apple_driving')],'EXT.TRF.APL.009B')
names(apple_driving)[2] <- 'index_dt'
names(apple_driving)[3] <- 'index_value'
names(apple_driving)[4] <- 'index_id'

apple_transit <- cbind(data[,c('area_id','Date','Apple_transit')],'EXT.TRF.APL.009C')
names(apple_transit)[2] <- 'index_dt'
names(apple_transit)[3] <- 'index_value'
names(apple_transit)[4] <- 'index_id'

doubling_days <- cbind(data[,c('area_id','Date','doubling_days')],'EXT.COV.DED.006B')
names(doubling_days)[2] <- 'index_dt'
names(doubling_days)[3] <- 'index_value'
names(doubling_days)[4] <- 'index_id'

#daily_policy <- cbind(data[,c('area_id','Date','Daily_Policy')],'GOL.DPS.DAY.0045')
#names(daily_policy)[2] <- 'index_dt'
#names(daily_policy)[3] <- 'index_value'
#names(daily_policy)[4] <- 'index_id'

#cumulative_policy <- cbind(data[,c('area_id','Date','Cumulative_Policy')],'GOL.DPP.CUL.0046')
#names(cumulative_policy)[2] <- 'index_dt'
#names(cumulative_policy)[3] <- 'index_value'
#names(cumulative_policy)[4] <- 'index_id'

#daily_eco_policy <- cbind(data[,c('area_id','Date','Daily_Economics_Policy_Score')],'GOL.DPE.DAY.0047')
#names(daily_eco_policy)[2] <- 'index_dt'
#names(daily_eco_policy)[3] <- 'index_value'
#names(daily_eco_policy)[4] <- 'index_id'

test <- rbind(recover,death,confirm,S1,S1_GENERAL,S2,S2_GENERAL,S3,S3_GENERAL,
              S4,S4_GENERAL,S5,S5_GENERAL,S6,S6_GENERAL,S7,population,newcases,newdeath,
              google_retail,google_grocery,google_parks,google_transit,google_workplace,
              google_resident,tomtom,citymap,ICBU_byr,ICBU_pay,
              flypig_avg_cabin,flypig_avg_flight,flypig_cabin,flypig_flight,flypig_home_flight,
              apple_walking,apple_driving,apple_transit,doubling_days)
write.csv(test,'/Users/qidiwang1/Desktop/test.csv')
