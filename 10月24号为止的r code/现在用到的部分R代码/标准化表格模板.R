data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')

merge <- data.frame(JDATE = data$JDATE, Date = data$Date, Country = data$Country, Total_death = data$death, Total_recover = data$Recover, Total_cases = data$confirm,
                    Total_tests = data$Total.tests, New_death = data$New.death,New_recover = data$New_Recover, New_case = data$New.cases, Rt = data$RT, 
                    Mobility_Index = data$Mobility_Index, Daily_Economics_Policy_Score = data$Daily_Economics_Policy_Score, 
                    Confirm_Doubling_Days = data$confirm_doubling_days,Death_Doubling_Days = data$death_doubling_days, 
                    smooth_confirm_doubling_days = data$smooth_confirm_doubling_days, smooth_death_doubling_days = data$smooth_death_doubling_days,Covid_Wave = data$COVID.wave,
                    Area_ID = data$area_id, iso2_name = data$alpha.2, iso3_name = data$alpha.3,
                    Population = data$Population, Development = data$Development, Income = data$Income, Region = data$region,
                    Sub_region = data$sub.region,
                    First_Industry = data$First_Industry, Second_Industry = data$Second_Industry, Third_Industry = data$Third_Industry,
                    C1_School.closing = data$C1_School.closing, C1_Flag = data$C1_Flag, C2_Workplace.closing = data$C2_Workplace.closing,
                    C2_Flag = data$C2_Flag, C3_Cancel.public.events = data$C3_Cancel.public.events, C3_Flag = data$C3_Flag, 
                    C4_Restrictions.on.gatherings = data$C4_Restrictions.on.gatherings, C4_Flag = data$C4_Flag, C5_Close.public.transport = data$C5_Close.public.transport,
                    C5_Flag = data$C5_Flag, C6_Stay.at.home.requirements = data$C6_Stay.at.home.requirements, C6_Flag = data$C6_Flag, 
                    C7_Restrictions.on.internal.movement = data$C7_Restrictions.on.internal.movement, C7_Flag = data$C7_Flag, 
                    C8_International.travel.controls = data$C8_International.travel.controls, E1_Income.support = data$E1_Income.support,
                    E1_Flag = data$E1_Flag, E2_Debt.contract.relief = data$E2_Debt.contract.relief, E3_Fiscal.measures = data$E3_Fiscal.measures, 
                    E4_International.support = data$E4_International.support, H1_Public.information.campaigns = data$H1_Public.information.campaigns,
                    H1_Flag = data$H1_Flag, H2_Testing.policy = data$H2_Testing.policy, H3_Contact.tracing = data$H3_Contact.tracing, 
                    H4_Emergency.investment.in.healthcare = data$H4_Emergency.investment.in.healthcare, H5_Investment.in.vaccines = data$H5_Investment.in.vaccines,
                    M1_Wildcard = data$M1_Wildcard)


merge <- data.frame(JDATE = data$JDATE, Date = data$Date, index_dt = as.character(as.Date(data$Date), "%Y%m%d"), 
                    Country = data$Country, Total_death = data$death, Total_recover = data$Recover, Total_cases = data$confirm,
                    Total_tests = data$Total.tests, New_death = data$New.death,New_recover = data$New_Recover, New_case = data$New.cases, Rt = data$RT, 
                    Mobility_Index = data$Mobility_Index, 
                    smooth_confirm_doubling_days = data$smooth_confirm_doubling_days,Daily_Economics_Policy_Score=data$Daily_Economics_Policy_Score,
                    Covid_Wave = data$COVID.wave,
                    Area_ID = data$area_id, iso2_name = data$alpha.2, iso3_name = data$alpha.3,
                    Population = data$Population, Development = data$Development, Income = data$Income, Region = data$region,
                    Sub_region = data$sub.region,
                    First_Industry = data$First_Industry, Second_Industry = data$Second_Industry, Third_Industry = data$Third_Industry)
View(merge)
write.csv(merge,'/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
library(writexl)
write_xlsx(merge, '/Users/qidiwang1/Desktop/疫情数据库/数据大表.xlsx')
