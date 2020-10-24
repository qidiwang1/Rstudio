#default theme for plotting time varying cfr estimates
#can be changed to multiple themes for different outputs - html report / publication graphic
cfr_plot_theme <- function(){
  t <- ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                      axis.text.x = ggplot2::element_text(size = 8),
                      axis.text.y = ggplot2::element_text(size = 8),
                      panel.grid = ggplot2::element_blank(),
                      plot.title = ggplot2::element_text(hjust = 0.5, size = 13),
                      text = ggplot2::element_text(size = 6))
  
  return(t)
}
# fit a Poisson GAM to deaths and known cases data, with a given value for the
# true CFR, and return the timeseries mean and 95% CI
get_one_timeseries <- function (true_cfr, data) {
  
  # fit model
  data <- data %>%
    dplyr::mutate(log_offset = log(cases_known * true_cfr/100))
  
  model <- mgcv::gam(deaths ~ s(date_num, k = 3) + offset(log_offset),
                     data = data,
                     family = stats::poisson)
  
  # predict timeseries without log-offset to get timeseries -log(reporting rate)
  pred_data <- data.frame(date_num = data$date_num,
                          log_offset = 0)
  preds <- stats::predict(model,
                          newdata = pred_data,
                          type = "link",
                          se.fit = TRUE)
  
  # parameters of distribution over log reporting rate
  mu <- -preds$fit
  sigma <- preds$se.fit
  
  # convert to expectation and 95% CI over reporting rate and return
  dplyr::tibble(
    estimate = exp(mu + (sigma ^ 2) / 2),
    lower = qlnorm(0.025, meanlog = mu, sdlog = sigma),
    upper = qlnorm(0.975, meanlog = mu, sdlog = sigma)
  )
  
}
#get time varying cfr data for a country
get_plot_data <- function(country_name, data = allTogether){
  
  true_cfr <- 1.4/100
  
  #filter country data and adjust date
  country_data <- data %>% 
    dplyr::filter(country == country_name) %>% 
    dplyr::mutate(date = date - zMean)
  
  #date where cumulative deaths passed 10
  death_threshold_date <- country_data %>% 
    dplyr::mutate(death_cum_sum = cumsum(new_deaths)) %>% 
    dplyr::filter(death_cum_sum >= 10) %>% 
    dplyr::pull(date) %>% 
    min()
  
  #return adjusted date and reporting_estimate
  cfr <- scale_cfr_temporal(country_data) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(reporting_estimate = true_cfr/cCFR) %>% 
    dplyr:: mutate(reporting_estimate = pmin(reporting_estimate, 1),
                   country = country_data$country,
                   date = country_data$date,
                   date_num = as.numeric(country_data$date),
                   deaths = country_data$new_deaths,
                   cases_known = cum_known_t) %>% 
    dplyr::filter(date >= death_threshold_date) %>% 
    dplyr::select(country, date, date_num, reporting_estimate, deaths, cases_known)
  
  return(cfr)
  
}
plot_data
#plot time varying cfr for a country
plot_country <- function(plot_data){
  
  # get timeseries with the expectation, lower, and upper bounds of true CFR
  expectation <- get_one_timeseries(cCFRBaseline, plot_data)*100
  lower <- get_one_timeseries(cCFREstimateRange[1], plot_data)*100
  upper <- get_one_timeseries(cCFREstimateRange[2], plot_data)*100
  
  estimate <- expectation$estimate
  ci_poly <- tibble::tibble(x = c(plot_data$date, rev(plot_data$date)),
                            y = c(upper$upper, rev(lower$lower)))
  
  # clip all of these to (0, 1]
  estimate <- pmin(estimate, 100)
  ci_poly$y <- pmin(ci_poly$y, 100)

  
  p <- plot_data %>% 
    ggplot2::ggplot() +
    ggplot2::theme_bw() + 
    #ggplot2::geom_point(aes(x = date, y = reporting_estimate), size = 0.2) + 
    #ggplot2::geom_path(aes(x = date, y = estimate), colour = 'red', size = 0.3) +
    ggplot2::geom_polygon(data = ci_poly, ggplot2::aes(x = x, y = y), fill = viridis::viridis_pal()(10)[6], alpha = 0.3) +
    ggplot2::coord_cartesian(ylim = c(0, 100)) +
    ggplot2::ylab("") +
    ggplot2::ggtitle(gsub("_", " ", plot_data$country %>% unique())) +
    cfr_plot_theme()
  
  return(p)
}



scale_cfr_temporal <- function(data_1_in, delay_fun = hospitalisation_to_death_truncated){
  
  case_incidence <- data_1_in$new_cases
  death_incidence <- data_1_in$new_deaths
  cumulative_known_t <- NULL # cumulative cases with known outcome at time tt
  # Sum over cases up to time tt
  for(ii in 1:nrow(data_1_in)){
    known_i <- 0 # number of cases with known outcome at time ii
    for(jj in 0:(ii - 1)){
      known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
      known_i <- known_i + known_jj
    }
    cumulative_known_t <- c(cumulative_known_t,known_i) # Tally cumulative known
  }
  
  # naive CFR value
  b_tt <- sum(death_incidence)/sum(case_incidence) 
  # corrected CFR estimator
  p_tt <- (death_incidence/cumulative_known_t) %>% pmin(.,1)
  
  data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
             cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
}

# Set parameters
zMean <- 13
zSD <- 12.7
zMedian <- 9.1
mu <- log(zMedian)
sigma <- sqrt(2*(log(zMean) - mu))


# set baseline level CFR
cCFRBaseline <- 1.4
cCFREstimateRange <- c(1.2, 1.7)

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}


# Load data -----------------------------------------------------
#httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDat <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/cov.csv')


allTogether <- allDat %>% 
  dplyr::arrange(countriesAndTerritories, dateRep) %>% 
  dplyr::mutate(dateRep = as.Date(dateRep,'%m/%d/%y'))%>% 
  dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
  dplyr::select(date, country, new_cases, new_deaths) %>%
  dplyr::filter(!country %in% c("CANADA", "Cases_on_an_international_conveyance_Japan")) %>%
  dplyr::group_by(country) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  #What is this doing?
  dplyr::group_by(country) %>%
  dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
  dplyr::filter(cum_deaths > 0) %>%
  dplyr::select(-cum_deaths)

# Plot rough reporting over time -----------------------------------------
plot_country_names <- allTogether %>% 
  dplyr::mutate(death_cum_sum = cumsum(new_deaths)) %>% 
  dplyr::filter(death_cum_sum >= 10) %>% 
  dplyr::mutate(max_deaths = max(death_cum_sum)) %>% 
  dplyr::arrange(-max_deaths) %>% 
  dplyr::group_by(country) %>% 
  dplyr::filter(dplyr::n() >= 8) %>%
  dplyr::pull(country) %>% 
  unique()

plot_data <- get_plot_data(country_name = "Spain")

ploy <- data.frame()
for (country_name in plot_country_names){
  plot_data <- get_plot_data(country_name = country_name)
  expectation <- get_one_timeseries(cCFRBaseline, plot_data)*100
  lower <- get_one_timeseries(cCFREstimateRange[1], plot_data)*100
  upper <- get_one_timeseries(cCFREstimateRange[2], plot_data)*100
  
  estimate <- expectation$estimate
  ci_poly <- tibble::tibble(x = c(plot_data$date, rev(plot_data$date)),
                            y = c(upper$upper, rev(lower$lower)))
  
  # clip all of these to (0, 1]
  estimate <- pmin(estimate, 100)
  ci_poly$y <- pmin(ci_poly$y, 100)
  expe <- data.frame(Date = plot_data$date, Country = as.character(country_name), estimate = estimate, 
                     upper = upper$upper, lower = lower$lower)
  ploy <- rbind(ploy,expe)
  
}

write.csv(ploy,'/Users/qidiwang1/Desktop/疫情数据库/underestimate.csv')





ploy$Date <- as.Date(ploy$Date)
for (a in levels(ploy$Country)){
  country <- ploy[ploy$Country==a,]
  p <- ggplot()+
    geom_line(aes(x=country$Date,y=country$estimate))+
    geom_line(aes(x=country$Date,y=country$upper))+
    geom_line(aes(x=country$Date,y=country$lower))+
    ggtitle(as.character(a))
  ggsave(p,filename = paste('/Users/qidiwang1/Desktop/new/',as.character(a),'.png'))
  
}



cfr_plots <- list()
for (country_name in plot_country_names){
  plot_data <- get_plot_data(country_name = country_name)
  
  p <- try(plot_country(plot_data = plot_data))

  
  if ('try-error' %in% class(p)){next}
  
  cfr_plots[[country_name]] = p
  
}
cfr_plots$United_States_of_America

cfr_plot_grid = gridExtra::arrangeGrob(grobs = cfr_plots,
                                       ncol = 4,
                                       left = "Percentage of symptomatic cases reported", 
                                       rot = 90)


ggplot2::ggsave('/Users/qidiwang1/Desktop/figure_1.png',
                cfr_plot_grid,
                width =11, 
                height = 25, 
                units = 'in', 
                dpi = 450)