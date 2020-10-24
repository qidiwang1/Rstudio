library(dplyr)
#default theme for plotting time varying cfr estimates
#can be changed to multiple themes for different outputs - html report / publication graphic
#cfr_plot_theme <- function(){
#  t <- ggplot2::theme(axis.title.x = element_blank(),
#                      axis.text.x = element_text(size = 4),
#                      panel.grid = element_blank(),
#                      plot.title = element_text(hjust = 0.5, size = 7),
#                      text = element_text(size = 6))
#  
#  return(t)
#}
#get time varying cfr data for a country

get_plot_data <- function(country_name, data = allTogetherClean ,CFRBaseline){
  
  #filter country data and adjust date
  country_data <- data %>% 
    dplyr::filter(country == country_name) %>% 
    dplyr::mutate(date = date - mean)
  
  #date where cumulative deaths passed 10
  death_threshold_date <- country_data %>% 
    dplyr::mutate(death_cum_sum = cumsum(new_deaths)) %>% 
    dplyr::filter(death_cum_sum >= 10) %>% 
    dplyr::pull(date) %>% 
    min()
  
  #return adjusted date and reporting_estimate
  cfr <- scale_cfr_temporal(country_data) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(reporting_estimate = CFRBaseline/cCFR) %>% 
    dplyr::mutate(reporting_estimate = pmin(reporting_estimate, 1),
                  country = country_data$country,
                  date = country_data$date,
                  date_num = as.numeric(country_data$date),
                  deaths = country_data$new_deaths,
                  cases_known = cum_known_t) %>% 
    dplyr::filter(date >= death_threshold_date) %>% 
    dplyr::select(country, date, date_num, reporting_estimate, deaths, cases_known)
  
  return(cfr)
  
}
# fit a Bayesian GP regression model to deaths and known cases data, integrating
# over uncertainty in the true CFR, and return the posterior mean and 95% CI
run_bayesian_model <- function (data, n_inducing = 5, verbose = TRUE) {
data <- allTogetherClean  
  # only fit to time points where there are known cases
  data <- data %>%
    dplyr::filter(cases_known > 0)
  
  n <- nrow(data)
  times <- seq(min(data$date_num), max(data$date_num))
  
  # GP parameters for squared-exponential kernel plus a bias term (intercept)
  # for reporting rate
  lengthscale <- greta::lognormal(4, 0.5)
  sigma <- greta::lognormal(-1, 1)
  temporal <- greta.gp::rbf(lengthscales = lengthscale,
                            variance = sigma ^ 2)
  intercept <- greta.gp::bias(1)
  reporting_kernel <- temporal + intercept
  
  # IID noise kernel for observation overdispersion (clumped death reports)
  sigma_obs <-greta::normal(0, 0.5, truncation = c(0, Inf))
  observation_kernel <- greta.gp::white(sigma_obs ^ 2)
  
  # combined kernel (marginalises a bunch of parameters for easier sampling)
  kernel <- reporting_kernel + observation_kernel
  
  # a set of inducing points at which to estimate the GPs; using a subset of
  # regressors approximation (put an inducing point at the last time, since we
  # care a lot about that estimate)
  inducing_points <- seq(min(times), max(times), length.out = n_inducing + 1)[-1]
  
  # GP for the (probit-) reporting rate
  z <- greta.gp::gp(times, inducing = inducing_points, kernel)
  
  # convert to probabilities
  reporting_rate <- greta::iprobit(z)
  
  # distribution over plausible baseline CFR values from China study. The 95%
  # CIs are symmetric around the estimate, so we assume it's an approximately
  # Gaussian distribution, truncated to allowable values.
  true_cfr_mean <- CFRBaseline
  true_cfr_sigma <- mean(abs(CFREstimateRange - CFRBaseline)) / 1.96
  baseline_cfr_perc <- greta::normal(true_cfr_mean, true_cfr_sigma, truncation = c(0, 100))
  
  # compute the expected number of deaths at each timepoint, given the true CFR,
  # number of reported cases with known outcomes, and reporting rate
  log_expected_deaths <-
    log(baseline_cfr_perc / 100) + log(data$cases_known) - log(reporting_rate) 
  expected_deaths <- exp(log_expected_deaths)
  
  # define sampling distribution
  greta::distribution(data$deaths) <- greta::poisson(expected_deaths)
  
  # construct the model
  m <- greta::model(reporting_rate)
  
  n_chains <- 50
  
  # sample initial values for hyperparameters from within their priors
  inits <- replicate(
    n_chains,
    greta::initials(
      lengthscale = rlnorm(1, 4, 0.5),
      sigma = abs(rnorm(1, 0, 0.5)),
      sigma_obs = abs(rnorm(1, 0, 0.5)),
      baseline_cfr_perc = max(0.001, min(99.999,
                                         rnorm(1, true_cfr_mean, true_cfr_sigma)
      ))
    ),
    simplify = FALSE
  )
  
  if (verbose) {
    country <- data$country[1]
    message("running model for ", country)
  }
  
  # draw a bunch of mcmc samples
  draws <- greta::mcmc(
    m,
    sampler = greta::hmc(Lmin = 15, Lmax = 20),
    chains = n_chains,
    warmup = 1000,
    n_samples = 1000,
    initial_values = inits,
    one_by_one = TRUE,
    verbose = verbose
  )
  
  # extend the number of chains until convergence (or give up)
  for (i in 1:5) {
    r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
    n_eff <- coda::effectiveSize(draws)
    decent_samples <- max(r_hats) <= 1.1 & min(n_eff) > 1000 
    if (!decent_samples) {
      if (verbose) {
        message("maximum R-hat: ", max(r_hats),
                "\nminimum n-eff: ", min(n_eff))
      }
      draws <- greta::extra_samples(draws, 2000, one_by_one = TRUE, verbose = verbose)
    }
  }
  
  # predict without IID noise (true reporting rate, without clumped death reporting)
  # could predict to more granular times here too
  z_smooth <- greta.gp::project(z, times, kernel = reporting_kernel)
  reporting_rate_smooth <- greta::iprobit(z_smooth)
  draws_pred <- greta::calculate(reporting_rate_smooth, values = draws)
  
  # get estimates
  draws_pred_mat <- as.matrix(draws_pred)
  
  # compute posterior mean and 95% credible interval and return
  tibble::tibble(
    date = data$date,
    estimate = colMeans(draws_pred_mat),
    lower = apply(draws_pred_mat, 2, quantile, 0.025),
    upper = apply(draws_pred_mat, 2, quantile, 0.975)
  )
  
}
# Define CFR function -----------------------------------------------------

# Function to work out correction CFR
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
  
  data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), deaths = death_incidence,
             cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
}
#plot time varying cfr for a country
plot_country <- function(plot_data, estimate, ci_poly){
  
  p <- plot_data %>% 
    ggplot2::ggplot() +
    ggplot2::theme_bw() + 
    ggplot2::geom_point(aes(x = date, y = reporting_estimate), size = 0.2) + 
    ggplot2::geom_path(aes(x = date, y = estimate), colour = 'red', size = 0.3) +
    ggplot2::geom_polygon(data = ci_poly, aes(x = x, y = y), fill = 'red', alpha = 0.2) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::ylab("Proportion of cases reported") +
    ggplot2::ggtitle(gsub("_", " ", plot_data$country %>% unique())) +
    cfr_plot_theme()
  
  return(p)
  
}
# setting baseline level CFR
CFRBaseline <- 1.4
CFREstimateRange <- c(1.2, 1.7)

# Set parameters
mean <- 13
median <- 9.1

mu <- log(median)
sigma <- sqrt(2*(log(mean) - mu))

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}
hospitalisation_to_death_truncated(3)
# Load data -----------------------------------------------------
#httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
#allDat <- readr::read_csv(tf)
#allDat <- read.csv('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv')
allDat <- read.csv('/Users/qidiwang1/Desktop/cov.csv')
# munging data into correct format and selecting countries with greater than 10 deaths
library(dplyr)
allTogetherClean <- allDat %>% 
  dplyr::arrange(countriesAndTerritories, dateRep) %>% 
  dplyr::mutate(dateRep = as.Date(dateRep, '%m/%d/%y'))%>% 
  dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
  dplyr::select(date, country, new_cases, new_deaths) %>%
  dplyr::filter(!country %in% c("CANADA", "Cases_on_an_international_conveyance_Japan")) %>%
  dplyr::group_by(country) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
  dplyr::filter(cum_deaths > 0) %>%
  dplyr::select(-cum_deaths)


# Plot rough reporting over time -----------------------------------------
plot_country_names <- allTogetherClean %>% 
  dplyr::mutate(death_cum_sum = cumsum(new_deaths)) %>% 
  dplyr::filter(death_cum_sum >= 10) %>%
  dplyr::mutate(max_deaths = max(death_cum_sum)) %>% 
  dplyr::group_by(country) %>%
  dplyr::summarise(max_deaths = dplyr::first(max_deaths),
                   observations = dplyr::n()) %>%
  dplyr::filter(observations >= 10) %>%
  dplyr::arrange(-max_deaths) %>% 
  dplyr::pull(country) %>%
  unique()


# running loop over all countries, fitting the model, saving the fit data and making each plot
cfr_plots <- list()
underreport_data <- data.frame()
for (country_name in plot_country_names){
  tryCatch({ 
    
    plot_data <- get_plot_data(country_name = country_name, CFRBaseline = CFRBaseline)
    prediction <- run_bayesian_model(plot_data)
    
    #saveRDS(prediction, paste0("/Users/qidiwang1/Desktop//fit_data/",country_name, "_fit" ,'.rds'))
    
    #ci_poly <- tibble::tibble(x = c(plot_data$date, rev(plot_data$date)),
     #                         y = c(prediction$upper, rev(prediction$lower)))
    underreport_data <- rbind(underreport_data,data.frame(Date = as.character(plot_data$date), Country = as.character(country_name),
                                                          estimate = prediction$estimate, upper = prediction$upper,
                                                          lower = prediction$lower))
    print(underreport_data)
    #p <- try(plot_country(plot_data, prediction$estimate, ci_poly))
    
    #@if ('try-error' %in% class(p)){next}
    
    #ggplot2::ggsave(paste0("/Users/qidiwang1/Desktop/output/", country_name, "_plot.pdf"),
    #                p,
    #                width = 8, 
    #                height = 10, 
    #                units = 'in', 
    #                useDingbats = FALSE,
     #               dpi = 400)
    
   # cfr_plots[[country_name]] = p 
    
  },
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
write.csv(underreport_data,'/Users/qidiwang1/Desktop/underreportrate.csv')
# arranging all of the plots in cfr_plots
#cfr_plot_grid = gridExtra::arrangeGrob(grobs = cfr_plots, ncol = 1)

# saving all of the plots as a .png, to make loading time on the .html not too long
#ggplot2::ggsave('/Users/qidiwang1/Desktop/output2/cfr_plot_grid.png',
#                cfr_plot_grid,
 #               width = 8, 
  #              height = 10, 
   #             units = 'in', 
    #            dpi = 400)

