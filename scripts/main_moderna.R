
# modified BUGS running script
# using 
# from Au (2022)

library(R2jags)
library(dplyr)

## study designs
# 1: RCT
# 2: retrospective cohort studies
# 3: prospective cohort studies
# 4: case-control

#################
# select outcome

outcome_names <-
  c("COVID_infection", "Symptomatic_infection", "Severe_Infection_All", "Hospitalizations", "Deaths")

outcome <- outcome_names[4]
dat_raw <- read.csv(file = here::here(glue::glue("data/BUGS_input_data_{outcome}.csv")))

dat_raw <- add_dummy_design_data(dat_raw)

dat_raw <- dat_raw[order(dat_raw$Design.ID), ]

# treatments across studies
arms <- dat_raw[, grepl("t\\d", names(dat_raw))]
na <- dat_raw$na
max_t_idx <- max(na)

# unique treatments per study designs
tlong <- 
  cbind(arms, id = dat_raw$Design.ID) |> 
  reshape2::melt(id.vars = "id") |> 
  group_by(id) |> 
  distinct(value) |> 
  tidyr::drop_na() |> 
  arrange(id, value)

data <-
  list(
    ntALL = max(arms, na.rm = TRUE),    # maximum number of treatments across all studies
    offset = c(0, which(diff(dat_raw$Design.ID) == 1), nrow(dat_raw)) + 1,  # index of first trial in each design
    nt_cumul =
      c(1, 1 + cumsum(pull(summarise(tlong, n())))),  # cumulative number/offset of unique treatments per study designs
    ts = pull(tlong),            # unique treatments per study design
    na = na,                     # number of arms per study
    t = arms,
    r = dat_raw[, grepl("r\\d", names(dat_raw))],  # number of events
    n = dat_raw[, grepl("n\\d", names(dat_raw))])  # sample size

para <- c("d",      # outcome at each time, log odds of intervention k
          "sdALL",  # overall between-trial heterogeneity
          "sdRCT",  # RCT heterogeneity
          "sdCC",   # heterogeneity between case-control studies
          "sdRC",   # heterogeneity between retrospective cohort studies
          "sdPC",   # heterogeneity between prospective cohort studies
          "lor",    # log odds ratio
          "rk",     # rank
          "SUCRA")

bugs_filename <- here::here("BUGS/bugs_code.txt")

# corresponding with other analyses
n.iter <- 100000
n.burn <- 20000
n.thin <- 80

jagsfit <- jags(
  data = data,
  inits = NULL,
  parameters.to.save = para,
  model.file = bugs_filename,
  n.chains = 3,
  n.iter = n.iter,
  n.burnin = n.burn,
  n.thin = n.thin,
  DIC = TRUE,
  working.directory = NULL,
  jags.seed = 123,
  refresh = n.iter / 50,
  progress.bar = "text",
  digits = 5,
  RNGname = c(
    "Wichmann-Hill",
    "Marsaglia-Multicarry",
    "Super-Duper",
    "Mersenne-Twister"),
  jags.module = c("glm", "dic"),
  quiet = FALSE)

save(jagsfit, file = glue::glue("data/jagsfit_{outcome}.RData"))
