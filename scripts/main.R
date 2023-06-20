
# modified script
# from Au (2022)

library(R2jags)
library(dplyr)

# 1: RCT
# 2: retrospective cohort studies
# 3: prospective cohort studies
# 4: case-control

load(file = "data/cleaned_covid_data.RData")

# remove data with NA
dat <- na.omit(dat)



# treatments across studies
times <- dat_raw[, grepl("t\\d", names(dat_raw))]
na <- apply(times, 1, \(x) sum(!is.na(x)))         # number of arms per study
max_t_idx <- max(na)

# unique treatments per study designs
tlong <- 
  cbind(times, id = dat_raw$design_idx) |> 
  reshape2::melt(id.vars = "id") |> 
  group_by(id) |> 
  distinct(value) |> 
  tidyr::drop_na() |> 
  arrange(id, value)

data <-
  list(
    ntALL = max(times, na.rm = TRUE),    # maximum number of treatments across all studies
    design_idx = dat_raw$design_idx,
    offset = c(0, which(diff(dat_raw$design_idx) == 1), nrow(dat_raw)) + 1,  # index of first trial in each design
    nt_cumul =
      c(1, 1 + cumsum(pull(summarise(tlong, n())))),  # cumulative number of unique treatments per study designs
    ts = pull(tlong),            # unique treatments per study design
    na = na,                     # number of arms per study
    t = times,
    r = dat_raw[, grepl("r\\d", names(dat_raw))],  # number of events
    n = dat_raw[, grepl("n\\d", names(dat_raw))])  # sample size

para <- c("d",      # outcome at each time, log odds of intervention k
          "sdALL",  # overall between-trial heterogeneity
          "sdRCT",  # RCT heterogeneity
          "sdCC",   # heterogeneity between case-control studies
          "sdRC",   # heterogeneity between retrospective cohort studies
          "sdPC",   # heterogeneity between prospective cohort studies
          "lor",    # log odds ratio
          "rk")     # rank

bugs_filename <- here::here("BUGS/bugs_code.txt")
n.iter <- 100000

jagsfit <- jags(
  data = data,
  inits = NULL,
  parameters.to.save = para,
  model.file = bugs_filename,
  n.chains = 3,
  n.iter = n.iter,
  n.burnin = floor(n.iter / 5),
  n.thin = max(1, floor((n.iter - floor(n.iter / 5)) / 1000)),
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

