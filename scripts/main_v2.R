
# modified script
# from Au (2022)

library(R2jags)
library(dplyr)

# 1: RCT
# 2: retrospective cohort studies
# 3: prospective cohort studies
# 4: case-control

filename <- ("data/export_VPB_main_docinfect_v2.csv")
dat_raw <-
  read.csv(filename, header = TRUE, fileEncoding = "UTF-8-BOM")

# times across studies
times <- dat_raw[, grepl("t\\d", names(dat_raw))]
na <- apply(times, 1, \(x) sum(!is.na(x)))  # number of points 
max_t_idx <- max(na)

tlong <- 
  cbind(times, id = dat_raw$design_idx) |> 
  reshape2::melt(id.vars = "id") |> 
  group_by(id) |> 
  distinct(value) |> 
  tidyr::drop_na() |> 
  arrange(id, value)

data <-
  list(
    ntALL = max(times, na.rm = TRUE),    # maximum time across all studies
    design_idx = dat_raw$design_idx,
    offset = c(0, which(diff(dat_raw$design_idx) == 1), nrow(dat_raw)) + 1,
    # cumulative number of unique time points per study designs
    nt_cumul = c(1, 1 + cumsum(pull(summarise(tlong, n())))),
    ts = pull(tlong),            # unique times per study design
    na = na,
    t = times,
    r = dat_raw[, grepl("r\\d", names(dat_raw))],
    n = dat_raw[, grepl("n\\d", names(dat_raw))])

para <- c("d",      # outcome at each time, log odds of intervention k
          "sdALL",  # overall between-trial heterogeneity
          "sdRCT",  # RCT heterogeneity
          "sdCC",   # heterogeneity between case-control studies
          "sdRC",   # heterogeneity between retrospective cohort studies
          "sdPC",   # heterogeneity between prospective cohort studies
          "lor",    # log odds ratio
          "rk")     # rank

bugs_filename <- "BUGS/VPB_main_docinfect_v2.bugs"

jagsfit <- jags(
  data = data,
  inits = NULL,
  parameters.to.save = para,
  model.file = bugs_filename,
  n.chains = 3,
  n.iter = 100000,
  n.burnin = floor(100000 / 5),
  n.thin = max(1, floor((
    100000 - floor(100000 / 5)) / 1000)),
  DIC = TRUE,
  working.directory = NULL,
  jags.seed = 123,
  refresh = 100000 / 50,
  progress.bar = "text",
  digits = 5,
  RNGname = c(
    "Wichmann-Hill",
    "Marsaglia-Multicarry",
    "Super-Duper",
    "Mersenne-Twister"),
  jags.module = c("glm", "dic"),
  quiet = FALSE)

