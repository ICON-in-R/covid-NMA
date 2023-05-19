
# original script
# from Au (2022)


library(R2jags)
library(dplyr)


filename <- ("data/export_VPB_main_docinfect.csv")
dat_raw <-
  read.csv(filename, header = TRUE, fileEncoding = "UTF-8-BOM")

# maximum number time points across studies
max_t_idx <- max(dat_raw$na..)

data <-
  list(
    ntALL = max(dat_raw[, 1:max_t_idx], na.rm = TRUE),  # maximum time across all studies
    t = dat_raw[, 1:max_t_idx],
    r = dat_raw[, (max_t_idx + 1):(max_t_idx * 2)],
    n = dat_raw[, (max_t_idx * 2 + 1):(max_t_idx * 3)],
    na = dat_raw$na..)

para <- c("d", "sdALL", "sdRCT", "sdCC", "sdRC", "sdPC", "lor", "rk")
bugs_filename <- "BUGS/VPB_main_docinfect.bugs"

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

traceplot(jagsfit)

