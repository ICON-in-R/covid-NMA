
# original script
# from Au (2022)

library(R2jags)
library(dplyr)


filename <- ("data/export_VPB_main_docinfect.csv")
dat_raw <-
  read.csv(filename, header = TRUE, fileEncoding = "UTF-8-BOM")

nvac <- max(dat_raw[, ncol(dat_raw)])

data <-
  list(
    ntALL = max(cbind(dat_raw[, 1:nvac]), na.rm = TRUE),
    t = cbind(dat_raw[, 1:nvac]),
    r = cbind(dat_raw[, (nvac + 1):(nvac * 2)]),
    n = cbind(dat_raw[, (nvac * 2 + 1):(nvac * 3)]),
    na = dat_raw[, ncol(dat_raw)])

para <- c("d", "sdALL", "sdRCT", "sdCC", "sdRC", "sdPC", "lor", "rk")
bugs_filename <- "VPB_main_docinfect.bugs"

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
