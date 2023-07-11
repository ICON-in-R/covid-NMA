
#' Add dummy design data
#'
#' If a design is missing in the data, include dummy entry
#' 
#' @param dat_raw BUGS input array
#'
#' @return
#' @export
#'
add_dummy_design_data <- function(dat_raw) {
  dat <- dat_raw
  
  if (!1 %in% dat_raw$Design.ID) {
    dat <- rbind(dat, NA)
    ns <- nrow(dat)
    dat[ns, c("X", "na", "n1", "n2", "r1", "r2", "t1", "t2")] <-
            c(0,    2,    2,    2,    1,    1,    1,    2)
    dat$Ref.ID[ns] <- -1
    dat$Study.design[ns] <- "Randomized clinical trial"
    dat$Design.ID[ns] <- 1
  }
  if (!3 %in% dat_raw$Design.ID) {
    dat <- rbind(dat, NA)
    ns <- nrow(dat)
    dat[ns, c("X", "na", "n1", "n2", "r1", "r2", "t1", "t2")] <-
            c(0,    2,    2,    2,    1,    1,    1,    2)
    dat$Ref.ID[ns] <- -3
    dat$Study.design[ns] <- "Prospective observational study"
    dat$Design.ID[ns] <- 3
  }
  if (!4 %in% dat_raw$Design.ID) {
    dat <- rbind(dat, NA)
    ns <- nrow(dat)
    dat[ns, c("X", "na", "n1", "n2", "r1", "r2", "t1", "t2")] <-
            c(0,    2,    2,    2,    1,    1,    1,    2)
    dat$Ref.ID[ns] <- -4
    dat$Study.design[ns] <- "Test-negative"
    dat$Design.ID[ns] <- 4
  }
  
  dat  
}



