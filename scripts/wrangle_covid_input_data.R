# clean COVID SLR data
# and prep for BUGS NMA analysis

library(dplyr)

###################
# read in raw data

dir_data <- "../../ICON/data/"

filename <- "4428_0013_Covid_Vaccine_Spikevax_DAT_All included studies_v1.2_11July2023.xlsx"

fileloc <- paste0(dir_data, filename)

xl_data <- readxl::read_xlsx(path = fileloc,
                             sheet = "for Nathan",
                             range = "A3:BW743")

# clean column names

colnamesA3 <- names(xl_data)

colnamesA2 <- readxl::read_xlsx(path = fileloc,
                                sheet = "for Nathan",
                                range = "A2:BW743") |> colnames()

colnamesA3[grepl(pattern = "\\.\\.\\.", colnamesA3)] <- NA
colnamesA2[grepl(pattern = "\\.\\.\\.", colnamesA2)] <- NA

colnames(xl_data) <- coalesce(colnamesA3, colnamesA2)

##############
# user inputs

# run base case scenario?
basecase <- TRUE

# first baseline, last intervention of interest
tx_levels <- c("Unvaccinated/Placebo", "PfizerBiontech", "Moderna")

# binary outcome variable
outcomes <-
  c("COVID.infection", "Symptomatic.infection", "Severe.Infection.All",
    "Severe.infections..WHO.ICU.addmission.", "Hospitalizations", "Deaths")

####################
# create input data

for (i in outcomes) {
  BUGS_input_data <- clean_covid_data(xl_data, i, basecase, tx_levels)
  
  file_append <- gsub("\\.", "_", i)
  write.csv(BUGS_input_data, file = glue::glue("data/BUGS_input_data_{file_append}.csv"))
}


