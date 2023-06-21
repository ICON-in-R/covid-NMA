# clean covid SLR data
# and prep for BUGS NMA analysis

library(dplyr)

dir_data <- "../../ICON/data/"
      # "N:/Organization/ICO/GHEORE/Projects/Moderna/4428-0013_Market Access activities for SPIKEVAX/06 Covid NMA/08 Evidence synthesis/NMA/"
  
filename <-
  paste0(dir_data, "4428_0013_Covid_Vaccine_Spikevax_DAT_All included studies_v1.0_20Junel2023.xlsx")

xl_data <- readxl::read_xlsx(path = filename,
                             sheet = "for Nathan",
                             range = "A3:BW743")

# clean column names

colnamesA3 <- names(xl_data)

colnamesA2 <- readxl::read_xlsx(path = filename,
                                sheet = "for Nathan",
                                range = "A2:BW743") |> colnames()

colnamesA3[grepl(pattern = "\\.\\.\\.", colnamesA3)] <- NA
colnamesA2[grepl(pattern = "\\.\\.\\.", colnamesA2)] <- NA

colnames(xl_data) <- coalesce(colnamesA3, colnamesA2)

##############
# user inputs

# run basecase scenario?
basecase <- TRUE

# first baseline, last intervention of interest
tx_levels <- c("Unvaccinated/Placebo", "PfizerBiontech", "Moderna")

# binary outcome variable
outcomes <-
  c("COVID.infection", "Symptomatic.infection", "Severe.Infection.All", "Hospitalizations", "Deaths")

####################
# create input data

for (i in outcomes) {
  BUGS_input_data <- clean_covid_data(xl_data, i)
  
  file_append <- gsub("\\.", "_", i)
  write.csv(BUGS_input_data, file = glue::glue("data/BUGS_input_data_{file_append}.csv"))
}


