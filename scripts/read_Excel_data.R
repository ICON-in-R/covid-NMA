
# read in Excel SLR data

library(readxl)
library(dplyr)

filename <-
  "../../Documents/ICON/data/4428_0013_Covid_Vaccine_Spikevax_DAT_All included studies_v0.4_06Junel2023_NMA_MN.xlsx"
  
dat <-
  read_excel(path = filename, sheet = "Efficacy", range = "A3:BU743") |> 
  filter(!is.na("Included in NMA"),
         `Included in NMA` == 1)

# ad-hoc fix of merged column heading
colnames1 <- names(dat)
colnames2 <- names(read_excel(path = filename, sheet = "Efficacy", range = "A2:BU2"))
colnames1[grepl(pattern = "\\.\\.", colnames1)] <- NA
colnames2[grepl(pattern = "\\.\\.", colnames2)] <- NA

colnames(dat) <- coalesce(colnames1, colnames2)

dat <-
  dat |> 
  select("Ref ID", "Study design", "Intervention name (full)", "Total N", "n of events")


save(dat, file = "data/cleaned_covid_data.RData")
write.csv(dat, file = "data/cleaned_covid_data.csv")
