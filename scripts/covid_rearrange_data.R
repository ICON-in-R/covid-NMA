
#

library(dplyr)


xl_data <- readxl::read_xlsx(path = "../../data/4428_0013_Covid_Vaccine_Spikevax_DAT_All included studies_v0.4_09Junel2023_NMA_MN_AC_SK.xlsx",
                             sheet = "for Nathan", range = "A3:BW743")

# clean column names

colnamesA3 <- names(xl_data)

colnamesA2 <- readxl::read_xlsx(path = "../../data/4428_0013_Covid_Vaccine_Spikevax_DAT_All included studies_v0.4_09Junel2023_NMA_MN_AC_SK.xlsx",
                             sheet = "for Nathan", range = "A2:BW743") |> colnames()

colnamesA3[grepl(pattern = "\\.\\.\\.", colnamesA3)] <- NA
colnamesA2[grepl(pattern = "\\.\\.\\.", colnamesA2)] <- NA

colnames(xl_data) <- coalesce(colnamesA3, colnamesA2)

# filter rows

dat <-
  xl_data |> 
  data.frame(check.names = TRUE) |> 
  dplyr::filter(Included.in.NMA == 1,
         is.na(Subgroup.Outcome.not.chosen.for.NMA),
         VE.against.infection == "Y") |> 
  select(Ref.ID, Study.design, Intervention.name..standardized., Total.N, n.of.events)


# dat <- read.csv(file = "data/cleaned_covid_data.csv")

dat_clean <- 
  dat |> 
  mutate(Intervention.name..standardized. = factor(Intervention.name..standardized.),
         interv_id = as.numeric(Intervention.name..standardized.)) |> 
  group_by(Ref.ID) |>
  arrange(interv_id) |> 
  mutate(tx = 1:n())
