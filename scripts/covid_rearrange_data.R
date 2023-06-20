
# clean covid data
# an prep for BUGS analysis

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
                is.na(Subgroup.Outcome.not.chosen.for.NMA) |     ##TODO: which is it we want?
                  Subgroup.Outcome.not.chosen.for.NMA == " " |
                  Subgroup.Outcome.not.chosen.for.NMA == "Base case",  #) |> 
                VE.against.infection == "Y") |>
  select(Ref.ID, Study.design, Intervention.name..standardized., Total.N, n.of.events)


# dat <- read.csv(file = "data/cleaned_covid_data.csv")

dat_clean <- 
  dat |> 
  mutate(
    n.of.events = ifelse(n.of.events == "NR", NA, n.of.events),
    Total.N = ifelse(Total.N == "NR", NA, Total.N),
    ##TODO: remove *
    Intervention.name..standardized. =
      ifelse(Intervention.name..standardized. == "Moderna (mRNA-1273)",
             "Moderna",
             ifelse(Intervention.name..standardized. == "No vaccine",
                    "Unvaccinated",
                    Intervention.name..standardized.)),
    Intervention.name..standardized. = ifelse(Intervention.name..standardized. == "Pfizer-BioNTech\r\nCoronaVac",
                                              "PfizerBiontech",
                                              Intervention.name..standardized.),
    Intervention.name..standardized. = factor(Intervention.name..standardized.),
    interv_id = as.numeric(Intervention.name..standardized.),
    Study.design = ifelse(Study.design %in% c("Prospective cohort study", "Prospective, observational study"),
                          "Prospective",
                          ifelse(Study.design %in% c("Retrospective cohort study", "Rerospective cohort study", "Retrospective observational study"),
                                 "Retrospective",
                                 Study.design))) |> 
  group_by(Ref.ID) |>
  arrange(interv_id) |> 
  mutate(tx = 1:n())

