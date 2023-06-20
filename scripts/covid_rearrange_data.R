
# clean covid data
# an prep for BUGS analysis

library(dplyr)


filename <-
  "../../ICON/data/4428_0013_Covid_Vaccine_Spikevax_DAT_All included studies_v0.4_09Junel2023_NMA_MN_AC_SK.xlsx"

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

# filter rows

# run basecase scenario?
basecase <- FALSE

dat <-
  xl_data |> 
  data.frame(check.names = TRUE) |>
  dplyr::filter(Included.in.NMA == 1,
                if (basecase) Base.case == "X" else TRUE,
                COVID.infection == "Y") |> 
  select(Ref.ID, Study.design, Design.ID, Intervention.name..standardized., Total.N, n.of.events)


# dat <- read.csv(file = "data/cleaned_covid_data.csv")

dat_clean <- 
  dat |> 
  mutate(
    n.of.events = ifelse(n.of.events == "NR", NA, n.of.events),
    Total.N = ifelse(Total.N == "NR", NA, Total.N),
    Intervention.name..standardized. = factor(Intervention.name..standardized.),
    Study.design = ifelse(Study.design %in% c("Prospective cohort study",
                                              "Prospective, observational study"),
                          yes = "Prospective",
                          no = ifelse(Study.design %in%
                                        c("Retrospective cohort study",
                                          "Rerospective cohort study",
                                          "Retrospective observational study"),
                                      yes = "Retrospective",
                                      no = Study.design))) |> 
  group_by(Ref.ID) |>
  arrange(Design.ID) |> 
  mutate(arm = 1:n())

##TODO:
## rearrange from long to wide format

# pivot_wider() by arm

library(reshape2)

tx_wide <- dcast(dat_clean, Ref.ID ~ arm, value.var = "Intervention.name..standardized.")
N_wide <- dcast(dat_clean, Ref.ID ~ arm, value.var = "Total.N")
n_wide <- dcast(dat_clean, Ref.ID ~ arm, value.var = "n.of.events")

xx <- 
  merge(tx_wide, N_wide, by = "Ref.ID") |> 
  merge(n_wide, by = "Ref.ID")

write.csv(xx, file = "data/BUGS_input_data.csv")
