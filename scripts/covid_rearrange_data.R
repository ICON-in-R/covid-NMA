

library(dplyr)

dat <- read.csv(file = "data/cleaned_covid_data.csv")

dat_clean <- 
  dat |> 
  mutate(Intervention.name..full. = factor(Intervention.name..full.),
         interv_id = as.numeric(Intervention.name..full.)) |> 
  group_by(Ref.ID) |>
  arrange(interv_id) |> 
  mutate(tx = 1:n())
