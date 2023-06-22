
# tables using BUGS output

library(tidyr)

outcome_names <-
  c("COVID_infection",
    "Symptomatic_infection",
    "Severe_Infection_All",
    "Hospitalizations",
    "Deaths")

tx_levels <- c("placebo", "pfizer", "moderna")

for (outcome in outcome_names) {
  load(glue::glue("data/jagsfit_{outcome}.RData"))
  
  BUGSoutput <- jagsfit$BUGSoutput
  simsmatrix <- BUGSoutput$sims.matrix
  
  # CROSS-TABLE FOR ODD RATIO
  
  tab_dat <-
    simsmatrix |> 
    as.data.frame() |> 
    mutate(or12 = exp(`lor[1,2]`),
           or13 = exp(`lor[1,3]`),
           or23 = exp(`lor[2,3]`)) |> 
    # summarise to median, lower and upper
    reframe(placebo_pfizer = round(quantile(or12, c(0.25, 0.5, 0.75)), 2),
            placebo_moderna = round(quantile(or13, c(0.25, 0.5, 0.75)), 2),
            pfizer_moderna = round(quantile(or23, c(0.25, 0.5, 0.75)), 2),
            q = c(2.5, 50, 97.5)) |>
    # diagonal elements
    mutate(placebo_placebo = 1,
           pfizer_pfizer = 1,
           moderna_moderna = 1) |> 
    reshape2::melt(id.vars = "q",
                   variable.name = "treatment") |> 
    dcast(treatment ~ q) |> 
    # concatenate values with bounds in brackets
    mutate(formatted = paste0(`50`, " (", `2.5`, ",", `97.5`, ")")) |> 
    # reference and comparison treatments
    separate_wider_delim(cols = treatment, delim = "_", names = c("ref", "comp")) |> 
    # rearrange treatment order
    mutate(ref = factor(ref, levels = tx_levels),
           comp = factor(comp, levels = tx_levels)) |> 
    select(-`2.5`, -`50`, -`97.5`) |> 
    dcast(comp ~ ref)
  
  write.csv(tab_dat, file = glue::glue("tables/TableOR_{outcome}.csv"))
  
  
  # CROSS-TABLE FOR PROBABILITY BETTER
  
  tab_better <-
    simsmatrix |> 
    as.data.frame() |> 
    select(starts_with("r")) |> 
    # logical preference
    mutate(best12 = `rk[1]` - `rk[2]` > 0,
           best13 = `rk[1]` - `rk[3]` > 0,
           best23 = `rk[2]` - `rk[3]` > 0) |> 
    summarise(placebo_pfizer = round(mean(best12), 2),
              placebo_moderna = round(mean(best13), 2),
              pfizer_moderna = round(mean(best23), 2)) |> 
    # diagonal elements
    mutate(placebo_placebo = 0,
           pfizer_pfizer = 0,
           moderna_moderna = 0) |> 
    reshape2::melt(variable.name = "treatment") |> 
    # reference and comparison treatments
    separate_wider_delim(cols = treatment, delim = "_", names = c("ref", "comp")) |> 
    mutate(ref = factor(ref, levels = tx_levels),
           comp = factor(comp, levels = tx_levels)) |> 
    dcast(comp ~ ref)
  
  write.csv(tab_better, file = glue::glue("tables/TableBetter_{outcome}.csv"))
}


