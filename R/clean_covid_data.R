
#' Clean COVID data
#'
#' @param xl_data Original data set from SLR
#' @param outcome_nm Outcome name
#' @param basecase Base case; logical
#' @param tx_levels Treatment levels
#'
#' @return
#' @export
#'
clean_covid_data <- function(xl_data, outcome_nm, basecase, tx_levels) {
  dat_clean <-
    xl_data |> 
    data.frame(check.names = TRUE) |>
    dplyr::filter(Included.in.NMA == 1,
                  if (basecase) Base.case == "X" else FALSE,
                  get({{outcome_nm}}) == "Y") |>                 
    mutate(
      n.of.events = as.numeric(ifelse(n.of.events == "NR", NA, n.of.events)),
      Total.N = as.numeric(ifelse(Total.N == "NR", NA, Total.N)),
      Event.rate = as.numeric(Event.rate),
      n.of.events = ifelse(is.na(n.of.events), Event.rate*Total.N, n.of.events),
      n.of.events = round(n.of.events, 0),
      Total.N = round(Total.N, 0),
      Intervention.name..standardized. = factor(Intervention.name..standardized.,
                                                levels = tx_levels),
      Study.design = ifelse(Study.design %in% c("Prospective cohort study",
                                                "Prospective, observational study"),
                            yes = "Prospective",
                            no = ifelse(Study.design %in%
                                          c("Retrospective cohort study",
                                            "Rerospective cohort study",
                                            "Retrospective observational study"),
                                        yes = "Retrospective",
                                        no = Study.design))) |> 
    select(Ref.ID, Study.design, Design.ID, Intervention.name..standardized., Total.N, n.of.events) |> 
    mutate(tx_id = as.numeric(Intervention.name..standardized.)) |> 
    group_by(Ref.ID) |>
    arrange(tx_id) |> 
    mutate(arm = 1:n(),
           na = n()) |>
    arrange(arm) |> 
    ungroup() |> 
    arrange(Design.ID, Ref.ID) |>
    rename(tx_name = Intervention.name..standardized.)
  
  # reshape to wide format for BUG model input
  
  BUGS_input_data <-
    dat_clean |> 
    as.data.frame() |> 
    reshape(idvar = c("Ref.ID", "Study.design", "Design.ID", "na"),
            timevar = "arm", direction = "wide") |> 
    relocate(starts_with("Total.N"),
             starts_with("n.of.events"),
             starts_with("tx_id"),
             .after = c("Ref.ID", "Design.ID", "na"))
  
  colnames(BUGS_input_data) <- 
    names(BUGS_input_data) |> 
    gsub("Total.N.", replacement = "n", x = _) |> 
    gsub("n.of.events.", replacement = "r", x = _) |> 
    gsub("tx_id.", replacement = "t", x = _)
  
  BUGS_input_data
}
