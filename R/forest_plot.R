
#' Forest plot
#'
#' @param outcome Name; string
#' @param vs_placebo Placebo as comparison otherwise Moderna vs all other comparisons; logical
#' @param logOR Scale; logical
#' @param vacc_effic Vaccine efficiency, defined as 1-OR; logical
#'
#' @import dplyr reshape2
#' @return
#' @export
#'
forest_plot <- function(outcome,
                        vs_placebo = TRUE,
                        logOR = FALSE,
                        vacc_effic = FALSE) {
  # rename vaccines
  new_labels <- 
    c(Pfizer = "Comirnaty",
      Moderna = "Spikevax",
      Placebo = "Placebo/Unvaccinated")
  
  # output from jags NMA model
  load(glue::glue("data/jagsfit_{outcome}.RData"))
  
  BUGSoutput <- jagsfit$BUGSoutput
  simsmatrix <- BUGSoutput$sims.matrix
  
  outcome_text <- 
    switch(outcome,
           COVID_infection = "Covid-19 infection",
           Symptomatic_infection = "Symptomatic Covid-19 infection",
           Severe_Infection_All = "Severe Covid-19 infection",
           Severe_infections__WHO_ICU_addmission_ = "Severe Covid-19 infection",
           Hospitalizations = "Hospitalisation due to Covid-19 infection",
           Deaths = "Death after Covid-19 infection")
  
  if (vs_placebo) {
    treatment_nm <- "Moderna"
    OR_nm <- "or12"
    ytitle <- if (vacc_effic) {
      glue::glue("Vaccine efficacy against {outcome_text}")
    } else {
      "Odds Ratio vs. Placebo/Unvaccinated"
    }
    txcolor <- c("#00CCFF", "#FF0000")
    favours_label <- c("favours treatment", "favours placebo")
  } else {
    treatment_nm <- "Placebo"
    OR_nm <- "or23"
    ytitle <- c("Odds Ratio of Spikevax vs. Other")
    txcolor <- c("#000000", "#00CCFF")
    favours_label <- c("favours Spikevax", "favours alternative")
  }
  
  plot_dat <- 
    simsmatrix |> 
    as.data.frame() |> 
    mutate(or12 = exp(`lor[1,2]`),
           or13 = exp(`lor[1,3]`),
           or23 = exp(`lor[2,3]`),
           # or31 = 1/or13,
           # or32 = or13/or12
           or12 = if(vacc_effic) 1-or12 else or12,
           or13 = if(vacc_effic) 1-or13 else or13,
           or23 = if(vacc_effic) 1-or23 else or23
    ) |> 
    reframe("{treatment_nm}" := quantile(or13, c(0.25, 0.5, 0.75)),
            "Pfizer" = quantile(!!as.name(OR_nm), c(0.25, 0.5, 0.75)),
            q = c(2.5, 50, 97.5)) |>
    reshape2::melt(id.vars = "q",
                   variable.name = "treatment") |> 
    dcast(treatment ~ q) |> 
    data.frame(check.names = TRUE) |> 
    mutate(treatment = factor(treatment, levels = c("Placebo", "Pfizer", "Moderna"))) |> 
    arrange(treatment) |> 
    mutate(X2.5 = if(vacc_effic) max(0, X2.5) else X2.5)    #TODO: hack so that ggplot doesn't remove segment 
  
  nt <- 3
  
  ORlabel1 <- format(round(plot_dat$X50, 2), nsmall = 2)
  ORlabel2 <- format(round(plot_dat$X2.5, 2), nsmall = 2)
  ORlabel3 <- format(round(plot_dat$X97.5, 2), nsmall = 2)
  ORlabel4 <- cbind(ORlabel2, ORlabel3)
  ORlabel5 <- rep(NA, nt - 1)
  ORlabel7 <- rep(NA, nt - 1)
  ORlabel8 <- rep(")", nt - 1)
  ORlabel <- rep(NA, nt - 1)
  
  for (k in 1:(nt - 1)) {
    ORlabel5[k] <- paste(ORlabel4[k, ], collapse = ",")
  }
  
  ORlabel6 <- cbind(ORlabel1, ORlabel5)
  for (k in 1:(nt - 1)) {
    ORlabel7[k] <- paste(ORlabel6[k, ], collapse = " (")
  }
  
  ORlabel9 <- cbind(ORlabel7, ORlabel8)
  for (k in 1:(nt - 1)) {
    ORlabel[k] <- paste(ORlabel9[k, ], collapse = "")
  }
  
  maxyscale <- max(plot_dat$X97.5)*1.3
  txnames <- plot_dat$treatment
  
  charttitle <- glue::glue("Hierarchical RE NMA model, {outcome_text}")
  
  gg <- 
    ggplot(plot_dat,
           aes(
             x = treatment,
             y = X50,
             ymin = X2.5,
             ymax = X97.5,
             colour = treatment)) +
    geom_pointrange() +
    coord_flip() +
    xlab("Intervention") +
    ylab(ytitle) +
    scale_colour_manual(values = txcolor) +
    labs(title = charttitle) +
    theme(title = element_text(vjust = 2)) +
    theme(axis.title.x = element_text(vjust = 0)) +
    theme(axis.title.y = element_text(vjust = 0.2)) +
    theme(panel.background = element_rect(fill = 'white', color = 'white')) +
    theme(axis.line = element_line(colour = "black", linewidth = 0.1)) +
    scale_x_discrete(labels = new_labels) +
    theme(legend.position = "none") +
    geom_hline(yintercept = c(1), linetype = "dotted")
  
  if (logOR) {
    gg +
      # scale_y_log10() +
      scale_y_continuous(trans = "log10",
                         expand = expansion(add = 0.5)) + #,
      # limits = c(min(X2.5), max(1, maxyscale))) +
      geom_text(aes(label = ORlabel, y = maxyscale + 0.5),
                size = 3, color = 'black') +
      geom_text(aes(label = favours_label,
                    x = 0.5, y = c(0.9, 1.1)), hjust = c(1,0),
                size = 3, color = 'black') +
      geom_text(aes(label = paste("DIC    pD \n",
                                  paste(c(DIC =  round(jagsfit$BUGSoutput$DIC, 2),
                                          pD = round(jagsfit$BUGSoutput$pD, 2)), collapse = " ")),
                    x = 0.5, y =  max(1, maxyscale) + 10),
                hjust = 1, size = 3, color = 'black')
    
  } else {
    gg +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(1, maxyscale) + 1)) +
      geom_text(aes(label = ORlabel, y = maxyscale * 0.9),
                size = 3, color = 'black') +
      geom_text(aes(label = favours_label,
                    x = 0.5, y = c(0.9, 1.1)), hjust = c(1,0),
                size = 3, color = 'black') +
          geom_text(aes(label = paste("DIC    pD \n",
                                      paste(c(DIC =  round(jagsfit$BUGSoutput$DIC, 2),
                                              pD = round(jagsfit$BUGSoutput$pD, 2)), collapse = " ")),
                        x = 0.5, y =  max(1, maxyscale) + 1),
                    hjust = 1, size = 3, color = 'black')
  }
}
