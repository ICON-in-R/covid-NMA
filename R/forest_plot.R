
#' Forest plot
#'
#' @param outcome 
#'
#' @return
#' @export
#'
forest_plot <- function(outcome) {
  
  load(glue::glue("data/jagsfit_{outcome}.RData"))
  
  BUGSoutput <- jagsfit$BUGSoutput
  simsmatrix <- BUGSoutput$sims.matrix
  
  plot_dat <- 
    simsmatrix |> 
    as.data.frame() |> 
    reframe(lor2 = quantile(`lor[1,2]`, c(0.25, 0.5, 0.75)),
            lor3 = quantile(`lor[1,3]`, c(0.25, 0.5, 0.75)),
            Pfizer = exp(lor2),
            Moderna = exp(lor3),
            q = c(2.5, 50, 97.5)) |>
    select(-lor2, -lor3) |> 
    reshape2::melt(id.vars = "q",
                   variable.name = "treatment") |> 
    dcast(treatment ~ q) |> 
    data.frame(check.names = TRUE) |> 
    mutate(treatment = factor(treatment))
  
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
  
  ytitle <- "Odds Ratio vs. Placebo"
  maxyscale<-max(plot_dat$X97.5)*1.3
  txcolor <- c("#000000", "#00CCFF", "#FF0000")
  txnames <- plot_dat$treatment
  charttitle <- glue::glue("Hierarchical RE NMA model, {outcome_names}")
  ytitle1b <- c("Odds Ratio of Moderna vs. Other")
  ytitle2 <- c("Probability of Covid-19 infection")
  
  ggplot(plot_dat,
         aes(
           x = treatment,
           y = X50,
           ymin = X2.5,
           ymax = X97.5,
           colour = treatment)) +
    geom_pointrange() +
    coord_flip() +
    geom_text(aes(label = ORlabel, y = maxyscale * 0.9),
              size = 3,
              color = 'black') +
    xlab("Treatment") +
    ylab(ytitle) +
    scale_colour_manual(values = txcolor[2:nt]) +
    labs(title = charttitle) +
    theme(title = element_text(vjust = 2)) +
    theme(axis.title.x = element_text(vjust = 0)) +
    theme(axis.title.y = element_text(vjust = 0.2)) +
    theme(panel.background = element_rect(fill = 'white', color = 'white')) +
    theme(axis.line = element_line(colour = "black", linewidth = 0.1)) +
    # scale_y_continuous(expand = c(0, 0), limits = c(0, maxyscale)) +
    # scale_x_discrete(labels = txnames[2:nt]) +
    theme(legend.position = "none") +
    geom_hline(yintercept = c(1), linetype = "dotted")
}
