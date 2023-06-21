
# forest plots

library(ggplot2)
require(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(Cairo)
library(R2OpenBUGS)
library(coda)
library(R.utils)

load("data/jagsfit_COVID_infection.RData")

BUGSoutput <- jagsfit$BUGSoutput
simsmatrix <- BUGSoutput$sims.matrix

treatmentOR1 <- treatment[2:nt]
firstOR1 <- min(grep("OR", rownames(results)))
lastOR1 <- firstOR1 + nt - 2
resultsOR1 <- data.frame(results[firstOR1:lastOR1, ], treatmentOR1)

treatmentORmaxt <- treatment[1:nt - 1]
lastORmaxt <- max(grep("OR", rownames(results)))
firstORmaxt <- lastORmaxt - (nt - 2)
resultsORmaxt <-
  data.frame(results[firstORmaxt:lastORmaxt, ], treatmentORmaxt)
resultsORmaxt$meaninv <- 1 / resultsORmaxt$mean
resultsORmaxt$X97.5.inv <- 1 / resultsORmaxt$X2.5.
resultsORmaxt$X2.5.inv <- 1 / resultsORmaxt$X97.5.
resultsORmaxt$X50.inv <- 1 / resultsORmaxt$X50.

firstT <- min(grep("T", rownames(results)))
lastT <- max(grep("T", rownames(results)))
resultsT <- data.frame(results[firstT:lastT, ], treatment)

rank <- sort(rep(1:nt, nt))
firstB <- min(grep("prk", rownames(results)))
lastB <- max(grep("prk", rownames(results)))
resultsB <- data.frame(results[firstB:lastB, ], rank, treatment)

maxyscale <- max(resultsOR1$X97.5.) * 1.3

ORlabel1 <- format(round(resultsOR1$X50., 2), nsmall = 2)
ORlabel2 <- format(round(resultsOR1$X2.5., 2), nsmall = 2)
ORlabel3 <- format(round(resultsOR1$X97.5., 2), nsmall = 2)
ORlabel4 <- cbind(ORlabel2, ORlabel3)
ORlabel5 <- rep(NA, (nt - 1))
ORlabel7 <- rep(NA, (nt - 1))
ORlabel8 <- rep(")", (nt - 1))
ORlabel <- rep(NA, (nt - 1))

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

plotOR1 <-
  ggplot(resultsOR1,
         aes(
           x = factor(treatmentOR1),
           y = X50.,
           ymin = X2.5.,
           ymax = X97.5.,
           colour = factor(treatmentOR1)
         )) +
  geom_pointrange() +
  coord_flip() +
  geom_text(aes(label = ORlabel, y = maxyscale * 0.9),
            size = 3,
            color = 'black') +
  xlab("Treatment") +
  ylab(ytitle1) +
  scale_colour_manual(values = txcolor[2:nt]) +
  labs(title = charttitle) +
  theme(title = element_text(vjust = +2)) +
  theme(axis.title.x = element_text(vjust = 0)) +
  theme(axis.title.y = element_text(vjust = 0.2)) +
  theme(panel.background = element_rect(fill = 'white', color = 'white')) +
  theme(axis.line = element_line(colour = "black", size = 0.1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, maxyscale)) +
  scale_x_discrete(labels = txnames[2:nt]) +
  theme(legend.position = "none") +
  geom_hline(yintercept = c(1), linetype = "dotted")

ggsave(
  filename = "plotOR1.jpg",
  width = 8,
  height = 5,
  units = "in",
  res = 500)


