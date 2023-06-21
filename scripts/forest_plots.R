

library(ggplot2)
require(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(Cairo)
library(R2OpenBUGS)
library(coda)
library(R.utils)


# CREATE GRAPHS

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

dev.off
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

jpeg(
  filename = "plotOR1.jpg",
  width = 8,
  height = 5,
  units = "in",
  res = 500
)
plot(plotOR1)
dev.off()


maxyscale2 <- max(resultsORmaxt$X97.5.inv) * 1.3
ORmaxtlabel1 <- format(round(resultsORmaxt$X50.inv, 2), nsmall = 2)
ORmaxtlabel2 <- format(round(resultsORmaxt$X2.5.inv, 2), nsmall = 2)
ORmaxtlabel3 <- format(round(resultsORmaxt$X97.5.inv, 2), nsmall = 2)
ORmaxtlabel4 <- cbind(ORmaxtlabel2, ORmaxtlabel3)
ORmaxtlabel5 <- rep(NA, (nt - 1))
ORmaxtlabel7 <- rep(NA, (nt - 1))
ORmaxtlabel8 <- rep(")", (nt - 1))
ORmaxtlabel <- rep(NA, (nt - 1))
for (k in 1:(nt - 1)) {
  ORmaxtlabel5[k] <- paste(ORmaxtlabel4[k, ], collapse = ",")
}
ORmaxtlabel6 <- cbind(ORmaxtlabel1, ORmaxtlabel5)
for (k in 1:(nt - 1)) {
  ORmaxtlabel7[k] <- paste(ORmaxtlabel6[k, ], collapse = " (")
}
ORmaxtlabel9 <- cbind(ORmaxtlabel7, ORmaxtlabel8)
for (k in 1:(nt - 1)) {
  ORmaxtlabel[k] <- paste(ORmaxtlabel9[k, ], collapse = "")
}
plotORmaxt <-
  ggplot(
    resultsORmaxt,
    aes(
      x = factor(treatmentORmaxt),
      y = X50.inv,
      ymin = X2.5.inv,
      ymax = X97.5.inv,
      colour = factor(treatmentORmaxt)
    )
  ) +
  geom_pointrange() +
  coord_flip() +
  geom_text(aes(label = ORmaxtlabel, y = maxyscale2 * 0.9),
            size = 3,
            color = 'black') +
  xlab("Treatment") +
  ylab(ytitle1b) +
  scale_colour_manual(values = txcolor[1:(nt - 1)]) +
  labs(title = charttitle) +
  theme(title = element_text(vjust = +2)) +
  theme(axis.title.x = element_text(vjust = 0)) +
  theme(axis.title.y = element_text(vjust = 0.2)) +
  theme(panel.background = element_rect(fill = 'white', color = 'white')) +
  theme(axis.line = element_line(colour = "black", size = 0.1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, maxyscale2)) +
  scale_x_discrete(labels = txnames[1:(nt - 1)]) +
  theme(legend.position = "none") +
  geom_hline(yintercept = c(1), linetype = "dotted")

jpeg(
  filename = "plotORmaxt.jpg",
  width = 8,
  height = 5,
  units = "in",
  res = 500
)
plot(plotORmaxt)
dev.off()


##    OR1 plot on LOG SCALE ############################
dev.off()
#maxyscale<-25
maxyscale <- max(resultsOR1$X97.5.) * 1.3

ORlabel1 <- format(round(resultsOR1$X50., 2), nsmall = 1)
ORlabel2 <- format(round(resultsOR1$X2.5., 2), nsmall = 1)
ORlabel3 <- format(round(resultsOR1$X97.5., 2), nsmall = 1)
ORlabel4 <- cbind(ORlabel2, ORlabel3)
ORlabel5 <- rep(NA, (nt - 1))
ORlabel6 <- rep(NA, (nt - 1))
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

plotOR1log <-
  ggplot(resultsOR1,
         aes(
           x = factor(treatmentOR1),
           y = X50.,
           ymin = X2.5.,
           ymax = X97.5.,
           colour = factor(treatmentOR1)
         )) +
  geom_pointrange(pch = 20) +
  coord_flip() +
  geom_text(aes(label = ORlabel, y = 10),
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
  theme(axis.line = element_line(colour = "black", size = 0.01)) +
  scale_y_log10(
    expand = c(0.001, 0),
    limits = c(0.5, 20),
    breaks = c(0.01, 0.1, 0.5, 1, 32)
  ) +
  scale_x_discrete(labels = txnames[2:nt]) +
  theme(legend.position = "none") +
  geom_hline(yintercept = c(1), linetype = "dotted")
jpeg(
  filename = "plotOR1log.jpg",
  width = 8,
  height = 5,
  units = "in",
  res = 500
)
plot(plotOR1log)
dev.off()


#    ORmaxt plot on LOG SCALE ############################
#maxyscale2<-22
maxyscale2 <- max(resultsORmaxt$X97.5.) * 3

ORmaxtlabel1 <- format(round(resultsORmaxt$X50.inv, 2), nsmall = 2)
ORmaxtlabel2 <- format(round(resultsORmaxt$X2.5.inv, 2), nsmall = 2)
ORmaxtlabel3 <- format(round(resultsORmaxt$X97.5.inv, 2), nsmall = 2)
ORmaxtlabel4 <- cbind(ORmaxtlabel2, ORmaxtlabel3)
ORmaxtlabel5 <- rep(NA, (nt - 1))
ORmaxtlabel7 <- rep(NA, (nt - 1))
ORmaxtlabel8 <- rep(")", (nt - 1))
ORmaxtlabel <- rep(NA, (nt - 1))
for (k in 1:(nt - 1)) {
  ORmaxtlabel5[k] <- paste(ORmaxtlabel4[k, ], collapse = ",")
}
ORmaxtlabel6 <- cbind(ORmaxtlabel1, ORmaxtlabel5)
for (k in 1:(nt - 1)) {
  ORmaxtlabel7[k] <- paste(ORmaxtlabel6[k, ], collapse = " (")
}
ORmaxtlabel9 <- cbind(ORmaxtlabel7, ORmaxtlabel8)
for (k in 1:(nt - 1)) {
  ORmaxtlabel[k] <- paste(ORmaxtlabel9[k, ], collapse = "")
}
plotORmaxtlog <-
  ggplot(
    resultsORmaxt,
    aes(
      x = factor(treatmentORmaxt),
      y = X50.inv,
      ymin = X2.5.inv,
      ymax = X97.5.inv,
      colour = factor(treatmentORmaxt)
    )
  ) +
  geom_pointrange(pch = 20) +
  coord_flip() +
  geom_text(aes(label = ORmaxtlabel, y = 5),
            size = 3,
            color = 'black') +
  xlab("Treatment") +
  ylab(ytitle1b) +
  scale_colour_manual(values = txcolor[1:(nt - 1)]) +
  labs(title = charttitle) +
  theme(title = element_text(vjust = +2)) +
  theme(axis.title.x = element_text(vjust = 0)) +
  theme(axis.title.y = element_text(vjust = 0.2)) +
  theme(panel.background = element_rect(fill = 'white', color = 'white')) +
  theme(axis.line = element_line(colour = "black", size = 0.1)) +
  scale_y_log10(
    expand = c(0.1, 0),
    limits = c(0.1, 20),
    breaks = c(0.01, 0.1, 0.5, 1, 5)
  ) +     #breaks=c(0.1,0.2,0.5,1,2,5,10,20))+
  scale_x_discrete(labels = txnames[1:(nt - 1)]) +
  theme(legend.position = "none") +
  geom_hline(yintercept = c(1), linetype = "dotted")

jpeg(
  filename = "plotORmaxtlog.jpg",
  width = 8,
  height = 5,
  units = "in",
  res = 500
)
plot(plotORmaxtlog)
dev.off()


resultsTlabel <- round(resultsT$X50., 2)
plotT <-
  ggplot(resultsT, aes(x = factor(treatment), X50., fill = factor(treatment))) +
  geom_bar(stat = "identity") +
  geom_text(aes(
    label = format(resultsTlabel, nsmall = 2),
    y = (X50. + 0.03)
  ), size = 3) +
  scale_fill_manual(values = txcolor) +
  scale_x_discrete(labels = txnames) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.),
                width = .1,
                color = 'gray') +
  xlab("Treatment") +
  ylab(ytitle2) +
  labs(title = charttitle) +
  theme(title = element_text(vjust = +2)) +
  theme(axis.title.x = element_text(vjust = 0)) +
  theme(axis.title.y = element_text(vjust = 0.2)) +
  theme(panel.background = element_rect(fill = 'white', color = 'white')) +
  theme(axis.line = element_line(colour = "black", size = 0.1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")

jpeg(
  filename = "plotT.jpg",
  width = 8,
  height = 5,
  units = "in",
  res = 500
)
plot(plotT)
dev.off()


plotB <-
  ggplot(resultsB, aes(
    x = rank,
    y = mean,
    group = treatment,
    colour = factor(treatment)
  )) +
  geom_line(size = 0.8) +
  xlab("Rank") +
  ylab("Probability") +
  labs(title = charttitle) +
  theme(title = element_text(vjust = +2)) +
  theme(legend.key = element_rect(fill = "white")) +
  guides(color = guide_legend(
    title = "",
    ncol = 1,
    keywidth = 2,
    order = 1
  )) +
  theme(axis.title.x = element_text(vjust = 0)) +
  theme(axis.title.y = element_text(vjust = 0.2)) +
  scale_color_manual(values = txcolor, labels = txnames) +
  theme(panel.background = element_rect(fill = 'white', color = 'white')) +
  theme(axis.line = element_line(colour = "black", size = 0.1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, nt)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))

jpeg(
  filename = "plotB.jpg",
  width = 8,
  height = 5,
  units = "in",
  res = 500
)
plot(plotB)
dev.off()


#CROSS-TABLE OF RELATIVE TREATMENT EFFECTS
firstOR <- min(grep("OR", rownames(results)))
lastOR <- max(grep("OR", rownames(results)))
resultsORmed <-
  matrix(format(round(results[firstOR:lastOR, 5], 2), nsmall = 2))
resultsORlow <-
  matrix(format(round(results[firstOR:lastOR, 3], 2), nsmall = 2))
resultsORhigh <-
  matrix(format(round(results[firstOR:lastOR, 7], 2), nsmall = 2))


rowCount <- nt
TableOR = matrix(nrow = rowCount , ncol = rowCount)
for (i in 1:(rowCount)) {
  for (j in 1:rowCount)
    if (i == j) {
      TableOR[i, j] <- 1
      
    }
}
TableORhigh = matrix(nrow = rowCount , ncol = rowCount)
for (i in 1:(rowCount)) {
  for (j in 1:rowCount)
    if (i == j) {
      TableORhigh[i, j] <- "1"
      
    }
}
TableORlow = matrix(nrow = rowCount , ncol = rowCount)
for (i in 1:(rowCount)) {
  for (j in 1:rowCount)
    if (i == j) {
      TableORlow[i, j] <- "1"
      
    }
}
TableOR[is.na(TableOR)] <- resultsORmed
TableORlow[is.na(TableORlow)] <- resultsORlow
TableORhigh[is.na(TableORhigh)] <- resultsORhigh


for (i in 1:(rowCount)) {
  for (j in 1:rowCount) {
    TableOR[i, j] <-
      paste(cbind(TableOR[i, j], " (", TableORlow[i, j], ",", TableORhigh[i, j], ")"),
            collapse = "")
    
  }
}
colnames(TableOR) <- txnames
rownames(TableOR) <- txnames
write.csv(TableOR, file = "TableOR.csv")


#CROSS-TABLE FOR PROBABILITY BETTER
firstBetter <- min(grep("better", rownames(results)))
lastBetter <- max(grep("better", rownames(results)))
resultsBetter <-
  matrix(format(round(results[firstBetter:lastBetter, 1], 2), nsmall = 2))

rowCount <- nt
TableBetter = matrix(nrow = rowCount , ncol = rowCount)
for (i in 1:(rowCount)) {
  for (j in 1:rowCount)
    if (i == j) {
      TableBetter[i, j] <- "na"
      
    }
}
TableBetter[is.na(TableBetter)] <- resultsBetter

colnames(TableBetter) <- txnames
rownames(TableBetter) <- txnames
write.csv(TableBetter, file = "TableBetter.csv")


#CROSS-TABLE OF PREDICTED RELATIVE TREATMENT EFFECTS
firstnewOR <- min(grep("newOddsR", rownames(results)))
lastnewOR <- max(grep("newOddsR", rownames(results)))
resultsnewORmed <-
  matrix(format(round(results[firstnewOR:lastnewOR, 5], 2), nsmall = 2))
resultsnewORlow <-
  matrix(format(round(results[firstnewOR:lastnewOR, 3], 2), nsmall = 2))
resultsnewORhigh <-
  matrix(format(round(results[firstnewOR:lastnewOR, 7], 2), nsmall = 2))


rowCount <- nt
TablenewOR = matrix(nrow = rowCount , ncol = rowCount)
for (i in 1:(rowCount)) {
  for (j in 1:rowCount)
    if (i == j) {
      TablenewOR[i, j] <- 1
      
    }
}
TablenewORhigh = matrix(nrow = rowCount , ncol = rowCount)
for (i in 1:(rowCount)) {
  for (j in 1:rowCount)
    if (i == j) {
      TablenewORhigh[i, j] <- "1"
      
    }
}
TablenewORlow = matrix(nrow = rowCount , ncol = rowCount)
for (i in 1:(rowCount)) {
  for (j in 1:rowCount)
    if (i == j) {
      TablenewORlow[i, j] <- "1"
      
    }
}
TablenewOR[is.na(TablenewOR)] <- resultsnewORmed
TablenewORlow[is.na(TablenewORlow)] <- resultsnewORlow
TablenewORhigh[is.na(TablenewORhigh)] <- resultsnewORhigh


for (i in 1:(rowCount)) {
  for (j in 1:rowCount) {
    TablenewOR[i, j] <-
      paste(cbind(TablenewOR[i, j], " (", TablenewORlow[i, j], ",", TablenewORhigh[i, j], ")"),
            collapse = "")
    
  }
}
colnames(TablenewOR) <- txnames
rownames(TablenewOR) <- txnames
write.csv(TablenewOR, file = "TablenewOR.csv")


#CROSS-TABLE FOR PROBABILITY BETTER PREDICTION FOR NEW COMPARISON
firstnewBetter <- min(grep("newbtter", rownames(results)))
lastnewBetter <- max(grep("newbtter", rownames(results)))
resultsnewBetter <-
  matrix(format(round(results[firstnewBetter:lastnewBetter, 1], 2), nsmall =
                  2))

rowCount <- nt
TablenewBetter = matrix(nrow = rowCount , ncol = rowCount)
for (i in 1:(rowCount)) {
  for (j in 1:rowCount)
    if (i == j) {
      TablenewBetter[i, j] <- "na"
      
    }
}
TablenewBetter[is.na(TablenewBetter)] <- resultsnewBetter

colnames(TablenewBetter) <- txnames
rownames(TablenewBetter) <- txnames
write.csv(TablenewBetter, file = "TablenewBetter.csv")
