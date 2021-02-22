library(tidyverse)
rm(list = ls())
# SETANDO A HOME COMO WORK DIR
setwd("~/Mestrado/TrabalhoMC2");
data <- read.table("data/data_t3-t4.txt", header = TRUE);
instances <- unique(as.character(data$inst));


# CALCALO DO TAMANHO DE
vargha.delaney <- function(r1, r2) {
  m <- length(r1);
  n <- length(r2);
  return ((sum(rank(c(r1, r2))[seq_along(r1)]) / m - (m + 1) / 2) / n);
}

compareColumns <- c("NSGA/MAR", "NSGA/SH", "NSGA/CPM");
gd <- matrix(nrow=length(instances), ncol=length(compareColumns), dimnames=list(instances, compareColumns));
hv <- matrix(nrow=length(instances), ncol=length(compareColumns), dimnames=list(instances, compareColumns));
best <- matrix(nrow=length(instances), ncol=length(compareColumns), dimnames=list(instances, compareColumns));

for (inst in instances)
{
  inst <- instances[which(instances==inst)];
  
  mar <- subset(data, inst == inst & config == "MAR");
  CPM <- subset(data, inst == inst & config == "CPM");
  sh <- subset(data, inst == inst & config == "SH");
  nsNSGA <- subset(data, inst == inst & config == "nsga150k2x");
  
  best[inst, "NSGA/MAR"] <- vargha.delaney(nsNSGA$best, mar$best);
  best[inst, "NSGA/SH"] <- vargha.delaney(nsNSGA$best, sh$best);
  best[inst, "NSGA/CPM"] <- vargha.delaney(nsNSGA$best, CPM$best);
  
  hv[inst, "NSGA/MAR"] <- vargha.delaney(nsNSGA$hv, mar$hv);
  hv[inst, "NSGA/SH"] <- vargha.delaney(nsNSGA$hv, sh$hv);
  hv[inst, "NSGA/CPM"] <- vargha.delaney(nsNSGA$hv, CPM$hv);
  
  gd[inst, "NSGA/MAR"] <- vargha.delaney(nsNSGA$gd, mar$gd);
  gd[inst, "NSGA/SH"] <-  vargha.delaney(nsNSGA$gd, sh$gd);
  gd[inst, "NSGA/CPM"] <- vargha.delaney(nsNSGA$gd, CPM$gd);
  
}
