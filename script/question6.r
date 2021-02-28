library(tidyverse)
rm(list=ls())

setwd("~/Mestrado/TrabalhoMC2");
data <- read.table("data/data_t5-t6.txt", header = TRUE);

# AGRUPANDO POR INSTANCIAS
instances <- unique(as.character(data$inst));

# VARGHA-DELANEY
 vargha.delaney <- function(r1, r2) {
     m <- length(r1);
     n <- length(r2);
     return ((sum(rank(c(r1, r2))[seq_along(r1)]) / m - (m + 1) / 2) / n);
}

compare <- c("BEST(NSGA/NSGASE)", "HV(NSGA/NSGASE)", "GD(NSGA/NSGASE)"); 
effectSize <- matrix(nrow=length(instances), ncol=length(compare), dimnames=list(instances, compare));

for (inst in instances)
{
     inst <- instances[which(instances==inst)];
     
     NSGA <- subset(data, inst == inst & config == "nsga150k2x");
     seNSGA <- subset(data, inst == inst & config == "nsga150k2xse");

     effectSize[inst, "BEST(NSGA/NSGASE)"] <- vargha.delaney(NSGA$best, seNSGA$best);
     effectSize[inst, "HV(NSGA/NSGASE)"] <- vargha.delaney(NSGA$hv, seNSGA$hv);
     effectSize[inst, "GD(NSGA/NSGASE)"] <- vargha.delaney(NSGA$gd, seNSGA$gd);
}

effectSize;
