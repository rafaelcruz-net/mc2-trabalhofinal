library(tidyverse)
rm(list=ls())

# DEFININDO HOMEWORK
setwd("~/Trabalho MCII");

# CARREGANDO BASE DE DADOS
data <- read.table("~/Trabalho Final - Dados/data_t5-t6.txt", header = TRUE);

# VARGHA-DELANEY
 vargha.delaney <- function(r1, r2) {
     m <- length(r1);
     n <- length(r2);
     return ((sum(rank(c(r1, r2))[seq_along(r1)]) / m - (m + 1) / 2) / n);
}
 
# CRIACAO DA 'TABELA'
compareColumns <- c("NSGA/NSGAne");
 

gd <- matrix(nrow=length(instances), ncol=length(compareColumns), dimnames=list(instances, compareColumns));
hv <- matrix(nrow=length(instances), ncol=length(compareColumns), dimnames=list(instances, compareColumns));
best <- matrix(nrow=length(instances), ncol=length(compareColumns), dimnames=list(instances, compareColumns));

for (inst in instances)
 {
     inst <- instances[which(instances==inst)];
     
     NSGA <- subset(data, inst == inst & config == "nsga150k2x");
     seNSGA <- subset(data, inst == inst & config == "nsga150k2xse");
          
     best[inst, "NSGA/NSGAne"] <- vargha.delaney(NSGA$best, seNSGA$best);
        
     hv[inst, "NSGA/NSGAne"] <- vargha.delaney(NSGA$hv, seNSGA$hv);
      
     gd[inst, "NSGA/NSGAne"] <- vargha.delaney(NSGA$gd, seNSGA$gd);
 
     
 }
