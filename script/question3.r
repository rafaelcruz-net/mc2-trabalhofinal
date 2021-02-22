library(tidyverse)
rm(list = ls())
# SETANDO A HOME COMO WORK DIR
setwd("~/Mestrado/TrabalhoMC2");
data <- read.table("data/data_t3-t4.txt", header = TRUE);
configs <- unique(as.character(data$config));
instances <- unique(as.character(data$inst));

# qualidade dos dados
ic <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));
hv <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));
gd <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));

for (config_ in configs)
{
  for (instance_ in instances)
  {
    instance_ <- instances[which(instances==instance_)];
    datarow <- subset(data, inst == instance_ & config == config_);
    ic[instance_, config_] <- mean(datarow$best);
    hv[instance_, config_] <- mean(datarow$hv);
    gd[instance_, config_] <- mean(datarow$gd);
  }
}