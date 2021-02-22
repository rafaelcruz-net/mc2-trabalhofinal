library(tidyverse)
rm(list = ls())

# SETANDO A HOME COMO WORK DIR
setwd("~/Mestrado/TrabalhoMC2");
data <- read.table("data/data_t1.txt", header = TRUE);

configs <- unique(as.character(data$config));
instances <- unique(data$inst);
mean <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));

for (config_ in configs)
{
  for (instances_ in instances)
  {
    mean [instances_, config_] <- mean(subset(data, inst == instances_ & config == config_)$gd);
  }
}  

col <- factor(c("bestconfig"));
best_config <- matrix(nrow=length(instances), ncol=length(col), dimnames=list(instances, col));
names <- names(mean[1,]);

for (instance_ in instances)
{
  instanceIndex <- match(instance_, instances);
  instanceGD <- mean[instanceIndex,];
  best <- match(min(instanceGD), instanceGD);
  best_config[instance_, ] <- (names[best]);
}




