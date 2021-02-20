library(tidyverse)

rm(list = ls())
data <- read.table("Mestrado/TrabalhoMC2/data/data_t1.txt", header = TRUE);

unique_configurations <- unique(as.character(data$config));
unique_instances <- unique(data$inst);

mean_gd <- matrix(nrow=length(unique_instances), ncol=length(unique_configurations), dimnames=list(unique_instances, unique_configurations));

for (config_ in unique_configurations )
{
  for (instances_ in unique_instances)
  {
    newdata <- subset(data, inst == instances_ & config == config_);
    mean_gd [instances_, config_] <- mean(newdata$gd);
  }
}  

#TODO CRIAR TABELA  
names <- names(mean_gd[1,]);

for (instance_ in unique_instances)
{
  instanceIndex <- match(instance_, unique_instances);
  instanceGD <- mean_gd[instanceIndex,];
  best <- match(min(instanceGD), instanceGD);
  print(paste("The best configuration for instance", instance_, "is", names[best], sep=" "));
}




