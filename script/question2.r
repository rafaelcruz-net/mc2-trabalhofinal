library(tidyverse)
rm(list = ls())

# SETANDO A HOME COMO WORK DIR
setwd("~/Mestrado/TrabalhoMC2");
data <- read.table("data/data_t2.txt", header = TRUE);

#FAZENDO UM DISTINCT PARA PEGAR CONFIG UNICAS E INST UNICAS
unique_instances <- unique(data$inst);

print("------------------------------------------------------------------------------------------------");
for (instance in unique_instances)
{
  instancesubset <- subset(data, inst == instance);
  pv <- kruskal.test(gd~config, data=instancesubset);
  print(paste("instance=", instance));
  print(pv);
}

print("------------------------------------------------------------------------------------------------");

for (instance in unique_instances)
{
  print("------------------------------------------------------------------------------------------------");
  instancesubset <- subset(data, inst == instance);
  wt <- pairwise.wilcox.test(instancesubset$gd, instancesubset$config, exact=F, p.adjust.method = "bonferroni")$p.value;
  print(wt);
  cat("\n");
}
