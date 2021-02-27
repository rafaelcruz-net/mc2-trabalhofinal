library(tidyverse)
rm(list = ls())

# SETANDO A HOME COMO WORK DIR
setwd("~/Mestrado/TrabalhoMC2");
data <- read.table("data/data_t2.txt", header = TRUE);

#FAZENDO UM DISTINCT PARA PEGAR CONFIG UNICAS E INST UNICAS
configurations <- unique(as.character(data$config));
instances <- unique(data$inst);


mean_best <- matrix(nrow=length(instances), ncol=length(configurations), dimnames=list(instances, configurations));
mean_hv <- matrix(nrow=length(instances), ncol=length(configurations), dimnames=list(instances, configurations));
mean_gd <- matrix(nrow=length(instances), ncol=length(configurations), dimnames=list(instances, configurations));
mean_spr <- matrix(nrow=length(instances), ncol=length(configurations), dimnames=list(instances, configurations));

sd_best <- matrix(nrow=length(instances), ncol=length(configurations), dimnames=list(instances, configurations));
sd_hv <- matrix(nrow=length(instances), ncol=length(configurations), dimnames=list(instances, configurations));
sd_gd <- matrix(nrow=length(instances), ncol=length(configurations), dimnames=list(instances, configurations));
sd_spr <- matrix(nrow=length(instances), ncol=length(configurations), dimnames=list(instances, configurations));


#PEGANDO A MEDIA E O DESVIO PADRAO
for (config_ in configurations)
{
  for (instances_ in instances)
  {
    newdata <- subset(data, inst == instances_ & config == config_);
    
    mean_best [instances_, config_] <- mean(newdata$best);
    mean_hv [instances_, config_] <- mean(newdata$hv);
    mean_gd [instances_, config_] <- mean(newdata$gd);
    mean_spr [instances_, config_] <- mean(newdata$spr);
    
    
    sd_best [instances_, config_] <- sd(newdata$best);
    sd_hv [instances_, config_] <- sd(newdata$hv);
    sd_gd [instances_, config_] <- sd(newdata$gd);
    sd_spr [instances_, config_] <- sd(newdata$spr);
  }
}


print("------------------------------------------------------------------------------------------------");
for (instance in instances)
{
  instancesubset <- subset(data, inst == instance);
  pv <- kruskal.test(gd~config, data=instancesubset);
  print(paste("instance=", instance));
  print(pv);
}

print("------------------------------------------------------------------------------------------------");

#PARA CADA INSTANCIA DETERMINA SE ESTA DIFERENÇA ESTATISTICA
for (instance_ in instances)
{
  instanceData_ <- subset(data, inst == instance_);
  instanceIndex <- match(instance_, instances);
  
  
  gd <- pairwise.wilcox.test(instanceData_$gd, instanceData_$config, p.adj="bonferroni", exact=F)$p.value;
  hv <- pairwise.wilcox.test(instanceData_$hv, instanceData_$config, p.adj="bonferroni", exact=F)$p.value;
  spr <- pairwise.wilcox.test(instanceData_$spr, instanceData_$config, p.adj="bonferroni", exact=F)$p.value;
  
  for (i in 1:2) 
  {
    for (j in 1:i)
    {
      if (gd[i,j] < 0.05)
      {
        print(paste("Diferença encontrada - GD entre", names(gd[1,])[i], "e", names(gd[,1])[j], sep=" "));
      }
      if (hv[i,j] < 0.05)
      {
        print(paste("Diferença encontrada - HV entre", names(hv[1,])[i], "e", names(hv[,1])[j], sep=" "));
      }
      if (spr[i,j] < 0.05)
      {
        print(paste("Diferença encontrada - SPR entre", names(spr[1,])[i], "e", names(spr[,1])[j], sep=" "));
      }
    }
  }
}




colname <- c( "nsga2x_m", "nsga2x_sd",   "nsga4x_m", "nsga4x_sd", "nsga8x_m", "nsga8x_sd");

gd <- matrix(nrow=6, ncol=6, dimnames=list(instances, colname));
gd [1:6,1:6] <- cbind(mean_gd[,1],sd_gd[,1] ,mean_gd[,2],sd_gd[,2],mean_gd[,3],sd_gd[,3]);

hv <- matrix(nrow=6, ncol=6, dimnames=list(instances, colname));
hv [1:6,1:6] <- cbind(mean_hv[,1],sd_hv[,1] ,mean_hv[,2],sd_hv[,2],mean_hv[,3],sd_hv[,3]);

spr <- matrix(nrow=6, ncol=6, dimnames=list(instances, colname));
spr [1:6,1:6] <- cbind(mean_spr[,1],sd_spr[,1] ,mean_spr[,2],sd_spr[,2],mean_spr[,3],sd_spr[,3]);

best <- matrix(nrow=6, ncol=6, dimnames=list(instances, colname));
best [1:6,1:6] <- cbind(mean_best[,1],sd_best[,1] ,mean_best[,2],sd_best[,2],mean_best[,3],sd_best[,3]);

df <- cbind(gd, hv, spr, best);
print(df);


