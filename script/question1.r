library(tidyverse)
rm(list = ls())

# SETANDO A HOME COMO WORK DIR
setwd("~/Mestrado/TrabalhoMC2");
data <- read.table("data/data_t1.txt", header = TRUE);

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


#PARA CADA INSTANCIA DETERMINA SE ESTA DIFERENÇA ESTATISTICA
for (instance_ in instances)
{
  instanceData_ <- subset(data, inst == instance_);
  instanceIndex <- match(instance_, instances);
  

  gd <- pairwise.wilcox.test(instanceData_$gd, instanceData_$config, p.adj="bonferroni", exact=F)$p.value;
  hv <- pairwise.wilcox.test(instanceData_$hv, instanceData_$config, p.adj="bonferroni", exact=F)$p.value;
  spr <- pairwise.wilcox.test(instanceData_$spr, instanceData_$config, p.adj="bonferroni", exact=F)$p.value;
  
  for (i in 1:5) 
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


#AGRUPA PARA MELHOR VISUALIZAÇÃO
colname <- c( "nsga5k2x_m_gd", "nsga5k2x_sd_gd",   "nsga10k2x_m_gd", "nsga10k2x_sd_gd",  "nsga20k2x_m_","nsga20k2x_sd",  "nsga50k2x_m","nsga50k2x_sd", "nsga100k2x_m","nsga100k2x_sd", "nsga150k2x_m","nsga150k2x_sd");
gd <- matrix(nrow=6, ncol=12, dimnames=list(instances, colname));
gd [1:6,1:12] <- cbind(mean_gd[,1],sd_gd[,1] ,mean_gd[,2],sd_gd[,2],mean_gd[,3],sd_gd[,3],mean_gd[,4],sd_gd[,4],mean_gd[,5],sd_gd[,5],mean_gd[,6],sd_gd[,6]);


colname <- c( "nsga5k2x_m", "nsga5k2x_sd",   "nsga10k2x_m", "nsga10k2x_sd",  "nsga20k2x_m","nsga20k2x_sd",  "nsga50k2x_m","nsga50k2x_sd", "nsga100k2x_m","nsga100k2x_sd", "nsga150k2x_m","nsga150k2x_sd");
hv <- matrix(nrow=6, ncol=12, dimnames=list(instances, colname));
hv [1:6,1:12] <- cbind(mean_hv[,1],sd_hv[,1] ,mean_hv[,2],sd_hv[,2],mean_hv[,3],sd_hv[,3],mean_hv[,4],sd_hv[,4],mean_hv[,5],sd_hv[,5],mean_hv[,6],sd_hv[,6]);

colname <- c( "nsga5k2x_m", "nsga5k2x_sd",   "nsga10k2x_m", "nsga10k2x_sd",  "nsga20k2x_m","nsga20k2x_sd",  "nsga50k2x_m","nsga50k2x_sd", "nsga100k2x_m","nsga100k2x_sd", "nsga150k2x_m","nsga150k2x_sd");
spr <- matrix(nrow=6, ncol=12, dimnames=list(instances, colname));
spr [1:6,1:12] <- cbind(mean_spr[,1],sd_spr[,1] ,mean_spr[,2],sd_spr[,2],mean_spr[,3],sd_spr[,3],mean_spr[,4],sd_spr[,4],mean_spr[,5],sd_spr[,5],mean_spr[,6],sd_spr[,6]);

colname <- c( "nsga5k2x_m", "nsga5k2x_sd",   "nsga10k2x_m", "nsga10k2x_sd",  "nsga20k2x_m","nsga20k2x_sd",  "nsga50k2x_m","nsga50k2x_sd", "nsga100k2x_m","nsga100k2x_sd", "nsga150k2x_m","nsga150k2x_sd");
best <- matrix(nrow=6, ncol=12, dimnames=list(instances, colname));
best [1:6,1:12] <- cbind(mean_best[,1],sd_best[,1] ,mean_best[,2],sd_best[,2],mean_best[,3],sd_best[,3],mean_best[,4],sd_best[,4],mean_best[,5],sd_best[,5],mean_best[,6],sd_best[,6]);


#FICOU COMPLEXO A VISUALIZAÇÃO
#df <- cbind(gd, hv, spr, best);


# VERIFICANDO A MELHOR COMBINAÇÃO
col <- factor(c("hv", "gd", "sp", "best"));
best_config <- matrix(nrow=length(instances), ncol=length(col), dimnames=list(instances, col));
for (instance_ in instances)
{
  instanceIndex <- match(instance_, instances);

  instanceHV <- mean_hv[instanceIndex,];
  best <- match(max(instanceHV), instanceHV);
  best_config[instance_, 1] <- (names(mean_hv[1,])[best]);
  
  instanceGD <- mean_gd[instanceIndex,];
  best <- match(min(instanceGD), instanceGD);
  best_config[instance_, 2] <- (names(mean_gd[1,])[best]);

  instanceSPR <- mean_spr[instanceIndex,];
  best <- match(min(instanceSPR), instanceSPR);
  best_config[instance_, 3] <- (names(mean_spr[1,])[best]);
  
  instanceBest <- mean_best[instanceIndex,];
  best <- match(max(instanceBest), instanceBest);
  best_config[instance_, 4] <- (names(mean_best[1,])[best]);

}