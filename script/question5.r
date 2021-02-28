library(tidyverse)
rm(list=ls())

setwd("~/Mestrado/TrabalhoMC2");
data <- read.table("data/data_t5-t6.txt", header = TRUE);

# AGRUPANDO POR CONFIGURAÇÕES
configs <- unique(as.character(data$config));
# AGRUPANDO POR INSTANCIAS
instances <- unique(as.character(data$inst));

#COMPARANDO A QUALIDADE DOS DADOS
compare <- c("BEST(NSGAII/NSGASE)", "HV(NSGAII/NSGASE)", "GD(NSGAII/NSGASE)"); 
quality <- matrix(nrow=length(instances), ncol=length(compare), dimnames=list(instances, compare));


for (instance_ in instances)
{
     instanceData_ <- subset(data, inst == instance_);
     instanceName <- instances[which(instances==instance_)];
        
     nsga150k2xse <- subset(data, inst == instanceName & config == "nsga150k2xse");
     nsga150k2x <- subset(data, inst == instanceName & config == "nsga150k2x");

     quality[instanceName, "BEST(NSGAII/NSGASE)"] <- wilcox.test(nsga150k2x$best, nsga150k2xse$best)$p.value;
     quality[instanceName, "HV(NSGAII/NSGASE)"] <- wilcox.test(nsga150k2x$hv, nsga150k2xse$hv)$p.value;
     quality[instanceName, "GD(NSGAII/NSGASE)"] <- wilcox.test(nsga150k2x$gd, mu=nsga150k2xse$gd[1])$p.value;
}


#CALCULO DA MEDIA E DO DESVIO PADRÃO
mean_ic <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));
mean_hv <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));
mean_gd <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));

sd_ic <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));
sd_hv <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));
sd_gd <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));

for (config_ in configs)
{
    for (instance_ in instances)
    {
       instance_ <- instances[which(instances==instance_)];
       datarow <- subset(data, inst == instance_ & config == config_);
       mean_ic[instance_, config_] <- mean(datarow$best) * 100;
       mean_hv[instance_, config_] <- mean(datarow$hv) * 100;
       mean_gd[instance_, config_] <- mean(datarow$gd) * 100;
       
       sd_ic[instance_, config_] <- sd(datarow$best);
       sd_hv[instance_, config_] <- sd(datarow$hv);
       sd_gd[instance_, config_] <- sd(datarow$gd);
    }
}

