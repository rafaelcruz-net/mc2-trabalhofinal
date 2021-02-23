library(tidyverse)
rm(list=ls())

# DEFININDO HOMEWORK
setwd("~/Trabalho MCII");

# CARREGANDO BASE DE DADOS
data <- read.table("~/Trabalho Final - Dados/data_t5-t6.txt", header = TRUE);

# AGRUPANDO POR CONFIGURAÇÕES
configs <- unique(as.character(data$config));

# AGRUPANDO POR INSTANCIAS
instances <- unique(as.character(data$inst));

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

