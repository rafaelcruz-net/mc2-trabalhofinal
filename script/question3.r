# limpa todas as variaveis
rm(list = ls());
setwd("~/Mestrado/TrabalhoMC2");
data <- read.table("data/data_t3-t4.txt", header = TRUE);

configs <- unique(as.character(data$config));
instances <- unique(as.character(data$inst));

compare <- c("BEST(NSGA/MAR)", "BEST(NSGA/SH)", "BEST(NSGA/CPM)", 
             "HV(NSGA/MAR)", "HV(NSGA/SH)", "HV(NSGA/CPM)",
             "GD(NSGA/MAR)", "GD(NSGA/SH)", "GD(NSGA/CPM)");

#COMPARANDO A QUALIDADE DOS DADOS
quality <- matrix(nrow=length(instances), ncol=length(compare), dimnames=list(instances, compare));

for (instance_ in instances)
{
  instanceData_ <- subset(data, inst == instance_);

  NSGA <- subset(instanceData_, config == "nsga150k2x");
  mar <- subset(instanceData_, config == "MAR");
  sh <- subset(instanceData_, config == "SH");
  cpm <- subset(instanceData_, config == "CPM");
  
  quality[instance_, "BEST(NSGA/MAR)"] <- wilcox.test(NSGA$best, mu=mar$best[1])$p.value; 
  quality[instance_, "BEST(NSGA/SH)"] <- wilcox.test(NSGA$best, mu=sh$best[1])$p.value; 
  quality[instance_, "BEST(NSGA/CPM)"] <- wilcox.test(NSGA$best, mu=cpm$best[1])$p.value; 

  quality[instance_, "HV(NSGA/MAR)"] <- wilcox.test(NSGA$hv, mu=mar$hv[1])$p.value; 
  quality[instance_, "HV(NSGA/SH)"] <- wilcox.test(NSGA$hv, mu=sh$hv[1])$p.value; 
  quality[instance_, "HV(NSGA/CPM)"] <- wilcox.test(NSGA$hv, mu=cpm$hv[1])$p.value; 
  
  quality[instance_, "GD(NSGA/MAR)"] <- wilcox.test(NSGA$gd, mu=mar$gd[1])$p.value; 
  quality[instance_, "GD(NSGA/SH)"] <- wilcox.test(NSGA$gd, mu=sh$gd[1])$p.value; 
  quality[instance_, "GD(NSGA/CPM)"] <- wilcox.test(NSGA$gd, mu=cpm$gd[1])$p.value; 
}


#CALCULO DA MEDIA E DO DESVIO PADRÃO
mean_ic <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));
mean_hv <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));
mean_gd <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instances, configs));

for (config_ in configs)
{
  for (instance_ in instances)
  {
    instance_ <- instances[which(instances==instance_)];
    newdata <- subset(data, inst == instance_ & config == config_);
    
    mean_ic[instance_, config_] <- mean(newdata$best);
    mean_hv[instance_, config_] <- mean(newdata$hv);
    mean_gd[instance_, config_] <- mean(newdata$gd);
  }
}

sd_ic <- matrix(nrow=length(instances), ncol=1, dimnames=list(instances, c("nsga150k2x")));
sd_hv <- matrix(nrow=length(instances), ncol=1, dimnames=list(instances, c("nsga150k2x")));
sd_gd <- matrix(nrow=length(instances), ncol=1, dimnames=list(instances, c("nsga150k2x")));

for (instance_ in instances)
{
  instance_ <- instances[which(instances==instance_)];
  newdata <- subset(data, inst == instance_ & config == "nsga150k2x");
  sd_ic[instance_, "nsga150k2x"] <- sd(newdata$best);
  sd_hv[instance_, "nsga150k2x"] <- sd(newdata$hv);
  sd_gd[instance_, "nsga150k2x"] <- sd(newdata$gd);
}
