rm(list = ls());
library("ggplot2");
setwd("~/Mestrado/TrabalhoMC2");

mar_file <- read.table("data/data_t7_Margarine_error1_frontier_obj.txt",header=TRUE);
mar_ACAD <- subset(mar_file, inst == 'ACAD');
mar_ACAD$Arquivo<-'MAR_ACAD'
mar_PARM <- subset(mar_file, inst == 'PARM');
mar_PARM$Arquivo<-'MAR_PARM'

cpm_file <- read.table("data/data_t7_CPM_error1_frontier_obj.txt",header=TRUE);
cpm_ACAD <- subset(cpm_file, inst == 'ACAD');
cpm_ACAD$Arquivo<-'CPM_ACAD'
cpm_PARM <- subset(cpm_file, inst == 'PARM');
cpm_PARM$Arquivo<-'CPM_PARM'

nensga_file <- read.table("data/data_t7_nsga_150k_c50_2x_error1_frontier_obj.txt",header=TRUE);
nensga_ACAD <- subset(nensga_file, inst == 'ACAD');
nensga_ACAD$Arquivo<-'nensga_ACAD'
nensga_PARM <- subset(nensga_file, inst == 'PARM');
nensga_PARM$Arquivo<-'nensga_PARM'


nsga_file <- read.table("data/data_t7_nsga_150k_c50_2x_noerror_frontier_obj.txt",header=TRUE);
nsga_ACAD <- subset(nsga_file, inst == 'ACAD');
nsga_ACAD$Arquivo<-'nsga_ACAD'
nsga_PARM <- subset(nsga_file, inst == 'PARM');
nsga_PARM$Arquivo<-'nsga_PARM'

sh_file <- read.table("data/data_t7_SecondHalf_error1_frontier_obj.txt",header=TRUE);
sh_ACAD <- subset(sh_file, inst == 'ACAD');
sh_ACAD$Arquivo<-'SH_ACAD'
sh_PARM <- subset(sh_file, inst == 'PARM');
sh_PARM$Arquivo<-'SH_PARM'

# por exemplo mks x noh
# Criar uma matriz (vazia?)
# preencher o X da matriz com o mks de cada uma
# preencher o Y da matriz com o noh de cada uma
# simplesmente plotar

ACAD <- rbind(mar_ACAD,cpm_ACAD,nensga_ACAD,nsga_ACAD,sh_ACAD)
PARM <- rbind(mar_PARM,cpm_PARM,nensga_PARM,nsga_PARM,sh_PARM)


attach(ACAD)

# SALVA O GRAFICO 1
png("data/result/graf1_acad.png", width = 1024, height = 768, res = 92);
graf1_acad <- ggplot(ACAD, aes(mks, cst))+
  geom_point(aes(color = Arquivo, shape = Arquivo))+
#  stat_smooth(aes(color = Arquivo, fill = Arquivo), 
#              method = "lm", alpha = 0.2)+
  scale_color_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) + 
  scale_fill_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) 

graf1_acad + theme_dark()
print(graf1_acad);  
dev.off();

# SALVA O GRAFICO 2
png("data/result/graf2_acad.png", width = 1024, height = 768, res = 92);

graf2_acad <- ggplot(ACAD, aes(noh, mks))+
  geom_point(aes(color = Arquivo, shape = Arquivo))+
  #  stat_smooth(aes(color = Arquivo, fill = Arquivo), 
  #              method = "lm", alpha = 0.2)+
  scale_color_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) + 
  scale_fill_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) 

graf2_acad + theme_dark()
print(graf2_acad);
dev.off();



png("data/result/graf3_acad.png", width = 1024, height = 768, res = 92);
graf3_acad <- ggplot(ACAD, aes(noh, cst))+
  geom_point(aes(color = Arquivo, shape = Arquivo))+
  #  stat_smooth(aes(color = Arquivo, fill = Arquivo), 
  #              method = "lm", alpha = 0.2)+
  scale_color_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) + 
  scale_fill_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) 

graf3_acad + theme_dark()

print(graf3_acad);  
dev.off();

attach(PARM)

png("data/result/graf1_param.png", width = 1024, height = 768, res = 92);  
graf1_parm <- ggplot(PARM, aes(mks, cst))+
    geom_point(aes(color = Arquivo, shape = Arquivo))+
    #  stat_smooth(aes(color = Arquivo, fill = Arquivo), 
    #              method = "lm", alpha = 0.2)+
    scale_color_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) + 
    scale_fill_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) 
  
graf1_parm + theme_dark()  
print(graf1_parm);
dev.off();
  

png("data/result/graf2_param.png", width = 1024, height = 768, res = 92);  
graf2_parm <- ggplot(PARM, aes(noh, mks))+
  geom_point(aes(color = Arquivo, shape = Arquivo))+
  #  stat_smooth(aes(color = Arquivo, fill = Arquivo), 
  #              method = "lm", alpha = 0.2)+
  scale_color_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) + 
  scale_fill_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) 

graf2_parm + theme_dark() 
print(graf2_parm);
dev.off();

png("data/result/graf3_param.png", width = 1024, height = 768, res = 92);  
graf3_parm <- ggplot(PARM, aes(noh, cst))+
  geom_point(aes(color = Arquivo, shape = Arquivo))+
  #  stat_smooth(aes(color = Arquivo, fill = Arquivo), 
  #              method = "lm", alpha = 0.2)+
  scale_color_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) + 
  scale_fill_manual(values = c("#E4F00A", "white", "#22FF00","pink","orange")) 

graf3_parm + theme_dark()  

print(graf3_parm);
dev.off();


 
