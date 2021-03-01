rm(list = ls());
library("ggplot2");
library("gridExtra");
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

ACAD <- rbind(mar_ACAD,cpm_ACAD,nensga_ACAD,nsga_ACAD,sh_ACAD)
PARM <- rbind(mar_PARM,cpm_PARM,nensga_PARM,nsga_PARM,sh_PARM)

attach(ACAD);
attach(PARM);

graf1_acad <- ggplot(ACAD, aes(x=mks, y=cst/1000))+
  geom_point(size=2, aes(shape=Arquivo, color=Arquivo)) +
  xlab("Markespan(days)") +
  ylab("Cost(1000$)")  +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1.0, vjust=1, size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.8)
  ) +
  scale_y_continuous(breaks = c(85,90,95, 100)) +
  scale_x_continuous(breaks = seq(120,170, by = 10)) +
  ggtitle("ACAD");

graf2_acad <- ggplot(ACAD, aes(x = noh * 8, y = mks))+
  geom_point(size=2, aes(shape=Arquivo, color=Arquivo)) +
  xlab("Overtime(hours)") +
  ylab("Markespan(days)") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1.0, vjust=1, size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.8)
  ) +
  scale_y_continuous(breaks = c(120,140,160)) +
  scale_x_continuous(breaks = seq(0,400, by = 100)) +
  ggtitle("ACAD");

graf3_acad <- ggplot(ACAD, aes(x = noh * 8, y = cst / 1000))+
  geom_point(size=2, aes(shape=Arquivo, color=Arquivo)) +
  xlab("Overtime(hours)") +
  ylab("Cost(1000$)") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1.0, vjust=1, size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.8)
  ) +
  scale_y_continuous(breaks = c(85,90,95, 100)) +
  scale_x_continuous(breaks = seq(0,400, by = 100)) +
  ggtitle("ACAD");

graf1_parm <- ggplot(PARM, aes(x = mks, y = cst / 1000))+
    xlab("Markespan(days)") +
    ylab("Cost(1000$)") +
    geom_point(size=2, aes(shape=Arquivo, color=Arquivo)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1.0, vjust=1, size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.8)
  ) +
  scale_y_continuous(breaks = c(210,230,250)) +
  scale_x_continuous(breaks = seq(300,420, by = 20)) +
  ggtitle("PARM");

graf2_parm <- ggplot(PARM, aes(x = noh * 8, y = mks))+
  geom_point(size=2, aes(shape=Arquivo, color=Arquivo)) +
  xlab("Overtime(hours)") +
  ylab("Markespan(days)") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1.0, vjust=1, size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.8)
  ) +
  scale_y_continuous(breaks = c(300,340,380,420)) +
  scale_x_continuous(breaks = seq(0,1000, by = 200)) +
  ggtitle("PARM");


graf3_parm <- ggplot(PARM, aes(x=noh*8, y=cst/1000))+
  geom_point(size=2, aes(shape=Arquivo, color=Arquivo)) +
  xlab("Overtime(hours)") +
  ylab("Cost(1000$)") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1.0, vjust=1, size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.8)
  ) +
  scale_y_continuous(breaks = c(210,230,250)) +
  scale_x_continuous(breaks = seq(0,1000, by = 200)) +
  ggtitle("PARM");

grid.arrange(graf1_acad, graf2_acad, graf3_acad, graf1_parm, graf2_parm, graf3_parm, ncol=3, nrow=2);
