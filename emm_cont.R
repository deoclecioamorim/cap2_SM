#AUTOR: Deoclecio J. Amorim
#E-MAIL: deocleciojardim@hotmail.com
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
#options(scipen = 999)# Este valor alto (999) evita o retorno dos números em
#notação científica.
rm(list=ls(all=T))#Limpando a memória

################################################################################
##---Apêndice do combTMB
################################################################################
##---Preliminares
##---Carrengando os pacotes
library(combTMB)
library(tidyverse)
library(emmeans)
##---Comando para deixar todos os títulos dos gráficos ggplot2 centralizados
personal_title = theme(plot.title =
                         element_text(face="bold",hjust = 0.5))

##---Marginalização
M4MCM <- combTMB(OT ~ Period * Status + (1|Donor), embryos,
                 family=poigamma, doMarginal = TRUE)


##---Médias marginais obtidas com o pacote emmeans
mm_M4MCM <- emmeans(M4MCM, ~ Status * Period, type="response")
mm_M4MCM <- data.frame(mm_M4MCM)
mm_M4MCM

##---Médias marginais para o modelo Poisson_simples
mm_ps <- emmeans(M1, ~ Status * Period, type="response")
mm_ps <- data.frame(mm_ps)
names(mm_ps)[3]<-"response"
mm_ps
##--Juntando as médias marginais do Poisson simples com o combinado
mm<-rbind(mm_ps, mm_M4MCM)
mm

##---Médias observadas

with(embryos,
     tapply(OT, list(Period, Status), mean))

Modelos <- factor(c("Poisson","Combinado"), levels = c("Poisson", "Combinado"))
media_obs <- data.frame(Modelos = rep(Modelos, each = 6),
                        mediaobs = rep(c( 22.91729,12.87891, 16.05333,
                                          22.65385,14.86601, 18.81356),each=1))

mmf <- cbind(media_obs, mm)
mmf

fig <- ggplot(mmf,aes(x=Status, y=response,color=Modelos))+facet_wrap(~Period)+
  geom_point(aes(x=Status, y=mediaobs,color=Models), shape = 8, size  = 1, colour = "magenta")+
  geom_point(shape = 20,size  = 2, position = position_dodge(width = 0.4))+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2,size  = 0.3,
                position = position_dodge(width = 0.4))+
  scale_color_manual(values = c("Poisson" = "black", "Combinado" = "blue")) +
  scale_y_continuous(
    breaks = seq(0, 30, 2),
    labels = seq(0, 30, 2))+
  theme_test(base_size =12, base_family = "sans")+theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black"),
        panel.spacing = unit(0, "cm"))+theme(legend.position = "top")+
  ylab("Nº de oócitos totais (OT)")+ xlab("Status das doadoras")



fig
