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
##---Comando para deixar todos os títulos dos gráficos ggplot centralizados
personal_title = theme(plot.title =
                         element_text(face="bold",hjust = 0.5))


##--MCs

M10 <- combTMB(cbind(D3, IVC-D3) ~ Period + Status + (1|Donor) + (1|Sire), 
               embryos, family=betabinomial)

##--Modelo beta-binomial (MLG)
M12 <- combTMB(cbind(D3, IVC-D3) ~ Period + Status, 
               embryos, family=betabinomial)

##--Valores Preditos
##--Modelo beta-binomial (M12)
data_pred<-predict(M12,type="response",se_fit=TRUE)
newdat <- embryos
newdat <- data.frame(newdat, pred=data_pred$fit, se=data_pred$se_fit)

##--Acrescentado os intervalos de predição de 90%
newdat <- mutate(newdat,
                 lower.ci = (pred - (qnorm(0.05, lower.tail = FALSE)* se)),
                 upper.ci = (pred + (qnorm(0.05, lower.tail = FALSE)* se)))

##--Period
(period.se <- as.numeric(with(newdat, tapply(se, Period, mean))))
(period.means <- as.numeric(with(newdat, tapply(pred, Period, mean))))
(period.lower.ci <- as.numeric(with(newdat, tapply(lower.ci, Period, mean))))
(period.upper.ci <- as.numeric(with(newdat, tapply(upper.ci, Period, mean))))

##--Status
(status.se <- as.numeric(with(newdat, tapply(se, Status, mean))))
(status.means <- as.numeric(with(newdat, tapply(pred, Status, mean))))
(status.lower.ci <- as.numeric(with(newdat, tapply(lower.ci, Status, mean))))
(status.upper.ci <- as.numeric(with(newdat, tapply(upper.ci, Status, mean))))

##--Criando os fatores Period e Status
Period <- factor(c("P1","P2"), levels = c("P1","P2"))
Status<- factor(c("D","H","M"),levels=c("D","H","M"))

##--Dados sumarizados
(mp_period_bb <-data.frame(Period,
                           prob=period.means,  
                           lower.ci=period.lower.ci,
                           upper.ci=period.upper.ci))

##---Acrescentando as médias observadas
with(embryos,tapply(D3/IVC,Period,mean))

(mp_period_bb<-mutate(mp_period_bb, mediaobs=c(0.7482085,0.7763982)))


##--Status
(mp_status_bb <-data.frame(Status,
                           prob=status.means,  
                           lower.ci=status.lower.ci,
                           upper.ci=status.upper.ci))

##--Acrescentando as médias observadas
with(embryos,tapply(D3/IVC,Status,mean))

(mp_status_bb <-mutate(mp_status_bb, mediaobs=c(0.7964167,0.7648248,0.7015636)))



##--Modelo combinado (M10)
##--Removendo os objetos
remove(data_pred, newdat)
data_pred<-predict(M10,re_form=~0,type="response",se_fit=TRUE)
head(data_pred$fit)
head(data_pred$se_fit)


newdat <- embryos
newdat <- data.frame(newdat, pred=data_pred$fit, se=data_pred$se_fit)

##--Acrescentado os intervalos de predição de 90%
newdat <- mutate(newdat,
                 lower.ci = (pred - (qnorm(0.05, lower.tail = FALSE)* se)),
                 upper.ci = (pred + (qnorm(0.05, lower.tail = FALSE)* se)))

##--Period
(period.se <- as.numeric(with(newdat, tapply(se, Period, mean))))
(period.means <- as.numeric(with(newdat, tapply(pred, Period, mean))))
(period.lower.ci <- as.numeric(with(newdat, tapply(lower.ci, Period, mean))))
(period.upper.ci <- as.numeric(with(newdat, tapply(upper.ci, Period, mean))))

##--Status
(status.se <- as.numeric(with(newdat, tapply(se, Status, mean))))
(status.means <- as.numeric(with(newdat, tapply(pred, Status, mean))))
(status.lower.ci <- as.numeric(with(newdat, tapply(lower.ci, Status, mean))))
(status.upper.ci <- as.numeric(with(newdat, tapply(upper.ci, Status, mean))))

##--Criando os fatores Period e Status
Period <- factor(c("P1","P2"), levels = c("P1","P2"))
Status<- factor(c("D","H","M"),levels=c("D","H","M"))

##--Dados sumarizados
(mp_period_cm <-data.frame(Period,
                           prob=period.means,  
                           lower.ci=period.lower.ci,
                           upper.ci=period.upper.ci))

##---Acrescentando as médias observadas
with(embryos,tapply(D3/IVC,Period,mean))

(mp_period_cm<-mutate(mp_period_cm, mediaobs=c(0.7482085,0.7763982)))



##--Status
(mp_status_cm<-data.frame(Status,
                          prob=status.means,  
                          lower.ci=status.lower.ci,
                          upper.ci=status.upper.ci))

##---Acrescentando a média observada
with(embryos,tapply(D3/IVC,Status,mean))
(mp_status_cm <-mutate(mp_status_cm, mediaobs=c(0.7964167,0.7648248,0.7015636)))


##--Data.frame para juntar os valores preditos dos modelos beta-binomial (M12) e 
##--combinado (M10)

##--Period
Modelos_1 <- factor(c("Beta-binomial","Beta-binomial",
                      "Combinado","Combinado"), levels = c("Beta-binomial", "Combinado"))

mpfperiod <- rbind(mp_period_bb, mp_period_cm)
mpfperiod <- data.frame(Modelos=Modelos_1,mpfperiod)
mpfperiod

Modelos_2<- factor(c("Beta-binomial","Beta-binomial",
                     "Beta-binomial","Combinado",
                     "Combinado","Combinado"), levels = c("Beta-binomial", "Combinado"))
mpfstatus<- rbind(mp_status_bb, mp_status_cm)
mpfstatus <- data.frame(Modelos=Modelos_2,mpfstatus)
mpfstatus


##--Figura com ggplot2

fig_period_prop <- ggplot(mpfperiod,aes(x=Period, y=prob,color=Modelos))+
  geom_point(aes(x=Period, y=mediaobs,color=Modelos), shape = 8, size  = 1, 
             colour = "magenta")+
  geom_point(shape = 20,size  = 2, position = position_dodge(width = 0.4))+
  geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                width = 0.2,size  = 0.3,
                position = position_dodge(width = 0.4))+
  scale_color_manual(values = c("Beta-binomial" = "black", "Combinado" = "blue")) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.01),
    labels = seq(0, 1, 0.01))+
  theme_test(base_size =12, base_family = "sans")+theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black"),
        panel.spacing = unit(0, "cm"))+theme(legend.position = "top")+
  ggtitle("(a)")+personal_title+
  ylab("Taxa de clivagem")+ xlab("Períodos do ano")


fig_status_prop <- ggplot(mpfstatus,aes(x=Status, y=prob,color=Modelos))+
  geom_point(aes(x=Status, y=mediaobs,color=Modelos), shape = 8, size  = 1,
             colour = "magenta")+
  geom_point(shape = 20,size  = 2, position = position_dodge(width = 0.4))+
  geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                width = 0.2,size  = 0.3,
                position = position_dodge(width = 0.4))+
  scale_color_manual(values = c("Beta-binomial" = "black", "Combinado" = "blue")) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.01),
    labels = seq(0, 1, 0.01))+
  theme_test(base_size =12, base_family = "sans")+theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black"),
        panel.spacing = unit(0, "cm"))+ggtitle("(b)")+personal_title+
  theme(legend.position = "top")+
  ylab("")+ xlab("Status das doadoras")


gridExtra::grid.arrange(fig_period_prop, fig_status_prop, ncol=2)

