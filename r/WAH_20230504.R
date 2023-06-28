# ANÁLISIS OGIVAS DE MADUREZ WAH.
# ICCAT 2023
# MIGUEL ÁNGEL PUERTO
# 2023 05 04
# Basado en:
#/home/josetxu/Desktop/2023_lankideak_20230222/ICCAT_LTA_WAH_20230502/material_zaharra/davidMacias_2022/
# helduaroa_2022/Premiazko_20220915/BON_L50_ICCAT_2021_20210505.R
# Mirar:
# /home/josetxu/Desktop/2023_lankideak_20230222/ICCAT_LTA_WAH_20230502/
# material_zaharra/LTA-SMTYP_2021eko-abenduaren/R/LTA_SMTYP_2021_20211203.R


#Limpiar consola de R:
#CRT+L


#R version 4.3.0 (2023-04-21) -- "Already Tomorrow"
#Copyright (C) 2023 The R Foundation for Statistical Computing
#Platform: x86_64-pc-linux-gnu (64-bit)
#RStudio 2023.03.0+386 "Cherry Blossom"
#Release (3c53477afb13ab959aeb5b34df1f10c237b256c3, 2023-03-09)
#for Ubuntu Jammy
#Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36
#(KHTML, like Gecko) rstudio/2023.03.0+386 Chrome/108.0.5359.179 Electron/22.0.3
#Safari/537.36

#library(FSA)
#library(magrittr)
library(plyr)
library(dplyr)
library(lubridate)
library(car)
library(ggplot2)
library(gridExtra)

# Preparar directorio de trabajo ---------------------------------------
getwd()
setwd('/home/josetxu/Desktop/2023_lankideak_20230222/ICCAT_LTA_WAH_20230502/input/')
getwd()
list.files()
rm(list=ls())

datuak <- read.csv('WAH_TOTAL_Rev DM_MAP_final_20230504.csv',
                   header = TRUE,
                   stringsAsFactors = FALSE)

#dim(datuak)
#[1] 467 3 
str(datuak) 
sum(is.na(datuak))
#[1] 3
# Kontuz! MAPuertoko jatorrizko fitxategian baliorik gabeko erregistroak
# 'N/A' gisa agertzen dira.
names(datuak)
#[1] "FL_cm"          "Sex"            "Maturity_stage"
str(datuak)
sum(is.na(datuak$FL_cm))
#[1] 3
datuak <- datuak[!is.na(datuak$FL_cm), ]
dim(datuak)
#[1] 464   3
unique(datuak$Sex)
#[1] "Female"       "Male"         "Undetermined"

unique(datuak$Maturity_stage)
#[1] "Inmature" "N/A"      "Mature"  
# Seleccionar 'Inmature' y 'Mature'-------------------------------
datuak <- datuak[datuak$Maturity_stage=='Inmature' |
                   datuak$Maturity_stage=='Mature', ]
#dim(datuak)
#[1] 213   3
#table(datuak$Maturity_stage)
#Inmature   Mature 
#36      177 
#renombrar--------------------------------------------------------
datuak$Maturity_stage <- ifelse(datuak$Maturity_stage=='Inmature','Immature',
                                'Mature')
#table(datuak$Maturity_stage)
#Immature   Mature 
#36      177 

datuak$Maturity_stage <- factor(datuak$Maturity_stage)
levels(datuak$Maturity_stage)
#[1] "Immature" "Mature"  
#head(datuak$Maturity_stage)
#head(as.numeric(datuak$Maturity_stage))
#tail(datuak$Maturity_stage)
#tail(as.numeric(datuak$Maturity_stage))


#Función length or age at which a certain percentage of fish is mature
lrPerc <- function(cf,p) (log(p/(1-p))-cf[[1]])/cf[[2]]

#logística todos los sexos
glm_all <- glm(Maturity_stage~FL_cm,data=datuak,family=binomial)
summary(glm_all)
Anova(glm_all)

#Ogiva de madurez todos los sexos
fig_WAH_20230504 <- 
  ggplot(datuak,
         aes(x=FL_cm,y=as.numeric(Maturity_stage)-1))+
  #geom_smooth(aes(colour=Stock_Sp),
  #            method="glm",method.args=list(family="binomial"),se=T,
  #            size=3)+
  #geom_smooth(data=datuak[datuak$Stock_Sp=='MEDI_NE ATL',],
  #            method="glm",method.args=list(family="binomial"),se=F,
  #   
  geom_smooth(method="glm",method.args=list(family="binomial"),se=T)+
  scale_y_continuous(name='proportion mature')+
  scale_x_continuous(name='FL (cm)',
                     breaks=seq(65,150,10),
                     limits=c(65,150))+
  #scale_color_manual(values=c('red','black'))+ 
  theme(legend.position='none',
        axis.text=element_text(size=10))

#logística hembras-----------------
#unique(datuak$Sex)
#[1] "Female" "Male"
#sum(datuak$Sex == 'Female')
#[1] 195
datuak_female <- datuak[datuak$Sex=='Female', ]
#dim(datuak_female)
#[1] 195   3
glm_female <- glm(Maturity_stage~FL_cm,data=datuak_female,
                  family=binomial)
summary(glm_female)
Anova(glm_female)

fig_WAH_FEMALE_20230504 <- 
  ggplot(datuak_female,
         aes(x=FL_cm,y=as.numeric(Maturity_stage)-1))+
  #geom_smooth(aes(colour=Stock_Sp),
  #            method="glm",method.args=list(family="binomial"),se=T,
  #            size=3)+
  #geom_smooth(data=datuak[datuak$Stock_Sp=='MEDI_NE ATL',],
  #            method="glm",method.args=list(family="binomial"),se=F,
  #   
  geom_smooth(method="glm",method.args=list(family="binomial"),se=T)+
  scale_y_continuous(name='proportion mature')+
  scale_x_continuous(name='FL (cm)',
                     breaks=seq(65,150,10),
                     limits=c(65,150))+
  #scale_color_manual(values=c('red','black'))+ 
  theme(legend.position='none',
        axis.text=element_text(size=10))


bcL_all    <- bootCase(glm_all,B=1000)
bcL_female <- bootCase(glm_female,B=1000)

L50.all      <- apply(bcL_all[,1:2],1,lrPerc,p=0.5)
L50.female   <- apply(bcL_female[,1:2],1,lrPerc,p=0.5)

#Intervalo de confianza L50 
ci.L50.all      <- quantile(L50.all,c(0.025,0.975))
#2.5%    97.5% 
#92.54470 96.60893 
ci.L50.female   <- quantile(L50.female,c(0.025,0.975))
#2.5%    97.5% 
#92.36949 96.55796 


#Otros gráficos------------------------------------------
#Ogivas de madurez (logísticas) all y female en el mismo panel

fig_WAH_OGIVAS_20230504 <- 
ggplot(datuak_female,
       aes(x=FL_cm,y=as.numeric(Maturity_stage)-1))+
  #geom_smooth(aes(colour=Stock_Sp),
  #            method="glm",method.args=list(family="binomial"),se=T,
  #            size=3)+
  #geom_smooth(data=datuak[datuak$Stock_Sp=='MEDI_NE ATL',],
  #            method="glm",method.args=list(family="binomial"),se=F,
  #   
  geom_smooth(method="glm",method.args=list(family="binomial"),se=T,
              size=3.5,fill='blue')+
  geom_smooth(data=datuak,
              method="glm",method.args=list(family="binomial"),se=T,
              colour='white', fill='white',alpha=0.6)+
  scale_y_continuous(name='proportion mature')+
  scale_x_continuous(name='FL (cm)',
                     breaks=seq(65,150,10),
                     limits=c(65,150))+
  #scale_color_manual(values=c('red','black'))+ 
  theme(legend.position='none',
        axis.text=element_text(size=10))

#Plot de densidad (bootstrap de L50) all y female
#con límites de confianza

fig_WAH_L50_20230504 <- 
ggplot(data=data.frame(L50=L50.all),aes(x=L50))+
  geom_density(colour='black',size=2.5)+
  geom_segment(x=quantile(L50.all,c(0.5)),
               y=0.0,
               xend=quantile(L50.all,c(0.5)),
               yend=0.42,
               colour='black',size=2.5)+
  geom_segment(x=ci.L50.all[1],
               y=0.0,
               xend=ci.L50.all[2],
               yend=0.0,
               colour='black',size=2.5)+
  geom_density(data=data.frame(L50=L50.female),
                 colour='red',size=1.0)+
  geom_segment(x=quantile(L50.female,c(0.5)),
               y=0.0,
               xend=quantile(L50.female,c(0.5)),
               yend=0.42,
               colour='red',size=1.0)+
  geom_segment(x=ci.L50.female[1],
               y=0.0,
               xend=ci.L50.female[2],
               yend=0.0,
               colour='red',size=1.0)+
  scale_x_continuous(name='L50 (cm)',
                     breaks=seq(88,99,1),
                     limits=c(88,99))+
  scale_y_continuous(name='')
  

#Estima puntual L50
(L50_punt.all <- quantile(L50.all,c(0.5)))
#50% 
#94.758 
(L50_punt.female <- quantile(L50.female,c(0.5)))
#50% 
#94.77704  

sessionInfo()      
  
  
#GUARDAR OBJETO:
save.image("/home/josetxu/Desktop/2023_lankideak_20230222/ICCAT_LTA_WAH_20230502/data/20230504_WAH.RData")
#---------------------------------------------------------------------------
# Hemen amaitzen da azterketa, bi mila eta hogeita hiruko maiatzaren hiruan
# Hemen amaitzen da azterketa, bi mila eta hogeita hiruko maiatzaren hiruan
#---------------------------------------------------------------------------

 
  
  
  
  
  
 




#logística diferencia entre stocks-----------------

glm_all <- glm(Maturity_stage~FL_cm*Stock_Sp,data=datuak,family=binomial)
summary(glm_all)
Anova(glm_all)
#Analysis of Deviance Table (Type II tests)
#Response: Maturity_stage
#            LR Chisq    Df     Pr(>Chisq)    
#FL_cm            599.27  1     <2e-16 ***
#Stock_Sp          97.22  1     <2e-16 ***
#FL_cm:Stock_Sp     0.63  1     0.4285    

#Ogivas de madurez por stock. Mismo panel
fig_LTA_20230503 <- 
ggplot(datuak,
       aes(x=FL_cm,y=as.numeric(Maturity_stage)-1))+
  #geom_smooth(aes(colour=Stock_Sp),
  #            method="glm",method.args=list(family="binomial"),se=T,
  #            size=3)+
  #geom_smooth(data=datuak[datuak$Stock_Sp=='MEDI_NE ATL',],
  #            method="glm",method.args=list(family="binomial"),se=F,
  #   
  geom_smooth(aes(colour=Stock_Sp),
              method="glm",method.args=list(family="binomial"),se=T)+
  scale_y_continuous(name='proportion mature')+
  scale_x_continuous(name='FL (cm)',
                     breaks=seq(20,100,10),
                     limits=c(20,100))+
  #scale_color_manual(values=c('red','black'))+ 
  theme(legend.position='none',
        axis.text=element_text(size=10))


#Diferencias L50 entre stocks-------------------------------
bcL_all <- bootCase(glm_all,B=1000)
#Warning message:
#'bootCase' is deprecated.
#Use 'Boot' instead.
#See help("Deprecated") and help("car-deprecated"). 
head(bcL_all)
tail(bcL_all)


L50.MEDI_NE_ATL <- apply(bcL_all[,1:2],1,lrPerc,p=0.5)
L50.NE_SE_ATL   <- apply(bcL_all[,1:2]+bcL_all[,3:4],1,lrPerc,p=0.5)
L50.diff        <- L50.MEDI_NE_ATL-L50.NE_SE_ATL
(p.L50.diff <- 2*min(c(mean(L50.diff>0),mean(L50.diff<0))))
#[1] 0

#Intervalo de confianza para la diferencia en L50
ci.L50.diff <- quantile(L50.diff,c(0.025,0.975))
#2.5%     97.5% 
#6.866709 10.634511
#Intervalo de confianza L50 para cada stock
ci.L50.MEDI_NE_ATL <- quantile(L50.MEDI_NE_ATL,c(0.025,0.975))
#2.5%    97.5% 
#48.74558 51.43220 
ci.L50.NE_SE_ATL   <- quantile(L50.NE_SE_ATL,c(0.025,0.975))
#2.5%    97.5% 
#40.21250 42.73938 

#glm individuales para cada stock----------------------------
#NO LOS USO
#levels(datuak$Stock_Sp)
#[1] "MEDI_NE ATL" "NE_SE_ATL"  
glm_MEDI_NE_ATL <- glm(Maturity_stage~FL_cm,
                       data=datuak$Stock_Sp=='MEDI_NE ATL',
                       family=binomial)

glm_NE_SE_ATL <- glm(Maturity_stage~FL_cm,
                       data=datuak$Stock_Sp=='NE_SE_ATL',
                       family=binomial)





sessionInfo()
#R version 4.3.0 (2023-04-21)
#Platform: x86_64-pc-linux-gnu (64-bit)
#Running under: Ubuntu 22.04.2 LTS

#Matrix products: default
#BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.20.so;  LAPACK version 3.10.0
#
#locale:
#[1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=es_ES.UTF-8       
#[4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=es_ES.UTF-8    LC_MESSAGES=en_US.UTF-8   
#[7] LC_PAPER=es_ES.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
#[10] LC_TELEPHONE=C             LC_MEASUREMENT=es_ES.UTF-8 LC_IDENTIFICATION=C       
#
#time zone: Europe/Madrid
#tzcode source: system (glibc)
#
#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#[1] ggplot2_3.4.2   car_3.1-2       carData_3.0-5   lubridate_1.9.2 dplyr_1.1.2    
#[6] plyr_1.8.8     
#
#loaded via a namespace (and not attached):
# [1] Matrix_1.5-4     gtable_0.3.3     crayon_1.5.2     vegan_2.6-4      compiler_4.3.0  
#[6] tidyselect_1.2.0 Rcpp_1.0.10      parallel_4.3.0   cluster_2.1.4    splines_4.3.0   
#[11] scales_1.2.1     lattice_0.21-8   R6_2.5.1         labeling_0.4.2   generics_0.1.3  
#[16] MASS_7.3-59      tibble_3.2.1     munsell_0.5.0    pillar_1.9.0     rlang_1.1.1     
#[21] utf8_1.2.3       timechange_0.2.0 cli_3.6.1        withr_2.5.0      magrittr_2.0.3  
#[26] mgcv_1.8-42      grid_4.3.0       rstudioapi_0.14  permute_0.9-7    lifecycle_1.0.3 
#[31] nlme_3.1-162     vctrs_0.6.2      glue_1.6.2       farver_2.1.1     abind_1.4-5     
#[36] fansi_1.0.4      colorspace_2.1-0 tools_4.3.0      pkgconfig_2.0.3 
# 
#GUARDAR OBJETO:
save.image("/home/josetxu/Desktop/2023_lankideak_20230222/ICCAT_LTA_WAH_20230502/data/20230503_LTA.RData")

#Estima puntual L50
(L50_punt.MEDI_NE_ATL <- quantile(L50.MEDI_NE_ATL,c(0.5)))
#50% 
#50.09779 
(L50_punt.NE_SE_ATL <- quantile(L50.NE_SE_ATL,c(0.5)))
#50% 
#41.41544 

#GUARDAR OBJETO:
save.image("/home/josetxu/Desktop/2023_lankideak_20230222/ICCAT_LTA_WAH_20230502/data/20230503_LTA.RData")
#---------------------------------------------------------------------------
# Hemen amaitzen da azterketa, bi mila eta hogeita hiruko maiatzaren hiruan
# Hemen amaitzen da azterketa, bi mila eta hogeita hiruko maiatzaren hiruan
#---------------------------------------------------------------------------






