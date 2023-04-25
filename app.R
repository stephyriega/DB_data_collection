
rm(list=ls())
library(shiny)
library(plyr) 
library(dplyr)
library(tidyverse)
library(rio)
library(ggplot2) 
theme_set(theme_classic())
library(shinythemes)
library(scales)
library(zoo)
library(xts) # To make the convertion data-frame / xts format
library(dygraphs)
library(htmlwidgets)
library(DT)
library(RColorBrewer)
library(tidyr)

#display.brewer.all() (Puede cambiar a 13 colores como max o alargar las paletas hasta necesario)

#---------------------------------------
#Cargamos los datos y realizamos cambios
#---------------------------------------

#importamos datos sobre la supervision

#NOTA: CAMBIAR "Stwphy" nombre de usuario
#setwd("C:\\Users\\Stephy\\Box\\El Salvador growth mindset\\07_Questionnaires&Data\\Endline_Quant\\02_DataCollection\\02 Quality control plan\\05_data\\02_survey\\Endline ES\\dta\\Supervision")

setwd("C:\\Users\\DELL\\Documents\\GitHub\\DB_Audit\\")



all_data <- import("survey.xlsx")  
all_data2 <- import("surveyor.xlsx")  

#convertinos el ano y mes en formato "yearmon"

all_data$FECHA <-substr(all_data$date_sup, 6, 11)
all_data2$FECHA <-substr(all_data2$date_sup, 6, 11)

#General: Quantity y duracion de audios
##Hallamos la Quantity de encuestados auditor, dia, Town, Group de edad, Team y Group
all_data_1 <- all_data %>% 
  group_by(auditor, FECHA, town_bl, g_age, teams, treatment ) %>%
  dplyr::summarize(n = n(), duration_min = mean(duration_min) ) 
all_data_1 <- as.data.frame(all_data_1)
colnames(all_data_1) <- c("AUDITOR", "FECHA", "MUN", "GAGE", "TEAM", "TREAT", "NUM", "DUR")
##convertimos ym en formato "Date"
all_data_1$ymd <- as.Date(as.character(all_data_1$FECHA), "%m-%d")
all_data_1$FECHA_ <- as.Date(all_data_1$ymd)

all_data_3 <- all_data %>% 
  group_by( FECHA, town_bl, g_age, teams, treatment ) %>%
  dplyr::summarize(n = n(), duration_min = mean(duration_min) ) 
all_data_3 <- as.data.frame(all_data_3)
colnames(all_data_3) <- c("FECHA", "MUN", "GAGE", "TEAM", "TREAT", "NUM", "DUR")
##convertimos ym en formato "Date"
all_data_3$ymd <- as.Date(as.character(all_data_3$FECHA), "%m-%d")
all_data_3$FECHA_ <- as.Date(all_data_3$ymd)

all_data_7 <- all_data %>% 
  group_by( FECHA) %>%
  dplyr::summarize(n = n(), duration_min = mean(duration_min) ) 
all_data_7 <- as.data.frame(all_data_7)
colnames(all_data_7) <- c( "FECHA", "NUM", "DUR")
##convertimos ym en formato "Date"
all_data_7$ymd <- as.Date(as.character(all_data_7$FECHA), "%m-%d")
all_data_7$FECHA_ <- as.Date(all_data_7$ymd)

all_data_8 <- all_data %>% 
  group_by(auditor, FECHA) %>%
  dplyr::summarize(n = n(), duration_min = mean(duration_min) ) 
all_data_8 <- as.data.frame(all_data_8)
colnames(all_data_8) <- c( "AUDITOR", "FECHA", "NUM", "DUR")
##convertimos ym en formato "Date"
all_data_8$ymd <- as.Date(as.character(all_data_8$FECHA), "%m-%d")
all_data_8$FECHA_ <- as.Date(all_data_8$ymd)

all_data_9 <- all_data %>% 
  group_by(town_bl, FECHA) %>%
  dplyr::summarize(n = n(), duration_min = mean(duration_min) ) 
all_data_9 <- as.data.frame(all_data_9)
colnames(all_data_9) <- c( "MUN", "FECHA", "NUM", "DUR")
##convertimos ym en formato "Date"
all_data_9$ymd <- as.Date(as.character(all_data_9$FECHA), "%m-%d")
all_data_9$FECHA_ <- as.Date(all_data_9$ymd)


all_data_10 <- all_data %>% 
  group_by(treatment, FECHA) %>%
  dplyr::summarize(n = n(), duration_min = mean(duration_min) ) 
all_data_10 <- as.data.frame(all_data_10)
colnames(all_data_10) <- c( "TREAT", "FECHA", "NUM", "DUR")
##convertimos ym en formato "Date"
all_data_10$ymd <- as.Date(as.character(all_data_10$FECHA), "%m-%d")
all_data_10$FECHA_ <- as.Date(all_data_10$ymd)

all_data_11 <- all_data %>% 
  group_by(g_age, FECHA) %>%
  dplyr::summarize(n = n(), duration_min = mean(duration_min) ) 
all_data_11 <- as.data.frame(all_data_11)
colnames(all_data_11) <- c( "GAGE", "FECHA", "NUM", "DUR")
##convertimos ym en formato "Date"
all_data_11$ymd <- as.Date(as.character(all_data_11$FECHA), "%m-%d")
all_data_11$FECHA_ <- as.Date(all_data_11$ymd)

all_data_12 <- all_data %>% 
  group_by(teams, FECHA) %>%
  dplyr::summarize(n = n(), duration_min = mean(duration_min) ) 
all_data_12 <- as.data.frame(all_data_12)
colnames(all_data_12) <- c( "TEAM", "FECHA", "NUM", "DUR")
##convertimos ym en formato "Date"
all_data_12$ymd <- as.Date(as.character(all_data_12$FECHA), "%m-%d")
all_data_12$FECHA_ <- as.Date(all_data_12$ymd)

#Audios
#Hallamos los indicadores auditor, dia, Town, Group de edad, Team y Group

all_data_2 <- all_data %>%
  group_by(auditor, FECHA, town_bl, g_age, teams, treatment, audio, quality, var_a) %>%
  dplyr::summarize(n())
all_data_2 <- as.data.frame(all_data_2)
colnames(all_data_2) <- c("AUDITOR", "FECHA", "MUN", "GAGE", "TEAM", "TREAT", "AUD", "QF_AUD", "Q_AUD", "N")
all_data_2$AUD_ <- as.factor(as.numeric(all_data_2$AUD))
all_data_2$Q_AUD_ <- as.factor(as.numeric(all_data_2$Q_AUD))
all_data_2$QF_AUD_ <- as.factor(as.numeric(all_data_2$QF_AUD))
##convertimos ym en formato "Date"
all_data_2$ymd <- as.Date(as.character(all_data_2$FECHA), "%m-%d")
all_data_2$FECHA_ <- as.Date(all_data_2$ymd)

all_data_2a <- all_data_2 %>% 
  group_by(FECHA) %>%
  dplyr::summarize(AUD_M = mean(AUD, na.rm=TRUE), QF_AUD_M = mean(QF_AUD, na.rm=TRUE), Q_AUD_M = mean(Q_AUD, na.rm=TRUE) ) 
all_data_2a <- as.data.frame(all_data_2a)
##convertimos ym en formato "Date"
all_data_2a$ymd<- as.Date(as.character(all_data_2a$FECHA), "%m-%d")
all_data_2a$FECHA_ <- as.Date(all_data_2a$ymd)

all_data_2b <- all_data_2 %>% 
  group_by(MUN, FECHA) %>%
  dplyr::summarize(AUD_M = mean(AUD, na.rm=TRUE), QF_AUD_M = mean(QF_AUD, na.rm=TRUE), Q_AUD_M = mean(Q_AUD, na.rm=TRUE) ) 
all_data_2b <- as.data.frame(all_data_2b)
##convertimos ym en formato "Date"
all_data_2b$ymd <- as.Date(as.character(all_data_2b$FECHA), "%m-%d")
all_data_2b$FECHA_ <- as.Date(all_data_2b$ymd)

all_data_2c <- all_data_2 %>% 
  group_by(TREAT, FECHA) %>%
  dplyr::summarize(AUD_M = mean(AUD, na.rm=TRUE), QF_AUD_M = mean(QF_AUD, na.rm=TRUE), Q_AUD_M = mean(Q_AUD, na.rm=TRUE) ) 
all_data_2c <- as.data.frame(all_data_2c)
##convertimos ym en formato "Date"
all_data_2c$ymd <- as.Date(as.character(all_data_2c$FECHA), "%m-%d")
all_data_2c$FECHA_ <- as.Date(all_data_2c$ymd)

all_data_2d <- all_data_2 %>% 
  group_by(GAGE, FECHA) %>%
  dplyr::summarize(AUD_M = mean(AUD, na.rm=TRUE), QF_AUD_M = mean(QF_AUD, na.rm=TRUE), Q_AUD_M = mean(Q_AUD, na.rm=TRUE) ) 
all_data_2d <- as.data.frame(all_data_2d)
##convertimos ym en formato "Date"
all_data_2d$ymd <- as.Date(as.character(all_data_2d$FECHA), "%m-%d")
all_data_2d$FECHA_ <- as.Date(all_data_2d$ymd)

all_data_2e <- all_data_2 %>% 
  group_by(TEAM, FECHA) %>%
  dplyr::summarize(AUD_M = mean(AUD, na.rm=TRUE), QF_AUD_M = mean(QF_AUD, na.rm=TRUE), Q_AUD_M = mean(Q_AUD, na.rm=TRUE) ) 
all_data_2e <- as.data.frame(all_data_2e)
##convertimos ym en formato "Date"
all_data_2e$ymd <- as.Date(as.character(all_data_2e$FECHA), "%m-%d")
all_data_2e$FECHA_ <- as.Date(all_data_2e$ymd)


#Encuestador
#Hallamos los indicadores auditor, dia, Town, Group de edad, Team y Group

all_data_4 <- all_data %>%
  group_by(auditor, FECHA, town_bl, g_age, teams, treatment, p_cons_cg, p_cons_cg_n ) %>%
  dplyr::summarize(n())
all_data_4 <- as.data.frame(all_data_4)
colnames(all_data_4) <- c("AUDITOR", "FECHA", "MUN", "GAGE", "TEAM", "TREAT", "CON_CG", "CON_CH", "N")
all_data_4$CON_CG_ <- as.factor(as.numeric(all_data_4$CON_CG))
all_data_4$CON_CH_ <- as.factor(as.numeric(all_data_4$CON_CH))
##convertimos ym en formato "Date"
all_data_4$ymd <- as.Date(as.character(all_data_4$FECHA), "%m-%d")
all_data_4$FECHA_ <- as.Date(all_data_4$ymd)

all_data_4a <- all_data_4 %>% 
  group_by(FECHA) %>%
  dplyr::summarize(CON_CG_M = mean(CON_CG, na.rm=TRUE), CON_CH_M = mean(CON_CH, na.rm=TRUE)) 
all_data_4a <- as.data.frame(all_data_4a)
##convertimos ym en formato "Date"
all_data_4a$ymd<- as.Date(as.character(all_data_4a$FECHA), "%m-%d")
all_data_4a$FECHA_ <- as.Date(all_data_4a$ymd)


all_data_4b <- all_data_4 %>% 
  group_by(MUN, FECHA) %>%
  dplyr::summarize(CON_CG_M = mean(CON_CG, na.rm=TRUE), CON_CH_M = mean(CON_CH, na.rm=TRUE) ) 
all_data_4b <- as.data.frame(all_data_4b)
##convertimos ym en formato "Date"
all_data_4b$ymd <- as.Date(as.character(all_data_4b$FECHA), "%m-%d")
all_data_4b$FECHA_ <- as.Date(all_data_4b$ymd)

all_data_4c <- all_data_4 %>% 
  group_by(TREAT, FECHA) %>%
  dplyr::summarize(CON_CG_M = mean(CON_CG, na.rm=TRUE), CON_CH_M = mean(CON_CH, na.rm=TRUE) ) 
all_data_4c <- as.data.frame(all_data_4c)
##convertimos ym en formato "Date"
all_data_4c$ymd <- as.Date(as.character(all_data_4c$FECHA), "%m-%d")
all_data_4c$FECHA_ <- as.Date(all_data_4c$ymd)

all_data_4d <- all_data_4 %>% 
  group_by(GAGE, FECHA) %>%
  dplyr::summarize(CON_CG_M = mean(CON_CG, na.rm=TRUE), CON_CH_M = mean(CON_CH, na.rm=TRUE) ) 
all_data_4d <- as.data.frame(all_data_4d)
##convertimos ym en formato "Date"
all_data_4d$ymd <- as.Date(as.character(all_data_4d$FECHA), "%m-%d")
all_data_4d$FECHA_ <- as.Date(all_data_4d$ymd)

all_data_4e <- all_data_4 %>% 
  group_by(TEAM, FECHA) %>%
  dplyr::summarize(CON_CG_M = mean(CON_CG, na.rm=TRUE), CON_CH_M = mean(CON_CH, na.rm=TRUE) ) 
all_data_4e <- as.data.frame(all_data_4e)
##convertimos ym en formato "Date"
all_data_4e$ymd <- as.Date(as.character(all_data_4e$FECHA), "%m-%d")
all_data_4e$FECHA_ <- as.Date(all_data_4e$ymd)



#Group2b
#Hallamos los indicadores auditor, dia, Town, Group de edad, Team y Group

all_data_5 <- all_data %>%
  group_by(auditor, FECHA, town_bl, g_age, teams, treatment, var_0, var_0_n ) %>%
  dplyr::summarize(n())
all_data_5 <- as.data.frame(all_data_5)
colnames(all_data_5) <- c("AUDITOR", "FECHA", "MUN", "GAGE", "TEAM", "TREAT", "USO_CG", "USO_CH", "N")
all_data_5$USO_CG_ <- as.factor(as.numeric(all_data_5$USO_CG))
all_data_5$USO_CH_ <- as.factor(as.numeric(all_data_5$USO_CH))
##convertimos ym en formato "Date"
all_data_5$ymd <- as.Date(as.character(all_data_5$FECHA), "%m-%d")
all_data_5$FECHA_ <- as.Date(all_data_5$ymd)


all_data_5a <- all_data_5 %>% 
  group_by(FECHA) %>%
  dplyr::summarize(USO_CG_M = mean(USO_CG, na.rm=TRUE), USO_CH_M = mean(USO_CH, na.rm=TRUE)) 
all_data_5a <- as.data.frame(all_data_5a)
##convertimos ym en formato "Date"
all_data_5a$ymd<- as.Date(as.character(all_data_5a$FECHA), "%m-%d")
all_data_5a$FECHA_ <- as.Date(all_data_5a$ymd)


all_data_5b <- all_data_5 %>% 
  group_by(MUN, FECHA) %>%
  dplyr::summarize(USO_CG_M = mean(USO_CG, na.rm=TRUE), USO_CH_M = mean(USO_CH, na.rm=TRUE) ) 
all_data_5b <- as.data.frame(all_data_5b)
##convertimos ym en formato "Date"
all_data_5b$ymd <- as.Date(as.character(all_data_5b$FECHA), "%m-%d")
all_data_5b$FECHA_ <- as.Date(all_data_5b$ymd)

all_data_5c <- all_data_5 %>% 
  group_by(TREAT, FECHA) %>%
  dplyr::summarize(USO_CG_M = mean(USO_CG, na.rm=TRUE), USO_CH_M = mean(USO_CH, na.rm=TRUE) ) 
all_data_5c <- as.data.frame(all_data_5c)
##convertimos ym en formato "Date"
all_data_5c$ymd <- as.Date(as.character(all_data_5c$FECHA), "%m-%d")
all_data_5c$FECHA_ <- as.Date(all_data_5c$ymd)

all_data_5d <- all_data_5 %>% 
  group_by(GAGE, FECHA) %>%
  dplyr::summarize(USO_CG_M = mean(USO_CG, na.rm=TRUE), USO_CH_M = mean(USO_CH, na.rm=TRUE) ) 
all_data_5d <- as.data.frame(all_data_5d)
##convertimos ym en formato "Date"
all_data_5d$ymd <- as.Date(as.character(all_data_5d$FECHA), "%m-%d")
all_data_5d$FECHA_ <- as.Date(all_data_5d$ymd)

all_data_5e <- all_data_5 %>% 
  group_by(TEAM, FECHA) %>%
  dplyr::summarize(USO_CG_M = mean(USO_CG, na.rm=TRUE), USO_CH_M = mean(USO_CH, na.rm=TRUE) ) 
all_data_5e <- as.data.frame(all_data_5e)
##convertimos ym en formato "Date"
all_data_5e$ymd <- as.Date(as.character(all_data_5e$FECHA), "%m-%d")
all_data_5e$FECHA_ <- as.Date(all_data_5e$ymd)


#Group4
all_data_6 <- all_data %>%
  group_by(auditor, FECHA, town_bl, g_age, teams, treatment, var_b, var_p ) %>%
  dplyr::summarize(n())
all_data_6 <- as.data.frame(all_data_6)
colnames(all_data_6) <- c("AUDITOR", "FECHA", "MUN", "GAGE", "TEAM", "TREAT", "CALF", "PREG", "N")
all_data_6$CALF_ <- as.factor(as.numeric(all_data_6$CALF))
all_data_6$PREG_ <- as.factor(as.numeric(all_data_6$PREG))
##convertimos ym en formato "Date"
all_data_6$ymd <- as.Date(as.character(all_data_6$FECHA), "%m-%d")
all_data_6$FECHA_ <- as.Date(all_data_6$ymd)

all_data_6a <- all_data_6 %>% 
  group_by(FECHA) %>%
  dplyr::summarize(CALF_M = mean(CALF, na.rm=TRUE), PREG_M = mean(PREG, na.rm=TRUE)) 
all_data_6a <- as.data.frame(all_data_6a)
##convertimos ym en formato "Date"
all_data_6a$ymd<- as.Date(as.character(all_data_6a$FECHA), "%m-%d")
all_data_6a$FECHA_ <- as.Date(all_data_6a$ymd)


all_data_6b <- all_data_6 %>% 
  group_by(MUN, FECHA) %>%
  dplyr::summarize(CALF_M = mean(CALF, na.rm=TRUE), PREG_M = mean(PREG, na.rm=TRUE) ) 
all_data_6b <- as.data.frame(all_data_6b)
##convertimos ym en formato "Date"
all_data_6b$ymd <- as.Date(as.character(all_data_6b$FECHA), "%m-%d")
all_data_6b$FECHA_ <- as.Date(all_data_6b$ymd)

all_data_6c <- all_data_6 %>% 
  group_by(TREAT, FECHA) %>%
  dplyr::summarize(CALF_M = mean(CALF, na.rm=TRUE), PREG_M = mean(PREG, na.rm=TRUE) ) 
all_data_6c <- as.data.frame(all_data_6c)
##convertimos ym en formato "Date"
all_data_6c$ymd <- as.Date(as.character(all_data_6c$FECHA), "%m-%d")
all_data_6c$FECHA_ <- as.Date(all_data_6c$ymd)

all_data_6d <- all_data_6 %>% 
  group_by(GAGE, FECHA) %>%
  dplyr::summarize(CALF_M = mean(CALF, na.rm=TRUE), PREG_M = mean(PREG, na.rm=TRUE) ) 
all_data_6d <- as.data.frame(all_data_6d)
##convertimos ym en formato "Date"
all_data_6d$ymd <- as.Date(as.character(all_data_6d$FECHA), "%m-%d")
all_data_6d$FECHA_ <- as.Date(all_data_6d$ymd)

all_data_6e <- all_data_6 %>% 
  group_by(TEAM, FECHA) %>%
  dplyr::summarize(CALF_M = mean(CALF, na.rm=TRUE), PREG_M = mean(PREG, na.rm=TRUE) ) 
all_data_6e <- as.data.frame(all_data_6e)
##convertimos ym en formato "Date"
all_data_6e$ymd <- as.Date(as.character(all_data_6e$FECHA), "%m-%d")
all_data_6e$FECHA_ <- as.Date(all_data_6e$ymd)

#Group4
all_data_13 <- all_data %>%
  group_by(auditor, FECHA, town_bl, g_age, teams, treatment, trans, sim ) %>%
  dplyr::summarize(n())
all_data_13 <- as.data.frame(all_data_13)
colnames(all_data_13) <- c("AUDITOR", "FECHA", "MUN", "GAGE", "TEAM", "TREAT", "TRANS", "SIM", "N")
all_data_13$TRANS_ <- as.factor(as.numeric(all_data_13$TRANS))
all_data_13$SIM_ <- as.factor(as.numeric(all_data_13$SIM))
##convertimos ym en formato "Date"
all_data_13$ymd <- as.Date(as.character(all_data_13$FECHA), "%m-%d")
all_data_13$FECHA_ <- as.Date(all_data_13$ymd)

all_data_13a <- all_data_13 %>%
  group_by(FECHA) %>%
  dplyr::summarize(TRANS_M = mean(TRANS, na.rm=TRUE), SIM_M = mean(SIM, na.rm=TRUE))
all_data_13a <- as.data.frame(all_data_13a)
##convertimos ym en formato "Date"
all_data_13a$ymd<- as.Date(as.character(all_data_13a$FECHA), "%m-%d")
all_data_13a$FECHA_ <- as.Date(all_data_13a$ymd)


all_data_13b <- all_data_13 %>%
  group_by(MUN, FECHA) %>%
  dplyr::summarize(TRANS_M = mean(TRANS, na.rm=TRUE), SIM_M = mean(SIM, na.rm=TRUE) )
all_data_13b <- as.data.frame(all_data_13b)
##convertimos ym en formato "Date"
all_data_13b$ymd <- as.Date(as.character(all_data_13b$FECHA), "%m-%d")
all_data_13b$FECHA_ <- as.Date(all_data_13b$ymd)

all_data_13c <- all_data_13 %>%
  group_by(TREAT, FECHA) %>%
  dplyr::summarize(TRANS_M = mean(TRANS, na.rm=TRUE), SIM_M = mean(SIM, na.rm=TRUE) )
all_data_13c <- as.data.frame(all_data_13c)
##convertimos ym en formato "Date"
all_data_13c$ymd <- as.Date(as.character(all_data_13c$FECHA), "%m-%d")
all_data_13c$FECHA_ <- as.Date(all_data_13c$ymd)

all_data_13d <- all_data_13 %>%
  group_by(GAGE, FECHA) %>%
  dplyr::summarize(TRANS_M = mean(TRANS, na.rm=TRUE), SIM_M = mean(SIM, na.rm=TRUE) )
all_data_13d <- as.data.frame(all_data_13d)
##convertimos ym en formato "Date"
all_data_13d$ymd <- as.Date(as.character(all_data_13d$FECHA), "%m-%d")
all_data_13d$FECHA_ <- as.Date(all_data_13d$ymd)

all_data_13e <- all_data_13 %>%
  group_by(TEAM, FECHA) %>%
  dplyr::summarize(TRANS_M = mean(TRANS, na.rm=TRUE), SIM_M = mean(SIM, na.rm=TRUE) )
all_data_13e <- as.data.frame(all_data_13e)
##convertimos ym en formato "Date"
all_data_13e$ymd <- as.Date(as.character(all_data_13e$FECHA), "%m-%d")
all_data_13e$FECHA_ <- as.Date(all_data_13e$ymd)

# 
all_data_14 <- all_data2 %>%
  group_by(encuestador, FECHA) %>%
  dplyr::summarize(cons_d = mean(cons, na.rm=TRUE),leng_d = mean(leng, na.rm=TRUE), var_0_d = mean(var_0, na.rm=TRUE), var_0_n_d = mean(var_0_n, na.rm=TRUE), var_p_d = mean(var_p, na.rm=TRUE), prom_2_d = mean(prom_2, na.rm=TRUE) )
all_data_14 <- as.data.frame(all_data_14)
colnames(all_data_14) <- c("ENCUESTADOR", "FECHA", "CON", "LENG", "U_AD", "U_N", "PREG", "IND")
##convertimos ym en formato "Date"
all_data_14$ymd <- as.Date(as.character(all_data_14$FECHA), "%m-%d")
all_data_14$FECHA_ <- as.Date(all_data_14$ymd)

# #Group4
# all_data_13 <- all_data %>%
#   group_by(auditor, FECHA, town_bl, g_age, teams, treatment, trans, sim ) %>%
#   dplyr::summarize(n())
# all_data_13 <- as.data.frame(all_data_13)
# colnames(all_data_13) <- c("AUDITOR", "FECHA", "MUN", "GAGE", "TEAM", "TREAT", "TRANS", "SIM", "N")
# all_data_13$TRANS_ <- as.factor(as.numeric(all_data_13$TRANS))
# all_data_13$SIM_ <- as.factor(as.numeric(all_data_13$SIM))
# ##convertimos ym en formato "Date"
# all_data_13$ymd <- as.Date(as.character(all_data_13$FECHA), "%m-%d")
# all_data_13$FECHA_ <- as.Date(all_data_13$ymd)


#AUD_M = mean(AUD, na.rm=TRUE)

#Datos
##Hallamos los indicadores auditor, dia, Town, Group de edad, Team y Group
# nb.cols <- 11
# myGreens <- colorRampPalette(brewer.pal(8, "Greens"))(nb.cols)
# 

#funciones

CV <- function(x){
  (sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE))*100
}

#---------------------------
#Comenzamos a generar la app
#---------------------------

ui <- fluidPage(
  #theme=shinytheme("cosmo"),
  titlePanel ( "Dashboard for data collection"),
  
  sidebarLayout(
    
    sidebarPanel(
      width=2,
      #Seleccion: fecha   
      sliderInput(
        inputId = "date_input",
        label="Dates",
        min=as.Date("2023-03-18","%Y-%m-%d"),
        max=as.Date("2023-06-22","%Y-%m-%d"),
        value=c(as.Date("2023-03-18"), as.Date("2023-06-22")),
        timeFormat = "%F"
      ),
      style = "background-color:#c3df9d;",
      
      selectInput(inputId="dep_button",label="Choose level:",choices=c("Time Series", "Cross Section"),multiple=FALSE
      ),
      
      conditionalPanel(
        condition = "input.dep_button == 'Cross Section'",
        
      
      #Seleccion:  Town
      checkboxGroupInput(
        inputId="town_input",
        label="Town",
        choices=unique(all_data$town_bl),
        selected=unique(all_data$town_bl),
        inline = FALSE
      ),
      #Seleccion:  Treatment group
      checkboxGroupInput(
        inputId="group_input",
        label="Group",
        choices=unique(all_data$treatment),
        selected=unique(all_data$treatment),
        inline=FALSE
      ),
      #Seleccion:  Age group
      checkboxGroupInput(
        inputId="age_input",
        label="Age group",
        choices=unique(all_data$g_age),
        selected=unique(all_data$g_age)
      ),
      #Seleccion: teams
      checkboxGroupInput(
        inputId="teams_input",
        label="Team",
        choices=unique(all_data$teams),
        selected=unique(all_data$teams)
      ),
      #prox: seleccion de encuestador condicionado a seleccion de Team
      style = "background-color:#c3df9d;"
    ),
    
    conditionalPanel(
      condition = "input.dep_button == 'Time Series'"
    )
      
    ),
    
    mainPanel(
      width=10,
      tabsetPanel(
        #General: Quantity y Minutes
        tabPanel("General",
                 br(),
                 
                 p("Main indicators: quantity of surveys and average survey duration", 
                   style="text-align:justify;color:black"
                 ),
                 
                 conditionalPanel(
                   condition = "input.dep_button == 'Time Series'",
                  
                   p("", 
                     style="text-align:justify;color:black"
                   ),
                   
                 p("General Information", 
                   style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                 ),
                 fluidRow(12,
                          column(6,plotOutput('plot1')),
                          column(6,plotOutput('plot2'))
                 ),
                 
                 selectInput(inputId="dep_button1",label="Choose level:",choices=c("Supervisor", "Town", "Treatment group", "Age group", "Team"),multiple=FALSE
                 ),
                 
                 conditionalPanel(
                   
                   
                   condition = "input.dep_button1 == 'Supervisor'",
                   
                   
                 p("Supervisor", 
                   style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                 ),
                 
                 fluidRow(12,
                          column(6,plotOutput('plot6')),
                          column(6,plotOutput('plot7'))
                 )
                 ),
                 
                 conditionalPanel(
                   
                   
                   condition = "input.dep_button1 == 'Town'",
                   
                 
                 p("Town", 
                   style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                 ),
                 
                 fluidRow(12,
                          column(6,plotOutput('plot15')),
                          column(6,plotOutput('plot16'))
                 )
                ),
                
                conditionalPanel(
                  
                  
                  condition = "input.dep_button1 == 'Treatment group'",
                  
                 
                 p("Group", 
                   style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                 ),
                 
                 fluidRow(12,
                          column(6,plotOutput('plot17')),
                          column(6,plotOutput('plot18'))
                 )
                 ),
                
                conditionalPanel(
                  
                  
                  condition = "input.dep_button1 == 'Age group'",
                 
                 p("Age group", 
                   style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                 ),
                 
                 fluidRow(12,
                          column(6,plotOutput('plot19')),
                          column(6,plotOutput('plot20'))
                 )
                 ),
                
                conditionalPanel(
                  
                  
                  condition = "input.dep_button1 == 'Team'",
                  
                
                 
                 p("Team", 
                   style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                 ),
                 
                 fluidRow(12,
                          column(6,plotOutput('plot21')),
                          column(6,plotOutput('plot22'))
                 )
                 )
                ),
                 
                 conditionalPanel(
                   
                   
                   condition = "input.dep_button == 'Cross Section'",
                   
                   p("", 
                     style="text-align:justify;color:black"
                   ),
                   
                   p("General Information", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   
                   fluidRow(12,
                            column(6,plotOutput('plot8')),
                            column(6,plotOutput('plot23'))
                   ),
                   fluidRow(12,
                            column(3,),
                            column(6,tableOutput('table6')),
                            column(3,tableOutput('table8'))
                   )
                 )
                 
                 
        ),
        
        #Audio: audios completos, calidad final y calidad modulos
        tabPanel("Audio",
                 br(),
                 
                 p("Audio indicators: Availability of audios, Supervision of audios and Quality of audios", 
                   style="text-align:justify;color:black"
                 ),
                
                 
                conditionalPanel(
                   
                   condition = "input.dep_button == 'Time Series'",
                   
                   p("General Information", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   fluidRow(12,
                            column(4,plotOutput('plot36')),
                            column(4,plotOutput('plot37')),
                            column(4,plotOutput('plot38'))
                   ),
                   
                   selectInput(inputId="dep_button2",label="Choose level:",choices=c("Town", "Treatment group", "Age group", "Team"),multiple=FALSE
                   ),
                   
                   conditionalPanel(
                     
                     
                     condition = "input.dep_button2 == 'Town'",
                     
                   
                   
                   p("Town", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   
                   fluidRow(12,
                            column(4,plotOutput('plot24')),
                            column(4,plotOutput('plot25')),
                            column(4,plotOutput('plot26'))
                   )
                   ),
                   
                   conditionalPanel(
                     
                     
                     condition = "input.dep_button2 == 'Treatment group'",
                   
                   p("Group", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   
                   fluidRow(12,
                            column(4,plotOutput('plot27')),
                            column(4,plotOutput('plot28')),
                            column(4,plotOutput('plot29'))
                   )
                   ),
                   
                   conditionalPanel(
                     
                     
                     condition = "input.dep_button2 == 'Age group'",
                   
                   p("Age group", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   
                   fluidRow(12,
                            column(4,plotOutput('plot30')),
                            column(4,plotOutput('plot31')),
                            column(4,plotOutput('plot32'))
                   )
                   ),
                   
                   conditionalPanel(
                     
                     
                     condition = "input.dep_button2 == 'Team'",
                     
                   
                   p("Team", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   
                   fluidRow(12,
                            column(4,plotOutput('plot33')),
                            column(4,plotOutput('plot34')),
                            column(4,plotOutput('plot35'))
                   )
                 )
                 ),
                
                conditionalPanel(
                  
                  condition = "input.dep_button == 'Cross Section'",
                  
                  p("All filters allowed", 
                    style="text-align:justify;color:black"
                  ),
                  
                  p("Reportada al inicio y al final", 
                    style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                  ),
                  fluidRow(12,
                           column(4,plotOutput('plot3')),
                           column(4,plotOutput('plot5')),
                           column(4,plotOutput('plot4'))
                  ),
                  
                  fluidRow(12,
                           column(2,),
                           column(8,tableOutput('table1')),
                           column(2,)
                  )
                )
        ),
        
        #Encuestador: Consentimiento, uso de material e indicaciones y revision al final
        
        tabPanel("Surveyor",
                 br(),
                 
                 
                 p("Surveyor indicators: performance in consent, explanation of the use of materials and indications  (for parent and child) and final perfomance review", 
                   style="text-align:justify;color:black"
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dep_button == 'Time Series'",
                   
                   
                   p("General Information", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   fluidRow(12,
                            column(4,plotOutput('plot39')),
                            column(4,plotOutput('plot40')),
                            column(4,plotOutput('plot41')),
                            column(4,plotOutput('plot42')),
                            column(4,plotOutput('plot43')),
                            column(4,plotOutput('plot44')) 
                   ),
                   
                   selectInput(inputId="dep_button3",label="Choose level:",choices=c("Town", "Treatment group", "Age group", "Team"),multiple=FALSE
                   ),
                   
                   conditionalPanel(
                     
                     
                     condition = "input.dep_button3 == 'Town'",
                   
                   
                   
                   p("Town", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   
                   fluidRow(12,
                            column(4,plotOutput('plot45')),
                            column(4,plotOutput('plot46')),
                            column(4,plotOutput('plot47')),
                            column(4,plotOutput('plot48')),
                            column(4,plotOutput('plot49')),
                            column(4,plotOutput('plot50'))
                   )
                   ),
                   
                   conditionalPanel(
                     
                     
                     condition = "input.dep_button3 == 'Treatment group'",
                   
                   
                   
                   p("Group", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   
                   fluidRow(12,
                            column(4,plotOutput('plot51')),
                            column(4,plotOutput('plot52')),
                            column(4,plotOutput('plot53')),
                            column(4,plotOutput('plot54')),
                            column(4,plotOutput('plot55')),
                            column(4,plotOutput('plot56'))
                   )
                   ),
                   
                   conditionalPanel(
                     
                     
                     condition = "input.dep_button3 == 'Age group'",
                   
                   
                   
                   p("Age group", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   
                   fluidRow(12,
                            column(4,plotOutput('plot57')),
                            column(4,plotOutput('plot58')),
                            column(4,plotOutput('plot59')),
                            column(4,plotOutput('plot60')),
                            column(4,plotOutput('plot61')),
                            column(4,plotOutput('plot62')),
                   )
                   ),
                   
                   conditionalPanel(
                     
                     
                     condition = "input.dep_button3 == 'Team'",
                   
                   p("Team", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   
                   fluidRow(12,
                            column(4,plotOutput('plot63')),
                            column(4,plotOutput('plot64')),
                            column(4,plotOutput('plot65')),
                            column(4,plotOutput('plot66')),
                            column(4,plotOutput('plot67')),
                            column(4,plotOutput('plot68'))
                   )
                   )
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dep_button == 'Cross Section'",
                   
                   
                 p("Consent", 
                   style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                 ),
                 fluidRow(12,
                          column(6,plotOutput('plot9')),
                          column(6,plotOutput('plot10'))
                 ),
                 fluidRow(12,
                          column(2,),
                          column(8,tableOutput('table2')),
                          column(2,)
                 ),
                 br(),
                 p("Use of materials and indications (for parent and child)", 
                   style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                 ),
                 fluidRow(12,
                          column(6,plotOutput('plot11')),
                          column(6,plotOutput('plot12'))
                 ),
                 fluidRow(12,
                          column(2,),
                          column(8,tableOutput('table3')),
                          column(2,)
                 ),
                 br(),
                 p("Final perfomance rating", 
                   style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                 ),
                 fluidRow(12,
                          column(6,plotOutput('plot13')),
                          column(6,plotOutput('plot14'))
                 ),
                 fluidRow(12,
                          column(2,),
                          column(8,tableOutput('table4')),
                          column(2,)
                 )
                 )
        ),
        tabPanel("Open answers",
                 
                 br(),
                 
                 p("Open answers indicators: Transcription accuracy and classification of open answer", 
                   style="text-align:justify;color:black"
                 ),
                
                 
                 conditionalPanel(
                   condition = "input.dep_button == 'Time Series'",
                   
                   p("Open answers", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   fluidRow(12,
                            column(6,plotOutput('plot69')),
                            column(6,plotOutput('plot70'))
                            #prox: graficos individuales condicionado a la seleccion y que se muestre el acumulado en parte superior 
                            #prox: medias moviles dias
                   ),
                   

                   selectInput(inputId="dep_button4",label="Choose level:",choices=c("Town", "Treatment group", "Age group", "Team"),multiple=FALSE
                   ),
                   
                   conditionalPanel(
                     
                     
                     condition = "input.dep_button4 == 'Town'",
                     
                     
                     p("Town", 
                       style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                     ),
                     
                     fluidRow(12,
                              column(6,plotOutput('plot73')),
                              column(6,plotOutput('plot74'))
                     )
                   ),
                   
                   conditionalPanel(
                     
                     
                     condition = "input.dep_button4 == 'Treatment group'",
                     
                     
                     p("Group", 
                       style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                     ),
                     
                     fluidRow(12,
                              column(6,plotOutput('plot75')),
                              column(6,plotOutput('plot76'))
                     )
                   ),
                   
                   conditionalPanel(
                     
                    
                     condition = "input.dep_button4 == 'Age group'",
                     
                     p("Age group", 
                       style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                     ),
                     
                     fluidRow(12,
                              column(6,plotOutput('plot77')),
                              column(6,plotOutput('plot78'))
                     )
                   ),
                   
                   conditionalPanel(
                     
                     
                     condition = "input.dep_button4 == 'Team'",
                     
                     
                     p("Team", 
                       style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                     ),
                     
                     fluidRow(12,
                              column(6,plotOutput('plot79')),
                              column(6,plotOutput('plot80'))
                     )
                   )
                 ),
                 
                 conditionalPanel(
                   
                   
                   condition = "input.dep_button == 'Cross Section'",
                   
                   p("Open answers", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   ),
                   
                   fluidRow(12,
                            column(6,plotOutput('plot81')),
                            column(6,plotOutput('plot82'))
                   ),
                   fluidRow(12,
                            column(2,),
                            column(8,tableOutput('table5')),
                            column(2,)
                   )
                 )
              ),
        
        tabPanel("Ranking",
                 
                 br(),
                 
                 p("Ranking of surveyors: summary, consent, open answer data, use of materials and guidelines (for parent and child), and review of questions (in order). ", 
                   style="text-align:justify;color:black"
                 ),
                 
                 
                 conditionalPanel(
                   condition = "input.dep_button == 'Time Series'",

                   fluidRow(12,
                            column(2,),
                            column(8,tableOutput('table7')),
                            column(2,)
                            #prox: graficos individuales condicionado a la seleccion y que se muestre el acumulado en parte superior 
                            #prox: medias moviles dias
                   )
                   ),
                 
                 conditionalPanel(
                   
                   condition = "input.dep_button == 'Cross Section'",
                   
                   p("Ranking of surveyors", 
                     style="text-align:justify;color:black;background-color:#c3df9d;padding:15px;border-radius:10px"
                   )
                 )
              )
            )
          )
      )
    )



server <- function(input,output){
  
  #General
  output$plot1 <-renderPlot({
    
    plot_dat1<-filter(
      all_data_7,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat1,
           aes(x=FECHA_, y=NUM))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Number of surveys",
           subtitle = "",
           caption = "",
           x = "Date", y = "Quantity",
           tag = "#1")      
  })
  

  output$plot2 <-renderPlot({
    
    plot_dat2<-filter(
      all_data_7,
       FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat2,
           aes(x=FECHA_, y=DUR, label=DUR))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Average survey duration",
           subtitle = "",
           caption = "",
           x = "Date", y = "Minutes",
           tag = "#2")      
  })
  
  #Audios
  output$plot3 <-renderPlot({
    
    plot_dat3<-filter(
      all_data_2,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat3,
           aes(x="", y=N, fill=AUD_))+
      geom_bar(stat="identity")+ 
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Greens")+
      theme_minimal()+
      labs(title = "Availability of audios",
           subtitle = "",
           caption = "",
           x = "", y = "",
           tag = "#3")      
  })
  
  output$plot4 <-renderPlot({
    
    plot_dat4<-filter(
      all_data_2,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat4,
           aes(x="", y=N, fill=Q_AUD_))+
      geom_bar(stat="identity")+
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Greens")+
      theme_minimal()+
      labs(title = "Supervision of audios",
           subtitle = "",
           caption = "",
           x = "", y = "",
           tag = "#4")      
  })
  
  output$plot5 <-renderPlot({
    
    plot_dat5<-filter(
      all_data_2,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat5,
           aes(x="", y=N, fill=QF_AUD_))+
      geom_bar(stat="identity")+
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Greens")+
      theme_minimal()+
      labs(title = "Quality of audios",
           subtitle = "",
           caption = "",
           x = "", y = "",
           tag = "#5")      
  })
  
  output$plot6 <-renderPlot({
    
    plot_dat6<-filter(
      all_data_8,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat6,
           aes(x=FECHA_, y=NUM, color=AUDITOR, label=NUM))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Quantity of surveys",
           subtitle = "",
           caption = "",
           x = "Date", y = "Quantity",
           tag = "#6")      
  })
  
  output$plot7 <-renderPlot({
    
    plot_dat7<-filter(
      all_data_8,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat7,
           aes(x=FECHA_, y=DUR, color=AUDITOR, label=DUR))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Average survey duration",
           subtitle = "",
           caption = "",
           x = "Date", y = "Minutes",
           tag = "#7")      
  })
  
  output$plot8 <-renderPlot({

    plot_dat8<-filter(
      all_data_1,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )

    ggplot(plot_dat8,
           aes(x="", y=NUM, fill=AUDITOR))+
      geom_bar(stat="identity")+
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Greens")+
      theme_minimal()+
      labs(title = "Quantity of surveys",
           subtitle = "",
           caption = "",
           x = NULL, y = NULL,
           tag = "#8")
  })
  
  
  output$plot23 <-renderPlot({
    
    plot_dat23<-filter(
      all_data_1,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat23,
           aes(x=AUDITOR, y=DUR, fill=AUDITOR))+
      geom_bar(stat="identity")+
      scale_fill_brewer(palette="Greens")+
      theme_minimal()+
      labs(title = "Minutes in total",
           subtitle = "",
           caption = "",
           x = NULL, y = NULL,
           tag = "#23")
  })
  

  output$plot9 <-renderPlot({
    
    plot_dat9<-filter(
      all_data_4,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat9,
           aes(x="", y=N, fill=CON_CG_))+
      geom_bar(stat="identity")+ 
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Greens")+
      theme_minimal()+
      labs(title = "In parents",
           subtitle = "",
           caption = "",
           x = "", y = "",
           tag = "#9")      
  })
  
  output$plot10 <-renderPlot({
    
    plot_dat10<-filter(
      all_data_4,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat10,
           aes(x="", y=N, fill=CON_CH_))+
      geom_bar(stat="identity")+ 
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Greens")+
      theme_minimal()+
      labs(title = "In child",
           subtitle = "",
           caption = "",
           x = "", y = "",
           tag = "#10")      
  })
  
  output$plot11 <-renderPlot({
    
    plot_dat11<-filter(
      all_data_5,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat11,
           aes(x="", y=N, fill=USO_CG_))+
      geom_bar(stat="identity")+ 
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Greens")+
      theme_minimal()+
      labs(title = "In parents",
           subtitle = "",
           caption = "",
           x = "", y = "",
           tag = "#11")      
  })
  
  output$plot12 <-renderPlot({
    
    plot_dat12<-filter(
      all_data_5,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat12,
           aes(x="", y=N, fill=USO_CH_))+
      geom_bar(stat="identity")+ 
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Greens")+
      theme_minimal()+
      labs(title = "In child",
           subtitle = "",
           caption = "",
           x = "", y = "",
           tag = "#12")      
  })
  
  output$plot13 <-renderPlot({
    
    plot_dat13<-filter(
      all_data_6,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat13,
           aes(x="", y=N, fill=CALF_))+
      geom_bar(stat="identity")+ 
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Greens")+
      theme_minimal()+
      labs(title = "PuntuaciÃÂÃÂ³n final",
           subtitle = "",
           caption = "",
           x = "", y = "",
           tag = "#13")      
  })
  
  
  output$plot14 <-renderPlot({
    
    plot_dat14<-filter(
      all_data_6,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat14,
           aes(x="", y=N, fill=PREG_))+
      geom_bar(stat="identity")+ 
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Greens")+
      theme_minimal()+
      labs(title = "Attention of the participants",
           subtitle = "",
           caption = "",
           x = "", y = "",
           tag = "#14")      
  })
  
  output$plot15 <-renderPlot({
    
    plot_dat15<-filter(
      all_data_9,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat15,
           aes(x=FECHA_, y=NUM, color=MUN, label=NUM))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Quantity of surveys",
           subtitle = "",
           caption = "",
           x = "Date", y = "Quantity",
           tag = "#15")      
  })
  
  output$plot16 <-renderPlot({

    plot_dat16<-filter(
      all_data_9,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )

    ggplot(plot_dat16,
           aes(x=FECHA_, y=DUR, color=MUN, label=DUR))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Average survey duration",
           subtitle = "",
           caption = "",
           x = "Date", y = "Minutes",
           tag = "#16")
  })
  
  output$plot17 <-renderPlot({
    
    plot_dat17<-filter(
      all_data_10,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat17,
           aes(x=FECHA_, y=NUM, color=TREAT, label=NUM))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Quantity of surveys",
           subtitle = "",
           caption = "",
           x = "Date", y = "Quantity",
           tag = "#17")      
  })
  
  output$plot18 <-renderPlot({
    
    plot_dat18<-filter(
      all_data_10,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat18,
           aes(x=FECHA_, y=DUR, color=TREAT, label=DUR))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Average survey duration",
           subtitle = "",
           caption = "",
           x = "Date", y = "Minutes",
           tag = "#18")
  })
  
  output$plot19 <-renderPlot({
    
    plot_dat19<-filter(
      all_data_11,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat19,
           aes(x=FECHA_, y=NUM, color=GAGE, label=NUM))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Quantity of surveys",
           subtitle = "",
           caption = "",
           x = "Date", y = "Quantity",
           tag = "#19")      
  })
  
  output$plot20 <-renderPlot({
    
    plot_dat20<-filter(
      all_data_11,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat20,
           aes(x=FECHA_, y=DUR, color=GAGE, label=DUR))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Average survey duration",
           subtitle = "",
           caption = "",
           x = "Date", y = "Minutes",
           tag = "#20")
  })
  
  output$plot21 <-renderPlot({
    
    plot_dat21<-filter(
      all_data_12,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat21,
           aes(x=FECHA_, y=NUM, color=TEAM, label=NUM))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Quantity of surveys",
           subtitle = "",
           caption = "",
           x = "Date", y = "Quantity",
           tag = "#21")      
  })
  
  output$plot22 <-renderPlot({
    
    plot_dat22<-filter(
      all_data_12,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat22,
           aes(x=FECHA_, y=DUR, color=TEAM, label=DUR))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+
      labs(title = "Average survey duration",
           subtitle = "",
           caption = "",
           x = "Date", y = "Minutes",
           tag = "#22")
  })
  
  output$plot36 <-renderPlot({

    plot_dat36<-filter(
      all_data_2a,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )

    ggplot(plot_dat36,
           aes(x=FECHA_, y=AUD_M, label=AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title="Availability of audios ",
           subtitle = "",
           caption = "",
           x = "Date", y = "Means",
           tag = "#36")
  })
  
  output$plot37 <-renderPlot({
    
    plot_dat37<-filter(
      all_data_2a,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    ggplot(plot_dat37,
           aes(x=FECHA_, y=QF_AUD_M, label=QF_AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title= "Supervision of audios ",
           subtitle = "",
           caption = "",
           x = "Date", y = "Means",
           tag = "#37")
  })
  
  output$plot38 <-renderPlot({
    
    plot_dat38<-filter(
      all_data_2a,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    ggplot(plot_dat38,
           aes(x=FECHA_, y=Q_AUD_M, label=Q_AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title= "Quality of audios ",
           subtitle = "",
           caption = "",
           x = "Date", y = "Means",
           tag = "#38")
  })
  
  output$plot24 <-renderPlot({
    
    plot_dat24<-filter(
      all_data_2b,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat24,
           aes(x=FECHA_, y=AUD_M, color= MUN, label=AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title="Availability of audios ",
           subtitle = "",
           caption = "",
           x = "Date", y = "Means",
           tag = "#24")
  })
  
  output$plot25 <-renderPlot({
    
    plot_dat25<-filter(
      all_data_2b,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    ggplot(plot_dat25,
           aes(x=FECHA_, y=QF_AUD_M,color= MUN,  label=QF_AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title= "Supervision of audios ",
           subtitle = "",
           caption = "",
           x = "Date", y = "Means",
           tag = "#25")
  })
  
  output$plot26 <-renderPlot({
    
    plot_dat26<-filter(
      all_data_2b,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    ggplot(plot_dat26,
           aes(x=FECHA_, y=Q_AUD_M, color= MUN, label=Q_AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title= "Quality of audios ",
           subtitle = "", 
           caption = "",
           x = "Date", y = "Means",
           tag = "#26")
  })  

  output$plot27 <-renderPlot({
    
    plot_dat27<-filter(
      all_data_2c,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat27,
           aes(x=FECHA_, y=AUD_M, color= TREAT, label=AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title="Availability of audios ",
           subtitle = "",
           caption = "",
           x = "Date", y = "Means",
           tag = "#27")
  })
  
  output$plot28 <-renderPlot({
    
    plot_dat28<-filter(
      all_data_2c,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    ggplot(plot_dat28,
           aes(x=FECHA_, y=QF_AUD_M,color= TREAT,  label=QF_AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title= "Supervision of audios ",
           subtitle = "",
           caption = "",
           x = "Date", y = "Means",
           tag = "#28")
  })
  
  output$plot29 <-renderPlot({
    
    plot_dat29<-filter(
      all_data_2c,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    ggplot(plot_dat29,
           aes(x=FECHA_, y=Q_AUD_M, color= TREAT, label=Q_AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title= "Quality of audios ",
           subtitle = "", 
           caption = "",
           x = "Date", y = "Means",
           tag = "#29")
  })  
  
  output$plot30 <-renderPlot({
    
    plot_dat30<-filter(
      all_data_2d,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat30,
           aes(x=FECHA_, y=AUD_M, color= GAGE, label=AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title="Availability of audios ",
           subtitle = "",
           caption = "",
           x = "Date", y = "Means",
           tag = "#30")
  })
  
  output$plot31 <-renderPlot({
    
    plot_dat31<-filter(
      all_data_2d,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    ggplot(plot_dat31,
           aes(x=FECHA_, y=QF_AUD_M,color= GAGE,  label=QF_AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title= "Supervision of audios ",
           subtitle = "",
           caption = "",
           x = "Date", y = "Means",
           tag = "#31")
  })
  
  output$plot32 <-renderPlot({
    
    plot_dat32<-filter(
      all_data_2d,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    ggplot(plot_dat32,
           aes(x=FECHA_, y=Q_AUD_M, color= GAGE, label=Q_AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title= "Quality of audios ",
           subtitle = "", 
           caption = "",
           x = "Date", y = "Means",
           tag = "#32")
  })  
  
  
  output$plot33 <-renderPlot({
    
    plot_dat33<-filter(
      all_data_2e,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    
    ggplot(plot_dat33,
           aes(x=FECHA_, y=AUD_M, color= TEAM, label=AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title="Availability of audios ",
           subtitle = "",
           caption = "",
           x = "Date", y = "Means",
           tag = "#33")
  })
  
  output$plot34 <-renderPlot({
    
    plot_dat34<-filter(
      all_data_2e,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    ggplot(plot_dat34,
           aes(x=FECHA_, y=QF_AUD_M,color= TEAM,  label=QF_AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title= "Supervision of audios ",
           subtitle = "",
           caption = "",
           x = "Date", y = "Means",
           tag = "#34")
  })
  
  output$plot35 <-renderPlot({
    
    plot_dat35<-filter(
      all_data_2e,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    ggplot(plot_dat35,
           aes(x=FECHA_, y=Q_AUD_M, color= TEAM, label=Q_AUD_M))+
      geom_line(linetype = "dotted")+
      geom_point(size=2)+ylim(0,100)+
      labs(title= "Quality of audios ",
           subtitle = "", 
           caption = "",
           x = "Date", y = "Means",
           tag = "#35")
  })
  
  #####

output$plot39 <-renderPlot({

  plot_dat39<-filter(
    all_data_4a,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat39,
         aes(x=FECHA_, y=CON_CG_M,  label=CON_CG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Question execution - parent ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag ="#39"
    )
})

output$plot40 <-renderPlot({

  plot_dat40<-filter(
    all_data_4a,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat40,
         aes(x=FECHA_, y=CON_CH_M,  label=CON_CH_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title="Question execution - child ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#40")
})

#
output$plot41 <-renderPlot({

  plot_dat41<-filter(
    all_data_6a,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )

  ggplot(plot_dat41,
         aes(x=FECHA_, y=CALF_M,  label=CALF_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Final performance rating ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#41")
})

output$plot44 <-renderPlot({

  plot_dat44<-filter(
    all_data_6a,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )

  ggplot(plot_dat44,
         aes(x=FECHA_, y=PREG_M,  label=PREG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Attention of the participants",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#44")
})



output$plot42 <-renderPlot({

  plot_dat42<-filter(
    all_data_5a,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )

  ggplot(plot_dat42,
         aes(x=FECHA_, y=USO_CG_M,  label=USO_CG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Use of materials and guidelines - parent ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#42")
})

output$plot43 <-renderPlot({

  plot_dat43<-filter(
    all_data_5a,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )

  ggplot(plot_dat43,
         aes(x=FECHA_, y=USO_CH_M,  label=USO_CH_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Use of materials and guidelines - child ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#43")
})

###

output$plot45 <-renderPlot({
  
  plot_dat45<-filter(
    all_data_4b,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat45,
         aes(x=FECHA_, y=CON_CG_M, color=MUN , label=CON_CG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Question execution - parent ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag ="#45"
    )
})

output$plot46 <-renderPlot({
  
  plot_dat46<-filter(
    all_data_4b,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat46,
         aes(x=FECHA_, y=CON_CH_M, color=MUN , label=CON_CH_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Question execution - child ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#46")
})

#
output$plot47 <-renderPlot({
  
  plot_dat47<-filter(
    all_data_6b,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat47,
         aes(x=FECHA_, y=CALF_M, color=MUN , label=CALF_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Final performance rating ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#47")
})

output$plot50 <-renderPlot({
  
  plot_dat50<-filter(
    all_data_6b,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat50,
         aes(x=FECHA_, y=PREG_M, color=MUN , label=PREG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Attention of the participants",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#50")
})


output$plot48 <-renderPlot({
  
  plot_dat48<-filter(
    all_data_5b,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat48,
         aes(x=FECHA_, y=USO_CG_M, color=MUN , label=USO_CG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Use of materials and guidelines - parent ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#48")
})

output$plot49 <-renderPlot({
  
  plot_dat49<-filter(
    all_data_5b,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat49,
         aes(x=FECHA_, y=USO_CH_M, color=MUN , label=USO_CH_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Use of materials and guidelines - child ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#49")
})

####

output$plot51 <-renderPlot({
  
  plot_dat51<-filter(
    all_data_4c,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat51,
         aes(x=FECHA_, y=CON_CG_M, color=TREAT , label=CON_CG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Question execution - parent ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag ="#51")
})

output$plot52 <-renderPlot({
  
  plot_dat52<-filter(
    all_data_4c,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat52,
         aes(x=FECHA_, y=CON_CH_M, color=TREAT , label=CON_CH_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Question execution - child ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#52")
})

#
output$plot53 <-renderPlot({
  
  plot_dat53<-filter(
    all_data_6c,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat53,
         aes(x=FECHA_, y=CALF_M, color=TREAT , label=CALF_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Final performance rating ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#53")
})

output$plot56 <-renderPlot({
  
  plot_dat56<-filter(
    all_data_6c,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat56,
         aes(x=FECHA_, y=PREG_M, color=TREAT , label=PREG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Attention of the participants",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#56")
})


output$plot54 <-renderPlot({
  
  plot_dat54<-filter(
    all_data_5c,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat54,
         aes(x=FECHA_, y=USO_CG_M, color=TREAT , label=USO_CG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Use of materials and guidelines - parent ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#54")
})

output$plot55 <-renderPlot({
  
  plot_dat55<-filter(
    all_data_5c,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat55,
         aes(x=FECHA_, y=USO_CH_M, color=TREAT , label=USO_CH_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Use of materials and guidelines - child ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#55")
})

# ###

####

output$plot57 <-renderPlot({
  
  plot_dat57<-filter(
    all_data_4d,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat57,
         aes(x=FECHA_, y=CON_CG_M, color=GAGE , label=CON_CG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Question execution - parent ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag ="#57")
})

output$plot58 <-renderPlot({
  
  plot_dat58<-filter(
    all_data_4d,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat58,
         aes(x=FECHA_, y=CON_CH_M, color=GAGE , label=CON_CH_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Question execution - child ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#58")
})

#
output$plot59 <-renderPlot({
  
  plot_dat59<-filter(
    all_data_6d,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat59,
         aes(x=FECHA_, y=CALF_M, color=GAGE , label=CALF_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Final performance rating ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#59")
})

output$plot62 <-renderPlot({
  
  plot_dat62<-filter(
    all_data_6d,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat62,
         aes(x=FECHA_, y=PREG_M, color=GAGE , label=PREG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Attention of the participants",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "62#62")
})


output$plot60 <-renderPlot({
  
  plot_dat60<-filter(
    all_data_5d,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat60,
         aes(x=FECHA_, y=USO_CG_M, color=GAGE , label=USO_CG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Use of materials and guidelines - parent ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#60")
})

output$plot61 <-renderPlot({
  
  plot_dat61<-filter(
    all_data_5d,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat61,
         aes(x=FECHA_, y=USO_CH_M, color=GAGE , label=USO_CH_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Use of materials and guidelines - child ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#61")
})

###

output$plot63 <-renderPlot({
  
  plot_dat63<-filter(
    all_data_4e,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat63,
         aes(x=FECHA_, y=CON_CG_M, color=TEAM , label=CON_CG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Question execution - parent ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag ="#63")
})

output$plot64 <-renderPlot({
  
  plot_dat64<-filter(
    all_data_4e,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat64,
         aes(x=FECHA_, y=CON_CH_M, color=TEAM , label=CON_CH_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Question execution - child ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#64")
})

#
output$plot65 <-renderPlot({
  
  plot_dat65<-filter(
    all_data_6e,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat65,
         aes(x=FECHA_, y=CALF_M, color=TEAM , label=CALF_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Final performance rating ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#65")
})

output$plot68 <-renderPlot({
  
  plot_dat68<-filter(
    all_data_6e,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat68,
         aes(x=FECHA_, y=PREG_M, color=TEAM , label=PREG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Attention of the participants",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#68")
})


output$plot66 <-renderPlot({
  
  plot_dat66<-filter(
    all_data_5e,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat66,
         aes(x=FECHA_, y=USO_CG_M, color=TEAM , label=USO_CG_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Use of materials and guidelines - parent ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#66")
})

output$plot67 <-renderPlot({
  
  plot_dat67<-filter(
    all_data_5e,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat67,
         aes(x=FECHA_, y=USO_CH_M, color=TEAM , label=USO_CH_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Use of materials and guidelines - child ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#67")
})

######

output$plot69 <-renderPlot({

  plot_dat69<-filter(
    all_data_13a,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )

  ggplot(plot_dat69,
         aes(x=FECHA_, y= TRANS_M, label= TRANS_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title="Accurate transcription",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#69")
})

output$plot70 <-renderPlot({

  plot_dat70<-filter(
    all_data_13a,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat70,
         aes(x=FECHA_, y= SIM_M, label= SIM_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Classification of open answer ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#70")
})


output$plot73 <-renderPlot({

  plot_dat73<-filter(
    all_data_13b,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )

  ggplot(plot_dat73,
         aes(x=FECHA_, y= TRANS_M, color= MUN, label= TRANS_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title="Accurate transcription",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#73")
})

output$plot74 <-renderPlot({

  plot_dat74<-filter(
    all_data_13b,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat74,
         aes(x=FECHA_, y= SIM_M,color= MUN,  label= SIM_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Classification of open answer ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#74")
})


output$plot75 <-renderPlot({

  plot_dat75<-filter(
    all_data_13c,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )

  ggplot(plot_dat75,
         aes(x=FECHA_, y= TRANS_M, color= TREAT, label= TRANS_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title="Accurate transcription",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#75")
})

output$plot76 <-renderPlot({

  plot_dat76<-filter(
    all_data_13c,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat76,
         aes(x=FECHA_, y= SIM_M,color= TREAT,  label= SIM_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Classification of open answer ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#76")
})


output$plot77 <-renderPlot({

  plot_dat77<-filter(
    all_data_13d,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )

  ggplot(plot_dat77,
         aes(x=FECHA_, y= TRANS_M, color= GAGE, label= TRANS_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title="Accurate transcription",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#77")
})

output$plot78 <-renderPlot({

  plot_dat78<-filter(
    all_data_13d,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat78,
         aes(x=FECHA_, y= SIM_M,color= GAGE,  label= SIM_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Classification of open answer ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#78")
})


output$plot79 <-renderPlot({

  plot_dat79<-filter(
    all_data_13e,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )

  ggplot(plot_dat79,
         aes(x=FECHA_, y= TRANS_M, color= TEAM, label= TRANS_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title="Accurate transcription",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#79")
})

output$plot80 <-renderPlot({

  plot_dat80<-filter(
    all_data_13e,
    FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  ggplot(plot_dat80,
         aes(x=FECHA_, y= SIM_M,color= TEAM,  label= SIM_M))+
    geom_line(linetype = "dotted")+
    geom_point(size=2)+ylim(0,100)+
    labs(title= "Classification of open answer ",
         subtitle = "",
         caption = "",
         x = "Date", y = "Means",
         tag = "#80")
})

output$plot81 <-renderPlot({
  
  plot_dat81<-filter(
    all_data_13,
    TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat81,
         aes(x="", y=N, fill=TRANS_))+
    geom_bar(stat="identity")+ 
    coord_polar("y", start=0)+
    scale_fill_brewer(palette="Greens")+
    theme_minimal()+
    labs(title = "Accurate transcription",
         subtitle = "",
         caption = "",
         x = "", y = "",
         tag = "#81")      
})

output$plot82 <-renderPlot({
  
  plot_dat82<-filter(
    all_data_13,
    TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  
  ggplot(plot_dat82,
         aes(x="", y=N, fill=SIM_))+
    geom_bar(stat="identity")+ 
    coord_polar("y", start=0)+
    scale_fill_brewer(palette="Greens")+
    theme_minimal()+
    labs(title = "Classification of open answer",
         subtitle = "",
         caption = "",
         x = "", y = "",
         tag = "#82")      
})



output$table1 <- renderTable({

  table_data1<-filter(
    all_data_2,
    TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
  )
  table_data1 <- table_data1%>%select(AUD, QF_AUD, Q_AUD)%>%
    mutate(AUD_M = mean(AUD, na.rm=TRUE), QF_AUD_M = mean(QF_AUD, na.rm=TRUE), Q_AUD_M = mean(Q_AUD, na.rm=TRUE), AUD_CV = CV(AUD), QF_AUD_CV = CV(QF_AUD),Q_AUD_CV = CV(Q_AUD) )
  head(table_data1, n=1)
})


  output$table2 <- renderTable({

    table_data2<-filter(
      all_data_4,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    table_data2 <- table_data2%>%select( CON_CG, CON_CH)%>%
      mutate(CON_CG_M = mean(CON_CG, na.rm=TRUE), CON_CH_M = mean(CON_CH, na.rm=TRUE), CON_CG_CV = CV(CON_CG), CON_CH_CV = CV(CON_CH))
    head(table_data2, n=1)
  })

  output$table3 <- renderTable({

    table_data3<-filter(
      all_data_5,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    table_data3 <- table_data3%>%select( USO_CG, USO_CH)%>%
      mutate(USO_CG_M = mean(USO_CG, na.rm=TRUE), USO_CH_M = mean(USO_CH, na.rm=TRUE), USO_CG_CV = CV(USO_CG), USO_CH_CV = CV(USO_CH))
    head(table_data3, n=1)
  })

  output$table4 <- renderTable({

    table_data4<-filter(
      all_data_6,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    table_data4 <- table_data4%>%select( PREG, CALF)%>%
      mutate(CALF_M = mean(CALF, na.rm=TRUE), PREG_M = mean(PREG, na.rm=TRUE), CALF_CV = CV(CALF), PREG_CV = CV(PREG))
    head(table_data4, n=1)
  })
  
  output$table5 <- renderTable({
    
    table_data5<-filter(
      all_data_13,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    table_data5 <- table_data5%>%select( TRANS, SIM)%>%
      mutate(TRANS_M = mean(TRANS, na.rm=TRUE), SIM_M = mean(SIM, na.rm=TRUE), TRANS_CV = CV(TRANS), SIM_CV = CV(SIM))
    head(table_data5, n=1)
  })
  
  output$table6 <- renderTable({

    table_data6<-filter(
      all_data_1,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    table_data6_1 <- table_data6%>%group_by(AUDITOR)%>%select(NUM, DUR)%>%
      mutate(NUM_M = sum(NUM, na.rm=TRUE), DUR_M = mean(DUR, na.rm=TRUE))
    table_data6_1_1 <- table_data6_1%>%group_by(AUDITOR)%>% summarise(NUM_P = first(NUM_M), DUR_P=first(DUR_M))
    table_data6 <- table_data6_1_1[order(table_data6_1_1$NUM_P), ]
    table_data6
  })
 
  output$table7 <- renderTable({

    table_data7<-filter(
      all_data_14,
      FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    table_data7_1<- table_data7%>%group_by(ENCUESTADOR)%>%select(CON, LENG, U_AD, U_N, PREG, IND)%>%
      mutate(CONS_M = mean(CON, na.rm=TRUE), LENG_M= mean(LENG, na.rm=TRUE), U_AD_M = mean(U_AD, na.rm=TRUE), U_N_M = mean(U_N, na.rm=TRUE), PREG_M = mean(PREG, na.rm=TRUE), IND_M = mean(IND, na.rm=TRUE))
    table_data7_1_1 <- table_data7_1%>%group_by(ENCUESTADOR)%>% summarise(IND_P = first(IND_M), CONS_P=first(LENG_M), LENG_P=first(LENG_M), U_AD_P=first(U_AD_M), U_N_P=first(U_N_M), PREG_P=first(PREG_M) )
    table_data7 <- table_data7_1_1[order(table_data7_1_1$IND_P), ]
    table_data7
  })

  output$table8 <- renderTable({
    
    table_data8<-filter(
      all_data_1,
      TEAM %in% input$teams_input & GAGE %in% input$age_input & TREAT %in% input$group_input & MUN %in% input$town_input & FECHA_>=input$date_input[1] & FECHA_<=input$date_input[2]
    )
    table_data8_1 <- table_data8%>%select(NUM, DUR)%>%mutate(NUM_M = sum(NUM, na.rm=TRUE), DUR_M = mean(DUR, na.rm=TRUE))
    table_data8 <- table_data8_1%>% summarise(NUM_P = first(NUM_M), DUR_P=first(DUR_M))
    table_data8
  })
  
}

#cons_M, leng_M, var_0_M, var_0_n_M, var_p_M, prom_2_M

#cons_M = mean(cons, na.rm=TRUE),leng_M = mean(leng, na.rm=TRUE), var_0_M = mean(var_0, na.rm=TRUE), var_0_n_M = mean(var_0_n, na.rm=TRUE), var_p_M = mean(var_p, na.rm=TRUE), prom_2_M = mean(prom_2, na.rm=TRUE) 

shinyApp(ui,server)

#mejoras del codigo:

#cuando se cambia a numeric, usar loop para todas las variables de la base
#clist<-c("AUD", "Q_AUD", "QF_AUD")

# for (i in clist){
#   all_data_2$i_ <- as.factor(as.numeric(all_data_2$i))
# }
