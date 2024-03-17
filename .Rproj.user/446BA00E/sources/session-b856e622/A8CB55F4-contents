#Skrypt

library(tidyverse)
library(psych)
library(effsize)
library(EnvStats)
library(lme4)
library(lmerTest)
library(emmeans)
library(datasets)
library(broom)

data <- read.csv("Badanie_csv.csv")

#CZYSZCZENIE BAZY

data_r <- data%>%
  mutate(LBQ_2R = 6-LBQ_2,
         LBQ_4R = 6-LBQ_4,
         LBQ_6R = 6-LBQ_6,
         LBQ_8R = 6-LBQ_8,
         LBQ_9R = 6-LBQ_9,
         LBQ_11R = 6-LBQ_11,
         LBQ_14R = 6-LBQ_14,
         LBQ_16R = 6-LBQ_16,
         LBQ_18R = 6-LBQ_18,
         LBQ_22R = 6-LBQ_22,
         LBQ_23R = 6-LBQ_23,
         LBQ_24R = 6-LBQ_24)
obliczone <- data_r %>%
  mutate(CIPS_O = CIPS_1+CIPS_2+CIPS_3+CIPS_4+CIPS_5+CIPS_6+CIPS_7+CIPS_8+CIPS_9+CIPS_10+CIPS_11+CIPS_12+CIPS_13+CIPS_14+CIPS_15+CIPS_16+CIPS_17+CIPS_18+CIPS_19+CIPS_20,
         FMPS_O = FMPS_1+FMPS_2+FMPS_3+FMPS_4+FMPS_5+FMPS_6+FMPS_7+FMPS_8+FMPS_9+FMPS_10+FMPS_11+FMPS_12+FMPS_13+FMPS_14+FMPS_15+FMPS_16+FMPS_17+FMPS_18+FMPS_19+FMPS_20+FMPS_21+FMPS_22+FMPS_23+FMPS_24+FMPS_25+FMPS_26+FMPS_27+FMPS_28+FMPS_29,
         FMPS_PS = FMPS_3+FMPS_5+FMPS_9+FMPS_13+FMPS_16+FMPS_21+FMPS_25,
         FMPS_COM = FMPS_6+FMPS_7+FMPS_10+FMPS_11+FMPS_15+FMPS_18+FMPS_20+FMPS_22+FMPS_28,
         FMPS_DAA = FMPS_14+FMPS_24+FMPS_26+FMPS_27,
         FMPS_PE = FMPS_1+FMPS_8+FMPS_12+FMPS_17+FMPS_23,
         FMPS_PC = FMPS_2+FMPS_4+FMPS_19+FMPS_29,
         LBQ_O=LBQ_1+LBQ_2R+LBQ_3+LBQ_4R+LBQ_5+LBQ_6R+LBQ_7+LBQ_8R+LBQ_9R+LBQ_10+LBQ_11R+LBQ_12+LBQ_13+LBQ_14R+LBQ_15+LBQ_16R+LBQ_17+LBQ_18R+LBQ_19+LBQ_20+LBQ_21+LBQ_22R+LBQ_23R+LBQ_24R,
         LBQ_WPF=LBQ_1+LBQ_7+LBQ_10+LBQ_16R+LBQ_22R+LBQ_24R,
         LBQ_BZR=LBQ_3+LBQ_5+LBQ_12+LBQ_14R+LBQ_18R+LBQ_23R,
         LBQ_PBS=LBQ_2R+LBQ_8R+LBQ_11R+LBQ_13+LBQ_17+LBQ_20,
         LBQ_R=LBQ_4R+LBQ_6R+LBQ_9R+LBQ_15+LBQ_19+LBQ_21)
df <- obliczone %>%
  select(Sex, Age, Home, Degree, School, Position, Break, Retirement, CIPS_O, FMPS_O, FMPS_COM, FMPS_PS, FMPS_DAA,FMPS_PE,FMPS_PC, LBQ_O, LBQ_WPF, LBQ_BZR, LBQ_PBS, LBQ_R) %>%
  filter(Break == "Nie" & Retirement == "Nie")

#STAT OPISOWE

mean_age <- mean(df$Age)
sd_age <- sd(df$Age)
statop_sex <- df %>%
  group_by(Sex) %>%
  summarise(wiek=mean(Age), SD=sd(Age), ogolny_cips=mean(CIPS_O), ogolny_fmps=mean(FMPS_O), ogolny_lbq=mean(LBQ_O), N=n())
statop_school <- df %>%
  group_by(School) %>%
  summarise(wiek=mean(Age), SD=sd(Age), ogolny_cips=mean(CIPS_O), ogolny_fmps=mean(FMPS_O), ogolny_lbq=mean(LBQ_O), N=n())
