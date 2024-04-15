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
library(ggpubr)
library(rstatix)
library(gridExtra)
library(multcompView)

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
  select(Participant, Sex, Age, Home, Degree, School, Position, Break, Retirement, CIPS_O, FMPS_O, FMPS_COM, FMPS_PS, FMPS_DAA,FMPS_PE,FMPS_PC, LBQ_O, LBQ_WPF, LBQ_BZR, LBQ_PBS, LBQ_R) %>%
  filter(Break == "Nie" & Retirement == "Nie")

#STAT OPISOWE

mean_age <- mean(df$Age)
sd_age <- sd(df$Age)
statop_sex <- df %>%
  group_by(Sex) %>%
  summarise(wiek=mean(Age), SD=sd(Age), ogolny_cips=mean(CIPS_O), ogolny_fmps=mean(FMPS_O), ogolny_lbq=mean(LBQ_O), N=n())
statop_school <- df %>%
  group_by(School) %>%
summarise(wiek=mean(Age), SD=sd(Age), M_CIPS=mean(CIPS_O), SD_CIPS=sd(CIPS_O), V_CIPS=((SD_CIPS/M_CIPS)*100), A_S_CIPS=skewness(CIPS_O), M_FMPS=mean(FMPS_O), SD_FMPS=sd(FMPS_O), V_FMPS=((SD_FMPS/M_FMPS)*100),A_S_FMPS=skewness(FMPS_O), M_LBQ=mean(LBQ_O), SD_LBQ=sd(LBQ_O), V_LBQ=((SD_LBQ/M_LBQ)*100),A_S_LBQ=skewness(LBQ_O), N=n())

summarise(M_rt=mean(Time), SD_rt=sd(Time), V_x=((SD_rt/M_rt)*100), A_Sx=skewness(Time))


df1 <- df %>%
  group_by(School)

shapiro.test(statop_school$ogolny_cips)
shapiro.test(statop_school$ogolny_fmps)
shapiro.test(statop_school$ogolny_lbq)

wykres1 <- ggplot(data=df1) +
  geom_boxplot(aes(x = School, y=CIPS_O))
wykres2 <- ggplot(data=df1) +
  geom_boxplot(aes(x = School, y=FMPS_O))
wykres3 <- ggplot(data=df1) +
  geom_boxplot(aes(x = School, y=LBQ_O))

CIPS_AOV <- anova(lm(CIPS_O ~ School, data = df1))
CIPS_AOV
FMPS_AOV <- anova(lm(FMPS_O ~ School, data = df1))
FMPS_AOV
LBQ_AOV <- anova(lm(LBQ_O ~ School, data = df1))
LBQ_AOV
res.aov <- aov(LBQ_O ~ School, data=df1)
summary(res.aov)

res.cips <- aov(CIPS_O ~ School, data=df1)

Tukeytest <- TukeyHSD(res.cips)

plot(Tukeytest)

ggplot(data=df1, mapping = aes(x=School, y=CIPS_O)) +
     geom_boxplot()+
  geom_jitter()

ggscatter(df1, x="School", y="CIPS_O", add="reg.line")

ggscatter(
  df1, x = "CIPS_O", y = "FMPS_O",
  color = "School", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = School)
  )

ggscatter(
  df1, x = "FMPS_O", y = "CIPS_O",
  add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"))
  )

 ggscatter(
  df1, x = "FMPS_O", y = "LBQ_O",
  add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"))
  )
ggscatter(
  df1, x = "CIPS_O", y = "LBQ_O",
  add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"))
  )

ggscatter(
  df1, x = "CIPS_O", y = "LBQ_O",
  color = "School", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = School)
  )
ggscatter(
  df1, x = "FMPS_O", y = "LBQ_O",
  color = "School", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = School)
  )

df2 <- df %>% group_by(School) %>% get_summary_stats(FMPS_O)

anova_test(data = df, formula = CIPS_O ~ FMPS_O + School, type = 3)
anova_test(data = df, formula = FMPS_O ~ CIPS_O + School, type = 3)
anova_test(data = df, formula = LBQ_O ~ FMPS_O + School, type = 3)
anova_test(data = df, formula = CIPS_O ~ LBQ_O + School, type = 3)
anova_test(data = df, formula = LBQ_O ~ CIPS_O + School, type = 3)
anova_test(data = df, formula = FMPS_O ~ LBQ_O  + School, type = 3)

manova_result <- manova(cbind(LBQ_O, CIPS_O, FMPS_O) ~ School, data = df)
print(summary(manova_result))

cor(x=df1$LBQ_O, y=df1$CIPS_O, method = "pearson")
cor_test<-cor.test(x=df1$LBQ_O, y=df1$CIPS_O, method = "pearson")

ggplot(df1, mapping = aes(x=CIPS_O, y=LBQ_O))+
  geom_jitter()+
  geom_smooth(method = "lm")
fit.lbq <- lm(CIPS_O ~ LBQ_O, df1)
summary(fit.lbq)
plot(fit.lbq)

cohen.d(df1$CIPS_O, df1$LBQ_O)

t.test()

cor_test<-cor.test(x=df1$LBQ_O, y=df1$FMPS_O, method = "pearson")

ggplot(df1, mapping = aes(x=FMPS_O, y=LBQ_O))+
  geom_jitter()+
  geom_smooth(method = "lm")
fit.fmpslbq <- lm(FMPS_O ~ LBQ_O, df1)
summary(fit.fmpslbq)
plot(fit.fmpslbq)

cohen.d(df1$FMPS_O, df1$LBQ_O)

covariance <- cov(df1$FMPS_O, df1$CIPS_O)

cor.test(df1$FMPS_O, df1$CIPS_O)
ggplot(df1, mapping = aes(x=FMPS_O, y=CIPS_O))+
  geom_jitter()+
  geom_smooth(method = "lm")
fit.fmpscips <- lm(FMPS_O ~ CIPS_O, df1)
summary(fit.fmpscips)
plot(fit.fmpscips)
Kowariancja <- (cor_test$estimate*sd(df1$CIPS_O)*sd(df1$FMPS_O))

#HIST

hist(df1$CIPS_O)
hist(df1$FMPS_O)
hist(df1$LBQ_O)
shapiro.test(df1$CIPS_O)
shapiro.test(df1$FMPS_O)
shapiro.test(df1$LBQ_O)

p1 <- ggplot(data=df1, aes(x=CIPS_O)) +
  geom_histogram(binwidth = 5, fill="lightpink", color="black") +
  geom_vline(aes(xintercept=mean(CIPS_O)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Histogram of CIPS score", x="CIPS score", y="Frequency")
p2 <- ggplot(data=df1, aes(x=FMPS_O)) +
  geom_histogram(binwidth = 5, fill="lightpink", color="black") +
  geom_vline(aes(xintercept=mean(FMPS_O)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Histogram of FMPS score", x="FMPS score", y="Frequency")
p3 <- ggplot(data=df1, aes(x=LBQ_O)) +
  geom_histogram(binwidth = 5, fill="lightpink", color="black") +
  geom_vline(aes(xintercept=mean(LBQ_O)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Histogram of LBQ score", x="LBQ score", y="Frequency")

combined_hist <- grid.arrange(p1, p2, p3, ncol=3)
