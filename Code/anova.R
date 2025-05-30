install.packages("effectsize")
library(effectsize)
library(pwr)
library(sjstats)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ez)


# Set folder and Input data
main.folder <- "H:/BifactorNonNormality-PsychPC2"
data.folder <- paste0(main.folder,"/Data/")
result.folder <- paste0(main.folder,"/Result/")



theta.mirt.wide <- read.csv(paste0(result.folder,"Person_Para_mirt_72conditions.csv"),header = TRUE)

theta.mirt.wide <- read.csv(paste0(result.folder,"theta_mirt_wide.csv"),header = TRUE)

i <- "thetaG.mirt.rmse"


df <- theta.mirt.wide%>%
  mutate(dv = !!sym(i)) 

# Assuming 'Factor', 'I', 'N', 'Fg', 'Fs', and 'MAP' are your variables
df$Factor <- as.factor(df$Factor)
df$I <- as.factor(df$I)
df$N <- as.factor(df$N)
df$Fg <- as.factor(df$Fg)
df$Fs <- as.factor(df$Fs)
df$MAP <- as.factor(df$MAP)
df$id <- as.factor(df$id)
df$id.rep <- as.factor(df$id.rep)

result <- ezANOVA(data = df, dv = dv, wid = id, within = MAP, between = .(Factor, I, N, Fg, Fs), type = 3, detailed = TRUE)


result <- ezANOVA(data = df, dv = dv, wid = id.rep, within = MAP, between = .(Factor, I, N, Fg, Fs), type = 3, detailed = TRUE)
# 
# memory.limit(size=25000)

anova_model <- aov(dv ~ Factor + I+ N+ Fg+ Fs + Error(id.rep/MAP), data = df)

anova_model <- aov(dv ~ Factor + I+ N+ Fg+ Fs + Error(id/MAP), data = df)


if (!requireNamespace("lme4", quietly = TRUE)) install.packages("lme4")
library(Matrix)
library(lme4)

lmer_model <- lmer(dv ~ Factor + I + N + Fg + Fs + (1|id.rep) + (1|id.rep:MAP), data = df)
lmer_model <- lmer(dv ~ Factor + I + N + Fg + Fs + (1|id.rep), data = df)





anova_summary <- summary(anova_model)
# Extracting the Sum Sq for each effect and for residuals from the summary

effect_table <- anova_summary[[1]]
error_table<- anova_summary[[2]]


SS_effect <- effect_table[[1]][["Sum Sq"]]
SS_error <- error_table[[1]][["Sum Sq"]]  # Adjust the index if necessary
# Manually calculate the total sum of squares
SS_total <- SS_effect + SS_error
# Calculate traditional eta squared
eta2 <- SS_effect / SS_total

