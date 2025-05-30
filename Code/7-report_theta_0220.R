#Merge convergence status and parameters
#para.theta.all <- left_join(para.data.all, files.all ,by="id.rep")

# install.packages("e1071")
#rm(list=ls())
library(tidyr)
library(dplyr)
library(readr)
library(parallel)
library(mirt)
library(stringr)
library(ggplot2)
library(e1071)

##### Calculate Bias and Rmse

######Infinite Number in MAP:


# Set folder and Input data
main.folder <- "H:/BifactorNonNormality-PsychPC2"
data.folder <- paste0(main.folder,"/Data/")
result.folder <- paste0(main.folder,"/Result/")
para.theta.all <- read.csv(paste0(result.folder,"thetas.csv"),header = TRUE)
#para.theta.all <- theta.data_file.all

#summary(para.theta.all)



#Filter nonconvergenced and infinite. Change formate
para.theta.filtered <- para.theta.all%>%
  #filter(mirt..bfconvergence != 0)%>%
  filter(!is.infinite(thetaG.mirt.map))%>%
  filter(!is.infinite(thetaS.avg.mirt.map))%>%
  filter(!is.infinite(thetaG.mirt.ml))%>%
  filter(!is.infinite(thetaS.avg.mirt.ml))


# t <- para.theta.all%>%
#   filter(id.rep == "4-10-1000-Skew(0)Kurt(0)-Skew(0)Kurt(0)-285")


#Calculate bias and rmse of MAP
#Calculate recovery coefficients for each replication under each condition 
theta.mirt.MAP.report <- para.theta.filtered %>%
  summarize(
    thetaG.mirt.map.bias = mean(as.numeric(thetaG.mirt.map) - as.numeric(thetaG.real)),
    thetaS.mirt.map.bias = mean(as.numeric(thetaS.avg.mirt.map) - as.numeric(thetaS.avg.real)),
    thetaG.mirt.map.rmse = sqrt(mean((as.numeric(thetaG.mirt.map) - as.numeric(thetaG.real))^2)),
    thetaS.mirt.map.rmse = sqrt(mean((as.numeric(thetaS.avg.mirt.map) - as.numeric(thetaS.avg.real))^2)),
    thetaG.mirt.map.cor = if (sd(thetaG.mirt.map) != 0 & sd(thetaG.real) != 0) cor(thetaG.mirt.map, thetaG.real) else NA,
    thetaS.mirt.map.cor = if (sd(thetaS.avg.mirt.map) != 0 & sd(thetaS.avg.real) != 0) cor(thetaS.avg.mirt.map, thetaS.avg.real) else NA,
    .by = c(id.rep, Factor, I, N, Fg, Fs)
          )
  # mutate(thetaG.mirt.map.bias.adjusted = thetaG.mirt.map.bias/(Factor*I),
  #        thetaS.mirt.map.bias.adjusted = thetaS.mirt.map.bias/(Factor*I),
  #        thetaG.mirt.map.rmse.adjusted = thetaG.mirt.map.rmse/(Factor*I),
  #        thetaS.mirt.map.rmse.adjusted = thetaS.mirt.map.rmse/(Factor*I)
  #        )



#summary(para.theta.filtered)


#Calculate bias and rmse of ML
#Calculate recovery coefficients for each replication under each condition 
theta.mirt.ML.report <- para.theta.filtered%>%
  summarize(
    thetaG.mirt.ml.bias = mean(as.numeric(thetaG.mirt.ml) - as.numeric(thetaG.real)),
    thetaS.mirt.ml.bias = mean(as.numeric(thetaS.avg.mirt.ml) - as.numeric(thetaS.avg.real)),
    thetaG.mirt.ml.rmse = sqrt(mean((as.numeric(thetaG.mirt.ml) - as.numeric(thetaG.real))^2)),
    thetaS.mirt.ml.rmse = sqrt(mean((as.numeric(thetaS.avg.mirt.ml) - as.numeric(thetaS.avg.real))^2)),
    thetaG.mirt.ml.cor = if (sd(thetaG.mirt.ml) != 0 & sd(thetaG.real) != 0) cor(thetaG.mirt.ml, thetaG.real) else NA,
    thetaS.mirt.ml.cor = if (sd(thetaS.avg.mirt.ml) != 0 & sd(thetaS.avg.real) != 0) cor(thetaS.avg.mirt.ml, thetaS.avg.real) else NA,
    .by = c(id.rep, Factor, I, N, Fg, Fs)
    #skewness and kurtosis
    # thetaG.real.skew = e1071::skewness(thetaG.real),
    # thetaG.real.kurt = e1071::kurtosis(thetaG.real),   
    # thetaS.real.skew = e1071::skewness(thetaS.avg.real),
    # thetaS.real.kurt = e1071::kurtosis(thetaS.avg.real),
    # thetaG.mirt.ml.skew = e1071::skewness(thetaG.mirt.ml),
    # thetaG.mirt.ml.kurt = e1071::kurtosis(thetaG.mirt.ml), 
    # thetaS.mirt.ml.skew = e1071::skewness(thetaS.avg.mirt.ml),
    # thetaS.mirt.ml.kurt = e1071::kurtosis(thetaS.avg.mirt.ml) 
          )
  # mutate(
  #   thetaG.mirt.ml.bias.adjusted = thetaG.mirt.ml.bias/(Factor*I),
  #   thetaS.mirt.ml.bias.adjusted = thetaS.mirt.ml.bias/(Factor*I),
  #   thetaG.mirt.ml.rmse.adjusted = thetaG.mirt.ml.rmse/(Factor*I),
  #   thetaS.mirt.ml.rmse.adjusted = thetaS.mirt.ml.rmse/(Factor*I)
  # )



##### Merge all Bias and Rmse into a longer table
theta.mirt.MAP.report.rename <- theta.mirt.MAP.report%>%
  rename(thetaG.mirt.bias = thetaG.mirt.map.bias,
         thetaS.mirt.bias = thetaS.mirt.map.bias,
         thetaG.mirt.rmse = thetaG.mirt.map.rmse,
         thetaS.mirt.rmse = thetaS.mirt.map.rmse,
         thetaG.mirt.cor = thetaG.mirt.map.cor,
         thetaS.mirt.cor = thetaS.mirt.map.cor
         # thetaG.mirt.bias.adjusted = thetaG.mirt.map.bias.adjusted,
         # thetaS.mirt.bias.adjusted = thetaS.mirt.map.bias.adjusted,
         # thetaG.mirt.rmse.adjusted = thetaG.mirt.map.rmse.adjusted,
         # thetaS.mirt.rmse.adjusted = thetaS.mirt.map.rmse.adjusted
         # thetaG.real.skew = thetaG.real.skew,
         # thetaG.real.kurt = thetaG.real.kurt,
         # thetaS.real.skew = thetaS.real.skew,
         # thetaS.real.kurt = thetaS.real.kurt,
         # thetaG.mirt.skew = thetaG.mirt.map.skew,
         # thetaG.mirt.kurt = thetaG.mirt.map.kurt,
         # thetaS.mirt.skew = thetaS.mirt.map.skew,
         # thetaS.mirt.kurt = thetaS.mirt.map.kurt
         )%>%
  mutate(MAP=as.factor(1))



theta.mirt.ML.report.rename <- theta.mirt.ML.report%>%
  rename(thetaG.mirt.bias = thetaG.mirt.ml.bias,
         thetaS.mirt.bias = thetaS.mirt.ml.bias,
         thetaG.mirt.rmse = thetaG.mirt.ml.rmse,
         thetaS.mirt.rmse = thetaS.mirt.ml.rmse,
         thetaG.mirt.cor = thetaG.mirt.ml.cor,
         thetaS.mirt.cor = thetaS.mirt.ml.cor
         # thetaG.mirt.bias.adjusted = thetaG.mirt.ml.bias.adjusted,
         # thetaS.mirt.bias.adjusted = thetaS.mirt.ml.bias.adjusted,
         # thetaG.mirt.rmse.adjusted = thetaG.mirt.ml.rmse.adjusted,
         # thetaS.mirt.rmse.adjusted = thetaS.mirt.ml.rmse.adjusted
         # thetaG.real.skew = thetaG.real.skew,
         # thetaG.real.kurt = thetaG.real.kurt,
         # thetaS.real.skew = thetaS.real.skew,
         # thetaS.real.kurt = thetaS.real.kurt,
         # thetaG.mirt.skew = thetaG.mirt.ml.skew,
         # thetaG.mirt.kurt = thetaG.mirt.ml.kurt,
         # thetaS.mirt.skew = thetaS.mirt.ml.skew,
         # thetaS.mirt.kurt = thetaS.mirt.ml.kurt
         )%>%
  mutate(MAP=as.factor(0))





# rm(theta.mirt.wide)
theta.mirt.wide <- rbind(theta.mirt.MAP.report.rename, theta.mirt.ML.report.rename)%>%
  mutate(
    Fg.num = Fg,
    Fs.num = Fs,
    Factor.num = Factor,
    I.num = I,
    N.num = N,
    Fg = ifelse(Fg == "Skew(0)Kurt(0)", "Normal  ", "Non-normal"),
    Fs = ifelse(Fs == "Skew(0)Kurt(0)", "Normal  ", "Non-normal"),
    Factor = paste0("Factor:",Factor),
    I = paste0("Item:",I),
    #N = paste0("Sample Size = ",N)
  )%>%
  mutate(Fg = factor(Fg, levels = c("Normal  ", "Non-normal")))%>%
  arrange(Fg)%>% 
  mutate(Fs = factor(Fs, levels = c("Normal  ", "Non-normal")))%>%
  arrange(Fs)%>%    
  mutate(Factor = factor(Factor, levels = c("Factor:2","Factor:4")))%>%
  arrange(Factor)%>%
  mutate(I = factor(I, levels = c("Item:5","Item:10","Item:20")))%>%
  arrange(I)%>%
  #mutate(N = factor(N, levels = c("Sample Size = 250","Sample Size = 500","Sample Size = 1000")))%>%
  mutate(N = factor(N, levels = c("250","500","1000")))%>%
  arrange(N)




#write.csv(theta.mirt.wide,paste0(result.folder,"theta_mirt_wide.csv"))



theta.mirt.long <- theta.mirt.wide %>%
  tidyr::pivot_longer(
    cols = thetaG.mirt.bias : thetaS.mirt.cor,
    # cols = thetaG.mirt.bias : thetaS.mirt.rmse.adjusted,
    names_to = "metrics_name",
    values_to = "value"
  )
#write.csv(theta.mirt.long,paste0(result.folder,"theta_mirt_long.csv"))









### 1. Bias of Theta

###'##########################################################################F1
###'##########################################################################
###'Bias of theta on the general factor (thetaG).MAP

df <- theta.mirt.long %>%
  filter(metrics_name == "thetaG.mirt.bias")%>%
  group_by(Factor,I,N,Fg,Fs,MAP)%>%
  summarise(value=mean(value, na.rm=TRUE))%>%
  mutate(Method=ifelse(MAP==1,"MAP","ML"))

plot <- ggplot(data = df, aes (x = N, y = value, group = Method))+
  geom_line(aes(color = interaction(Fg,Fs),linetype = Method)) +
  
  # geom_line(aes(linetype = Fs)) +
  scale_color_manual(values = c("#000000","#CC0000", "#999999", "#FF6969")) +
  # geom_bar(aes(fill = Method),stat = "identity", position = "dodge") +
  # scale_fill_manual(values = c("#000000", "#99CCFF","#999999","#2272B2")) +
  facet_grid(Fg*Fs~Factor*I)+
  labs(title = "Bias of Personal Ability on General Factor")+
  labs(x = "Sample Size", y = "Bias")+
  theme(axis.text.x=element_text(angle=0, vjust=0,hjust=0.5, size = 10),
        plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, linetype='dashed', col = 'black')

plot
setwd(main.folder)
ggsave("graph/Theta-bias-Fg.jpg", plot = plot)


#2
# df <- theta.mirt.long %>%
#   filter(metrics_name == "thetaG.mirt.bias")%>%
#   # group_by(Factor,I,N,Fg,MAP)%>%
#   # summarise(value=mean(value, na.rm=TRUE))%>%
#   mutate(Method=ifelse(MAP==1,"MAP","ML"))
# 
# plot <- ggplot(data = df, aes (x = interaction(Factor,I), y = value, group = Method))+
#   # geom_line(aes(color = Fg,linetype = Fs)) +
#   # scale_color_manual(values = c("#000000","#CC0000", "#999999", "#FF6969")) +
#   geom_line(aes(linetype = Method)) +
#   
#   # geom_bar(aes(fill = Method),stat = "identity", position = "dodge") +
#   # scale_fill_manual(values = c("#000000", "#99CCFF","#999999","#2272B2")) +
#   facet_grid(N~Fg*Fs)+
#   labs(title = "Bias of Personal Ability on General Factor")+
#   labs(x = "Factor and Item number", y = "Bias")+
#   theme(axis.text.x=element_text(angle=0, vjust=0,hjust=0.5, size = 10),
#         plot.title = element_text(hjust = 0.5))+
#   geom_hline(yintercept = 0, linetype='dashed', col = 'black')
# 
# plot
# # setwd(main.folder)
# # ggsave("graph/Theta-bias-Fg.jpg", plot = plot)


###'##########################################################################
###'##########################################################################




###'##########################################################################F1
###'##########################################################################
###'Bias of theta on the specific factor (thetaS)

df <- theta.mirt.long %>%
  filter(metrics_name == "thetaS.mirt.bias")%>%
  group_by(Factor,I,N,Fg,Fs,MAP)%>%
  summarise(value=mean(value, na.rm=TRUE))%>%
  mutate(Method=ifelse(MAP==1,"MAP","ML"))

#summary(df)

plot <- ggplot(data = df, aes (x = N, y = value, group = Method))+
  geom_line(aes(color = interaction(Fg,Fs),linetype = Method)) +
  scale_color_manual(values = c("#000000","#CC0000", "#999999", "#FF6969")) +
  facet_grid(Fg*Fs~Factor*I)+
  labs(title = "Bias of Personal Ability on Specific Factor")+
  labs(x = "Sample Size", y = "Bias")+
  theme(axis.text.x=element_text(angle=0, vjust=0,hjust=0.5, size = 10),
        plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, linetype='dashed', col = 'black')

plot
#ggsave("graph/Theta-bias-Fs.jpg", plot = plot)
###'##########################################################################
###'##########################################################################



###'##########################################################################F1
###'##########################################################################
###'RMSE of theta on the specific factor (thetaG) MAP and ML

df <- theta.mirt.long %>%
  filter(metrics_name == "thetaG.mirt.rmse")%>%
  group_by(Factor,I,N,Fg,Fs,MAP)%>%
  summarise(value=mean(value, na.rm=TRUE))%>%
  mutate(Method=ifelse(MAP==1,"MAP","ML"))

plot <- ggplot(data = df, aes (x = N, y = value, group = Method))+
  geom_line(aes(color = interaction(Fg,Fs),linetype = Method)) +
  scale_color_manual(values = c("#000000","#CC0000", "#999999", "#FF6969")) +
  facet_grid(Fg*Fs~Factor*I)+
  ggtitle("RMSE of Personal Ability on General Factor")+
  labs(x = "Sample Size", y = "RMSE")+
  theme(axis.text.x=element_text(angle=0, vjust=0,hjust=0.5, size = 10),
        plot.title = element_text(hjust = 0.2))+
  geom_hline(yintercept = 0, linetype='dashed', col = 'black')

plot
ggsave("graph/Theta-RMSE-Fg.jpg", plot = plot)
###'##########################################################################
###'##########################################################################





###'##########################################################################F1
###'##########################################################################
###'RMSE of theta on the specific factor (thetaS)

df <- theta.mirt.long %>%
  filter(metrics_name == "thetaS.mirt.rmse")%>%
  group_by(Factor,I,N,Fg,Fs,MAP)%>%
  summarise(value=mean(value, na.rm=TRUE))%>%
  mutate(Method=ifelse(MAP==1,"MAP","ML"))

plot <- ggplot(data = df, aes (x = N, y = value, group = Method))+
  geom_line(aes(color = interaction(Fg,Fs),linetype = Method)) +
  scale_color_manual(values = c("#000000","#CC0000", "#999999", "#FF6969")) +
  facet_grid(Fg*Fs~Factor*I)+
  labs(title = "RMSE of Personal Ability on Specific Factor")+
  labs(x = "Sample Size", y = "RMSE")+
  theme(axis.text.x=element_text(angle=0, vjust=0,hjust=0.5, size = 10),
        plot.title = element_text(hjust = 0.2))+
  geom_hline(yintercept = 0, linetype='dashed', col = 'black')

plot
ggsave("graph/Theta-RMSE-Fs.jpg", plot = plot)
###'##########################################################################
###'##########################################################################



###'##########################################################################F1
###'##########################################################################
###'Correlation of theta on the specific factor (thetaG)

df <- theta.mirt.long %>%
  filter(metrics_name == "thetaG.mirt.cor")%>%
  group_by(Factor,I,N,Fg,Fs,MAP)%>%
  summarise(value=mean(value, na.rm=TRUE))%>%
  mutate(Method=ifelse(MAP==1,"MAP","ML"))


plot <- ggplot(data = df, aes (x = N, y = value, group = Method))+
  geom_line(aes(color = interaction(Fg,Fs),linetype = Method)) +
  scale_color_manual(values = c("#000000","#CC0000", "#999999", "#FF6969")) +
  facet_grid(Fg*Fs~Factor*I)+
  labs(title = "Correlation between Estimated Theta and Simulated Theta on General Factor")+
  labs(x = "Sample Size", y = "Correlation")+
  theme(axis.text.x=element_text(angle=0, vjust=0,hjust=0.5, size = 10),
        plot.title = element_text(hjust = 0.2))+
  geom_hline(yintercept = 0, linetype='dashed', col = 'black')

plot

ggsave("graph/Theta-cor-Fg.jpg", plot = plot)
###'##########################################################################
###'##########################################################################




###'##########################################################################F1
###'##########################################################################
###'Cor of theta on the specific factor (thetaS)

df <- theta.mirt.long %>%
  filter(metrics_name == "thetaS.mirt.cor")%>%
  group_by(Factor,I,N,Fg,Fs,MAP)%>%
  summarise(value=mean(value, na.rm=TRUE))%>%
  mutate(Method=ifelse(MAP==1,"MAP","ML"))

plot <- ggplot(data = df, aes (x = N, y = value, group = Method))+
  geom_line(aes(color = interaction(Fg,Fs),linetype = Method)) +
  scale_color_manual(values = c("#000000","#CC0000", "#999999", "#FF6969")) +
  facet_grid(Fg*Fs~Factor*I)+
  labs(title = "Correlation between Estimated Theta and Simulated Theta on General Factor")+
  labs(x = "Sample Size", y = "Correlation")+
  theme(axis.text.x=element_text(angle=0, vjust=0,hjust=0.5, size = 10),
        plot.title = element_text(hjust = 0.2))+
  geom_hline(yintercept = 0, linetype='dashed', col = 'black')

plot
#ggsave("graph/Theta-cor-Fs.jpg", plot = plot)

###'##########################################################################
###'##########################################################################











################################################################################
### 4. IMPACT of all conditions

#ANOVA for bias including ML and MAP
install.packages("pwr")
install.packages("sjstats")
library(pwr)
library(sjstats)
library(tidyverse)
library(ggpubr)
library(rstatix)


#theta.mirt.wide <- read.csv(paste0(result.folder,"theta_mirt_wide.csv"),header = TRUE)

theta.mirt.wide <- as.data.frame(theta.mirt.wide)


ges.all <- data.frame(Var = character(),
                        Effect = character(),
                        ges = numeric(),
                        stringsAsFactors = FALSE)

i <- "thetaG.mirt.bias"

for (i in c("thetaG.mirt.bias","thetaS.mirt.bias","thetaG.mirt.rmse","thetaS.mirt.rmse","thetaG.mirt.cor","thetaS.mirt.cor")){
  result <- anova_test(
    data = theta.mirt.wide,
    dv = i,
    wid = "id.rep",  
    between = c(Factor, I, N, Fg, Fs),  
    within = c(MAP),
    covariate = NULL,
    type = 3,
    effect.size = "ges"
  )
  
  # theta.mirt.wide[58698,]
  
  ges <- get_anova_table(result)
  
  ges.data <- cbind(i,ges$Effect,ges$ges)
  
  ges.data <- as.data.frame(ges.data)  # Convert to data frame
  
  colnames(ges.data) <- c("Var", "Effect", "ges")
  
  ges.data <- as.data.frame(ges.data)  # Convert to data frame
  

  
  ges.all <- rbind(ges.all, ges.data)
}


ges.data.05 <- ges.all %>%
  dplyr::filter(as.numeric(ges) > 0.05)




filter <- unique(ges.data.05$Effect)

ges.result <- ges.all%>%
  filter(Effect %in% filter)%>%
  mutate(ges=round(as.numeric(ges),3))


source.wide <- ges.result %>%
  pivot_wider(
    names_from = "Var",
    values_from = "ges"
  ) 






write.csv(source.wide, paste0(result.folder,"ANOVA_all3.csv"))



library(car) 

#install.packages("lme4")  # Install the 'lme4' package if not already installed
library(lme4)             # Load the 'lme4' package
library(lmerTest)

# Run the mixed-effects ANOVA using lmer()
model2 <- lmer(thetaG.mirt.bias ~ Factor * I * N * Fg * Fs + (1|MAP), data = theta.mirt.wide)



model1 <- aov(thetaG.mirt.bias ~ Factor*I*N*Fg*Fs*MAP, data = theta.mirt.wide)


#am2 <- anova(model2)
model.stats <- effectsize::eta_squared(model2)
model.stats.005 <- model.stats %>%
  mutate(src_variation = i)%>%
  filter(Eta2_partial >= 0.05)%>%
  select(src_variation,Parameter,Eta2_partial)





# Print the model summary
summary(model1)

summary(model2)


install.packages("ez")  # Install the 'ez' package if not already installed
library(ez)             # Load the 'ez' package

# Run the mixed-effects ANOVA using ezANOVA
model3 <- ezANOVA(data = theta.mirt.wide, dv = thetaG.mirt.bias, wid = MAP, within = .(Factor,I,N,Fg,Fs), type = 1)

# Extract the eta-squared value for each effect
eta_squared <- model3$ANOVA[,"eta_sq"]

# Print the eta-squared values
print(eta_squared)




install.packages("car")  # Install the 'car' package if not already installed
library(car)             # Load the 'car' package


# Run the mixed-effects ANOVA using aov()
model4 <- aov(thetaG.mirt.bias ~ Factor*I*N*Fg*Fs + Error(MAP/(Factor*I*N*Fg*Fs)), data = theta.mirt.wide)

# Calculate eta-squared using Anova()
eta_squared <- etaSquared(model)

# Print the eta-squared value
print(eta_squared)











co.vec <- c("2-5-250","2-5-500","2-5-1000","2-5-2000",
            "2-10-250","2-10-500","2-10-1000","2-10-2000",
            "3-5-250","3-5-500","3-5-1000","3-5-2000",
            "3-10-250","3-10-500","3-10-1000","3-10-2000",
            "4-5-250","4-5-500","4-5-1000","4-5-2000",
            "4-10-250","4-10-500","4-10-1000","4-10-2000"
)
#table.all.convergence <- read.csv(paste0(result.folder,"convergence.csv"))


head(para.theta.all)



#ML thetaG.mirt.ml convergence rate

convergence.ana.ml.thetaG <- para.theta.all %>%
  mutate(condition=paste0(Factor,"-",I,"-",N))%>%
  group_by(condition)%>%
  summarise(con.ratio = sum(!is.infinite(thetaG.mirt.ml)) / n())%>% 
  mutate(condition = factor(condition, levels = co.vec))%>%
  arrange(condition)  


ggplot(data = convergence.ana.ml.thetaG, aes(x = condition, y = con.ratio)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=-45, vjust=1,hjust=0),
        plot.title = element_text(hjust = 0.5))






#ML thetaS.mirt.ml convergence rate

convergence.ana.ml.thetaS <- para.theta.all %>%
  mutate(condition=paste0(Factor,"-",I,"-",N))%>%
  group_by(condition)%>%
  summarise(con.ratio = sum(!is.infinite(thetaS.avg.mirt.ml)) / n())%>% 
  mutate(condition = factor(condition, levels = co.vec))%>%
  arrange(condition)  


ggplot(data = convergence.ana.ml.thetaS, aes(x = condition, y = con.ratio)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=-45, vjust=1,hjust=0),
        plot.title = element_text(hjust = 0.5))



#ML thetaG.mirt.map convergence rate

convergence.ana.map.thetaG <- para.theta.all %>%
  mutate(condition=paste0(Factor,"-",I,"-",N))%>%
  group_by(condition)%>%
  summarise(con.ratio = sum(!is.infinite(thetaG.mirt.map)) / n())%>% 
  mutate(condition = factor(condition, levels = co.vec))%>%
  arrange(condition)  


ggplot(data = convergence.ana.map.thetaG, aes(x = condition, y = con.ratio)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=-45, vjust=1,hjust=0),
        plot.title = element_text(hjust = 0.5))






#ML thetaS.mirt.map convergence rate

convergence.ana.map.thetaS <- para.theta.all %>%
  mutate(condition=paste0(Factor,"-",I,"-",N))%>%
  group_by(condition)%>%
  summarise(con.ratio = sum(!is.infinite(thetaS.avg.mirt.map)) / n())%>% 
  mutate(condition = factor(condition, levels = co.vec))%>%
  arrange(condition)  


ggplot(data = convergence.ana.map.thetaS, aes(x = condition, y = con.ratio)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=-45, vjust=1,hjust=0),
        plot.title = element_text(hjust = 0.5))


t <- cbind(convergence.ana.ml.thetaG,convergence.ana.ml.thetaS)

write.csv(convergence.ana.ml.thetaG, paste0(result.folder,"convergence.ML.csv"))
