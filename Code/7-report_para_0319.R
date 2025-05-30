#Merge convergence status and parameters
#para.data_file.all <- left_join(para.data.all, files.all ,by="id.rep")
# rm(list=ls())
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(export)#Output graph to ppt/word
#install.packages("psych")
library(psych)


# Set folder and Input data

main.folder <- "C:/Users/julia/OneDrive - The University of Alabama/Research/2023_2_BifactorNonNormality"
data.folder <- paste0(main.folder,"/Data/")
result.folder <- paste0(main.folder,"/Result/")
ppt.folder <- paste0(main.folder, "/graph_ppt")
dir.create(ppt.folder)



#Input main data
para.data_file.all <- read.csv(paste0(result.folder,"parameters.csv"),header = TRUE)
#para.data_file.all <- parameters
# summary(para.data_file.all)
# 
# unique(para.data_file.all$N)

para.data_file.all2 <- para.data_file.all%>%
  #filter(mirt..bfconvergence != 0)%>%
  filter(is.na(c1.mirt) == 0)



#Calculate the delta of each parameters

para.data_file.all3 <- para.data_file.all2%>%
  mutate(#aG.Flex.delta= as.numeric(aG.FlexMIRT)-as.numeric(aG.real),
    #aS.Flex.delta= as.numeric(aS.FlexMIRT)-as.numeric(aS.real),
    aS.real= as.numeric(aS.real),
    aG.mirt.delta= as.numeric(aG.mirt)-as.numeric(aG.real),
    aS.mirt.delta= as.numeric(aS.mirt)-as.numeric(aS.real),
    c1.mirt.delta= as.numeric(c1.mirt)-as.numeric(c1.real),
    c2.mirt.delta= as.numeric(c2.mirt)-as.numeric(c2.real),
    c3.mirt.delta= as.numeric(c3.mirt)-as.numeric(c3.real)
  )

# sum(para.data_file.all$mirt..bfconvergence == 0)
# sum(para.data_file.report$mirt..bfconvergence == 0)

# summary(para.data_file.all2)

cor(para.data_file.all3$aG.real,para.data_file.all3$aG.mirt)


#Calculate bias and rmse
para.data_file.summarise <- para.data_file.all3%>%
  #group_by(id.rep, Factor, I, N, Fg, Fs)%>%
  summarise(a_GenFactor_bias = round(mean(aG.mirt.delta, na.rm = TRUE),3), 
            a_SpeFactor_bias = round(mean(aS.mirt.delta, na.rm = TRUE),3),
            c1_bias = round(mean(c1.mirt.delta, na.rm = TRUE),3),
            c2_bias = round(mean(c2.mirt.delta, na.rm = TRUE),3),
            c3_bias = round(mean(c3.mirt.delta, na.rm = TRUE),3),
            c_bias = round((c1_bias+c2_bias+c3_bias)/3,3),
            a_GenFactor_rmse = round(sqrt(mean((aG.mirt.delta)^2, na.rm = TRUE)),3), 
            a_SpeFactor_rmse = round(sqrt(mean((aS.mirt.delta)^2, na.rm = TRUE)),3), 
            c1_rmse = round(sqrt(mean((c1.mirt.delta)^2, na.rm = TRUE)),3), 
            c2_rmse = round(sqrt(mean((c2.mirt.delta)^2, na.rm = TRUE)),3), 
            c3_rmse = round(sqrt(mean((c3.mirt.delta)^2, na.rm = TRUE)),3),
            c_rmse = round((c1_rmse+c2_rmse+c3_rmse)/3,3),
            #Correlation between estimated value (Y) and real value (X)
            a_GenFactor_cor = round(cor(aG.real,aG.mirt),3), 
            a_SpeFactor_cor = round(cor(aS.real,aS.mirt),3), 
            c1_cor = round(cor(c1.real,c1.mirt),3), 
            c2_cor = round(cor(c2.real,c2.mirt),3), 
            c3_cor = round(cor(c3.real,c3.mirt),3),
            c_cor = round((c1_cor+c2_cor+c3_cor)/3,3),
            #Calculate values of each condition and replication, for boxplot
            #.by = c(id.rep, Factor, I, N, Fg, Fs)
            #Calculate values of each condition, for mean
            .by = c(Factor, I, N, Fg, Fs)
  ) 






write.csv(para.data_file.summarise,paste0(result.folder,"summa.csv"))

#summary(para.data_file.summarise)



#Merge all Bias and Rmse into a longer table
para.data_file.long <- as.data.frame(para.data_file.summarise) %>%
  pivot_longer(
    cols = a_GenFactor_bias : c_cor,
    names_to = "metrics_name",
    values_to = "value"
  )%>%
  mutate(
    Fg.num = Fg,
    Fs.num = Fs,
    Factor.num = Factor,
    I.num = I,
    N.num = N,
    Fg = ifelse(Fg == "Skew(0)Kurt(0)", "Normal  ", "Non-normal"),
    Fs = ifelse(Fs == "Skew(0)Kurt(0)", "Normal  ", "Non-normal"),
    Factor = paste0("Factor:",Factor)
    # I = paste0("Item:",I),
    #N = paste0("Sample Size = ",N)
  )%>%
  mutate(Fg = factor(Fg, levels = c("Normal  ", "Non-normal")))%>%
  arrange(Fg)%>% 
  mutate(Fs = factor(Fs, levels = c("Normal  ", "Non-normal")))%>%
  arrange(Fs)%>%   
  mutate(Factor = factor(Factor, levels = c("Factor:2","Factor:4")))%>%
  arrange(Factor)%>%
  # mutate(I = factor(I, levels = c("Item:5","Item:10","Item:20")))%>%
  # arrange(I)%>%
  #mutate(N = factor(N, levels = c("Sample Size = 250","Sample Size = 500","Sample Size = 1000")))%>%
  mutate(N = factor(N, levels = c("250","500","1000")))%>%
  arrange(N)

# unique(para.data_file.long$Fg)
# 
# summary(para.data_file.long)
# describe(para.data_file.long)
# str(para.data_file.long)



# summary(convergence.ana)

#1. Bias

###'##########################################################################F1
###'##########################################################################
###'Bias of parameter "a" on the general factor (aG).

library(dplyr)

df <- para.data_file.long %>%
  filter(metrics_name == "a_GenFactor_bias" | metrics_name == "a_SpeFactor_bias" | metrics_name == "c_bias") %>%
  summarise(value = mean(value), .by = c(metrics_name, Fg, N, I)) %>%
  mutate(Sample_Size = N,
         metrics_name = case_when(
           metrics_name == "a_GenFactor_bias" ~ "ag",
           metrics_name == "a_SpeFactor_bias" ~ "as",
           metrics_name == "c_bias" ~ "c",
           TRUE ~ metrics_name # This line is optional; it handles any cases not specified above
         ))



plot <- ggplot(data = df, aes(x = N, y = value, group = Fg)) +
  geom_line(aes(linetype = Fg)) +
  scale_linetype_manual(values = c("solid", "dashed", "F1", "dotted")) +
  facet_grid(metrics_name ~ I) +
  labs(#title = "Bias in Estimation of Item Parameters",
    x = "Sample Size", y = "Bias",
    linetype = "Non-normality on the General Factor (GF)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top") + # Move legend to top
  geom_hline(yintercept = 0, linetype = "dotted", color = 'black') 
# geom_hline(yintercept = 0.05 * 2.8, linetype = "dotted", color = 'red') +
# geom_hline(yintercept = -0.05* 2.8, linetype = "dotted", color = 'red')




plot





#2. RMSE

###'##########################################################################F1
###'##########################################################################
###'RMSE of parameter "a" on the general factor (aG).
library(dplyr)

df <- para.data_file.long %>%
  filter(metrics_name == "a_GenFactor_rmse" | metrics_name == "a_SpeFactor_rmse" | metrics_name == "c_rmse") %>%
  summarise(value = mean(value), .by = c(metrics_name, Fg, N, I)) %>%
  mutate(Sample_Size = N,
         metrics_name = case_when(
           metrics_name == "a_GenFactor_rmse" ~ "ag",
           metrics_name == "a_SpeFactor_rmse" ~ "as",
           metrics_name == "c_rmse" ~ "c",
           TRUE ~ metrics_name # This line is optional; it handles any cases not specified above
         ))



plot <- ggplot(data = df, aes(x = N, y = value, group = Fg)) +
  geom_line(aes(linetype = Fg)) +
  scale_linetype_manual(values = c("solid", "dashed", "F1", "dotted")) +
  facet_grid(metrics_name ~ I) +
  labs(#title = "RMSE in Estimation of Item Parameters",
    x = "Sample Size", y = "RMSE",
    linetype = "Non-normality on the General Factor (GF)") + # Correctly place legend title here
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top") + # Move legend to top
  geom_hline(yintercept = 0, linetype = "dotted", color = 'black')+
  geom_hline(yintercept = 0.5, linetype = "dotted", color = 'red')



plot

# For Interaction between I and N
df <- para.data_file.long %>%
  filter(metrics_name == "a_SpeFactor_rmse") %>%
  summarise(value = mean(value), .by = c(metrics_name, Fg, N, I)) %>%
  mutate(Sample_Size = N,
         metrics_name = case_when(
           metrics_name == "a_GenFactor_rmse" ~ "ag",
           metrics_name == "a_SpeFactor_rmse" ~ "as",
           metrics_name == "c_rmse" ~ "c",
           TRUE ~ metrics_name # This line is optional; it handles any cases not specified above
         ))%>%
  mutate(N = paste0("Sample Size: ",N))%>%
  mutate(N = factor(N, levels = c("Sample Size: 250","Sample Size: 500","Sample Size: 1000")))%>%
  arrange(N)%>%
  mutate(I = factor(I, levels = c("5","10","20")))%>%
  arrange(I)


ggplot(data = df, aes(x = I, y = value, group = Fg)) +
  geom_line(aes(linetype = Fg)) +
  scale_linetype_manual(values = c("solid", "dashed", "F1", "dotted")) +
  facet_grid( ~ N) +
  labs(#title = "RMSE in Estimation of Item Parameters",
    x = "Item Number per Specific Factor", 
    y = "RMSE",
    linetype = "General Factor") + # Correctly place legend title here
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right") + 
  geom_hline(yintercept = 0, linetype = "dotted", color = 'black')


###'##########################################################################
###'##########################################################################




#3. IMPACT of all Factors of Conditions



#ANOVA for bias
install.packages("pwr")

library(pwr)
library(sjstats)
library(rstatix)
library(stats)

model.all <- data.frame(src_variation = character(),
                        term = character(),
                        etasq = numeric(),
                        stringsAsFactors = FALSE)

for (i in c("a_GenFactor_bias","a_SpeFactor_bias","c1_bias","c2_bias","c3_bias", "c_bias")){
  model <- aov(as.formula(paste0(i, " ~ Factor*I*N*Fg*Fs")), data=para.data_file.summarise)
  model.stats <- anova_stats(model, digits = 3)
  model.stats.06 <- model.stats %>%
    mutate(src_variation = i)%>%
    filter(etasq >= 0.06)%>%
    select(src_variation,term,etasq)
  
  model.all <- rbind(model.all,model.stats.06)
}


filter <- unique(model.all$term)

model.all <- data.frame(src_variation = character(),
                        term = character(),
                        etasq = numeric(),
                        stringsAsFactors = FALSE)

for (i in c("a_GenFactor_bias","a_SpeFactor_bias","c1_bias","c2_bias","c3_bias", "c_bias")){
  model <- aov(as.formula(paste0(i, " ~ Factor*I*N*Fg*Fs")), data=para.data_file.summarise)
  model.stats <- anova_stats(model, digits = 3)
  model.stats.06 <- model.stats %>%
    mutate(src_variation = i)%>%
    filter(term %in% filter)%>%
    select(src_variation,term,etasq)
  
  model.all <- rbind(model.all,model.stats.06)
}

source.bias <- model.all




#ANOVA for RMSE

model.all <- data.frame(src_variation = character(),
                        term = character(),
                        etasq = numeric(),
                        stringsAsFactors = FALSE)

for (i in c("a_GenFactor_rmse","a_SpeFactor_rmse","c1_rmse","c2_rmse","c3_rmse", "c_rmse")){
  model <- aov(as.formula(paste0(i, " ~ Factor*I*N*Fg*Fs")), data=para.data_file.summarise)
  model.stats <- anova_stats(model, digits = 3)
  model.stats.06 <- model.stats %>%
    mutate(src_variation = i)%>%
    filter(etasq >= 0.06)%>%
    select(src_variation,term,etasq)
  
  model.all <- rbind(model.all,model.stats.06)
}



filter <- unique(model.all$term)

model.all <- data.frame(src_variation = character(),
                        term = character(),
                        etasq = numeric(),
                        stringsAsFactors = FALSE)

for (i in c("a_GenFactor_rmse","a_SpeFactor_rmse","c1_rmse","c2_rmse","c3_rmse", "c_rmse")){
  model <- aov(as.formula(paste0(i, " ~ Factor*I*N*Fg*Fs")), data=para.data_file.summarise)
  model.stats <- anova_stats(model, digits = 3)
  model.stats.06 <- model.stats %>%
    mutate(src_variation = i)%>%
    filter(term %in% filter)%>%
    select(src_variation,term,etasq)
  
  model.all <- rbind(model.all,model.stats.06)
}

source.rmse <- model.all




#ANOVA for Correlation

model.all <- data.frame(src_variation = character(),
                        term = character(),
                        etasq = numeric(),
                        stringsAsFactors = FALSE)

for (i in c("a_GenFactor_cor","a_SpeFactor_cor","c1_cor","c2_cor","c3_cor", "c_cor")){
  model <- aov(as.formula(paste0(i, " ~ Factor*I*N*Fg*Fs")), data=para.data_file.summarise)
  model.stats <- anova_stats(model, digits = 3)
  model.stats.06 <- model.stats %>%
    mutate(src_variation = i)%>%
    filter(etasq >= 0.06)%>%
    select(src_variation,term,etasq)
  
  model.all <- rbind(model.all,model.stats.06)
}


filter <- unique(model.all$term)

model.all <- data.frame(src_variation = character(),
                        term = character(),
                        etasq = numeric(),
                        stringsAsFactors = FALSE)

for (i in c("a_GenFactor_cor","a_SpeFactor_cor","c1_cor","c2_cor","c3_cor", "c_cor")){
  model <- aov(as.formula(paste0(i, " ~ Factor*I*N*Fg*Fs")), data=para.data_file.summarise)
  model.stats <- anova_stats(model, digits = 3)
  model.stats.06 <- model.stats %>%
    mutate(src_variation = i)%>%
    filter(term %in% filter)%>%
    select(src_variation,term,etasq)
  
  model.all <- rbind(model.all,model.stats.06)
}

source.cor <- model.all



source.all <- rbind(source.bias, source.rmse, source.cor)


source.wide <- source.all %>%
  pivot_wider(
    names_from = "src_variation",
    values_from = "etasq"
  ) 


write.csv(source.wide, paste0(result.folder,"ANOVA_para2.csv"))








################################################################################
########################0. Convergence Rate#####################################
################################################################################
co.vec <- c("2-5-250","2-5-500","2-5-1000",
            "2-10-250","2-10-500","2-10-1000",
            "2-20-250","2-20-500","2-20-1000",            
            "4-5-250","4-5-500","4-5-1000",
            "4-10-250","4-10-500","4-10-1000",
            "4-20-250","4-20-500","4-20-1000"
)
table.all.convergence <- read.csv(paste0(result.folder,"convergence.csv"))

head(table.all.convergence)

convergence.ana <- table.all.convergence %>%
  mutate(condition=paste0(Factor,"-",I,"-",N))%>%
  group_by(condition)%>%
  summarise(con.ratio = sum(mirt..bfconvergence!=0) / n())%>% 
  mutate(condition = factor(condition, levels = co.vec))%>%
  arrange(condition)  


ggplot(data = convergence.ana, aes(x = condition, y = con.ratio)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=-45, vjust=1,hjust=0),
        plot.title = element_text(hjust = 0.5))







para.data_file.summarise_n <- para.data_file.all2 %>%
  summarise(count = n(),
            .by = c(Fg,Fs, I,Factor,N)) %>%
  mutate(I = ifelse(I=="Item = 5", 5, ifelse(I=="Item = 10", 10, 20))) %>%
  mutate(Factor = ifelse(Factor=="Factor = 2", 2, 4)) %>%
  mutate(each = count/(I*Factor))

ggplot(data = para.data_file.summarise_n, aes(x = interaction(Fg,Fs,I,Factor,N), y = each)) +
  geom_col() +
  coord_flip() +  # This flips the axes
  theme(axis.text.x=element_text(angle=-45, vjust=1,hjust=0),
        axis.text.y=element_text(angle=0),  # Adjust if needed for better label presentation
        plot.title = element_text(hjust = 0.5))

summary(para.data_file.all2)





################Table of Means and SDs


table_mean <- tibble()
# i = "Fs"
factors <- c("Factor", "I", "N", "Fg", "Fs")

for (i in factors){
  df <- para.data_file.all3%>%
    group_by(!!sym(i)) %>%
    summarise(a_GenFactor_bias = round(mean(aG.mirt.delta, na.rm = TRUE),3), 
              a_SpeFactor_bias = round(mean(aS.mirt.delta, na.rm = TRUE),3),
              c1_bias = round(mean(c1.mirt.delta, na.rm = TRUE),3),
              c2_bias = round(mean(c2.mirt.delta, na.rm = TRUE),3),
              c3_bias = round(mean(c3.mirt.delta, na.rm = TRUE),3),
              c_bias = round((c1_bias+c2_bias+c3_bias)/3,3),
              a_GenFactor_rmse = round(sqrt(mean((aG.mirt.delta)^2, na.rm = TRUE)),3), 
              a_SpeFactor_rmse = round(sqrt(mean((aS.mirt.delta)^2, na.rm = TRUE)),3), 
              c1_rmse = round(sqrt(mean((c1.mirt.delta)^2, na.rm = TRUE)),3), 
              c2_rmse = round(sqrt(mean((c2.mirt.delta)^2, na.rm = TRUE)),3), 
              c3_rmse = round(sqrt(mean((c3.mirt.delta)^2, na.rm = TRUE)),3),
              c_rmse = round((c1_rmse+c2_rmse+c3_rmse)/3,3),
              
              a_GenFactor_bias_sd = round(sd(aG.mirt.delta, na.rm = TRUE),3), 
              a_SpeFactor_bias_sd = round(sd(aS.mirt.delta, na.rm = TRUE),3),
              c1_bias_sd = round(sd(c1.mirt.delta, na.rm = TRUE),3),
              c2_bias_sd = round(sd(c2.mirt.delta, na.rm = TRUE),3),
              c3_bias_sd = round(sd(c3.mirt.delta, na.rm = TRUE),3),
              c_bias_sd = round((c1_bias_sd+c2_bias_sd+c3_bias_sd)/3,3),
              a_GenFactor_rmse_sd = round(sqrt(sd((aG.mirt.delta)^2, na.rm = TRUE)),3), 
              a_SpeFactor_rmse_sd = round(sqrt(sd((aS.mirt.delta)^2, na.rm = TRUE)),3), 
              c1_rmse_sd = round(sqrt(sd((c1.mirt.delta)^2, na.rm = TRUE)),3), 
              c2_rmse_sd = round(sqrt(sd((c2.mirt.delta)^2, na.rm = TRUE)),3), 
              c3_rmse_sd = round(sqrt(sd((c3.mirt.delta)^2, na.rm = TRUE)),3),
              c_rmse_sd = round((c1_rmse_sd+c2_rmse_sd+c3_rmse_sd)/3,3)
    ) %>%
    mutate(variable = i, condition = as.character(!!sym(i))) %>%
    select(-!!sym(i))
  
  
  table_mean <- bind_rows(table_mean, df)
}
write.csv(table_mean, "table_mean_item.csv")


#table_mean <- read.csv("result/table_mean_item.csv")


table_mean_sd <- table_mean%>%
  mutate(a_GenFactor_bias = paste0( round(a_GenFactor_bias,2), "(", round(a_GenFactor_bias_sd,2) , ")" ), 
         a_SpeFactor_bias = paste0( round(a_SpeFactor_bias,2), "(", round(a_SpeFactor_bias_sd,2) , ")" ), 
         c1_bias = paste0( round(c1_bias,2), "(", round(c1_bias_sd,2) , ")" ), 
         c2_bias = paste0( round(c2_bias,2), "(", round(c2_bias_sd,2) , ")" ), 
         c3_bias = paste0( round(c3_bias,2), "(", round(c3_bias_sd,2) , ")" ),  
         c_bias = paste0( round(c_bias,2), "(", round(c_bias_sd,2) , ")" ), 
         a_GenFactor_rmse = paste0( round(a_GenFactor_rmse,2), "(", round(a_GenFactor_rmse_sd,2) , ")" ), 
         a_SpeFactor_rmse = paste0( round(a_SpeFactor_rmse,2), "(", round(a_SpeFactor_rmse_sd,2) , ")" ), 
         c1_rmse = paste0( round(c1_rmse,2), "(", round(c1_rmse_sd,2) , ")" ), 
         c2_rmse = paste0( round(c2_rmse,2), "(", round(c2_rmse_sd,2) , ")" ), 
         c3_rmse = paste0( round(c3_rmse,2), "(", round(c3_rmse_sd,2) , ")" ),  
         c_rmse = paste0( round(c_rmse,2), "(", round(c_rmse_sd,2) , ")" )
  )
write.csv(table_mean_sd, "table_mean_sd_item.csv")
