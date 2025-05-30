rm(list=ls())
library(tidyr)
library(dplyr)
library(readr)
library(parallel)
library(mirt)
library(stringr)
main.folder <- "H:/BifactorNonNormality-PsychPC2"
data.folder <- paste0(main.folder,"/Data/")
dir.create(data.folder)
result.folder <- paste0(main.folder,"/Result20250517/")


#For Test Code
rep=1000
Factor=4
I=20
N=1000
Fg=2
Fs=2
i=1000


################################################################################
################################ Get Parameters ################################
################################################################################
############# Skewness of theta (personal ability)###########################

now <- Sys.time() 
# Input all files' name and combine real_parameters files' name
# files <- read.csv(paste0(result.folder,"convergence.csv"))%>%
#   select(-X.1,-X)
files <- read.csv(paste0(result.folder,"convergence.csv"))


files.all <- files %>%
  #filter(mirt..bfconvergence != 0)%>%
  mutate(
    realtheta = paste0(id.rep,"-realtheta.csv"),
    mirttheta_map = paste0(id.rep,"-mirttheta_map.csv"),
    mirttheta_ml = paste0(id.rep,"-mirttheta_ml.csv"),         
    data.folder.function = paste0(data.folder,Factor,"-",I,"-",N,"/", 
                                  Factor,"-",I,"-",N,"-",Fg,"-",Fs,"/")#,
  )
head(files.all)



theta.data.all <- data.frame()

# i <- 72000

for (i in 1:nrow(files.all)){
  if (!file.exists(files.all$data.folder.function[i])) {break}  
  #setwd(files.all$data.folder.function[i])

  Factor <- files.all$Factor[i]
  I <- files.all$I[i]
  N <- files.all$N[i]
  
  #Get id.rep and theta.id
  id.rep <- files.all$id.rep[i]
  theta.id <- paste0(id.rep,"-", 1:N)

  
  #input theta.real from each realtheta.csv, Only extract first specific factor. I didn't change the name of thetaS.avg.real, but now it refers only first specific factor     
    theta.data.real <- read.csv(paste0(files.all$data.folder.function[i], files.all$realtheta[i]),header = FALSE)
    theta.data.real <- theta.data.real %>%
      # mutate(thetaG.real = theta.data.real[,1], thetaS.avg.real = rowMeans(theta.data.real[,-1]))%>%
      mutate(thetaG.real = theta.data.real[,1], thetaS.1.real = theta.data.real[,2], thetaS.2.real = theta.data.real[,3])%>%
      select(thetaG.real,thetaS.1.real,thetaS.2.real)
  
  
  #input mirttheta_map.csv
    theta.data.mirt.map <- read.csv(paste0(files.all$data.folder.function[i], files.all$mirttheta_map[i]),header = TRUE)
    theta.data.mirt.map <- theta.data.mirt.map %>%
      # mutate(thetaG.mirt.map = theta.data.mirt.map[,2], thetaS.avg.mirt.map = rowMeans(theta.data.mirt.map[,-c(1,2)]))%>%
      mutate(thetaG.mirt.map = theta.data.mirt.map[,2], thetaS.1.mirt.map = theta.data.mirt.map[,3], thetaS.2.mirt.map = theta.data.mirt.map[,4])%>%
      select(thetaG.mirt.map,thetaS.1.mirt.map,thetaS.2.mirt.map)
  
  #input mirttheta_ml.csv
    theta.data.mirt.ml <- read.csv(paste0(files.all$data.folder.function[i], files.all$mirttheta_ml[i]),header = TRUE)
    theta.data.mirt.ml <- theta.data.mirt.ml %>%
      mutate(thetaG.mirt.ml = theta.data.mirt.ml[,2], thetaS.1.mirt.ml = theta.data.mirt.ml[,3], thetaS.2.mirt.ml = theta.data.mirt.ml[,4])%>%
      select(thetaG.mirt.ml,thetaS.1.mirt.ml,thetaS.2.mirt.ml)
  
       
  #Combine  parameters.real, parameters.FlexMIRT and parameters.mirt
  theta.data <- cbind(id.rep, theta.id, theta.data.real, theta.data.mirt.map, theta.data.mirt.ml)
  
  #Gather all parameters of each replications
  theta.data.all<- rbind(theta.data.all,theta.data)
}
  


#Merge convergence status and parameters
#theta.data_file.all <- left_join(theta.data.all, files.all ,by="id.rep")
theta.data_file.all <- merge(theta.data.all, files, by="id.rep", all.x=TRUE)


###Output results of skewness of theta.FlexMIRT

write.table(theta.data_file.all, paste0(result.folder,"thetas.csv"),row.names = FALSE, sep=",")



Sys.time()-now







###'################
###'Testing



library(ggplot2)
colnames(theta.data.real)  <- c("thetaG","thetaS1","thetaS2","thetaS3")    

ggplot(data=theta.data.real)+
  geom_density(aes(x = thetaG, color = "thetaG")) +
  geom_density(aes(x = thetaS1, color = "thetaS1")) +
  geom_density(aes(x = thetaS2, color = "thetaS2")) +
  geom_density(aes(x = thetaS3, color = "thetaS3")) 
