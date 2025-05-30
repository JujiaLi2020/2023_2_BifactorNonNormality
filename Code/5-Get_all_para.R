
library(tidyr)
library(dplyr)
library(readr)
library(parallel)
library(mirt)
library(stringr)
# main.folder <- "H:/BifactorNonNormality-PsychPC2"
# data.folder <- paste0(main.folder,"/Data/")

main.folder <- "C:/Users/julia/OneDrive - The University of Alabama/Research/2023_2_BifactorNonNormality"
data.folder <- paste0(main.folder,"/Data/")

result.folder <- paste0(main.folder,"/Result20250517/")



#For Test Code
rep=1000
Factor=4
I=20
N=1000
Fg=2
Fs=2
i=72000

################################################################################
################################ Get Parameters ################################
################################################################################
############# Skewness of theta (personal ability)###########################
skew <- c(0, 2)
kurt <- c(0, 7)
skew.cut <- c(.25, .25)
kurt.cut <- c(1, 1)
vec <- as.data.frame(cbind(skew,kurt,skew.cut,kurt.cut))

skewlevel <- vec %>%
  mutate(name = paste0("Skew(",skew,")Kurt(",kurt,")"))      

skew.Fg <- skewlevel[Fg,1]
kurt.Fg <- skewlevel[Fg,2]
skew.Fs <- skewlevel[Fs,1]
kurt.Fs <- skewlevel[Fs,2]
skew.Fg.name <- skewlevel[Fg,5]
skew.Fs.name <- skewlevel[Fs,5]
skew.Fg.cut <- skewlevel[Fg,3]
kurt.Fg.cut <- skewlevel[Fs,4]
skew.Fs.cut <- skewlevel[Fg,3]
kurt.Fs.cut <- skewlevel[Fs,4]
  
# Input all files' name and combine real_parameters files' name
# files <- read.csv(paste0(result.folder,"convergence.csv"))%>%
#   select(-X.1,-X)
files <- read.csv(paste0(result.folder,"convergence.csv"))

files.all <- files %>%
  ###Drop replications of which difference bw skewness of simulated samples and population are over 0.5;
  ###Drop replications of which difference bw kurtosis of simulated samples and population are over 2.   Data:Jan/23/2024
  # filter(mirt_map_skew_kurt_check==1,
  #        mirt_ml_skew_kurt_check==1)%>%
  
  mutate(realcsvfile = paste0(id.rep,"-realparameters.csv"),
           FlexMIRTprmfile = paste0("Syn",id.rep,"-prm.txt"),
           data.folder.function = paste0(data.folder,Factor,"-",I,"-",N,"/", 
          Factor,"-",I,"-",N,"-",Fg,"-",Fs,"/")
         )

para.data.all <- data.frame()

for (i in 1:nrow(files.all)){
  if (!file.exists(files.all[i,16])) {break}  
  setwd(files.all[i,16])

  Factor <- as.numeric(files.all[i,8])
  I <- as.numeric(files.all[i,9])
  N <- as.numeric(files.all[i,10])
  
  #Get id.rep and item.num
  id.rep <- files.all[i,1]
  item.num <- seq(1:(Factor*I))
    
  #input parameter.real from each realparameters.csv      
  if (file.exists(files.all[i,14])) {
    para.data.real <- read.csv(files.all[i,14],header = FALSE)
    para.data.real <- data.frame(lapply(para.data.real, function(x) as.numeric(as.character(x))))
    colnames(para.data.real) <- c("aG.real",paste0("aS.real",1:Factor),"c3.real","c2.real","c1.real")
    para.data.real.aS <- unite(para.data.real, aS.real, 2:(Factor+1),na.rm=TRUE)
    para.data.real.aS <- para.data.real.aS[,c("aG.real","aS.real","c3.real","c2.real","c1.real")]
  } else {
    para.data.real.aS <- data.frame(matrix(NA,Factor*I,5))
    colnames(para.data.real.aS) <- c("aG.real","aS.real","c3.real","c2.real","c1.real")
  }


  #input parameter.FlexMIRT
  if (file.exists(files.all[i,15])) { 
    para.data.FlexMIRT <- read.delim(files.all[i,15] , header = FALSE)
    para.data.FlexMIRT <- head(para.data.FlexMIRT, - 2) 
    para.data.FlexMIRT <- para.data.FlexMIRT[,-(1:6)]
    colnames(para.data.FlexMIRT) <- c("c3.FlexMIRT","c2.FlexMIRT","c1.FlexMIRT","aG.FlexMIRT",paste0("aS.FlexMIRT",1:(length(para.data.FlexMIRT)-4)))
    para.data.FlexMIRT$aS.FlexMIRT <- rowSums(para.data.FlexMIRT[,5:ncol(para.data.FlexMIRT)])
    para.data.FlexMIRT.aS <- para.data.FlexMIRT[,c("aG.FlexMIRT","aS.FlexMIRT","c3.FlexMIRT","c2.FlexMIRT","c1.FlexMIRT")]
  } else {
    para.data.FlexMIRT.aS  <- data.frame(matrix(NA,Factor*I,5))
    colnames(para.data.FlexMIRT.aS) <- c("aG.FlexMIRT","aS.FlexMIRT","c3.FlexMIRT","c2.FlexMIRT","c1.FlexMIRT")
  }
    
    
  #input parameter.mirt::bfactor()
  if (file.exists(files.all[i,5])) {     
    para.data.mirt <- read.csv(files.all[i,5],header = TRUE)
    para.data.mirt.aS <- rowSums(para.data.mirt[,3:(Factor+2)])
    para.data.mirt.aS <- cbind(para.data.mirt,para.data.mirt.aS)
    para.data.mirt.aS <- para.data.mirt.aS[,c("a1","para.data.mirt.aS","d1","d2","d3")]
    colnames(para.data.mirt.aS) <- c("aG.mirt","aS.mirt","c3.mirt","c2.mirt","c1.mirt")   
  } else {
    para.data.mirt.aS  <- data.frame(matrix(NA,Factor*I,5))
    colnames(para.data.mirt.aS) <- c("aG.mirt","aS.mirt","c3.mirt","c2.mirt","c1.mirt")
  }
   
     
  #Combine  parameters.real, parameters.FlexMIRT and parameters.mirt
  para.data <- cbind(id.rep, item.num, para.data.real.aS, para.data.FlexMIRT.aS, para.data.mirt.aS)

  #Gather all parameters of each replications
  para.data.all<- rbind(para.data.all,para.data)
}
  


#Merge convergence status and parameters
#para.data_file.all <- left_join(para.data.all, files.all ,by="id.rep")
para.data_file.all <- merge(para.data.all, files, by="id.rep", all.x=TRUE)




###Output results of skewness of theta.FlexMIRT

write.table(para.data_file.all, paste0(result.folder,"parameters.csv"),row.names = FALSE, sep=",")


