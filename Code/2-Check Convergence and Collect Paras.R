
library(tidyr)
library(dplyr)
library(readr)
library(parallel)
library(mirt)
library(stringr)

library(moments)
# main.folder <- "H:/BifactorNonNormality-PsychPC2"
main.folder <- "C:/Users/julia/OneDrive - The University of Alabama/Research/2023_2_BifactorNonNormality"
data.folder <- paste0(main.folder,"/Data/")




# rep=200
# Factor=4
# I=10
# N=1000
# Fg=1
# Fs=1



######Function ####################################################################
################################ Get Convergence ##################################
###################################################################################

Convergence <- function(Factor,I,N,Fg,Fs,rep){
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
  
  
  
  # for (Fg in 1:G){
  #   for (Fs in 1:S){
  
  id.rep <- vector()
  id.item <- vector()
  
  id.com <- paste0(Factor,"-",I,"-",N)
  data.folder.function <- paste0(data.folder,id.com,"/", id.com,"-",
                                 skew.Fg.name,"-",skew.Fs.name,"/")
  
  if (!file.exists(data.folder.function)) {next}
  setwd(data.folder.function)
  
  ###Create result table
  table <- matrix("Miss",rep,13)
  colnames(table) <- c("id.rep","Flexirtfile","Flexirtready","Flexirtconvergence","mirtcsvfile","mirtcsvready","mirt::bfconvergence","Factor","I","N","Fg","Fs","rep")
  
  ###Create unique id for each condition and each replication
  for (i in 1:rep){
    table[i,1] <- id.rep[i]<- paste0(id.com,"-",skew.Fg.name,"-",skew.Fs.name,"-",i)
  }
  
  
  ###########################################################################
  ###Add FlexMIRT results
  
  ###Check whether irt.txt is existed, for checking convergence 
  table[,2] <- paste0("Syn",id.com,"-",skew.Fg.name,"-",skew.Fs.name,"-",seq(1:rep),"-irt.txt")
  
  file.list.check.Flex <- list.files(pattern="*irt.txt")      
  table[which(table[,2] %in% file.list.check.Flex),3] <- "ok"
  
  ###Get Convergence from "irt.txt"
  convergence <- vector()
  
  for (i in 1:rep){
    ###Get string from existing irt.txt files        
    if (table[i,3]=="ok"){
      file_str <- read_file(paste0(data.folder.function,table[i,2]))
      
      ###Get Convergence Setting
      position <- str_locate(file_str, "Maximum number of cycles:")
      position2 <- str_locate(file_str, "Convergence criterion")
      start <- as.numeric(position[1,2])+1
      end <- as.numeric(position2[1,1])-1
      Convergence_setting_str <- substr(file_str, start, end)
      Convergence_setting <- gsub("[^[:digit:]]", "", Convergence_setting_str)
      
      ###Get Convergence cycles completed
      position3 <- str_locate(file_str, "Number of cycles completed:")
      position4 <- str_locate(file_str, "Maximum parameter change")
      start2 <- as.numeric(position3[1,2])+1
      end2 <- as.numeric(position4[1,1])-1
      Convergence_completed_str <- substr(file_str, start2, end2)
      Convergence_completed <- gsub("[^[:digit:]]", "", Convergence_completed_str)       
      
      convergence[i] <- ifelse (Convergence_setting!=Convergence_completed, Convergence_completed, 0)
      
      table[i,4] <- convergence[i]
    } else {
      table[i,4] <- 0
    }
    
  }   
  
  
  ###########################################################################
  ###Add mirt::bfactor() results
  
  ###Check whether mirtpara.csv is existed, for checking convergence 
  table[,5] <- paste0(id.com,"-",skew.Fg.name,"-",skew.Fs.name,"-",seq(1:rep),"-mirtpara.csv")
  
  file.list.check.mirt <- list.files(pattern="*-mirtpara.csv")      
  table[which(table[,5] %in% file.list.check.mirt),6] <- "ok"     
  
  #####Here, we do not analysis convergence of mirtpara. 
  #####We have prepare convergence of mirtpara and store them in mirtpara.csv.
  for (i in 1:rep){
    ###Get table from existing mirtpara.csv files        
    if (table[i,6]=="ok"){
      mirtpara <- read_csv(paste0(data.folder.function,table[i,5]),show_col_types = FALSE)
      
      ###Get first convergence in mirtpara.csv as the convergence for this replication,
      ###because only one convergence status for all items in one replication.
      table[i,7] <- mirtpara$converge.iter[1]
      table[i,8] <- Factor
      table[i,9] <- I
      table[i,10] <- N
      table[i,11] <- skew.Fg.name
      table[i,12] <- skew.Fs.name
      table[i,13] <- i
    }
  }
  table.all.convergence<- rbind(table.all.convergence,table)      
  
  
  #   }#End of loop Fs
  # }#End of loop Fg
  return(table.all.convergence)  
}








######RUN########################################
##########Run Function Convergence()#############
#################################################
#Get a clear table.para.full for storing parameters.


#2024.1.21
table.all.convergence <- data.frame(matrix(NA,0,13))
colnames(table.all.convergence) <- c("id.rep","Flexirtfile","Flexirtready","Flexirtconvergence","mirtcsvfile","mirtcsvready","mirt::bfconvergence","Factor","I","N","Fg","Fs","rep")



Sys.time()
for (Factor in c(2)){
  for (I in c(50)){
    for (N in c(5000)){
      for (Fg in 1:2){
        for (Fs in 1:2){

          table.all.convergence <- Convergence(Factor, I, N, Fg, Fs, rep = 30)
        }
      }
    }
  }
}

Sys.time()


result.folder <- paste0(main.folder,"/Result20250517/")
dir.create(result.folder)

write.table(table.all.convergence, paste0(result.folder,"convergence.csv"),row.names = FALSE, sep=",")

