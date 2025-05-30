# rm(list=ls())
# install.packages("SimMultiCorrData")
# install.packages("dplyr")
# install.packages("mirt")
# install.packages("parallel")
# install.packages("tidyr")
# install.packages("semTools")
# install.packages("moments")

library(SimMultiCorrData)
library(dplyr)
library(mirt)
library(parallel)
library(tidyr)
library(semTools)
library(moments)

# #main.folder <- "H:/BifactorNonNormality-PsychPC2"
# main.folder <- "C:/Users/julia/OneDrive - The University of Alabama/Research/2023_2_BifactorNonNormality"
# dir.create(main.folder)
# data.folder <- paste0(main.folder,"/Data/")
# dir.create(data.folder)


################################################################################
######### Simulate and estimate Data ###########################################
################################################################################
sim_rep <- function(vec){
  library(SimMultiCorrData)
  library(dplyr)
  library(mirt)
  library(parallel)
  library(tidyr)
  library(semTools)
  library(moments)
  

  Factor=as.numeric(vec["Factor"]) 
  I=as.numeric(vec["I"])
  N=as.numeric(vec["N"])
  Fg=as.numeric(vec["Fg"])
  Fs=as.numeric(vec["Fs"])
  rep.all=as.numeric(vec["rep"])
  
  main.folder <- "C:/Users/julia/OneDrive - The University of Alabama/Research/2023_2_BifactorNonNormality"
  data.folder <- paste0(main.folder,"/Data/")
  
  # Work folder setting - Factor, I, N
  data.2.folder <- paste0(data.folder, Factor, "-", I, "-", N, "/")
  dir.create(data.2.folder, recursive = TRUE, showWarnings = FALSE)
  
  # Setting of Skewness and Kurtosis
  skew <- c(0, 2)
  kurt <- c(0, 7)
  skew.cut <- c(.25, .25)
  kurt.cut <- c(1, 1)
  skewlevel <- data.frame(skew, kurt, skew.cut, kurt.cut) %>%
    mutate(name = paste0("Skew(", skew, ")Kurt(", kurt, ")"))
  
  skew.Fg <- skewlevel[Fg, 1]
  kurt.Fg <- skewlevel[Fg, 2]
  skew.Fs <- skewlevel[Fs, 1]
  kurt.Fs <- skewlevel[Fs, 2]
  skew.Fg.name <- skewlevel[Fg, "name"]
  skew.Fs.name <- skewlevel[Fs, "name"]
  skew.Fg.cut <- skewlevel[Fg, 3]
  kurt.Fg.cut <- skewlevel[Fs, 4]
  skew.Fs.cut <- skewlevel[Fg, 3]
  kurt.Fs.cut <- skewlevel[Fs, 4]
  
  data.3.folder <- paste0(data.2.folder, Factor, "-", I, "-", N, "-", skew.Fg.name, "-", skew.Fs.name, "/")
  dir.create(data.3.folder, recursive = TRUE, showWarnings = FALSE)
  
  
  # Setting of Skewness and Kurtosis
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

  # Factor=4
  # I=10
  # N=250
  # Fg=2
  # Fs=2
  # rep.all=10
  # data.2.folder <- paste0(data.folder,Factor,"-",I,"-",N,"/")
  
  # baseseed = 1234
  
  ############# parameter Setting ##############################################
  #a[,1] is discrimination of general factor (slope parameters in mirt::simdata)
  a <- matrix(rep(NA,Factor*I*(Factor+1)),Factor*I)
  a[,1] <- runif(Factor*I, 1.1, 2.8)
  
  #a[,2:K] is discrimination of specific factor (slope parameters in mirt::simdata)
  for (i in 2:(Factor+1)){
    a[((i-2)*I+1):((i-2)*I+I),i] <- runif(I,0,1.5)
  }
  
  #d is threshold (a matrix/vector of intercepts in mirt::simdata)
  #We have 4 categories with 3 thresholds
  d <- matrix(rep(NA,Factor*I*3),Factor*I)
  d[,3] <- runif(Factor*I,-2,-0.67)
  d[,2] <- runif(Factor*I,-0.67,0.67)
  d[,1] <- runif(Factor*I,0.67,2)
  ##############################################################################
  
  
  
  


  
  #### Simulate Theta and response data
  for (rep in 1:rep.all){
    
   # Initialize counter for successful replications
   successful_replications <- 0  
    
   # Adjusted loop to ensure total successful replications meet rep.all
   while(successful_replications == 0) {

    

        
    ###Simulate Theta  
    #item type - graded response model (GRM)
    items <- rep('graded', Factor*I)
        
        
    #Get latent traits of population on General Factor    
    tryR <- 10
    test <- TRUE
    theta <- matrix(NA,N,Factor+1)
        
        
    while(test){
      theta.Fg.sim <- semTools::mvrnonnorm(N,rep(0,tryR),diag(tryR),
                                            rep(skew.Fg,tryR),
                                            rep(kurt.Fg,tryR))
          
          
      loc <- which(abs(moments::skewness(theta.Fg.sim) - skew.Fg)<=skew.Fg.cut &
                  abs(moments::kurtosis(theta.Fg.sim) - 3- kurt.Fg)<=kurt.Fg.cut)
          
      if(length(loc)>0){
        theta[,1] <- theta.Fg.sim[,loc[1]]
        test <- FALSE
        }
      }
        
        
    #Get latent traits of population on Specific Factor 
    tryR <- 10
    test <- TRUE
        
    while(test){
      theta.Fs.sim <- semTools::mvrnonnorm(N,rep(0,tryR),diag(tryR),
                                               rep(skew.Fs,tryR),
                                               rep(kurt.Fs,tryR))
          
          
      loc <- which(abs(moments::skewness(theta.Fs.sim) - skew.Fs)<=skew.Fs.cut &
                      abs(moments::kurtosis(theta.Fs.sim) - 3- kurt.Fs)<=kurt.Fs.cut)
          
          
      if(length(loc)>=Factor){
        theta[,-1] <- theta.Fs.sim[,loc[1:(Factor)]]
        test <- FALSE
        }
      }
        
        
    ### Get response data
    dataset <- simdata(a, d, N, itemtype = items, Theta = theta)
        

        
    ############# Estimation ######################################### 
    #Define "specific" para for bfactor() 
    bfactor.index <- matrix(rep(NA,Factor*I),I)
    for (i in 1:Factor){
      bfactor.index[,i] <- rep(i,I)
      }
    bfactor.specific <- as.vector(bfactor.index)
        
      # Your existing code for simulating data and setting up for estimation
      
      # Simulate Theta, generate response data, and other setup steps
      
      # Estimation with mirt::bfactor
      mod <- mirt::bfactor(dataset, bfactor.specific, technical=list(NCYCLES=6000))
      
      #Calculate latent ability (theta.mirt)
      scores.map <- mirt::fscores(mod, method='MAP', max_theta=9)
      scores.ml <- mirt::fscores(mod, method='ML', max_theta=9)
      
      
      # Extract estimated parameters
      para <- coef(mod, simplify = TRUE)
      para2 <- para$items
      
      
      # Check if all estimated a parameters are positive
      if((!all(scores.map == 0) & !all(scores.ml == 0))) {
        # If yes, proceed with saving results
        
        # Increment successful replications counter
        successful_replications <- 1
        
        
        #If the result is converged, convergence time will be added, else "0" will be shown.
        converge.iter <- ifelse  (mod@OptimInfo[["converged"]],mod@OptimInfo[["iter"]],0)
        para3 <- cbind(para2,converge.iter)
        


        
        # Output pre-name for all replication
        namehead <- paste0(Factor,"-",I,"-",N,"-",skew.Fg.name,"-",skew.Fs.name,"-",rep)
        
        
        ### Output Simulation
        # write parameters of real file (from runif()) 
        parameters <- cbind(a,d)
        parameters.name <- paste0(data.3.folder, namehead, "-realparameters.csv")      
        write.table(parameters,file = parameters.name,sep=",", row.names=FALSE, col.names=FALSE)
        
        # write theta of real (population)  file    
        theta.name <- paste0(data.3.folder, namehead, "-realtheta.csv")
        write.table(theta,file = theta.name, sep=",",row.names=FALSE, col.names=FALSE)
        
        # write data (response scores) file
        data.name <- paste0(data.3.folder, namehead, "-data.csv")
        write.table(dataset,file = data.name, sep=",",row.names=FALSE, col.names=FALSE)
        
        
        
        ### Output Estimation
        mirtpara.name <- paste0(data.3.folder, namehead, "-mirtpara.csv")
        mirtmod.name <- paste0(data.3.folder, namehead, "-mirtmod.rds")
        mirttheta.map.name <- paste0(data.3.folder, namehead, "-mirttheta_map.csv")
        mirttheta.ml.name <- paste0(data.3.folder, namehead, "-mirttheta_ml.csv")
        
        
        #Write mirtpara.csv (Item parameters)
        write.csv(para3, mirtpara.name, na = "NA")
        
        #write mod
        saveRDS(mod, mirtmod.name)
        
        #Write mirttheta.csv (Person parameters)
        write.csv(scores.map, mirttheta.map.name, na = "NA")
        write.csv(scores.ml, mirttheta.ml.name, na = "NA")
        
        
        
        
        ### write normal distribution flexmirt syntax file
        syn.name <- paste0(data.3.folder,"Syn",Factor,"-",I,"-",N,"-",skew.Fg.name,"-",skew.Fs.name,"-",rep,".flexmirt")
        
        Flex.Constraints <- c()
        FC<- c()
        for (j in 1:Factor){
          FC <- paste0("Free(v",(j-1)*I+1,"-v",j*I,"), Slope(",j+1,");")
          Flex.Constraints <- paste(Flex.Constraints,FC)
        }
        
        flexsyn <- paste0("
        <Project>
          Title = '",syn.name,"';
        Description = 'Output from r';
        
        <Options>
          Mode = Calibration;
          saveSCO = Yes;
          Score = MAP;
          GOF = Complete;
          Quadrature = 21,3.5;
          Processors = 8;
          SavePRM = Yes;
          MaxE = 2000;
        
        <Groups>
          %Group1%
          File = '",data.name,"';
          Varnames = v1-v",Factor*I,";
          N = ",N,";
          Ncats(v1-v",Factor*I,") = 4;
          Model(v1-v",Factor*I,") = graded(4);
          Dimensions = ",(Factor+1),";
          Primary = 1;
        
        <Constraints>
          Fix(v1-v",Factor*I,"), Slope;
          Free(v1-v",Factor*I,"), Slope(1);",
                          Flex.Constraints)
        
        writeLines(flexsyn, syn.name)
        
        

        
        
        
      } else {
        # If any a parameter is not positive, do not save the results and do not increment the counter
        # The loop will continue attempting until successful_replications equals rep.all
      }
      
      # Additional steps as necessary, e.g., adjusting file names or paths to save unique results per successful replication
      
    }
    
  }# End of sloop of rep
  ############# End of Simulate response data ############################                

}




################################################################################
########################### Multi-Core Function ################################
################################################################################
sim_bf <- function(Factor, I, N, Fg, Fs, rep, cores) {

  
  # Run function in parallel
  cl <- makeCluster(cores)
  on.exit(stopCluster(cl))
  
  # Prepare data for parallel execution
  params <- expand.grid(Factor = Factor, I = I, N = N, Fg = Fg, Fs = Fs, rep = rep, stringsAsFactors = FALSE)

  
  # Export necessary variables and functions to the cluster
  clusterExport(cl, varlist = c("sim_rep", "params"))
  
  # Execute in parallel
  parLapply(cl, split(params, seq(nrow(params))), sim_rep)
}


################################################################################
############################## Log of Running ##################################
################################################################################
###5/15/2025
Factor = c(2)
I = c(50)
N = c(5000)
Fg = c(2)
Fs = c(1)
rep = 30
cores = parallel::detectCores () -14

now <- Sys.time()
sim_bf(Factor, I, N, Fg, Fs, rep, cores)
Sys.time()-now



###1/25/2024
Factor = c(2,4)
I = c(5,10,20)
N = c(250,500,1000)
Fg = c(1,2)
Fs = c(1,2)
rep = 1000
cores = parallel::detectCores () -2


now <- Sys.time()
sim_bf(Factor, I, N, Fg, Fs, rep, cores)
Sys.time()-now

# 
# Factor=2
# I=5
# N=250
# Fg=1
# Fs=1
# rep=100



