# ==============================
# Simulations of multi-anual trends
# Nuno Prista, SLU Aqua, 2022 @ Konsekvensanalysis project
	# based on developments under Nuno Prista, SLU Aqua, 2019, 2022 @ KustMonitoring project
# ==============================

rm(list=ls()); gc()

# loads packages
library(data.table)
library(Kendall)
  library(parallel) # detectCores
  library(foreach) # parallel computing
  library(doMC) # parallel computing
  library(doRNG) # Generic Reproducible Parallel Backend for 'foreach' Loops

# auto: loads functions
source("000_Funs/func_do_job_trend.r")

# user: select site 
site <- "Stensjön"	

# user: sets dirs
	dir_inputs_sim_res<-"003_SamplingSims/"
	dir_outputs<-"004_TrendSims/"; 

# user: define sampling frequency (1 for annual, 2 for every other year)
target_sampfreq<-c(1,2)
if(any(!target_sampfreq %in% c(1:2))) print("omdrev not define")

# user: select nsims (used to load sim results - should be the same as set in 003a)
	nsims<-5 # 10000
 
  # auto: loads simulation data
  sim_pop<-readRDS(file=paste(dir_inputs_sim_res, site, "_",nsims,"sims_res_pop.rds", sep="")) 

# auto: loads simulations
	load(file=paste0(dir_inputs_sim_res, site,"_",nsims,"sims_res_pop_prep.RData", sep="")) 
 
# user: settings
	# user: number of simulations to run
	nsims_trend <- 5 # 10000
	# cpus
		# info only: number of cpus available
		totcpus<-detectCores(all.tests = FALSE, logical = TRUE); print(totcpus)
		# user: number of cpus that should be used [defaults to half number available but can be changed by user]
		ncpus <- floor(totcpus/2) # can be set independently by user
		#ncpus <- 1 # can be set independently by user
 
 # auto: defines a few objects to keep during memory clean ups
 keep_objects <- c("keep_objects", "nsims", "nsims_trend", "ncpus", "target_sampfreq","sim_pop",
                 "site","ptc0", "dt0","dir_outputs","do_job_trend")	
 
  # prepares dataset
  dt0 <- sim_pop[, `:=`(ID = paste(SampSize, variable)),]
  
  # removes unecessary object
  rm(sim_pop); gc()
  

  for(sampfreq in target_sampfreq)
  {  
    
    cat(paste("#===============================\n", "sampfreq ", sampfreq,"\n#===============================\n"))  
    
    ls1<-split(dt0, dt0$ID) 
    
      # initiates time counter
      ptc<-Sys.time()	
      
      # initiates parallel
      registerDoMC(ncpus)
      
     
      res_model <- foreach (i=1:nsims_trend, .options.RNG=123) %dorng% {
        do_job_trend(lo=i, ls1 = ls1, sampfreq)
      }
      gc()
      res_model<-rbindlist(res_model)
      # prints time counter
      ptc1<-Sys.time()-ptc
      cat(paste("#=================\n", "Elapsed time = ", ptc1, "\n#=================\n"))  
      
    cat("saving RDS...\n")
    ptc<-Sys.time()  	
    saveRDS(res_model, file = paste(dir_outputs,site,"_sampfreq",sampfreq,"_", nsims,"_", nsims_trend,"_trendsims_res_model.rds",sep=""))		
    print(Sys.time()-ptc)			
    
    #Sys.sleep(180)
    rm(list=ls()[!ls() %in% keep_objects])
    gc()
    
  }
