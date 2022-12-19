# ==============================
# Simulations of multi-anual trends
# Nuno Prista, SLU Aqua, 2022 @ Konsekvensanalysis project
	# based on developments under Nuno Prista, SLU Aqua, 2019, 2022 @ KustMonitoring project
# ==============================

rm(list=ls()); gc()

# loads packages
library(data.table)
library(Kendall)
  library(foreach) # parallel computing
  library(doMC) # parallel computing
  library(doRNG) # Generic Reproducible Parallel Backend for 'foreach' Loops

source("000_Funs/func_do_job_trend.r")

# settings
parallel::detectCores(all.tests = FALSE, logical = TRUE)
parallel_type <- "foreach_mc" # "foreach_mc" # "none"

# user: select site 
site <- "Stensjön"	

keep_objects <- c("keep_objects", "nsim", "cpus", "target_omdrev","sim_pop",
                 "site","ptc0", "dt0","dir_outputs","do_job_trend")	

 
# user: sets dirs
	dir_inputs_sim_res<-"003_SamplingSims/"
	dir_outputs<-"004_TrendSims/"; 
	#if(!file.exists(dir_outputs)) dir.create(dir_outputs, showWarnings = FALSE, recursive=T)

# user: define omdrev
target_omdrev<-c(1)
if(any(!target_omdrev %in% c(1:2))) print("omdrev not define")

 
  # loads simulation data
  sim_pop<-readRDS(file=paste(dir_inputs_sim_res, site, "_3sims_res_pop.rds", sep="")) 

# loads simulations
	load(file=paste(dir_inputs_sim_res, site, "_3sims_res_pop_prep.RData", sep="")) 
 
 
	# user: number of simulations to run
	nsim <- 3 # 1000
	# cpus
		# info only: number of cpus available
		n_cpus<-detectCores(all.tests = FALSE, logical = TRUE); print(n_cpus)
		# user: number of cpus that should be used [defaults to half number available but can be changed by user]
		cpus <- floor(n_cpus/2) # can be set independently by user
		cpus <- 1 # can be set independently by user
 
  
  # prepares dataset
  dt0 <- sim_pop[, `:=`(ID = paste(SampSize, variable)),]
  
  # removes unecessary object
  rm(sim_pop); gc()
  

  for(omdrev in target_omdrev)
  {  
    
    cat(paste("#===============================\n", "omdrev ", omdrev,"\n#===============================\n"))  
    
    ls1<-split(dt0, dt0$ID) 
    
      # initiates time counter
      ptc<-Sys.time()	
      
      # initiates parallel
      registerDoMC(cpus)
      
      # starts the seeds
      #set.seed(123)			
      # job
      
      res_model <- foreach (i=1:nsim, .options.RNG=123) %dorng% {
        #require(data.table)
        do_job_trend(lo=i, ls1 = ls1, omdrev)
      }
      gc()
      res_model<-rbindlist(res_model)
      # prints time counter
      ptc1<-Sys.time()-ptc
      cat(paste("#=================\n", "Elapsed time = ", ptc1, "\n#=================\n"))  
      
    cat("saving RDS...\n")
    ptc<-Sys.time()  	
    saveRDS(res_model, file = paste(dir_outputs,site,"_omdrev_",omdrev,"_", nsim,"_regrsims_res_model.rds",sep=""))		
    print(Sys.time()-ptc)			
    
    #Sys.sleep(180)
    rm(list=ls()[!ls() %in% keep_objects])
    gc()
    
  }
