# ==============================
# Simulations of stratified designs
# Nuno Prista, SLU Aqua, 2022 @ Konsekvensanalysis project
	# based on developments under Nuno Prista, SLU Aqua, 2019, 2022 @ KustMonitoring project
# ==============================

rm(list=ls()); gc()

library(data.table)
library(parallel) # detectCores
library(foreach) # parallel computing
library(doMC) # parallel computing
library(doRNG) # Generic Reproducible Parallel Backend for 'foreach' Loops

# sources required functions
source("000_Funs/func_do_summary_stratified_mean_time_series.r")
source("000_Funs/func_do_additional_calcs_slab.r", enc="UTF-8")
source("000_Funs/func_do_job.r")	
source("000_Funs/func_do_sampling_stratified.r")
source("000_Funs/func_do_sampling_stratified_timeseries.r")
source("000_Funs/func_sampleFromAGroup.r")
source("000_Funs/func_do_summary_stratified_mean.r")

# user: select site 
site <- "Stensjön"

# user: settings
	# user: number of simulations to run
	nsims <- 3 # 10000
	# cpus
		# info only: number of cpus available
		totcpus<-detectCores(all.tests = FALSE, logical = TRUE); print(totcpus)
		# user: number of cpus that should be used [defaults to half number available but can be changed by user]
		ncpus <- floor(totcpus/2) # can be set independently by user
		ncpus <- 1 # can be set independently by user

# user: sets dirs
	dir_inputs_main<-"001_Inputs/prepared"
	dir_inputs_sampsize<-"002_SampleSizes"
	dir_outputs<-"003_SamplingSims"; 
	if(!file.exists(dir_outputs)) dir.create(dir_outputs, showWarnings = FALSE, recursive=T)

# reads data
	load(file=paste0(dir_inputs_main,"/",site,".Rdata"))

# reads samp size table
	samp_size_table<-read.table(paste0(dir_inputs_sampsize,"/",site,"_sample_sizes_to_sim.txt"), header=T, sep="\t")

# ==================	
# calculates time series of indicator "true values" [using "do_summary_stratified_mean_time_series"]		
# ==================
	res_pop<-c()
	res_strata<-c()
		for (i in target_vars)
		{
		res_pop<-rbind(res_pop, do_summary_stratified_mean_time_series(x = dt_site, target_var = i, strata_var = "DepthStratum", strata_size_var = "NStations", period_var = "Year")$pop_res)
		res_strata<-rbind(res_strata, do_summary_stratified_mean_time_series(x = dt_site, target_var = i, strata_var = "DepthStratum", strata_size_var = "NStations", period_var = "Year")$stratum_res)
		}
		# adds special slab totals
		tmp<-rbindlist(lapply(split(dt_site, dt_site$Year), function(x) {
		  do_additional_calcs_slab(x)}), idcol="Year")
	res_pop <- rbind(res_pop, data.table(type="simple",variable=tmp$variable, period=tmp$Year, N=tmp$N, n=tmp$n, H=tmp$H, expect_strata=NA, samp_strata=NA, n_strata = NA, st_mean = tmp$st_mean, st_var_x=NA, st_var_mean=NA, st_se_mean=NA, st_rse_mean=NA, clow_mean=NA, chigh_mean=NA) )
	colnames(res_pop)[colnames(res_pop)=="period"]<-"Year"
	colnames(res_strata)[colnames(res_strata)=="period"]<-"Year"
	res_pop$ID<-paste(res_pop$variable, res_pop$Year); sum(duplicated(res_pop$ID))==0

# ==================
# additional preparations
# ==================

# creates sampling id	
	dt_site$sampId<-1:nrow(dt_site)	

# creates sampling strata
	dt_site[,YearDepthStratum:= paste(Year,DepthStratum),]
		
# set Sampling Design list (sampDes)	
	sampDes <- list (stratified = TRUE, strata_var = "YearDepthStratum", period_var="Year", samp_sizes = data.frame(YearDepthStratum = names(b<-table(dt_site$YearDepthStratum)), OrigSampSizes = c(b), row.names=NULL))

# set Sampling Options list 
	# prepares samp_size_table
	samp_size_table$YearDepthStratum<-paste(samp_size_table$Year, samp_size_table$DepthStratum)
	samp_size_table<-cbind(samp_size_table[ncol(samp_size_table)],samp_size_table[-ncol(samp_size_table)])
	# creates Sampling options (sampOpt)
	sampOpt<-list(n_sims = 1, stratified = TRUE, strata_var = "YearDepthStratum", period_var = "Year", samp_sizes = samp_size_table, 
				replacement = TRUE,  sample_all_available = TRUE, sample_all_available_warning = TRUE)	

# save settings
	ptc0<-Sys.time() # records system time [for documentation only]
	filename<-paste0(site,"_",nsims,"sims_settings.RData")
	save(target_vars, nsims, ncpus, site, sampOpt, ptc0, file=paste0(dir_outputs,"/",filename) )
		
# =======================================				
# bootstrapping - implemented in parallel 
# =======================================

# initiates parallel
registerDoMC(ncpus)
print(getDoParWorkers())

# starts the seeds
set.seed(123)

# runs simulations [some NA warnings may be issued associated to qt]
options(warn=0)
out <- foreach (i=1:nsims) %dorng% {
									do_job(lo=i)
									}

# process objects
sim_res_pop<-rbindlist(lapply(out, function(x) {x$sim_res_pop}), idcol=TRUE)
sim_res_strata<-rbindlist(lapply(out, function(x) {x$sim_res_strata}), idcol=TRUE)
sim_samples<-rbindlist(lapply(out, function(x) {x$sim_samples}), idcol=TRUE)

# =======================================
# final save of results
# =======================================
cat("saving RDS...\n")
saveRDS(sim_res_pop, file = paste0(dir_outputs,"/",site,"_",nsims,"sims_res_pop.rds"))			
saveRDS(sim_res_strata, file = paste0(dir_outputs,"/",site,"_",nsims,"sims_res_strata.rds"))
saveRDS(sim_samples, file = paste0(dir_outputs,"/",site,"_",nsims,"sims_sim_samples.rds"))		
