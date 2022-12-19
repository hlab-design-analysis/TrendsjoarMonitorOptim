# ==============================
# Preparation of simulations results (Population level)
# Nuno Prista, SLU Aqua, 2022 @ Konsekvensanalysis project
	# based on developments under Nuno Prista, SLU Aqua, 2019, 2022 @ KustMonitoring project
# ==============================

rm(list=ls()); gc()

library(data.table)

# sources required functions
source("000_Funs/func_do_summary_stratified_mean_time_series.r")
source("000_Funs/func_do_additional_calcs_slab.r", enc="UTF-8")

# user: select site
	site<-"Stensjön" 

# user: sets dir
	dir_inputs_data<-"001_Inputs/prepared/"
	dir_inputs_sim_res <- "003_SamplingSims/"
	dir_outputs_sim_prep <- "003_SamplingSims/"

# loads original data			
	load(file=paste0(dir_inputs_data,"/",site,".RData"))
 
# loads sim settings and simulated data (population)
	load(file=paste(dir_inputs_sim_res, site,"_3sims_settings.RData", sep=""))
	sim_pop<-readRDS(file=paste(dir_inputs_sim_res, site,"_3sims_res_pop.rds", sep="")) 

	# deletes unnecessary columns
		sim_pop <- sim_pop[,!c(".id")]
	# creates useful IDs
		sim_pop[, ID:=paste(Year, variable),]
		setkey(sim_pop, ID)

	# =============
	# Present ("true") estimates: calculation
	# =============
				
	print(".calculating present (true) estimates..")
				
	# runs stratified mean on site data set
		res_pop<-c()
		for (i in target_vars)
		{
		res_pop<-rbind(res_pop, do_summary_stratified_mean_time_series(x = dt_site, target_var = i, strata_var = "DepthStratum", strata_size_var = "NStations", period_var = "Year")$pop_res)
		}
		
		tmp<-rbindlist(lapply(split(dt_site, dt_site$Year), function(x) {
		  do_additional_calcs_slab(x)}), idcol="Year")
		res_pop <- rbind(res_pop, data.table(type="simple",variable=tmp$variable, period=tmp$Year, N=tmp$N, n=tmp$n, H=tmp$H, expect_strata=NA, samp_strata=NA, n_strata = NA, st_mean = tmp$st_mean, st_var_x=NA, st_var_mean=NA, st_se_mean=NA, st_rse_mean=NA, clow_mean=NA, chigh_mean=NA) )		
		
			# fixes columns
			colnames(res_pop)[colnames(res_pop)=="period"]<-"Year"
			# creates IDs
			res_pop$ID<-paste(res_pop$Year,res_pop$variable); sum(duplicated(res_pop$ID))==0
			# sets to data.table
			res_pop<-data.table(res_pop)
			# sets key
			setkey(res_pop, ID)

		# a look into the final structure of the objects
			res_pop[,.N, c("ID")]

			
	# =============
	# Present ("true") estimates: Bootstrap CIs
	# =============	
	
			print(".calculating Bootstrap CIs of present (true) estimates..")	
			
	# Calculates bootstrap CIs from replicates
			#bootlow_perc, boothigh_perc = percentile bootstrap centered on true mean
		
		sim_pop[, `:=`(true_mean = res_pop[sim_pop]$st_mean, true_var_x = res_pop[sim_pop]$st_var_x, true_var_mean = res_pop[sim_pop]$st_var_mean, true_clow_mean = res_pop[sim_pop]$clow_mean, true_chigh_mean = res_pop[sim_pop]$chigh_mean, true_rse_mean = res_pop[sim_pop]$st_rse_mean),]

		ls1<-split(sim_pop, paste(sim_pop$Year, sim_pop$SampSize, sim_pop$variable))
		ls2<-lapply(ls1, function(x){
							a<-quantile(x$mean, c(0.025, 0.975), na.rm=T); 
							b<-quantile(x$mean-mean(x$mean, na.rm=T), c(0.025, 0.975), na.rm=T); 
							ci <- x$true_mean[1]+b; 
							data.frame(Area= x$Area[1], Season= x$Season[1], Year = x$Year[1], variable=x$variable[1], SampSize = x$SampSize[1], 
											true_mean = x$true_mean[1], true_var_x =x$true_var_x[1], true_var_mean =x$true_var_mean[1],  true_rse_mean = x$true_rse_mean[1], true_clow_mean = x$true_clow_mean[1], true_chigh_mean = x$true_chigh_mean[1], 
											#boot_mean_n = mean(x$n_tot, na.rm=T), boot_mean = mean(x$mean, na.rm=T), boot_var_x = mean(x$var, na.rm=T), boot_var_mean=var(x$mean, na.rm=T), boot_se=sqrt(var(x$mean, na.rm=T)), boot_rse = round(sqrt(var(x$mean, na.rm=T))/x$true_mean[1]*100,1),
											boot_mean_n = mean(x$n_tot, na.rm=T), boot_mean = mean(x$mean, na.rm=T), boot_var_mean=var(x$mean, na.rm=T), boot_se=sqrt(var(x$mean, na.rm=T)), boot_rse = round(sqrt(var(x$mean, na.rm=T))/x$true_mean[1]*100,1),
													bootlow = a[1], boothigh = a[2], 
														bootlow_perc = ci[1], boothigh_perc = ci[2], 
															bootNA=sum(is.na(x$mean))) })
		boot_res_pop <- rbindlist(ls2)
		boot_res_pop$SampSize<-factor(boot_res_pop$SampSize)
		
		# clean up
		gc(); rm(ls1, ls2, sim_pop); gc()
		
		# adds bootstrap ci with normal approx (See article)
		boot_res_pop$bootlow_aprox<-boot_res_pop$true_mean-1.96*sqrt(boot_res_pop$boot_var_mean) 
		boot_res_pop$boothigh_aprox<-boot_res_pop$true_mean+1.96*sqrt(boot_res_pop$boot_var_mean)

			# a look into the final structure of the objects
			table(boot_res_pop$Year, boot_res_pop$SampSize, boot_res_pop$variable)
			#table(sim_samples[repl==1,]$Year, sim_samples[repl==1,]$SampSize)
			tapply(boot_res_pop$true_mean,list(boot_res_pop$Year, boot_res_pop$SampSize, boot_res_pop$variable), mean)

	# =============
	# Save prepared objects
	# =============	
			
		print(".saving objects...")		
		filename<-paste(site,"_",nsim,"sims_res_pop_prep.RData",sep="");
		save(boot_res_pop, res_pop, target_vars, nsim, cpus, site, sampOpt, file = paste0(dir_outputs,filename))			
		print(paste(dir_outputs_sim_prep,filename,sep=""))
		cat(".total time: ", Sys.time()-ptc0,"\n")


