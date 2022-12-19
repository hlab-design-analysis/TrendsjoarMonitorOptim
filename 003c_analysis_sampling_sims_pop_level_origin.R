# ==============================
# Preparation graphs and tables of simulations results (Population level): original sample size
# Nuno Prista, SLU Aqua, 2022 @ Konsekvensanalysis project
	# based on developments under Nuno Prista, SLU Aqua, 2019, 2022 @ KustMonitoring project
# ==============================

rm(list=ls()); graphics.off()

# auto: loads libraries
library(data.table)

# auto: loads functions
source("000_Funs/func_do_summary_stratified_mean_time_series.r")

# user: select site
site<-"Stensjön" 

# user: sets dir
dir_inputs_data<-"001_Inputs/prepared/"
dir_inputs_sim_res <- "003_SamplingSims/"
dir_outputs <- "003_SamplingSims/Analysis_Original/"; dir.create(dir_outputs, recursive=T, showWarnings=FALSE)

# user: select nsims (used to load sim results - should be the same as set in 003a)
	nsims<-5 # 10000

# auto: loads sample data (output from script 001)
	load(file=paste0(dir_inputs_data,site,".Rdata"))
# loads simulations
	load(file=paste0(dir_inputs_sim_res, site,"_",nsims,"sims_res_pop_prep.RData", sep="")) 

# some formating
boot_res_pop$Year<-as.numeric(as.character(boot_res_pop$Year))

# updates target_vars with lake level variables
target_vars<-c('AbCyW','BabborB','BgäddaB','BlakeB','BmörtB','BpiscAbbBNet','BtotalB','gmLabboB','gmLmörtB','MeanW','NabborB','Narter','NgäddaB','NlakeB','NmörtB','Nspecies','NtotalB','pCyp','pPiscPerc','SDn','SDw')


# ===================	
# Plot of Original and Bootstrap estimates and CIs: Population level	
# ===================	
	
		for (target_var in target_vars)
		{
		print(target_var)
			# subsets
			b<-boot_res_pop[variable==target_var,list(Year, SampSize, true_mean, true_clow_mean, true_chigh_mean, boot_mean, bootlow_aprox, boothigh_aprox, bootlow_perc, boothigh_perc)]
			b$id<-as.integer(as.character(b$Year))
			
			# graphs
				if(sum(!is.na(b$true_mean))>0) ylimite <- c(min(c(b$bootlow_perc), na.rm=T), max(c(b$boothigh_perc), na.rm=T)*1.3)
					windows(7,5)
					tit<-paste(target_var)
					plot(b$boothigh_perc~b$id, type="n", ylim=ylimite, ylab="", xlab="", las=2, main=tit, xlim=c(min(as.numeric(boot_res_pop$Year)), max(as.numeric(boot_res_pop$Year))+1), cex.axis=1.2, cex.lab=1.2)
					if(max(b$boothigh_perc)<1000) mtext("Indikatorvärde", side = 2, line = 3) else mtext("Indikatorvärde", side = 2, line = 3.3)
				counter<-0
				for(sampsize in c("N"))
				{
					a<-b[SampSize==sampsize,]
					
					if(sum(!is.na(a$true_mean))>0)
					{
					segments(x0 = a$id+counter, y0 = a$bootlow_perc, x1 = a$id+counter, y1 = a$boothigh_perc, col="blue", lwd=2)
					points(a$boot_mean~c(a$id+counter), col="blue", pch=19, cex=0.8)
					points(a$true_mean~c(a$id+counter), col="black", pch=4, cex=0.8)
					counter <- counter+0.08
					} else print(".no data")
				}

				legend("topright", legend=c("bootstrap medelvärde","uppmätt medelvärde","bootstrap konfidensintervall"), lty=c(0,0,1), lwd = c(0,0,2), col=c("blue","black","blue"), pch=c(19, 4, -1),cex=0.9)
			# save plot	
				savePlot(paste(dir_outputs, paste(site, target_var, sep="_"), ".png", sep=""), type="png")
		
				}
		graphics.off()
	
	