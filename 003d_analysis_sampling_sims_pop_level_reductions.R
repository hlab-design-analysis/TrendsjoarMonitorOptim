# ==============================
# Preparation graphs and tables of simulations results (Population level): reduction scenarios
# Nuno Prista, SLU Aqua, 2022 @ Konsekvensanalysis project
	# based on developments under Nuno Prista, SLU Aqua, 2019, 2022 @ KustMonitoring project
# ==============================

rm(list=ls()); graphics.off()

# auto: loads libraries
library(data.table)
library(xlsx)

# auto: loads functions
source("000_Funs/func_do_summary_stratified_mean_time_series.r")

# user: select site
site<-"Stensjön" 

# user: sets dir
dir_inputs_data<-"001_Inputs/prepared/"
dir_inputs_sim_res <- "003_SamplingSims/"
dir_outputs <- "003_SamplingSims/Analysis_Reduction/"; dir.create(dir_outputs, recursive=T, showWarnings=FALSE)

# user: select nsims (used to load sim results - should be the same as set in 003a)
	nsims<-5 # 10000

# auto: loads sample data (output from script 001)
	load(file=paste0(dir_inputs_data,site,".Rdata"))
# auto: loads simulations
	load(file=paste0(dir_inputs_sim_res, site,"_",nsims,"sims_res_pop_prep.RData", sep="")) 

# some formating
	boot_res_pop$SampSize<-factor(boot_res_pop$SampSize, levels=c('N','n21','n18','n15','n12'), ordered=T)
	boot_res_pop$Year<-as.numeric(as.character(boot_res_pop$Year))

# updates target_vars with lake level variables
target_vars<-c('AbCyW','BabborB','BgäddaB','BlakeB','BmörtB','BpiscAbbBNet','BtotalB','gmLabboB','gmLmörtB','MeanW','NabborB','Narter','NgäddaB','NlakeB','NmörtB','Nspecies','NtotalB','pCyp','pPiscPerc','SDn','SDw')

	# =====================================
	# Descriptive analysis - quantiles of boot_rse per variable*SampSize
	# =====================================	
	
		a<-droplevels(boot_res_pop[variable %in% target_vars,])
		do.call("rbind",tapply(a$boot_rse, a$SampSize, summary))
		ls1<-split(a, a$variable)
		ls2<-lapply(ls1, function(x){tapply(x$boot_rse, x$SampSize, summary) })
		lapply(ls2, function(x) do.call("rbind",x))


		# creates intermediate results
		out0<-round(tapply(boot_res_pop$boot_rse, list(boot_res_pop$variable, boot_res_pop$SampSize), function(x){quantile(x,probs=0.05, na.rm=T)}),1)
		out1<-round(tapply(boot_res_pop$boot_rse, list(boot_res_pop$variable, boot_res_pop$SampSize), mean, na.rm=T),1)
		out2<-round(tapply(boot_res_pop$boot_rse, list(boot_res_pop$variable, boot_res_pop$SampSize), median, na.rm=T),1)
		out3<-round(tapply(boot_res_pop$boot_rse, list(boot_res_pop$variable, boot_res_pop$SampSize), function(x){quantile(x,probs=0.95, na.rm=T)}),1)

		# creates and saves final summary of results
		a<-rbind(data.frame(Indicator = target_vars, CV = "min(5%)", out0[target_vars,], row.names=NULL), data.frame(Indicator = target_vars, CV = "median", out2[target_vars,], row.names=NULL), data.frame(Indicator = target_vars, CV = "max(95%)", out3[target_vars,], row.names=NULL))
		a$Indicator<-factor(a$Indicator, levels=target_vars)
		write.xlsx(t(a[order(a$Indicator),]), file= paste(dir_outputs,site , "_summary_pop_level.xlsx", sep=""), col.names = FALSE)
			

	# ===================	
	# Plot of Bootstrap CIs: Population level	
	# ===================	
		
		for (target_var in target_vars)
		{
		# use in debugging: target_var<-"BabborB"
		print(target_var)
			# subsets
			b<-boot_res_pop[variable==target_var,list(Year, SampSize, true_mean, true_clow_mean, true_chigh_mean, boot_mean, bootlow_aprox, boothigh_aprox, bootlow_perc, boothigh_perc)]
			b$id<-as.integer(b$Year)
			if(sum(!is.na(b$true_mean))>0) ylimite <- c(min(c(b$bootlow_perc, b$bootlow_aprox), na.rm=T), max(c(b$boothigh_perc,b$boothigh_aprox), na.rm=T)*1.3)
				windows(7,5)
				tit<- target_var
				ylabel <- "Indikatorvärde"
				plot(b$boothigh_perc~b$id, type="n", ylim=ylimite, ylab = ylabel, xlab="year", las=2, main=tit, xlim=c(min(b$id), max(b$id)+1))
				
			counter<-0
			for(sampsize in levels(b$SampSize))
			{
				print(sampsize)
				a<-b[SampSize==sampsize,]
				
				if(sum(!is.na(a$true_mean))>0)
				{
				if(counter==0) color<-"red" else color<-"blue"
				#inside_ids <- a[as.numeric(eval(paste(true_mean))) %between% list(clow_mean, chigh_mean),]$id
				#outside_ids <- a$id[!a$id %in% inside_ids]
				
				segments(x0 = a$id+counter, y0 = a$bootlow_perc, x1 = a$id+counter, y1 = a$boothigh_perc, col=color, lwd=2)
				#points(a$boot_mean~c(a$id+counter), col="red", pch=19, cex=0.5)
				points(a$true_mean~c(a$id+counter), col="black", pch=19, cex=0.5)
				counter <- counter+0.15
				} else print(".no data")
			}
				legend("topright", legend=c("uppmätt medelvärde","bootstrap konfidensintervall (presens)","bootstrap konfidensintervall (reducerade)"), lty=c(0,1,1), lwd=c(0,2,2), col=c("black","red","blue"), pch=c(19, -1, -1),cex=0.85)
				
				savePlot(paste(dir_outputs, paste(site, target_var, sep="_"), ".png", sep=""), type="png")

		}

graphics.off()


