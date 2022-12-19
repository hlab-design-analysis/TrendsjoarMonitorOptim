# ==============================
# Graphs and tables of multi-anual trends
# Nuno Prista, SLU Aqua, 2022 @ Konsekvensanalysis project
	# based on developments under Nuno Prista, SLU Aqua, 2019, 2022 @ KustMonitoring project
# ==============================

rm(list=ls()); graphics.off()

# loads packages
library(data.table)
library(Kendall)

# user: select site 
site <- "Stensjön"	

# user: sets dir
dir_inputs_data<-"001_Inputs/prepared/"
dir_inputs_sampling_sim_res <- "003_SamplingSims/"
dir_inputs_trend_sims <- "004_TrendSims/"
dir_outputs_trend_original <- "004_TrendSims/Analysis_Original/"; dir.create(dir_outputs_trend_original, recursive=T, showWarnings=FALSE)
dir_outputs_trend_reduction <- "004_TrendSims/Analysis_Reduction/"; dir.create(dir_outputs_trend_reduction, recursive=T, showWarnings=FALSE)

# user: define sampling frequency (1 for annual, 2 for every other year)
target_sampfreq<-c(1)
if(any(!target_sampfreq %in% c(1:2))) print("omdrev not define")

# user: select nsims and nsims_trend (used to load sim results - should be the same as set in 004a)
	nsims<-5 # 10000
	nsims_trend<-5 # 10000

# =======================	
# Loading
# =======================

# auto: loads sample data (output from script 001)
	load(file=paste0(dir_inputs_data,site,".Rdata"))
 
# auto: loads simulations
	load(file=paste0(dir_inputs_sampling_sim_res, site,"_",nsims,"sims_res_pop_prep.RData", sep="")) 
 
# auto: loads regression simulations
	if(1 %in% target_sampfreq) res_sampfreq1<-data.table(readRDS(file=paste(dir_inputs_trend_sims, site,"_sampfreq1_",nsims,"_", nsims_trend,"_trendsims_res_model.rds", sep="")))
	if(2 %in% target_sampfreq) res_sampfreq2<-data.table(readRDS(file=paste(dir_inputs_trend_sims, site,"_sampfreq2_",nsims,"_", nsims_trend,"_trendsims_res_model.rds", sep="")))

	# some formatting
		target_vars <- c('AbCyW','BabborB','BgäddaB','BlakeB','BmörtB','BpiscAbbBNet','BtotalB','gmLabboB','gmLmörtB','MeanW','NabborB','Narter','NgäddaB','NlakeB','NmörtB','Nspecies','NtotalB','pCyp','pPiscPerc','SDn','SDw')
		res_pop$Year<-as.integer(as.character(res_pop$Year))
	
# =======================	
# Computes original ("true") trend
# =======================
	
	# assigns true slope
	ls1<-split(res_pop, res_pop$variable)
	ls2<-lapply(ls1, function(x, site1 = site)
			{
			model_linear<-lm(st_mean~Year,data=x)
			model_Kendall<-Kendall(x$st_mean,x$Year)
			x<-data.frame(Area = site1, variable = x$variable[1], 
							model_linear_true_slope = coef(model_linear)[2], 
							model_linear_true_pvalue = summary(model_linear)[[4]][8], 
							model_linear_true_sign = summary(model_linear)[[4]][8]<0.05,
							model_kendall_true_tau = model_Kendall[[1]][[1]], 
							model_kendall_true_pvalue = model_Kendall[[2]][[1]], 
							model_kendall_true_sign = model_Kendall[[2]][[1]]<0.05)								
			x
			})
		res_trend<-do.call("rbind",ls2)
		write.xlsx(res_trend, file=paste0(dir_outputs_trend_original, site, "_origin_trend_",nsims,"_",nsims_trend,"_trendsims.xlsx"), col.names = TRUE)
		
# =======================	
# Compiles "true" trend
# =======================
	
	# compiles results
		if(1 %in% target_sampfreq){
		res_sampfreq1$model_linear_true_slope<-res_trend$model_linear_true_slope[match(res_sampfreq1$variable, res_trend$variable)]
		res_sampfreq1$model_linear_true_pvalue<-res_trend$model_linear_true_pvalue[match(res_sampfreq1$variable, res_trend$variable)]
		res_sampfreq1$model_linear_true_sign<-res_trend$model_linear_true_sign[match(res_sampfreq1$variable, res_trend$variable)]
		res_sampfreq1$model_kendall_true_tau<-res_trend$model_kendall_true_tau[match(res_sampfreq1$variable, res_trend$variable)]
		res_sampfreq1$model_kendall_true_pvalue<-res_trend$model_kendall_true_pvalue[match(res_sampfreq1$variable, res_trend$variable)]
		res_sampfreq1$model_kendall_true_sign<-res_trend$model_kendall_true_sign[match(res_sampfreq1$variable, res_trend$variable)]
		}
		if(2 %in% target_sampfreq){
		res_sampfreq2$model_linear_true_slope<-res_trend$model_linear_true_slope[match(res_sampfreq2$variable, res_trend$variable)]
		res_sampfreq2$model_linear_true_pvalue<-res_trend$model_linear_true_pvalue[match(res_sampfreq2$variable, res_trend$variable)]
		res_sampfreq2$model_linear_true_sign<-res_trend$model_linear_true_sign[match(res_sampfreq2$variable, res_trend$variable)]
		res_sampfreq2$model_kendall_true_tau<-res_trend$model_kendall_true_tau[match(res_sampfreq2$variable, res_trend$variable)]
		res_sampfreq2$model_kendall_true_pvalue<-res_trend$model_kendall_true_pvalue[match(res_sampfreq2$variable, res_trend$variable)]
		res_sampfreq2$model_kendall_true_sign<-res_trend$model_kendall_true_sign[match(res_sampfreq2$variable, res_trend$variable)]
		}

    
	compiled_res<-sapply(paste0("sampfreq",target_sampfreq), function(x) NULL)
		
		for (sampfreq in paste0("sampfreq",target_sampfreq))
		{
		print(sampfreq)
			assign("dt1",eval(parse(text=paste0("res_",sampfreq))))
			table(dt1$SampSize)
			ordered_SampSizes<-	c("N",names(table(dt1$SampSize))[-1][order(as.numeric(gsub("n","",names(table(dt1$SampSize))[grepl(names(table(dt1$SampSize)), pat="n")])), decreasing=T)][-length(names(table(dt1$SampSize)))])
			dt1$SampSize<-factor(dt1$SampSize, levels=ordered_SampSizes, ordered=T)
			# check on variable order (should yield TRUE)
			if(any(table(dt1$SampSize, dt1$variable)!=nsims_trend)) stop("check: simulations missing")
		
		
			# biases and variances
				dt1$same_linear_trend <- (dt1$model_linear_true_slope <= 0 & dt1$linear_slope <= 0) | (dt1$model_linear_true_slope > 0 & dt1$linear_slope > 0)
				dt1$same_linear_trend_and_signif <- ( (dt1$model_linear_true_slope <= 0 & dt1$linear_slope <= 0 ) | (dt1$model_linear_true_slope > 0 & dt1$linear_slope > 0) ) & dt1$model_linear_true_sign==dt1$linear_sign

				dt1$same_kendall_trend <- (dt1$model_kendall_true_tau <= 0 & dt1$kendall_tau <= 0) | (dt1$model_kendall_true_tau > 0 & dt1$kendall_tau > 0)
				dt1$same_kendall_trend_and_signif <- ( (dt1$model_kendall_true_tau <= 0 & dt1$kendall_tau <= 0 ) | (dt1$model_kendall_true_tau > 0 & dt1$kendall_tau > 0) ) & dt1$model_kendall_true_sign==dt1$kendall_sign

				
				# same general linear trend
				res1 <- dt1[variable %in% target_vars, sum(same_linear_trend, na.rm=T), list(SampSize, variable)]; res1
				dcast(res1, SampSize~variable)
				
				# same general linear trend and significance
				res2 <- dt1[variable %in% target_vars, sum(same_linear_trend_and_signif, na.rm=T), list(SampSize, variable)]; res2
				out_linear_1<-dcast(res2, SampSize~variable)
				
				out_linear_1 <- rbind(data.frame("slope",t(round(res_trend[rownames(res_trend) %in% target_vars,3],5))),
								data.frame("slope_signif",t(as.character(res_trend[rownames(res_trend) %in% target_vars,5]))),out_linear_1,use.names=FALSE)
				colnames(out_linear_1) <- c("NA",target_vars)
				write.xlsx( out_linear_1, file = paste0(dir_outputs_trend_reduction, site, "_identical_linear_",sampfreq,"_",nsims,"_",nsims_trend,".xlsx"), row.names=FALSE)

				# linear NAs
				res3 <- dt1[variable %in% target_vars, sum(is.na(same_linear_trend_and_signif)), list(SampSize, variable)]; res3
				dcast(res3, SampSize~variable)

				# same general kendall trend
				res1_k <- dt1[variable %in% target_vars, sum(same_kendall_trend, na.rm=T), list(SampSize, variable)]; res1_k
				dcast(res1_k, SampSize~variable)
				
				# same general kendall trend and significance
				res2_k <- dt1[variable %in% target_vars, sum(same_kendall_trend_and_signif, na.rm=T), list(SampSize, variable)]; res2_k
				out_kendall_1<-dcast(res2_k, SampSize~variable)

				out_kendall_1 <- rbind(data.frame("tau",t(round(res_trend[rownames(res_trend) %in% target_vars,6],5))),
								data.frame("tau_signif",t(as.character(res_trend[rownames(res_trend) %in% target_vars,8]))),out_kendall_1,use.names=FALSE)
				colnames(out_kendall_1) <- c("NA",target_vars)
				write.xlsx( out_kendall_1, file = paste0(dir_outputs_trend_reduction, site, "_identical_kendall_",sampfreq,"_",nsims,"_",nsims_trend,".xlsx"), row.names=FALSE)

				# kendal NAs
				res3_k <- dt1[variable %in% target_vars, sum(is.na(same_kendall_trend_and_signif)), list(SampSize, variable)]; res3_k
				dcast(res3_k, SampSize~variable)

				compiled_res[[sampfreq]]$linear<-out_linear_1
				compiled_res[[sampfreq]]$kendall<-out_kendall_1

}


	# =======================	
	# Combined table
	# =======================


	combined_res_linear<-data.table(SampSize=compiled_res[[1]][["linear"]]$SampSize
	
	



