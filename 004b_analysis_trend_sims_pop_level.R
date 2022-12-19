# ==============================
# Graphs and tables of multi-anual trends
# Nuno Prista, SLU Aqua, 2022 @ Konsekvensanalysis project
	# based on developments under Nuno Prista, SLU Aqua, 2019, 2022 @ KustMonitoring project
# ==============================

rm(list=ls()); graphics.off()

library(data.table)
library(Kendall)

# user: select site 
site <- "Stensjön"	

# user: sets dir
dir_inputs_data<-"001_Inputs/prepared/"
dir_inputs_samp_sims <- "003_SamplingSims/"
dir_inputs_trend_sims <- "004_TrendSims/"
dir_outputs_trend_original <- "004_TrendSims/Analysis_Original/"; dir.create(dir_outputs_trend_original, recursive=T, showWarnings=FALSE)
dir_outputs_trend_reduction <- "004_TrendSims/Analysis_Reduction/"; dir.create(dir_outputs_trend_reduction, recursive=T, showWarnings=FALSE)

# user: define omdrev
target_omdrev<-c(1)
if(any(!target_omdrev %in% c(1:3))) print("omdrev not define")


# =======================	
# Loading
# =======================

	# loads original data	# dt_site		
		load(file=paste(dir_inputs_data,site,".Rdata",sep=""))
 
 	# loads simulations # res_pop, boot_res_pop, nsim
		load(file=paste(dir_inputs_samp_sims, site,"_3sims_res_pop_prep.rdata", sep="")) 
 
	# loads regression simulations
		res_omdrev1<-data.table(readRDS(file=paste(dir_inputs_trend_sims, site,"_omdrev_1_",nsim,"_regrsims_res_model.rds", sep="")))
		#res_omdrev2<-data.table(readRDS(file=paste(dir_inputs_trend_sims, site,"_omdrev_2_",nsim,"_regrsims_res_model.rds", sep="")))
		#res_omdrev3<-data.table(readRDS(file=paste(dir_sim_outputs, site, "_", scenario,"_omdrev_3_1000_regrsims_res_model.rds", sep="")))

	# some formatting
		target_vars <- c('AbCyW','BabborB','BgäddaB','BlakeB','BmörtB','BpiscAbbBNet','BtotalB','gmLabboB','gmLmörtB','MeanW','NabborB','Narter','NgäddaB','NlakeB','NmörtB','Nspecies','NtotalB','pCyp','pPiscPerc','SDn','SDn2','SDw')
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
		write.csv(res_trend, file=paste(dir_outputs_trend_original, site, "_origin_trend_",nsim,"_1000.csv", sep=""))
		
# =======================	
# Computes omdrev trend
# =======================
	
	# compiles results		
		res_omdrev1$model_linear_true_slope<-res_trend$model_linear_true_slope[match(res_omdrev1$variable, res_trend$variable)]
		res_omdrev1$model_linear_true_pvalue<-res_trend$model_linear_true_pvalue[match(res_omdrev1$variable, res_trend$variable)]
		res_omdrev1$model_linear_true_sign<-res_trend$model_linear_true_sign[match(res_omdrev1$variable, res_trend$variable)]
		res_omdrev1$model_kendall_true_tau<-res_trend$model_kendall_true_tau[match(res_omdrev1$variable, res_trend$variable)]
		res_omdrev1$model_kendall_true_pvalue<-res_trend$model_kendall_true_pvalue[match(res_omdrev1$variable, res_trend$variable)]
		res_omdrev1$model_kendall_true_sign<-res_trend$model_kendall_true_sign[match(res_omdrev1$variable, res_trend$variable)]
		
		res_omdrev2$model_linear_true_slope<-res_trend$model_linear_true_slope[match(res_omdrev2$variable, res_trend$variable)]
		res_omdrev2$model_linear_true_pvalue<-res_trend$model_linear_true_pvalue[match(res_omdrev2$variable, res_trend$variable)]
		res_omdrev2$model_linear_true_sign<-res_trend$model_linear_true_sign[match(res_omdrev2$variable, res_trend$variable)]
		res_omdrev2$model_kendall_true_tau<-res_trend$model_kendall_true_tau[match(res_omdrev2$variable, res_trend$variable)]
		res_omdrev2$model_kendall_true_pvalue<-res_trend$model_kendall_true_pvalue[match(res_omdrev2$variable, res_trend$variable)]
		res_omdrev2$model_kendall_true_sign<-res_trend$model_kendall_true_sign[match(res_omdrev2$variable, res_trend$variable)]

		#res_omdrev3$model_linear_true_slope<-res_trend$model_linear_true_slope[match(res_omdrev3$variable, res_trend$variable)]
		#res_omdrev3$model_linear_true_pvalue<-res_trend$model_linear_true_pvalue[match(res_omdrev3$variable, res_trend$variable)]
		#res_omdrev3$model_linear_true_sign<-res_trend$model_linear_true_sign[match(res_omdrev3$variable, res_trend$variable)]
		# res_omdrev3$model_kendall_true_tau<-res_trend$model_kendall_true_tau[match(res_omdrev3$variable, res_trend$variable)]
		# res_omdrev3$model_kendall_true_pvalue<-res_trend$model_kendall_true_pvalue[match(res_omdrev3$variable, res_trend$variable)]
		# res_omdrev3$model_kendall_true_sign<-res_trend$model_kendall_true_sign[match(res_omdrev3$variable, res_trend$variable)]

    
	compiled_res<-sapply(paste0("omdrev",target_omdrev), function(x) NULL)
		
		for (omdrev in paste0("omdrev",target_omdrev))
		{
		print(omdrev)
			assign("dt1",eval(parse(text=paste0("res_",omdrev))))
			table(dt1$SampSize)
			ordered_SampSizes<-	c("N",names(table(dt1$SampSize))[-1][order(as.numeric(gsub("n","",names(table(dt1$SampSize))[grepl(names(table(dt1$SampSize)), pat="n")])), decreasing=T)][-length(names(table(dt1$SampSize)))])
			dt1$SampSize<-factor(dt1$SampSize, levels=ordered_SampSizes, ordered=T)
			if(any(table(dt1$SampSize, dt1$variable)!=3)) stop("check: simulations missing")
		
		
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
				write.csv( out_linear_1, file = paste(dir_results, site, "_", scenario,"_identical_linear_",omdrev,"_",nsim,"_1000.csv", sep=""), row.names=FALSE)

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
# check target variables
				write.csv( out_kendall_1, file = paste(dir_results, site, "_", scenario,"_identical_kendall",omdrev,"_",nsim,"_1000.csv", sep=""), row.names=FALSE)

				# kendal NAs
				res3_k <- dt1[variable %in% target_vars, sum(is.na(same_kendall_trend_and_signif)), list(SampSize, variable)]; res3_k
				dcast(res3_k, SampSize~variable)

				compiled_res[[omdrev]]$linear<-out_linear_1
				compiled_res[[omdrev]]$kendall<-out_kendall_1

}


	# =======================	
	# Combined table
	# =======================


	combined_res_linear<-data.table(SampSize=compiled_res[[1]][["linear"]]$SampSize
	
	



