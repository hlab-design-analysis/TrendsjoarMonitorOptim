do_job_sampling <- function(lo){				

# wrapper to simulate sampling
# Nuno Prista, SLU Aqua, 2019 @  Kustmonitoring
		
				
# simulates sampling
	dt_site<-as.data.frame(dt_site)
	sim_sampId <- do_sampling_stratified_timeseries(sampDes, sampOpt, dataset = dt_site, sampId = "sampId")

# fetches sampled rows from dataset based on sampId + creates variable with "SampSizeName"
	sim_samples<-lapply(sim_sampId, function(x, a1 = dt_site) lapply( x, function (y, a2 = a1) {y <- as.data.table(data.frame(SampSize = y[1], a2[y[-1],])) } ) )

# separates replicates into years
		# [[1]] - SampSize being tested
		# [[2]] - Replicate
		# [[3]] - Year
		sim_samples<-lapply(sim_samples, function(x) lapply(x, function(y) split(y, y$Year)))
			# demonstration
			# table(sim_samples[[1]][[1]][[3]][,c("Year","DepthStratum")])
			# table(sim_samples[[2]][[1]][[3]][,c("Year","DepthStratum")])
			# table(sim_samples[[2]][[1]][[1]][,c("Year","DepthStratum")])
	rm(sim_sampId, dt_site)
	gc()
# calculates the mean and its statistics
	#target_vars<-c('Cod38N','CodN','CyprinidsB','CyprinidsN','Eel45N','EelN','EelpoutN','Fish30N','Fish40N','Flounder30N','FlounderN','HerringN','MesopredatorsN','Perch25N','PerchB','PerchN','PikeN','Pikeperch40N','PikeperchN','Piscivores30N','PiscivoresN','TotalcatchN','WhitefishN')
	# target_vars<-c('CyprinidsB','CyprinidsN','PerchB','PerchN','TotalcatchN','WhitefishN')
	# target_vars<-c('CyprinidsB','CyprinidsN')
	# target_vars<-c('CodN','CyprinidsB','CyprinidsN','EelN','EelpoutN','FlounderN','HerringN','MesopredatorsN','PerchB','PerchN','PikeN','PikeperchN','PiscivoresN','WhitefishN')

	# simple mean of all stations
		# the following lapply runs "do_summary_simple_mean" on each of target_vars
	
		# res_simple<-lapply(sim_samples, function(y) { lapply(y, function (y1, target_vars1 = target_vars){

		# do.call("rbind",lapply(target_vars1, function(x, x1 = y1)  res = data.table(SampSize = x1$SampSize[1], do_summary_simple_mean (x = x1, target_var=x)$pop_res) ))

		# } )})



		# stratified mean of all stations
		# the following lapply runs "do_summary_stratified_mean" on each of target_vars
		# res_stratified<-lapply(sim_samples, function(z) { z<-lapply(z, function(y) {y<-lapply(y, function (y1, target_vars1 = target_vars){

		# a<- lapply(target_vars1, function(x, x1 = y1)  
							# {
							# a<-do_summary_stratified_mean(x = x1, target_var = x, strata_var = "DepthStratum", strata_size_var = "NStations", klab=TRUE)

							# res = list(
									# pop_res = data.table(Area = x1$Area[1], Season = x1$Season[1], Year = x1$Year[1], SampSize = x1$SampSize[1], repl = lo, a$pop_res), 
									# stratum_res = data.table(Area = x1$Area[1], Season = x1$Season[1], Year = x1$Year[1], SampSize = x1$SampSize[1], repl = lo, a$stratum_res)
									# )
							# res		
							# } 
					# )
			# a <- list(pop_res = rbindlist(lapply(a, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(a, function(x) x<-x$stratum_res)))
		# a
		
		# });
		# y<-list(pop_res = rbindlist(lapply(y, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(y, function(x) x<-x$stratum_res)))
		# y
		
		# })

		# z<-list(pop_res = rbindlist(lapply(z, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(z, function(x) x<-x$stratum_res)))
		# })
		# res_stratified<-list(pop_res = rbindlist(lapply(res_stratified, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(res_stratified, function(x) x<-x$stratum_res)))
		

		# stratified mean of all stations
		# the following lapply runs "do_summary_stratified_mean" on each of target_vars
		res_stratified<-lapply(sim_samples, function(z) { z<-lapply(z, function(y) {y<-lapply(y, function (y1, target_vars1 = target_vars){

		a<- lapply(target_vars1, function(x, x1 = y1)  
							{
							a<-do_summary_stratified_mean(x = x1, target_var = x, strata_var = "DepthStratum", strata_size_var = "N")
              
							res = list(
									pop_res = data.table(Area = x1$Area[1], Season = x1$Season[1], Year = x1$Year[1], SampSize = x1$SampSize[1], repl = lo, a$pop_res), 
									stratum_res = data.table(Area = x1$Area[1], Season = x1$Season[1], Year = x1$Year[1], SampSize = x1$SampSize[1], repl = lo, a$stratum_res)
									)
							res		
							} 
					)
			list(pop_res = rbindlist(lapply(a, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(a, function(x) x<-x$stratum_res)))

		
		})
		list(pop_res = rbindlist(lapply(y, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(y, function(x) x<-x$stratum_res)))
		})
		list(pop_res = rbindlist(lapply(z, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(z, function(x) x<-x$stratum_res)))
		})
		res_stratified<-list(pop_res = rbindlist(lapply(res_stratified, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(res_stratified, function(x) x<-x$stratum_res)))

	res_stratified$pop_res$repl<-lo
	res_stratified$stratum_res$repl<-lo
		
	#sim_res_pop <- do.call("rbind", lapply(out, function(x0){do.call("rbind",lapply(x0$res, function(x){y<- do.call("rbind",lapply(x, function(x1) { y1<-do.call("rbind",lapply(x1,function(x2){y2<-do.call("rbind",lapply(x2,function(x3){y3<-x3$pop_res; y3}));y2}));y1}));y}))}))
	#sim_res_strata <- do.call("rbind", lapply(out, function(x0){do.call("rbind",lapply(x0$res, function(x){y<- do.call("rbind",lapply(x, function(x1) { y1<-do.call("rbind",lapply(x1,function(x2){y2<-do.call("rbind",lapply(x2,function(x3){y3<-x3$stratum_res; y3}));y2}));y1}));y}))}))
	# sim_res_pop <- rbindlist(lapply(res_stratified, function(x){y<- do.call("rbind",lapply(x, function(x1) { y1<-do.call("rbind",lapply(x1,function(x2){y2<-do.call("rbind",lapply(x2,function(x3){y3<-x3$pop_res; y3}));y2}));y1}));y}),idcol=TRUE)
	# sim_res_pop$repl<-lo

	# sim_res_strata <- rbindlist(lapply(res_stratified, function(x){y<- do.call("rbind",lapply(x, function(x1) { y1<-do.call("rbind",lapply(x1,function(x2){y2<-do.call("rbind",lapply(x2,function(x3){y3<-x3$stratum_res; y3}));y2}));y1}));y}),idcol=TRUE)
	# sim_res_strata$repl<-lo

	# 20220914: slab: calculates special population totals
	tmp<-rbindlist(lapply(sim_samples, function(y) {
	                    w<-y
	                    z<-lapply(w, function(u) {u1<-rbindlist(u, idcol="Year"); 
	                                              ls1<-split(u1, u1$Year)
	                                              ls2<-lapply(ls1, do_additional_calcs_slab)
	                                              rbindlist(ls2, idcol="Year")})
	                    z<-rbindlist(z, idcol="repl")
	                    z}), idcol="SampSize")
	pop_res_slab<-data.table(Area=res_stratified$pop_res$Area[1], Season = "NotApplicable", Year=tmp$Year, SampSize=tmp$SampSize,  repl=tmp$repl,  type="simple",variable=tmp$variable, expect_strata="NotApplicable", samp_strata="NotApplicable", n_tot=tmp$N, n_strata="NotApplicable", mean = tmp$st_mean, var_mean=NA, se_mean=NA, rse_mean=NA, tdist=NA, error_margin=NA, clow_mean=NA, chigh_mean=NA)

	sim_samples <- rbindlist(lapply(sim_samples, function(y) {w<-rbindlist(y[[1]],idcol=FALSE); w}),idcol=FALSE)
			sim_samples$repl<-lo
	#res1 <- rbindlist(lapply(res_stratified, function(x){rbindlist( lapply(x, function(y) {rbindlist (y, idcol="Year") } ), idcol="repl") }))
	
	#	20220914: adapted to s-lab special population totals	
	list(sim_res_pop = rbind(res_stratified$pop_res,pop_res_slab), 	sim_res_strata = res_stratified$stratum_res, sim_samples = sim_samples)
	
}
