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
		

	# calculates additional population-level indicators
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
	
	list(sim_res_pop = rbind(res_stratified$pop_res,pop_res_slab), 	sim_res_strata = res_stratified$stratum_res, sim_samples = sim_samples)
	
}
