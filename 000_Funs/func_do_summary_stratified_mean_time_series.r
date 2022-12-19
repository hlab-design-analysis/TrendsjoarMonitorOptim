do_summary_stratified_mean_time_series <- function (x, target_var, strata_var, strata_size_var, period_var, pop_confint_type="tdist"){

	# calculates stratified mean statistics
	# Nuno Prista, SLU Aqua, Sweden
	# Developed @ KustMonitoring Review 2019
				
	#x is a dataframe values with 
		# target values in target_var variable
		# a strata_var variable
		# a strata_size variable
		# period_var is a variable with the time series (e.g., Year, Year*Quarter)
		# strata_to_keep is a vector of strata to be used in computation of target_var
		# pop_confint_type defines type of confidence_interval to use in pop level [ATTT: strata level is not changed, always fixed on qt(0.975,n-1)]
        # "tdist" [default]: uses qt(0,975, n-H) when determining pop level confidence interval
        # "conservative: uses 2*se when determining pop level confidence interval
		
  #ATT: note that fpc is not included in the variance formula [does not apply where pseudo strata sizes are in use]
 
  
	require(data.table)

	if(!is.data.table(x) & !is.data.frame(x)) stop ("x must be data.frame or data.table")
	if(!is.data.table(x)) x<-data.table(x)

# ==========================
# Stratified (st) results
# ==========================
	values<-parse(text=target_var)
	stratum<-parse(text=strata_var)
	size<-parse(text=strata_size_var)
	period<-parse(text=period_var)
	
	strata_to_keep<-unique(x[,eval(stratum)])
		
	if (is.null(period_var)) { stop("must define period var\n")}

	if (!is.null(period_var))
		{

		# stratified variance
			# strata summary
				b1<-x[,list(stratum_size=eval(size)[1], variable = target_var, n=sum(!is.na(eval(values))), mean_x = mean(eval(values), na.rm=TRUE ), var_x = var(eval(values), na.rm=TRUE ), var_mean_x = var(eval(values), na.rm=TRUE )/sum(!is.na(eval(values))), se_mean_x = sd(eval(values), na.rm=TRUE )/sqrt(sum(!is.na(eval(values))))),list(eval(period), eval(stratum))][order(period, stratum)]
				if(!any(b1$n<=1)) # condition to handle situations where only 1 or 0 effective samples exists
				    { 
				  		b1[, `:=`(rse_mean_x = round(se_mean_x/mean_x*100,1), clow_mean = mean_x-qt(0.975,n-1)*se_mean_x, chigh_mean = mean_x+qt(0.975,n-1)*se_mean_x),]
				} else {
				  b1[, `:=`(rse_mean_x = round(se_mean_x/mean_x*100,1), clow_mean = NA, chigh_mean = NA),]
				}

			# stratified mean, stratified variance and stratified variance of the mean
				# note: stratified variance (according to http://www.math.chalmers.se/Stat/Grundutb/CTH/mve155/1617/chapter7.pdf)
				b2<-b1[, list( N=sum(stratum_size), n=sum(n), H = length(stratum), expect_strata = paste(strata_to_keep, collapse=","), samp_strata = paste(unique(stratum), collapse=","), n_strata = paste(n, collapse=","), st_mean = sum(mean_x*stratum_size/sum(stratum_size) ), st_var_x = sum((mean_x-sum(mean_x*stratum_size/sum(stratum_size) ))^2)+sum(var_x*stratum_size/sum(stratum_size)), st_var_mean = sum( 1*(var_mean_x)* ( (stratum_size/sum(stratum_size))^2) )   ), list(period)]
				# alternative calc (same results)
						# see above
			# stratified se of the mean
				b2[, st_se_mean:=sqrt(st_var_mean), period]
				b2[, st_rse_mean:=round(st_se_mean/st_mean*100,1), period]
				if(pop_confint_type=="tdist") {
				  b2[, clow_mean := st_mean-qt(0.975,n-H)*st_se_mean, period]
				  b2[, chigh_mean := st_mean+qt(0.975,n-H)*st_se_mean, period]				
				}
				if(pop_confint_type=="conservative") {
				  b2[, clow_mean := st_mean-2*st_se_mean, period]
				  b2[, chigh_mean := st_mean+2*st_se_mean, period]				
				}				
				
		out<-list(pop_res = data.frame(type="stratified", variable = target_var, b2), stratum_res = b1, type="stratified")
		
		}
out		
}		
