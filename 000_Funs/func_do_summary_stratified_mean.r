do_summary_stratified_mean <- function (x, target_var, strata_var, strata_size_var, klab){

	# calculates stratified mean statistics
	# Nuno Prista, SLU Aqua, Sweden
	# Developed @ KustMonitoring Review 2019
				
	#x is a dataframe values with 
		# target values in target_var variable
		# a strata_var variable
		# a strata_size variable

	require(data.table)

	if(!is.data.table(x) & !is.data.frame(x)) stop ("x must be data.frame or data.table")
	if(!is.data.table(x)) x<-data.table(x)
				
# ==========================
# Stratified (st) results
# ==========================

	values<-parse(text=target_var)
	stratum<-parse(text=strata_var)
	size<-parse(text=strata_size_var)
	
	strata_to_keep<-unique(x[,eval(stratum)])
	
	# stratified variance
		# strata summary
			b1<-x[,list(stratum_size=eval(size)[1], variable = target_var, n=sum(!is.na(eval(values))), mean_x = mean(eval(values), na.rm=T), var_x = var(eval(values), na.rm=T), var_mean_x = var(eval(values), na.rm=T)/sum(!is.na(eval(values))), se_mean_x = sd(eval(values), na.rm=T)/sqrt(sum(!is.na(eval(values))))), eval(stratum)][order(eval(stratum))]; 
			b1[, `:=`(rse_mean_x = round(se_mean_x/mean_x*100,1), tvalue = qt(0.975,n-1), error_margin = qt(0.975,n-1)*se_mean_x, clow_mean_x = mean_x-qt(0.975,n-1)*se_mean_x, chigh_mean_x=mean_x+qt(0.975,n-1)*se_mean_x),stratum]
		# stratified mean
			st_mean<-sum(b1$mean_x*b1$stratum_size/sum(b1$stratum_size))
		# stratified variance of the mean
			#st_var_mean<-sum(b1$var_mean_x*(b1$stratum_size/sum(b1$stratum_size))^2)
			st_var_mean<-sum( 1*( ( b1$stratum_size/sum(b1$stratum_size) )^2 )*(b1$var_mean_x) ) # according to Lohr (2009), pg 79 [assumes f=1] 
		# stratified se of the mean
			st_se_mean<-sqrt(st_var_mean)
		# stratified cv/rse of the mean 
			st_rse_mean<-round(st_se_mean/st_mean*100,1)
		
	list(pop_res = data.frame(type="stratified", variable = target_var, expect_strata = paste(strata_to_keep, collapse=","), samp_strata = paste(b1$stratum, collapse=","), n_tot = sum(b1$n), n_strata = paste(b1$n,collapse=","), mean = st_mean, var_mean = st_var_mean, se_mean = st_se_mean, rse_mean = st_rse_mean, tdist = qt(0.975,sum(b1$n)-1), error_margin = qt(0.975,sum(b1$n)-1)*st_se_mean, clow_mean=st_mean-qt(0.975,sum(b1$n)-1)*st_se_mean, chigh_mean=st_mean+qt(0.975,sum(b1$n)-1)*st_se_mean), stratum_res = data.frame(b1), type="stratified")
}		
