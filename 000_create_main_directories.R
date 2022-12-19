# ==============================
# Sets up directory structure and checks availability of main functions
# Nuno Prista, SLU Aqua, 2022 @ Konsekvensanalysis project
	# based on developments under Nuno Prista, SLU Aqua, 2019, 2022 @ KustMonitoring project
# ==============================

# checks if directory for 000_Funs exists [if not, creates it]
if(!file.exists("000_Funs")) dir.create("000_Funs")

# checks if 000_Funs has the necessary functions (should yield TRUE) [note: these need to manually copied]
required_funs<-c("func_do_summary_stratified_mean_time_series.r",
			"func_do_additional_calcs_slab.r",
			"func_do_job_sampling.r",
			"func_do_job_trend.r",	
			"func_do_sampling_stratified.r",
			"func_do_summary_stratified_mean.r",
			"func_do_sampling_stratified_timeseries.r",
			"func_sampleFromAGroup.r"
			)
all(file.exists(paste("000_Funs",required_funs,sep="/")))

# creates directory structure for inputs
dir.create("001_Inputs")
dir.create("001_Inputs/raw")
dir.create("001_Inputs/prepared")

# creates directory structure for sample sizes
dir.create("002_SampleSizes")

# creates directory structure for simulations of sampling
dir.create("003_SamplingSims")

# creates directory structure for simulations of trend
dir.create("004_TrendSims")