# ==============================	
# Preparation of sample size files
# Nuno Prista, SLU Aqua, 2022 @ Konsekvensanalysis project
	# based on developments under Nuno Prista, SLU Aqua, 2019, 2022 @ KustMonitoring project
# ==============================

rm(list=ls())

# loads libraries
library(data.table)

# user: select site 
	site <- "Stensjön"

# user: sets dirs
	dir_inputs <- "001_Inputs/prepared/"
	dir_outputs <- "002_SampleSizes/"
	if(!file.exists(dir_outputs)) dir.create(dir_outputs, recursive=T, showWarnings=FALSE)

# loads prepared data
	load(file=paste(dir_inputs,"/",site,".Rdata",sep=""))		

# QCA should be TRUE
	all(dt_site[,.N, list(Year, DepthStratum)][order(Year, DepthStratum)]==dt_site[,length(unique(Station)), list(Year, DepthStratum)][order(Year, DepthStratum)])

# prepares sample sizes			
	out<-dt_site[,list(Year, DepthStratum,N)][order(Year, DepthStratum,N)]

# user: you can tune the below to your specific example (but keep the use of "n" as prefix - it is looked for in a later script)
	out$n21<-7
	out$n18<-6
	out$n15<-5
	out$n12<-4

# saves sample size file
	write.table(out, file=paste0(dir_outputs, "/" ,site, "_sample_sizes_to_sim.txt"), row.names=FALSE, sep="\t")


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
		
	#fyke nets
	
		# for (site in c("Barsebäck","Fjällbacka_Cold","Fjällbacka_Warm","Kullen","Älgöfjorden"))
			# {
		# load(file=paste(dir_inputs,site,".Rdata",sep=""))		
		# print(table(dt_site$Gear))	
		# browser()	
		# # QCA should be 0
		# print(sum(!dt_site[,.N, list(Year, DepthStratum)][order(Year, DepthStratum)]==dt_site[,length(unique(Station)), list(Year, DepthStratum)][order(Year, DepthStratum)]))
			
		# out<-dt_site[,.N, list(Year, DepthStratum)][order(Year, DepthStratum)]

		# write.table(out, file=paste(dir_outputs, site, ".txt", sep=""))
		
			# }		
		
		
		

	
	

	

	
	
	
	
	table(dt_site$Station, dt_site$DepthStratum, dt_
	
