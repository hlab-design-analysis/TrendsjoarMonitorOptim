# ==============================
# General data preparation
# Nuno Prista, SLU Aqua, 2022 @ Konsekvensanalysis project
	# based on developments under Nuno Prista, SLU Aqua, 2019, 2022 @ KustMonitoring project
# ==============================

rm(list=ls())
library(data.table)
library(skimr)

# user: select site 
	site <- "Stensjön"

# user: sets dirs
	dir_inputs <- "001_Inputs/raw/"
	dir_outputs <- "001_Inputs/prepared/"
	if(!file.exists(dir_outputs)) dir.create(dir_outputs, recursive=T, showWarnings=FALSE)

# user: reads raw data
	a<-fread(file=paste(dir_inputs,"Input_FiskvariablerPerBnät.txt",sep=""), header=T, na.strings=c("NA"," ",""), dec=",")

# define target_vars
	target_vars <- c('NabborB','BabborB','NgäddaB','BgäddaB','NlakeB','BlakeB','NmörtB','BmörtB','Narter','NtotalB','BtotalB','mlg10Labbo','mlg10Lmört','BpiscAbbBNet')

# formatting 	
	a$Area<-a$NAMN
	a$Year<-substr(a$DATUM1,1,4)
	a$Season<-"NotApplicable"
	a$Gear<-a$NETTYP; table(a$Gear, useNA="al")
	a$GearCode<-a$NETKOD; table(a$GearCode, useNA="al")
	a$RepeatedDay<-1
	a$DepthStratum<-a$ZON; table(a$DepthStratum, useNA="al")
		a$DepthStratum[a$DepthStratum==0]<-"0-3m"
		a$DepthStratum[a$DepthStratum==3]<-"3-6m"
		a$DepthStratum[a$DepthStratum==6]<-"6-12m"; table(a$DepthStratum, useNA="al")
	a$'Depth'<-"NotApplicable"
	a$Disturbance<-"NEJ"
	a$Station<-paste(a$DATUM1, a$NETNR, sep="_")
	colnames(a)[colnames(a)=="BpiscAbbB-Net"]<-"BpiscAbbBNet"
	
# sets target vars to numeric
	skim(a[,..target_vars])
	for (i in target_vars) {
		a[[i]]<-as.numeric(a[[i]])
		}
	skim(a[,..target_vars])
		
# handles NAs in target_vars
	#mlg10Labbo, mlg10Lmört are lengths only defined where fish were caught so they also remain as NAs
	for (i in target_vars[!target_vars %in% c("mlg10Labbo","mlg10Lmört")]) {a[[i]][is.na(a[[i]])]<-0}
	skim(a[,..target_vars])

# Factorizes DepthStratum
	a$DepthStratum<-factor(a$DepthStratum, levels = c("0-3m","3-6m","6-12m"))
	
# No of stations 
	table(a$Year, a$DepthStratum)
	tapply(a$Station, list(a$Year, a$DepthStratum), function(x)length(unique(x)))
	
	# check on duplicates [should yield 3*TRUE]
	sum(apply(table(a$Year[a$DepthStratum=="0-3m"],a$Station[a$DepthStratum=="0-3m"]),2,sum)>1)==0
	sum(apply(table(a$Year[a$DepthStratum=="3-6m"],a$Station[a$DepthStratum=="3-6m"]),2,sum)>1)==0
	sum(apply(table(a$Year[a$DepthStratum=="6-12m"],a$Station[a$DepthStratum=="6-12m"]),2,sum)>1)==0

# handles strata sizes
	if(site=="Stensjön"){
		a$NStations[a$DepthStratum=="0-3m"] <- 8
		a$NStations[a$DepthStratum=="3-6m"] <- 8
		a$NStations[a$DepthStratum=="6-12m"] <- 8
		}	

# saves prepared data
	final_cols<-c('Area','Year','Season','Gear','GearCode','RepeatedDay','DepthStratum','Depth','Disturbance','Station','NStations',target_vars)
	dt_site<-a[,..final_cols]
	save(dt_site, target_vars, file=paste(dir_outputs, site,".Rdata",sep=""))
