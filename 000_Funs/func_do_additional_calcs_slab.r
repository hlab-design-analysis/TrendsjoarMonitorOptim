	do_additional_calcs_slab<-function(x)
	{
	  require(data.table)

	#print(x)
	# Nspecies
	Nspecies<-sum(any(x[,.(BabborB)]!=0)+any(x[,.(NgäddaB)]!=0)+any(x[,.(NlakeB)]!=0)+any(x[,.(Narter)]!=0))

	#MeanW
	if(sum(x[['NtotalB']]==0)!=sum(x[['BtotalB']]==0)) stop("check MeanW")	# redundant but kept to facilitate later isolation into function
	MeanW<-sum(x[['BtotalB']])/sum(x[['NtotalB']])
	MeanW[!is.finite(MeanW)]<-NA

	#gmLabboB
	if(sum(is.na(x[['mlg10Labbo']]))!=sum(x[['NabborB']]==0)) stop("check mlg10Labbo")
	gmLabboB<-10^(sum(x[['mlg10Labbo']]*x[['NabborB']], na.rm=T)/sum(x[['NabborB']]))
	gmLabboB[!is.finite(gmLabboB)]<-NA

	#gmLmörtB
	if(sum(is.na(x[['mlg10Lmört']]))!=sum(x[['NmörtB']]==0)) stop("check mlg10Lmört")	
	gmLmörtB<-10^(sum(x[['mlg10Lmört']]*x[['NmörtB']], na.rm=T)/sum(x[['NmörtB']]))
	gmLmörtB[!is.finite(gmLmörtB)]<-NA
	
	# AbCyW
	if(sum(x[['BabborB']]==0)!=sum(x[['NabborB']]==0)) stop("check AbCyW")	
	if(sum(x[['NmörtB']]==0)!=sum(x[['BmörtB']]==0)) stop("check AbCyW")	
	AbCyW<-sum(x[['BabborB']])/sum(x[['BmörtB']])
	AbCyW[!is.finite(AbCyW)]<-NA

	# pCyp
	if(sum(x[['NmörtB']]==0)!=sum(x[['BmörtB']]==0)) stop("check AbCyW") # redundant but kept to facilitate later isolation into function
	pCyp<-sum(x[['BmörtB']])/sum(x[['BtotalB']])
	pCyp[!is.finite(pCyp)]<-NA	

	

	#pPiscPerc
	pPiscPerc<-sum(x[['BpiscAbbBNet']])/sum(x[['BtotalB']])

	#SDn
	SDn<-1/((1/sum(x[['NtotalB']]))^2*sum(sum(x[['NabborB']])^2+sum(x[['NgäddaB']])^2+sum(x[['NlakeB']])^2+sum(x[['NmörtB']])^2))
	
	#SDw
	SDw<-1/((1/sum(x[['BtotalB']]))^2*sum(sum(x[['BabborB']])^2+sum(x[['BgäddaB']])^2+sum(x[['BlakeB']])^2+sum(x[['BmörtB']])^2))
	
	# produces final object
	out<-cbind(t(data.frame(Nspecies, MeanW, gmLabboB, gmLmörtB, AbCyW, pCyp, pPiscPerc, SDn, SDw)),
	      t(data.frame(Nspecies = nrow(x), MeanW = sum(!x[['NtotalB']]==0), gmLabboB = sum(!is.na(x$mlg10Labbo)),  gmLmörtB = sum(!is.na(x$mlg10Lmört)), 
	                   AbCyW = sum(!x[['BmörtB']]==0), pCyp = sum(!x[['BtotalB']]==0), pPiscPerc = sum(!x[['BtotalB']]==0), SDn = sum(!x[['NtotalB']]==0), SDw = sum(!x[['BtotalB']]==0))))
	
	out<-data.frame(variable=rownames(out),st_mean = out[,1], N= out[1,2], n=out[,2], H=0,row.names=NULL)
	
	#data.frame(Nspecies, MeanW, gmLabboB, gmLmörtB, AbCyW, pCyp, pPiscPerc, SDn, SDw)
	out
	}
	
	#test
	#do_additional_calcs_slab(a)
	#do_additional_calcs_slab(a[sample(1:nrow(a),size=1),])
	