    
	do_job_trend<-function(lo, ls1, years){

      out<-rbindlist(lapply(ls1, function(x, omdrev1 = omdrev) {
        # selects years
        # samples from 1:omdrev1 to select the starting point of the series
        # samples series with the same length [eg, if years 2002:2018 (i.e., 17 years) and omdrev = 2 samples 8 years; if omdrev = 3 samples 5 years
         if(omdrev1 %in% c(2,3)) 
           {
           years_index<-seq(sample(1:omdrev1,1), length(unique(x$Year))%/%omdrev1*omdrev1, by = omdrev1) 
           } else {
                  years_index<-as.numeric(as.factor(unique(x$Year)))
                  }
        # samples sim_pop subset
        dt2<-x[Year %in% unique(Year)[years_index],.SD[sample(.N, 1)],by = Year][,list(mean,Year)]
        
        
        dt2$Year<-as.numeric(dt2$Year)
        
        # fits linear model
        model.linear <- lm(mean~Year, data=dt2)
        # fits kendall tau
        #model.kendall1<-cor.test(dt2$mean,as.numeric(as.character(dt2$Year)), method="kendall")
        model.kendall<-Kendall(dt2$mean,dt2$Year)
        
        
        # stores results 
        data.table(SampSize = x$SampSize[1], variable = x$variable[1], omdrev = omdrev1, years = paste(dt2$Year, collapse=","), 
                                    linear_slope = coef(model.linear)[2], linear_pvalue = summary(model.linear)[[4]][8], linear_sign = summary(model.linear)[[4]][8]<0.05, 
                                        #kendall_tau1 =  model.kendall1[[4]][[1]], kendall_sign1 = model.kendall1[[3]][[1]]<0.05,
                                          kendall_tau =  model.kendall[[1]][[1]], kendall_pvalue = model.kendall[[2]][[1]], kendall_sign = model.kendall[[2]][[1]]<0.05)
      }))
      out$repl<-lo
      out	
    }
