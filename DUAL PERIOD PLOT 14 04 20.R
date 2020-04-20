# CHAPTER 4 (method section 4.4.2)
#FOR TWO EQUAL PERIODS OF CAMERA-TRAPPING, ONE IN WINTER AND ONE IN THE FOLLOWING SPRING
##SIMULATIONS TO PLOT SAMPLING DURATION IN DAYS ON THE X AXIS AND PROBABILITY OF DETECTING A REST ON THE Y AXIS
#SO THAT THE SAMPLING DURATION REQUIRED TO HAVE DETECTED 95% OF RESTS CAN BE CALCULATED

#EITHER RUN CODE UPTO, AND INCLUDING LINE 80 AS A DEMONSTRATION OF HOW THE CODE APPLIES TO ONE SITE, OR
# RUN ALL OF THE CODE TO THE END TO PRODUCE A PLOT CONTAINING A LINE FOR EACH SITE 9AND WINTER-SPRIONG PERIOD)

#Before running code, create a working directory and copy all the data files to this directory
rm(list=ls())
setwd(dir="")
##########################
#load data for each site CRAILING
loops<-read.csv("CRAILING.csv")
# REMOVE ANY NAs here (i.e. not winter or spring)
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
winter<-loops[loops$SEASON=="WINTER",]
spring<-loops[loops$SEASON=="SPRING",]
# SET THE THRESHOLD VALUE OF PROPORTION OF DAYS WHERE CTs SHOULD BE OPERATIONAL, 6 DAYS IN 7
threshold<-(6/7)
# THIS IS THE MAXIMUM SAMPLE DSURATION PER SEASON IT IS WHICH EVER SEASON IS SHORTEST IN THE DATASET
max.CT.window<-min(c(length(winter$WINTER.DAY),length(spring$SPRING.DAY)))
# ct.window WILL BE POPULATED WITH ALL POSSIBLE CT WINDOWS (1 to longest)
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0

# THIS LOOP RUNS FOR EACH POSSIBLE CT WINDOW
for(j in 1:max.CT.window){
  
  # THIS RECORDS THE CT WINDOW THAT j IS ON
  ct.window[j+1]<-j  
  
  # CREATE BLANK MATRICES THAT WILL BE POPULATED IF A REST IS DETECTED ON GIVEN START DATE FOR WINTER AND SPRING 
  #THESE WILL BE POPULATED AS THE NEXT LOOP OPERATES
  is.rest.spring<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  is.rest.winter<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.spring.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.winter.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  
  # THIS LOOP RUNS FOR EACH POSSIBLE START DAY WITHING WINTER
  for(k in 1:(length(winter$WINTER.DAY)-(j-1))){
    
    # THIS LOOP RUNS FOR EACH POSSIBLE START DAY WITHIN SPRING
    for(i in 1:(length(spring$SPRING.DAY)-(j-1))){
      is.rest.spring[i,k]<-sum(spring$ALL.RESTS[i:(i+j-1)])>0;
      is.rest.winter[i,k]<-sum(winter$ALL.RESTS[k:(k+j-1)])>0;
      
      CTdays.spring.prop[i,k]<-(sum(spring$CAMS[i:(i+j-1)])/j)>=threshold
      CTdays.winter.prop[i,k]<-(sum(winter$CAMS[k:(k+j-1)])/j)>=threshold
    }
    
  }
  
  is.rest.spring[CTdays.spring.prop!=1 & is.rest.spring<1]<-(3)
  is.rest.winter[CTdays.winter.prop!=1 & is.rest.winter<1]<-(3)
  
  false.negative<-sum((is.rest.spring+is.rest.winter)==0,na.rm=T)
  true.positive1<-sum((is.rest.spring+is.rest.winter)==1,na.rm=T)
  true.positive2<-sum((is.rest.spring+is.rest.winter)==2,na.rm=T)
  null.period<-sum((is.rest.spring+is.rest.winter)>2,na.rm=T)
  
  true.pos.prop[j+1]<-(true.positive1+true.positive2)/(true.positive1+true.positive2+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive1+true.positive2+false.negative+null.period)
  
}

(data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3))))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")

#AS THERE ARE TWO EQUAL PERIODS OF CAMERA TRAPPING, THE NUMBER OF CT DAYS MUST BE MULTIPLIED
#BY 2 TO MAKE THE PLOTS COMPARABLE IN TERMS OF THE TOTAL SURVEY EFFORT FOR EACH SAMPLING APPROACH
two.periods<-2*data1$CT.window
data2<-cbind(two.periods,data1)

plot(data2$True.positives~data2$two.periods,type="l", xlim=c(1,110),cex=2, xlab="Total sampling duration(d) for two equal camera-trapping periods",ylab="Probability of detecting a rest")

abline(h=0.95,lty=2)
# MIN SAMPLE DURATION (PER SEASON) FOR 95% PROPBABILITY OF DETECTING REST
sum(data1$True.positives<0.95)
######################################################################
######################################################################
#New site: FROGDEN, DATE CYCLE 3
loops<-read.csv("FROGDEN1.csv")
loops<-loops[loops$DATE.CYCLE==3,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
winter<-loops[loops$SEASON=="WINTER",]
spring<-loops[loops$SEASON=="SPRING",]
threshold<-(6/7)
max.CT.window<-min(c(length(winter$WINTER.DAY),length(spring$SPRING.DAY)))
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0

for(j in 1:max.CT.window){
  
   ct.window[j+1]<-j  
  
    is.rest.spring<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  is.rest.winter<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.spring.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.winter.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  
   for(k in 1:(length(winter$WINTER.DAY)-(j-1))){
    
    for(i in 1:(length(spring$SPRING.DAY)-(j-1))){
      is.rest.spring[i,k]<-sum(spring$ALL.RESTS[i:(i+j-1)])>0;
      is.rest.winter[i,k]<-sum(winter$ALL.RESTS[k:(k+j-1)])>0;
      
      CTdays.spring.prop[i,k]<-(sum(spring$CAMS[i:(i+j-1)])/j)>=threshold
      CTdays.winter.prop[i,k]<-(sum(winter$CAMS[k:(k+j-1)])/j)>=threshold
    }
    
  }
  
  is.rest.spring[CTdays.spring.prop!=1 & is.rest.spring<1]<-(3)
  is.rest.winter[CTdays.winter.prop!=1 & is.rest.winter<1]<-(3)
  
  false.negative<-sum((is.rest.spring+is.rest.winter)==0,na.rm=T)
  true.positive1<-sum((is.rest.spring+is.rest.winter)==1,na.rm=T)
  true.positive2<-sum((is.rest.spring+is.rest.winter)==2,na.rm=T)
  null.period<-sum((is.rest.spring+is.rest.winter)>2,na.rm=T)
  
  true.pos.prop[j+1]<-(true.positive1+true.positive2)/(true.positive1+true.positive2+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive1+true.positive2+false.negative+null.period)
  
}

(data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3))))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
two.periods<-2*data1$CT.window
data2<-cbind(two.periods,data1)
points(data2$True.pos~data2$two.periods,type="l")

##########################################################################
#New site: FROGDEN2, DATE CYCLE 1
loops<-read.csv("FROGDEN2.csv")
loops<-loops[loops$DATE.CYCLE==1,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
winter<-loops[loops$SEASON=="WINTER",]
spring<-loops[loops$SEASON=="SPRING",]
threshold<-(6/7)
max.CT.window<-min(c(length(winter$WINTER.DAY),length(spring$SPRING.DAY)))
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0
for(j in 1:max.CT.window){
  
  ct.window[j+1]<-j  
  
  is.rest.spring<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  is.rest.winter<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.spring.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.winter.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  
    for(k in 1:(length(winter$WINTER.DAY)-(j-1))){
    
    for(i in 1:(length(spring$SPRING.DAY)-(j-1))){
      is.rest.spring[i,k]<-sum(spring$ALL.RESTS[i:(i+j-1)])>0;
      is.rest.winter[i,k]<-sum(winter$ALL.RESTS[k:(k+j-1)])>0;
      
      CTdays.spring.prop[i,k]<-(sum(spring$CAMS[i:(i+j-1)])/j)>=threshold
      CTdays.winter.prop[i,k]<-(sum(winter$CAMS[k:(k+j-1)])/j)>=threshold
    }
    
  }
  
  is.rest.spring[CTdays.spring.prop!=1 & is.rest.spring<1]<-(3)
  is.rest.winter[CTdays.winter.prop!=1 & is.rest.winter<1]<-(3)
  
  false.negative<-sum((is.rest.spring+is.rest.winter)==0,na.rm=T)
  true.positive1<-sum((is.rest.spring+is.rest.winter)==1,na.rm=T)
  true.positive2<-sum((is.rest.spring+is.rest.winter)==2,na.rm=T)
  null.period<-sum((is.rest.spring+is.rest.winter)>2,na.rm=T)
  
  true.pos.prop[j+1]<-(true.positive1+true.positive2)/(true.positive1+true.positive2+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive1+true.positive2+false.negative+null.period)
  
}

(data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3))))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
two.periods<-2*data1$CT.window
data2<-cbind(two.periods,data1)
points(data2$True.pos~data2$two.periods,type="l")

##########################################################################
#New site: FROGDEN 2, DATE CYCLE 2
loops<-read.csv("FROGDEN2.csv")
loops<-loops[loops$DATE.CYCLE==2,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
winter<-loops[loops$SEASON=="WINTER",]
spring<-loops[loops$SEASON=="SPRING",]
threshold<-(6/7)
max.CT.window<-min(c(length(winter$WINTER.DAY),length(spring$SPRING.DAY)))
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0
for(j in 1:max.CT.window){
  
  ct.window[j+1]<-j  
  
  is.rest.spring<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  is.rest.winter<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.spring.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.winter.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  
  for(k in 1:(length(winter$WINTER.DAY)-(j-1))){
    
    for(i in 1:(length(spring$SPRING.DAY)-(j-1))){
      is.rest.spring[i,k]<-sum(spring$ALL.RESTS[i:(i+j-1)])>0;
      is.rest.winter[i,k]<-sum(winter$ALL.RESTS[k:(k+j-1)])>0;
      
      CTdays.spring.prop[i,k]<-(sum(spring$CAMS[i:(i+j-1)])/j)>=threshold
      CTdays.winter.prop[i,k]<-(sum(winter$CAMS[k:(k+j-1)])/j)>=threshold
    }
    
  }
  
  is.rest.spring[CTdays.spring.prop!=1 & is.rest.spring<1]<-(3)
  is.rest.winter[CTdays.winter.prop!=1 & is.rest.winter<1]<-(3)
  
  false.negative<-sum((is.rest.spring+is.rest.winter)==0,na.rm=T)
  true.positive1<-sum((is.rest.spring+is.rest.winter)==1,na.rm=T)
  true.positive2<-sum((is.rest.spring+is.rest.winter)==2,na.rm=T)
  null.period<-sum((is.rest.spring+is.rest.winter)>2,na.rm=T)
  
  true.pos.prop[j+1]<-(true.positive1+true.positive2)/(true.positive1+true.positive2+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive1+true.positive2+false.negative+null.period)
  
}

(data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3))))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
two.periods<-2*data1$CT.window
data2<-cbind(two.periods,data1)
points(data2$True.pos~data2$two.periods,type="l")
########################################################################
#New site: GORDON
loops<-read.csv("GORDON.csv")
loops<-loops[loops$DATE.CYCLE==1,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
winter<-loops[loops$SEASON=="WINTER",]
spring<-loops[loops$SEASON=="SPRING",]
threshold<-(6/7)
max.CT.window<-min(c(length(winter$WINTER.DAY),length(spring$SPRING.DAY)))
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0
for(j in 1:max.CT.window){
  
  ct.window[j+1]<-j  
  
  is.rest.spring<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  is.rest.winter<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.spring.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.winter.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  
  for(k in 1:(length(winter$WINTER.DAY)-(j-1))){
    
    for(i in 1:(length(spring$SPRING.DAY)-(j-1))){
      is.rest.spring[i,k]<-sum(spring$ALL.RESTS[i:(i+j-1)])>0;
      is.rest.winter[i,k]<-sum(winter$ALL.RESTS[k:(k+j-1)])>0;
      
      CTdays.spring.prop[i,k]<-(sum(spring$CAMS[i:(i+j-1)])/j)>=threshold
      CTdays.winter.prop[i,k]<-(sum(winter$CAMS[k:(k+j-1)])/j)>=threshold
    }
    
  }
  
  is.rest.spring[CTdays.spring.prop!=1 & is.rest.spring<1]<-(3)
  is.rest.winter[CTdays.winter.prop!=1 & is.rest.winter<1]<-(3)
  
  false.negative<-sum((is.rest.spring+is.rest.winter)==0,na.rm=T)
  true.positive1<-sum((is.rest.spring+is.rest.winter)==1,na.rm=T)
  true.positive2<-sum((is.rest.spring+is.rest.winter)==2,na.rm=T)
  null.period<-sum((is.rest.spring+is.rest.winter)>2,na.rm=T)
  
  true.pos.prop[j+1]<-(true.positive1+true.positive2)/(true.positive1+true.positive2+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive1+true.positive2+false.negative+null.period)
  
}

(data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3))))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
two.periods<-2*data1$CT.window
data2<-cbind(two.periods,data1)
points(data2$True.pos~data2$two.periods,type="l")
########################################################################
#New site:LEARMOUTH, DATE CYCLE 1
loops<-read.csv("LEARMOUTH.csv")
loopsL1<-loops[loops$DATE.CYCLE==1,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
winter<-loops[loops$SEASON=="WINTER",]
spring<-loops[loops$SEASON=="SPRING",]
threshold<-(6/7)
max.CT.window<-min(c(length(winter$WINTER.DAY),length(spring$SPRING.DAY)))
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0
for(j in 1:max.CT.window){
  
  ct.window[j+1]<-j  
  
  is.rest.spring<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  is.rest.winter<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.spring.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.winter.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  
  for(k in 1:(length(winter$WINTER.DAY)-(j-1))){
    
    for(i in 1:(length(spring$SPRING.DAY)-(j-1))){
      is.rest.spring[i,k]<-sum(spring$ALL.RESTS[i:(i+j-1)])>0;
      is.rest.winter[i,k]<-sum(winter$ALL.RESTS[k:(k+j-1)])>0;
      
      CTdays.spring.prop[i,k]<-(sum(spring$CAMS[i:(i+j-1)])/j)>=threshold
      CTdays.winter.prop[i,k]<-(sum(winter$CAMS[k:(k+j-1)])/j)>=threshold
    }
    
  }
  
  is.rest.spring[CTdays.spring.prop!=1 & is.rest.spring<1]<-(3)
  is.rest.winter[CTdays.winter.prop!=1 & is.rest.winter<1]<-(3)
  
  false.negative<-sum((is.rest.spring+is.rest.winter)==0,na.rm=T)
  true.positive1<-sum((is.rest.spring+is.rest.winter)==1,na.rm=T)
  true.positive2<-sum((is.rest.spring+is.rest.winter)==2,na.rm=T)
  null.period<-sum((is.rest.spring+is.rest.winter)>2,na.rm=T)
  
  true.pos.prop[j+1]<-(true.positive1+true.positive2)/(true.positive1+true.positive2+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive1+true.positive2+false.negative+null.period)
  
}

(data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3))))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
two.periods<-2*data1$CT.window
data2<-cbind(two.periods,data1)
points(data2$True.pos~data2$two.periods,type="l")
########################################################################
#New site: LEARMOUTH, DATE CYCLE 2
loops<-read.csv("LEARMOUTH.csv")
loops<-loops[loops$DATE.CYCLE==2,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
winter<-loops[loops$SEASON=="WINTER",]
spring<-loops[loops$SEASON=="SPRING",]
threshold<-(6/7)
max.CT.window<-min(c(length(winter$WINTER.DAY),length(spring$SPRING.DAY)))
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0
for(j in 1:max.CT.window){
  
  ct.window[j+1]<-j  
  
  is.rest.spring<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  is.rest.winter<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.spring.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.winter.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  
  for(k in 1:(length(winter$WINTER.DAY)-(j-1))){
    
    for(i in 1:(length(spring$SPRING.DAY)-(j-1))){
      is.rest.spring[i,k]<-sum(spring$ALL.RESTS[i:(i+j-1)])>0;
      is.rest.winter[i,k]<-sum(winter$ALL.RESTS[k:(k+j-1)])>0;
      
      CTdays.spring.prop[i,k]<-(sum(spring$CAMS[i:(i+j-1)])/j)>=threshold
      CTdays.winter.prop[i,k]<-(sum(winter$CAMS[k:(k+j-1)])/j)>=threshold
    }
    
  }
  
  is.rest.spring[CTdays.spring.prop!=1 & is.rest.spring<1]<-(3)
  is.rest.winter[CTdays.winter.prop!=1 & is.rest.winter<1]<-(3)
  
  false.negative<-sum((is.rest.spring+is.rest.winter)==0,na.rm=T)
  true.positive1<-sum((is.rest.spring+is.rest.winter)==1,na.rm=T)
  true.positive2<-sum((is.rest.spring+is.rest.winter)==2,na.rm=T)
  null.period<-sum((is.rest.spring+is.rest.winter)>2,na.rm=T)
  
  true.pos.prop[j+1]<-(true.positive1+true.positive2)/(true.positive1+true.positive2+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive1+true.positive2+false.negative+null.period)
  
}

(data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3))))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
two.periods<-2*data1$CT.window
data2<-cbind(two.periods,data1)
points(data2$True.pos~data2$two.periods,type="l")
#####################################################################
#New site:TORQUHAN
loops<-read.csv("TORQUHAN.csv")
#select date cycle
loops<-loops[loops$DATE.CYCLE==2,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
winter<-loops[loops$SEASON=="WINTER",]
spring<-loops[loops$SEASON=="SPRING",]
threshold<-(6/7)
max.CT.window<-min(c(length(winter$WINTER.DAY),length(spring$SPRING.DAY)))
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0
for(j in 1:max.CT.window){
  
  ct.window[j+1]<-j  
  
  is.rest.spring<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  is.rest.winter<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.spring.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  CTdays.winter.prop<-matrix(ncol=nrow(winter),nrow=nrow(spring))
  
  for(k in 1:(length(winter$WINTER.DAY)-(j-1))){
    
    for(i in 1:(length(spring$SPRING.DAY)-(j-1))){
      is.rest.spring[i,k]<-sum(spring$ALL.RESTS[i:(i+j-1)])>0;
      is.rest.winter[i,k]<-sum(winter$ALL.RESTS[k:(k+j-1)])>0;
      
      CTdays.spring.prop[i,k]<-(sum(spring$CAMS[i:(i+j-1)])/j)>=threshold
      CTdays.winter.prop[i,k]<-(sum(winter$CAMS[k:(k+j-1)])/j)>=threshold
    }
    
  }
  
  is.rest.spring[CTdays.spring.prop!=1 & is.rest.spring<1]<-(3)
  is.rest.winter[CTdays.winter.prop!=1 & is.rest.winter<1]<-(3)
  
  false.negative<-sum((is.rest.spring+is.rest.winter)==0,na.rm=T)
  true.positive1<-sum((is.rest.spring+is.rest.winter)==1,na.rm=T)
  true.positive2<-sum((is.rest.spring+is.rest.winter)==2,na.rm=T)
  null.period<-sum((is.rest.spring+is.rest.winter)>2,na.rm=T)
  
  true.pos.prop[j+1]<-(true.positive1+true.positive2)/(true.positive1+true.positive2+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive1+true.positive2+false.negative+null.period)
  
}

(data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3))))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
two.periods<-2*data1$CT.window
data2<-cbind(two.periods,data1)
points(data2$True.pos~data2$two.periods,type="l",lty=3)
####################################################################
###############################################################
