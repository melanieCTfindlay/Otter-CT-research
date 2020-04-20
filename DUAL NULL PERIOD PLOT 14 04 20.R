# CHAPTER 4 (method section 4.4.2)

#FOR TWO EQUAL PERIODS OF CAMERA-TRAPPING, ONE IN WINTER AND ONE IN THE FOLLOWING SPRING
##SIMULATIONS TO PLOT REJECTED DATA WHERE INDIVIDUAL SAMPLING WINDOWS DO NOT MEET THE THRESHOLD
#OF 6 FUNCTIONING DAYS OUT OF 7 AND WHERE THE SAME INDIVIDUAL SAMPLING PERIOD DOES NOT RECORD
#A REST (IN THIS WAY, SAMPLING PERIODS WHERE A REST IS RECORDED ARE RETAINED AS VALID)
#PLOTTED AS THE PROPORTION OF INDIVIDUAL SAMPLING WINDOWS LOST ON THE y AXIS
#AND SAMPLING DURATION ON THE X AXIS

#EITHER RUN CODE UPTO, AND INCLUDING LINE X AS A DEMONSTRATION OF HOW THE CODE APPLIES TO ONE SITE, OR
# RUN ALL OF THE CODE TO THE END TO PRODUCE A PLOT CONTAINING A LINE FOR EACH SITE (AND WINTER-SPRING PERIOD)

#Before running code, create a working directory and copy all the data files to this directory
rm(list=ls())
setwd(dir="")
##########################
#load data for each site CRAILING
loops<-read.csv("CRAILING.csv")
# REMOVE ANY NAs here (i.e. not winter or spring)
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
#SELECT WINTER RECORDS
winter<-loops[loops$SEASON=="WINTER",]
#SELECT SPRING RECORDS
spring<-loops[loops$SEASON=="SPRING",]
# SET THE THRESHOLD VALUE OF PROPORTION OF DAYS WHERE CTs SHOULD BE OPERATIONAL, 6 DAYS IN 7
threshold<-(6/7)
# THIS IS THE MAXIMUM SAMPLE DSURATION PER SEASON IT IS WHICH EVER SEASON IS SHORTEST IN THE DATASET
max.CT.window<-min(c(length(winter$WINTER.DAY),length(spring$SPRING.DAY)))

# WILL BE POPULATED WITH ALL POSSIBLE CT WINDOWS (1 to longest)
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0

# THIS LOOP RUNS FOR EACH POSSIBLE CT WINDOW
for(j in 1:max.CT.window){
  
  # JUST RECORDS THE CT WINDOW THAT j IS ON
  ct.window[j+1]<-j  
  
  # BLANK MATRICES RECORDING IF REST DETECTED ON GIVEN START DATE FOR WINTER AND SPRING 
   # THESE WILL BE POPULATED AS THE NEXT LOOP OPERATES
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

###PLOT DATA
plot(null.period.prop~data1$CT.window,type="l", xlim=c(0,110),ylim=c(0,0.8),cex=2,xlab="Sampling duration (d) for two equal camera-trapping periods", ylab="Proportion of null data")
######################################################################
#New site: FROGDEN1, DATE CYCLE 3
loops<-read.csv("FROGDEN1.csv")
#select date cycle
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

points(null.period.prop~data1$CT.window,type="l")
##########################################################################
#New site: FROGDEN 2, DATE CYCLE 1
loops<-read.csv("FROGDEN2.csv")
#select date cycle
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

points(null.period.prop~data1$CT.window,type="l")
##########################################################################
#New site: FROGDEN2, DATE CYCLE 2
loops<-read.csv("FROGDEN2.csv")
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

points(null.period.prop~data1$CT.window,type="l")
########################################################################
#New site:GORDON
loops<-read.csv("GORDON.csv")
#select date cycle
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

points(null.period.prop~data1$CT.window,type="l")
########################################################################
#New site: LEARMOUTH, DATE CYCLE 1 
loops<-read.csv("LEARMOUTH.csv")
#select date cycle
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
points(null.period.prop~data1$CT.window,type="l")
########################################################################
#New site: LEARMOUTH, DATE CYCLE 2
loops<-read.csv("LEARMOUTH.csv")
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

points(null.period.prop~data1$CT.window,type="l")
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

points(null.period.prop~data1$CT.window,type="l", lty=3)
####################################################################
################################################################
#END
