# CHAPTER 4 (method section 4.4.2)
#SINGLE PERIOD OF CAMERA-TRAPPING DURING THE WINTER-SPRING PERIOD
##SIMULATIONS TO PLOT SAMPLING DURATION IN DAYS ON THE X AXIS AND PROBABILITY OF DETECTING A REST ON THE Y AXIS
#SO THAT THE SAMPLING DURATION REQUIRED TO HAVE DETECTED 95% OF RESTS CAN BE CALCULATED

#EITHER RUN CODE UPTO, AND INCLUDING LINE 62 AS A DEMONSTRATION OF HOW THE CODE APPLIES TO ONE SITE, OR
# RUN ALL OF THE CODE TO THE END TO PRODUCE A PLOT CONTAINING A LINE FOR EACH SITE 9AND WINTER-SPRIONG PERIOD)

#Before running code, create a working directory and copy all the data files to this directory
rm(list=ls())
setwd(dir="")
##########################
#load data for each site CRAILING
loops<-read.csv("CRAILING.csv")
# REMOVE ANY NAs here (i.e. not winter or spring)
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
# SET THE THRESHOLD VALUE OF PROPORTION OF DAYS WHERE CTs SHOULD BE OPERATIONAL, 6 DAYS IN 7
threshold<-(6/7)
# THIS IS THE MAXIMUM SAMPLE DURATION
max.CT.window<-nrow(loops)

ct.window<-0 # WILL BE POPULATED WITH ALL POSSIBLE CT WINDOWS (1 to longest)
true.pos.prop<-0
null.period.prop<-0

# THIS LOOP RUNS FOR EACH POSSIBLE CT WINDOW
for(j in 1:max.CT.window){
  
  # THIS JUST RECORDS THE CT WINDOW THAT j IS ON
  ct.window[j+1]<-j  
  
  # BLANK VECTOR RECORDING IF REST IS DETECTED ON GIVEN START DATE FOR WINTER AND SPRING 
  is.rest<-vector()
  
  CTdays.prop<-vector()
  
  # THIS LOOP RUNS FOR EACH POSSIBLE START DAY WITHIN SEASON
  for(i in 1:(nrow(loops)-(j-1))){
    is.rest[i]<-sum(loops$ALL.RESTS[i:(i+j-1)])>0;
    
    CTdays.prop[i]<-(sum(loops$CAMS[i:(i+j-1)])/j)>=threshold
    
  }
  
  is.rest[is.rest==0 & CTdays.prop!=1]<-2 
 
   # THIS ASSIGNS ONE OF THREE 3 codes: 0 = no rest detected, 1 = rest detected, 2 = not enough CT dats to tell (i.e. below threshold days) 
  null.period<-length(is.rest[is.rest==2])
  true.positive<-length(is.rest[is.rest==1])
  false.negative<-length(is.rest[is.rest==0])
  
  true.pos.prop[j+1]<-(true.positive)/(true.positive+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive+false.negative+null.period)
  
}

data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3)))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
data1
plot(data1$True.positives~data1$CT.window,type="l",xlim=c(1,110), cex=2,xlab="Sampling duration(d) for a single camera-trapping period",ylab="Probability of detecting a rest")
abline(h=0.95,lty=2)
sum(data1$True.positives<0.95) # MIN SAMPLE DURATION (PER SEASON) FOR 95% PROPBABILITY OF DETECTING REST
######################################################################
######################################################################
#New site: frogden1, SELECTING DATE.CYCLE 3
loops<-read.csv("FROGDEN1.csv")
loops<-loops[loops$DATE.CYCLE==3,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
threshold<-(6/7)
max.CT.window<-nrow(loops)
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0

for(j in 1:max.CT.window){
  
    ct.window[j+1]<-j  
  
    is.rest<-vector()
  
  CTdays.prop<-vector()
  
  for(i in 1:(nrow(loops)-(j-1))){
    is.rest[i]<-sum(loops$ALL.RESTS[i:(i+j-1)])>0;
    
    CTdays.prop[i]<-(sum(loops$CAMS[i:(i+j-1)])/j)>=threshold
    
  }
  
  is.rest[is.rest==0 & CTdays.prop!=1]<-2 
  
  null.period<-length(is.rest[is.rest==2])
  true.positive<-length(is.rest[is.rest==1])
  false.negative<-length(is.rest[is.rest==0])
  
  true.pos.prop[j+1]<-(true.positive)/(true.positive+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive+false.negative+null.period)
  
}

data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3)))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
data1
points(data1$True.positives~data1$CT.window,type="l")
sum(data1$True.positives<0.95) 

##########################################################################
##New site: FROGDEN2, SELECTING DATE.CYCLE 1
loops<-read.csv("FROGDEN2.csv")
#select date cycle
loops<-loops[loops$DATE.CYCLE==1,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
threshold<-(6/7)
max.CT.window<-nrow(loops)
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0
for(j in 1:max.CT.window){
  
    ct.window[j+1]<-j  
  
    is.rest<-vector()
  
  CTdays.prop<-vector()
  
  
  for(i in 1:(nrow(loops)-(j-1))){
    is.rest[i]<-sum(loops$ALL.RESTS[i:(i+j-1)])>0;
    
    CTdays.prop[i]<-(sum(loops$CAMS[i:(i+j-1)])/j)>=threshold
    
  }
  
  is.rest[is.rest==0 & CTdays.prop!=1]<-2 
  
  null.period<-length(is.rest[is.rest==2])
  true.positive<-length(is.rest[is.rest==1])
  false.negative<-length(is.rest[is.rest==0])
  
  true.pos.prop[j+1]<-(true.positive)/(true.positive+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive+false.negative+null.period)
  
}

data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3)))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
data1
points(data1$True.positives~data1$CT.window,type="l")
sum(data1$True.positives<0.95)
##########################################################################
# #New site: FROGDEN2, SELECTING DATE.CYCLE 2
loops<-read.csv("FROGDEN2.csv")
loops<-loops[loops$DATE.CYCLE==2,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
threshold<-(6/7)
max.CT.window<-nrow(loops)
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0

for(j in 1:max.CT.window){
  
 
  ct.window[j+1]<-j  
  
    is.rest<-vector()
  
  CTdays.prop<-vector()
  
  for(i in 1:(nrow(loops)-(j-1))){
    is.rest[i]<-sum(loops$ALL.RESTS[i:(i+j-1)])>0;
    
    CTdays.prop[i]<-(sum(loops$CAMS[i:(i+j-1)])/j)>=threshold
    
  }
  
  is.rest[is.rest==0 & CTdays.prop!=1]<-2 
  
  null.period<-length(is.rest[is.rest==2])
  true.positive<-length(is.rest[is.rest==1])
  false.negative<-length(is.rest[is.rest==0])
  
  true.pos.prop[j+1]<-(true.positive)/(true.positive+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive+false.negative+null.period)
  
}

data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3)))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
data1
points(data1$True.positives~data1$CT.window,type="l")
sum(data1$True.positives<0.95) 

########################################################################
#New site: GORDON
loops<-read.csv("GORDON.csv")
loops<-loops[loops$DATE.CYCLE==1,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
threshold<-(6/7)
max.CT.window<-nrow(loops)
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0

for(j in 1:max.CT.window){
  
  ct.window[j+1]<-j  
  
    is.rest<-vector()
  
  CTdays.prop<-vector() 
  
    for(i in 1:(nrow(loops)-(j-1))){
    is.rest[i]<-sum(loops$ALL.RESTS[i:(i+j-1)])>0;
    
    CTdays.prop[i]<-(sum(loops$CAMS[i:(i+j-1)])/j)>=threshold
    
  }
  
  is.rest[is.rest==0 & CTdays.prop!=1]<-2 
  
  null.period<-length(is.rest[is.rest==2])
  true.positive<-length(is.rest[is.rest==1])
  false.negative<-length(is.rest[is.rest==0])
  
  true.pos.prop[j+1]<-(true.positive)/(true.positive+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive+false.negative+null.period)
  
}

data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3)))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
data1
points(data1$True.positives~data1$CT.window,type="l")
sum(data1$True.positives<0.95) 

########################################################################
#New site LEARMOUTH1 
loops<-read.csv("LEARMOUTH.csv")
loopsL1<-loops[loops$DATE.CYCLE==1,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
threshold<-(6/7)
max.CT.window<-nrow(loops)
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0

for(j in 1:max.CT.window){
  
   ct.window[j+1]<-j  
  
    is.rest<-vector()
  
  CTdays.prop<-vector()
  
  
    for(i in 1:(nrow(loops)-(j-1))){
    is.rest[i]<-sum(loops$ALL.RESTS[i:(i+j-1)])>0;
    
    CTdays.prop[i]<-(sum(loops$CAMS[i:(i+j-1)])/j)>=threshold
    
  }
  
  is.rest[is.rest==0 & CTdays.prop!=1]<-2 
  
  null.period<-length(is.rest[is.rest==2])
  true.positive<-length(is.rest[is.rest==1])
  false.negative<-length(is.rest[is.rest==0])
  
  true.pos.prop[j+1]<-(true.positive)/(true.positive+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive+false.negative+null.period)
  
}

data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3)))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
data1
points(data1$True.positives~data1$CT.window,type="l")
sum(data1$True.positives<0.95) 
########################################################################
#New site:LEARMOUTH2 
loops<-read.csv("LEARMOUTH.csv")
loops<-loops[loops$DATE.CYCLE==2,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),] 
threshold<-(6/7)
max.CT.window<-nrow(loops)
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0

for(j in 1:max.CT.window){
  
    ct.window[j+1]<-j  
  
  is.rest<-vector()
  
  CTdays.prop<-vector()
  
     
  for(i in 1:(nrow(loops)-(j-1))){
    is.rest[i]<-sum(loops$ALL.RESTS[i:(i+j-1)])>0;
    
    CTdays.prop[i]<-(sum(loops$CAMS[i:(i+j-1)])/j)>=threshold
    
  }
  
  is.rest[is.rest==0 & CTdays.prop!=1]<-2 
  
  null.period<-length(is.rest[is.rest==2])
  true.positive<-length(is.rest[is.rest==1])
  false.negative<-length(is.rest[is.rest==0])
  
  true.pos.prop[j+1]<-(true.positive)/(true.positive+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive+false.negative+null.period)
  
}

data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3)))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
data1
points(data1$True.positives~data1$CT.window,type="l")
sum(data1$True.positives<0.95) 

#####################################################################
#New site: TORQUHAN
loops<-read.csv("TORQUHAN.csv")
loops<-loops[loops$DATE.CYCLE==2,]
loops<-loops[!is.na(loops$WINT.SPR.DAY),]
threshold<-(6/7)
max.CT.window<-nrow(loops)
ct.window<-0 
true.pos.prop<-0
null.period.prop<-0

for(j in 1:max.CT.window){
  
 
  ct.window[j+1]<-j  
  
  is.rest<-vector()
  
  CTdays.prop<-vector()
  
   for(i in 1:(nrow(loops)-(j-1))){
    is.rest[i]<-sum(loops$ALL.RESTS[i:(i+j-1)])>0;
    
    CTdays.prop[i]<-(sum(loops$CAMS[i:(i+j-1)])/j)>=threshold
    
  }
  
  is.rest[is.rest==0 & CTdays.prop!=1]<-2 
  
  null.period<-length(is.rest[is.rest==2])
  true.positive<-length(is.rest[is.rest==1])
  false.negative<-length(is.rest[is.rest==0])
  
  true.pos.prop[j+1]<-(true.positive)/(true.positive+false.negative)
  null.period.prop[j+1]<-(null.period)/(true.positive+false.negative+null.period)
  
}

data1<-na.omit(data.frame(ct.window,round(true.pos.prop,3),round(null.period.prop,3)))
names(data1)<-c("CT.window","True.positives","Proportion.nulls")
data1
points(data1$True.positives~data1$CT.window,type="l",lty=3)
sum(data1$True.positives<0.95) 

#############################################################
#############################################################
#END
#############################################################