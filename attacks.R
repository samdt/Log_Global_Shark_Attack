#Logistic Regression Case Study
library(dplyr)
library(ggplot2)
library(data.table)



#Function to delete cols with > cutoff_na missing values, as written in quality dataframe
delete=function(object, quality, cutoff_na=.25)
{
  #getting the variable names with number of missing values more than cut-off
  a=quality$VariableName[quality$MissingPtage>cutoff_na]
  a=as.character(a)
  a
  col=1:length(a)
  i=1
  for (i in 1:length(a))
  {
    col[i]=grep(a[i], names(object))
  }
  col
  
  #deleting the above columns from main db and quality db
  object=object[,-col]
  return(object)
}



#function to reorder object - targetvar +  factor + quantitative variables
reorder_factor_quant=function(object)
{
  factor=data.frame(1:nrow(object))
  quant=data.frame(1:nrow(object))
  char=data.frame(1:nrow(object))
  i=1
  l=1
  m=1
  n=1
  
  while(i<ncol(object))
  {
    if(class(object[,i])=="factor")
    {
      cat(i, "factor")
      factor=cbind(factor, object[,i])
      names(factor)[l+1]=names(object)[i]
      l=l+1
    }
    else if (class(object[,i])=="character")
    {
      cat(i,"character")  
      char=cbind(char, object[,i])
      names(factor)[m+1]=names(object)[i]
      m=m+1
    }
    else if ((class(object[,i]))=="integer" | class(object[,i])=="numeric")
    {
      cat(i, "quant")
      quant=cbind(quant, object[,i])
      names(quant)[n+1]=names(object)[i]
      n=n+1
    }
    i=i+1
  }  
  names(quant)
  object_factor=data.frame(factor[,-1])
  object_quant=data.frame(quant[,-1])
  #object_char=char[,-1]
  names(object_factor)=names(factor)[2:ncol(factor)]
  names(object_quant)=names(quant)[2:ncol(quant)]
  
  #Removing the event variable from our analysis
  cn=grep("Fatal", names(object_factor))
  Fatal=object_factor[,cn]
  object_factor=object_factor[,-cn]
  object_factor=cbind(Fatal, object_factor)
  
  #Reordering object datasets acc to factor var and quantitative variables
  object=cbind(object_factor,object_quant)
  return(object)
}





#Main Code
setwd("C:\\Users\\Vaibhav\\Desktop\\BA\\jigsaw_acad\\R_Sessions\\Case_Studies\\global-shark-attacks")
oj=read.csv("attacks.csv", na.strings = c("","NA","UNKNOWN", "#VALUE!"))
?read.csv
str(oj)
summary(oj)
object=oj

#Target variable: Fatal

a=grep("Fatal..Y.N.", names(object))
names(object)[a]="Fatal"
table(object$Fatal)#shows "N " as a sep factor
summary(object$Fatal)#shows" N" as a sep factor

object$Fatal[object$Fatal=="y"]="Y"
object$Fatal[object$Fatal=="n"]="N"
object$Fatal[object$Fatal=="F"]="N"
object$Fatal[object$Fatal==" N"]="N"
object$Fatal[object$Fatal=="N "]="N"

#Delete the rows with NA in the target variable
object=object[-which(is.na(object$Fatal)),]

object$Year[object$Year<1942]
object$Year[object$Year<1000]=NA #all values <1000 are less likely

object$Fatal=as.factor(as.character(object$Fatal))

table(object$Sex)#shows "M " as a sep factor
object$Sex[object$Sex=="M "]="M"
object$Sex[object$Sex=="N"]=NA
object$Sex[object$Sex=="."]=NA
object$Sex[object$Sex=="lli"]=NA
object$Sex=as.factor(as.character(object$Sex))#to reduce the number of factors

summary(object)

#quality=quality_report(object)#calling the function quality

#Preparing the quality report
quality=data.frame(names(object))

names(quality)="VariableName"
quality$datatype=class(object[,names(object)])
len=length(names(object))
i=1
for(i in 1:len)
{
  quality$datatype[i]=class(object[,i])
  quality$NoOfRecords[i] = nrow(object)
  quality$UniqueRecords[i]=length(unique(object[,i]))
  quality$DataAvailable[i]= quality$NoOfRecords[i]-sum(is.na(object[,i]))
  quality$AvailablePercent[i]=round(quality$DataAvailable[i]/quality$NoOfRecords[i],2)
  quality$Missing[i]=sum(is.na(object[,i]))
  quality$MissingPtage[i] = round(quality$Missing[i]/quality$NoOfRecords[i],2)  
  quality$Minimum[i]=ifelse(quality$datatype[i]=="factor","Factor",min(object[,i],na.rm=T))
  quality$Maximum[i]=ifelse(quality$datatype[i]=="factor","Factor",max(object[,i],na.rm=T))
  quality$Mean[i]=ifelse(quality$datatype[i]=="factor","Factor", mean(object[,i],na.rm=T))
  quality$five_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(object[,i],5/100, na.rm = T)) 
  quality$ten_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(object[,i],10/100, na.rm = T)) 
  quality$twentyfive_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(object[,i],25/100, na.rm = T)) 
  quality$fifty_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(object[,i],50/100, na.rm = T)) 
  quality$seventyfive_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(object[,i],75/100, na.rm = T))
  quality$ninety_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(object[,i],90/100, na.rm = T))
  quality$ninetyfive_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(object[,i],95/100, na.rm = T))
}  

#delete the cols with NA values > .26
cutoff=.26
object=delete(object, quality,cutoff)

colSums(is.na(object))
summary(object)
names(object)


# reorder object - targetvar +  factor + quantitative variables
object=reorder_factor_quant(object)
head(object)
summary(object)
#so 1st var is dependent var, 2nd var to last-1 var are factor vars and last is quant var
object_factor=object[1:(ncol(object)-1)]

#Analyse factor type variables and finding level-wise Fatal rate
factors_all_lvls=data.frame()#will comprise of level-wise Fatal rate
factors_3lvls=data.frame()#will comprise of Fatal-rate when levels are clubbed to 3 levels
decoding_factors=data.frame()#will comprise of decoding of clubbed levels (to new imputed levels)
Levels0FatalRate=data.frame()#will comprise of factor-levels with Fatal rate=0. We will impute these levels with 0.

for(i in 2:ncol(object_factor))#1st column is dependent variable Fatal
  #we have to do analysis of independent variables (col no. 2 onwards)
{
  object[,i]=as.factor(object[,i])
  object%>%count(Fatal,levels=object[,i])%>%filter(Fatal==1)->dat1
  #dat1 will comprise of levels with Fatal=1
  object%>%filter(object[,i]%in%dat1$levels)->datC2
  rn=which(object[,i]%in%dat1$levels)
  dat1$N=unclass(unname(datC2%>%group_by(datC2[,i])%>%summarize(n())))[[2]]
  dat1$Fatal_Rate=round(dat1$n/dat1$N,3)
  dat1$Total_Levels=rep(length(dat1$levels), length(dat1$levels))
  dat1$Fatal=NULL
  dat1$Col_Num=i
  dat1$Var_Name<-rep(names(object)[i],nrow(dat1))
  
  #Missing values imputation for factor variables:
  if(is.na(dat1$levels[nrow(dat1)]))
  {
    dat1$Total_Levels=dat1$Total_Levels-1
    diff=1:(nrow(dat1)-1)
    for(j in 1:(nrow(dat1)-1))
    {
      diff[j]=dat1$Fatal_Rate[nrow(dat1)]-dat1$Fatal_Rate[j]
    }
    a=(min(Mod(diff)))
    b=sum(diff%in%a)#if a exists in diff then b=1 and if -a exsits in diff then b=0
    row_n=ifelse((b!=0), which(diff%in%a), which(diff%in%(-1*a)))
    p=dat1[row_n,"levels"]#so we will impute the value mentioned in the col level of dat1 dataframe 
    object[is.na(object[,i]),i]=p
    dat1=dat1[-nrow(dat1),] #we need not include the NA values for our Fatal analysis
  }
  factors_all_lvls=rbind(factors_all_lvls, dat1) 
  
  #Identify the factor variables that have levels > 3, and clubbing the levels to 3
  if(dat1$Total_Levels[1]>3)
  {
    dat1%>%mutate(levels=ntile(Fatal_Rate,3))%>%group_by(levels)%>%summarize(n=sum(n), N=sum(N))->dat2
    dat2$Fatal_Rate=dat2$n/dat2$N
    dat2$Total_Levels=3
    dat2$Col_Num=i
    dat2$Var_Name<-names(object)[i]
    dat1%>%mutate(imputed_lvl=ntile(Fatal_Rate,3))%>%count(imputed_lvl, levels)->dat4
    dat4$Var_Name=names(object)[i]
    dat4$Col_Num=i
    dat1=dat2
    decoding_factors=rbind(decoding_factors, dat4)
  }  
  factors_3lvls=rbind(factors_3lvls, dat1)#factors_3lvls dataset after clubbing the levels to 3
  #  print(i)
  
  if(length(object[-rn,i])==0) #if there are no rows with 0 Fatal-rate then we skip the further code in for-loop
    next
  
  #if there are rows which have been skipped from above analysis of object[,i], these are the rows with Fatal-rate=0
  #we will append these rows to Level0FatalRate and eventually impute these levels by level0
  levels=unique(object[-rn,i])#to get the levels with 0 Fatal-rate
  imputed_lvl=rep(0, length(levels))
  datC1=data.frame(imputed_lvl=imputed_lvl, levels=levels)
  datC1$Var_Name=dat1$Var_Name[1]
  datC1$Col_Num=dat1$Col_Num[1]
  Levels0FatalRate=rbind(Levels0FatalRate, datC1)#The levels with 0 Fatal rate are included in df Level0FatalRate
  #We will eventually club the Levels0FatalRate DataFrame with dataframe level_decoding
  #All the levels in Levels0FatalRate will be coded to Level 0 in object database
}
rm(dat1)
rm(dat4)
rm(dat2)
rm(datC2)
rm(datC1)
rm(p)

write.csv(factors_all_lvls,"factors_all_lvls.csv")#comprises of Fatal-rate for all levels
write.csv(factors_3lvls,"factors_3lvls.csv")
#analysing factor dataframe, there should be a reasonable difference (atleast 2%) in the even

#further reducing the levels of factor type variables based on event-rate. 
#club together the factors if event rate difference is small (chosing 2% difference)
#making correponding entry in decoding_factors dataset as well
factors_final_clubbed=data.frame()
c=1
factors_3lvls=factors_3lvls[order(factors_3lvls$Col_Num),]
for(i in 1:nrow(factors_3lvls))
{
  if(factors_3lvls$Col_Num[i]==c) {next}
  c=factors_3lvls$Col_Num[i]
  
  dat1=factors_3lvls[factors_3lvls$Col_Num==c,]
  num_rows=nrow(dat1)
  if(num_rows==3)
  {
    x=dat1$Fatal_Rate[1]-dat1$Fatal_Rate[2]
    y=dat1$Fatal_Rate[2]-dat1$Fatal_Rate[3]
    z=dat1$Fatal_Rate[1]-dat1$Fatal_Rate[3]
    
    if((x)^2<.0004 & y^2<.0004)
    {
      dat1$n[1]=dat1$n[2]+dat1$n[3]+dat1$n[1]
      dat1$N[1]=dat1$N[2]+dat1$N[3]+dat1$N[1]
      del=c(2,3)#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=1
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==2]=dat1$levels[1]
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==3]=dat1$levels[1]
      
      #if there is no-existing entry in the combined-factor db, then we can make a new entry
      #as this dataset will be useful for substitution in the main 'object' dataset
      
      #if level reduction is happening for the first time, then we need make fresh entry in decoding_factors
      if(sum(decoding_factors$Col_Num %in% c)==0)
      {
        dat=data.frame(imputed_lvl=dat1$levels[1], levels=levels(object[,c])[c(2,3)], n=1, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
      }
    }
    else if(x^2<.0004)
    {
      dat1$n[1]=dat1$n[1]+dat1$n[2]
      dat1$N[1]=dat1$N[1]+dat1$N[2]
      del=2#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=2
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==2]=dat1$levels[1]
      if(sum(decoding_factors$Col_Num %in% c)==0)
      {
        dat=data.frame(imputed_lvl=dat1$levels[1], levels=levels(object[,c])[2], n=1, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
      }
    }
    else if(y^2<.0004)
    {
      dat1$n[2]=dat1$n[2]+dat1$n[3]
      dat1$N[2]=dat1$N[2]+dat1$N[3]
      del=3#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=2
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==3]=dat1$levels[2]
      if(sum(decoding_factors$Col_Num %in% c)==0)
      {
        dat=data.frame(imputed_lvl=dat1$levels[2], levels=levels(object[,c])[3], n=1, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
      }
    }  
  }
  if((num_rows==2))
  {
    x=dat1$Fatal_Rate[1]-dat1$Fatal_Rate[2]
    if(x^2<.0004)
    {
      dat1$n[1]=dat1$n[1]+dat1$n[2]
      dat1$N[1]=dat1$N[1]+dat1$N[2]
      del=2#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=1
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==2]=dat1$levels[1]
      if(sum(decoding_factors$Col_Num %in% c)==0)
      {
        dat=data.frame(imputed_lvl=dat1$levels[1], levels=levels(object[,c])[2], n=1, Var_Name = dat1$Var_Name, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
      }
    }
  }
  dat1$Fatal_Rate=dat1$n/dat1$N
  factors_final_clubbed=rbind(factors_final_clubbed,dat1)
}        

rm(dat1)

#For the levels with  Fatal-rate = 0 (i.e not even 1 entry in the level has Fatal =1)
#we will impute those values by 0 - and accordingly assign in decoding_factors
#i.e we'll append Levels0FatalRate to decoding_factors
decoding_factors$n=NULL
decoding_factors=rbind(decoding_factors, Levels0FatalRate)
#So, dataframe: decoding_factors contains the decoding of all the clubbed levels to original levels

#Imputing the values in the main object data-set as per the decoding_factors
for(i in 1:nrow(decoding_factors))
{
  rn=which(object[, decoding_factors$Col_Num[i]] %in% decoding_factors$levels[i])
  object[,decoding_factors$Col_Num[i]]=as.character(object[,decoding_factors$Col_Num[i]])
  object[rn,decoding_factors$Col_Num[i]]=decoding_factors$imputed_lvl[i]
}
colSums(is.na(object)) #confirms no missing values


write.csv(decoding_factors, "decoding_factors.csv")
write.csv(factors_final_clubbed,"factors_final_clubbed.csv")


#converting all the factor-type columns into factor class back
for (i in 1:ncol(object_factor))
{
  object[,i]=as.factor(object[,i])
}
summary(object[,(1:ncol(object_factor))])

object_factor=object[,c(1:ncol(object_factor))]
dat=data.frame()

#if number of levels = 1, then we'll not include the variable for modeling
for(i in 1:ncol(object_factor))
{
  if(length((levels(object_factor[,i])))==1)
  {
    dat=rbind(dat,i)
  }
}
names(dat)="col_num"
object=object[,-dat$col_num]
object_factor=object_factor[,-dat$col_num]
summary(object[,1:ncol(object_factor)])
#note: the decoding of the levels is given in decoding_factors










object$Injury=NULL
quality=quality[-which(quality$VariableName=="Injury"),]
quality$VariableName==names(object)
object$Investigator.or.Source=NULL
quality=quality[-which(quality$VariableName=="Investigator.or.Source"),]
object$Name=NULL
quality=quality[-which(quality$VariableName=="Name"),]
object$Location=NULL
quality=quality[-which(quality$VariableName=="Location"),]

names(object)==quality$VariableName
summary(object)
#Too many levels in Country Area and Activity because of which decision tree will not give good result

#We will find event rate and club the factors with similar rate. 
#We notice factor vars have more than 3 levels - so difficult to analyze









#Trying to create decision tree
library(rpart)
mod<-rpart(Fatal~.,data=object[,-1],method="class", control=rpart.control(maxdepth=1))
summary(mod)
library(rattle)
fancyRpartPlot(mod)
mod


#Preparing quality report
quality=data.frame(names(oj))

names(quality)="VariableName"
quality$datatype=class(oj[,names(oj)])
len=length(names(oj))
i=1
for(i in 1:len)
{
  quality$datatype[i]=class(oj[,i])
  quality$NoOfRecords[i] = nrow(oj)
  quality$UniqueRecords[i]=length(unique(oj[,i]))
  quality$DataAvailable[i]= quality$NoOfRecords[i]-sum(is.na(oj[,i]))
  quality$AvailablePercent[i]=round(quality$DataAvailable[i]/quality$NoOfRecords[i],2)
  quality$Missing[i]=sum(is.na(oj[,i]))
  quality$MissingPtage[i] = round(quality$Missing[i]/quality$NoOfRecords[i],2)  
  quality$Minimum[i]=ifelse(quality$datatype[i]=="factor","Factor",min(oj[,i],na.rm=T))
  quality$Maximum[i]=ifelse(quality$datatype[i]=="factor","Factor",max(oj[,i],na.rm=T))
  quality$Mean[i]=ifelse(quality$datatype[i]=="factor","Factor", mean(oj[,i],na.rm=T))
  quality$five_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],5/100, na.rm = T)) 
  quality$ten_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],10/100, na.rm = T)) 
  quality$twentyfive_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],25/100, na.rm = T)) 
  quality$fifty_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],50/100, na.rm = T)) 
  quality$seventyfive_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],75/100, na.rm = T))
  quality$ninety_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],90/100, na.rm = T))
  quality$ninetyfive_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],95/100, na.rm = T))
}  

length(names(quality))

write.csv(quality,"quality.csv")


#target variable is Fatal - 0 or 1 
object=oj
summary(object$Fatal)#only 23.92% of values have high Fatal rate

#How many missing values in the data
colSums(is.na(object))

#all the variables with high number of missing values, as per quality report
#we'll understand them and exclude for our modelling, if not critical for Fatal understanding
cutoff_na=.25 #declaring the percentage cut off of missing values


#getting the variable names with number of missing values more than cut-off
a=quality$VariableName[quality$MissingPtage>cutoff_na]
a=as.character(a)
a

col=length(a)
for (i in 1:length(a))
{
  col[i]=grep(a[i], names(object))}
class(col)
#deleting the above columns from main db and quality db
object=object[,-col]
a=which(quality$MissingPtage>cutoff_na)
quality=quality[-a,]


#let's seperate factor variables and qualitative variables for our analysis
quality_factor=quality[quality$datatype=="factor" | (quality$datatype=="integer" & quality$UniqueRecords<20),]
ser_num=1:nrow(quality_factor)
quality_factor=cbind(ser_num, quality_factor)
p=which(quality$VariableName %in% quality_factor$VariableName)
quality_quant=quality[-p,]
ser_num=(nrow(quality_factor) + 1):(nrow(quality_quant) + nrow(quality_factor))
quality_quant=cbind(ser_num, quality_quant)


#doing the above re-ordering in main object dataset as well
a=quality$VariableName[quality$datatype=="factor" | (quality$datatype=="integer" & quality$UniqueRecords<20)]
a=as.character(a)
col=1:length(a)
for (i in 1:length(a))
{
  col[i]=grep(a[i], names(object))
  #  print(i)
}
object_factor=object[,col]
object_quant=object[,-col]



#Removing the event variable from our analysis
cn=grep("Fatal", names(object_factor))
#quality_row1=quality_factor[cn,]
#quality_factor=quality_factor[-cn,]
#quality_factor=rbind(quality_row1, quality_factor)
Fatal=object_factor[,cn]
object_factor=object_factor[,-cn]
object_factor=cbind(Fatal, object_factor)

#Reordering quality and object datasets acc to factor var and quantitative variables
#quality=rbind(quality_factor, quality_quant)
object=cbind(object_factor,object_quant)

head(object)
colSums(is.na(object))

#Analysing quantitative variables
Quant_DA=data.frame()
x=ncol(object_factor)+1

#decile-wise (/n-quantile-wise) binning of quantitative variables, and finding the corresponding event rate:
for(i in x:ncol(object))
{
  p=as.name(names(object)[i])
  for(j in 10:1)
  {
    max=unclass(object%>%mutate(Lvl = ntile(object[,i],n=j))%>%group_by(Lvl)%>%summarize(round(max(p),2))%>%unname())[[2]]
    if(max[1]!=0)#if the max value is 0, then we will try making j-1 quantiles for this variable
    { 
      break
    }
  }
  dat1=data.frame(1:(j+1), 0) #for levels + 1 NA level
  names(dat1)=c("Lvl", "Num_High_Fatals")
  dat1[(j+1),1]=NA #11th row represents the number of NA values
  dat1$Num_High_Fatals=rep(0,(j+1))
  object%>%mutate(Lvl = ntile(object[,i],n=j))->object
  object%>%group_by(Lvl)%>%count(Fatal,Lvl)%>%filter(Fatal==1)->dat2
  dat1$Num_High_Fatals=rep(0,(j+1))
  dat1$Num_High_Fatals[which(dat1$Lvl %in% dat2$Lvl)]=dat2$n
  dat1$FatalVariable = names(object)[i]
  Total = unclass(object%>%group_by(Lvl)%>%summarize(n())%>%unname())[[2]]
  if(length(Total)!=j+1) Total[j+1]=NA
  dat1$Total = Total
  dat1$Fatal_Rate=round(dat1$Num_High_Fatals/dat1$Total,2)
  dat1$Col_Num=i
  dat1$DataType=class(object[,i])
  max[j+1]=NA 
  dat1$Max_Lvl=max
  min=unclass(object%>%group_by(Lvl)%>%summarize(round(min(p),2))%>%unname())[[2]]
  min[j+1]=NA
  dat1$Min_Lvl=min
  mean=unclass(object%>%group_by(Lvl)%>%summarize(round(mean(p),2))%>%unname())[[2]]
  mean[j+1]=NA
  dat1$Mean_Lvl=mean
  dat1$levels = j+1
  Quant_DA=rbind(Quant_DA,dat1)
  #  print(i)
  object$Lvl=NULL
}
rm(dat1)
write.csv(Quant_DA, "quantitative_data_analysis.csv")


#Missing values imputation of quantitative data. 
# Impute the value of NAs in each col acc. to Fatal rate

j=1
rn=0
for(i in 1:(nrow(Quant_DA)))
{ 
  if(i!=(rn[j]+1)) next 
  #if we reach row number = last row of dat data-frame, then re-start the loop
  #i.e we can skip the loop for all rows but the first row of each column number
  
  j=Quant_DA$levels[i]
  p=Quant_DA$Col_Num[i]
  Quant_DA%>%filter(Col_Num==p)->dat
  diff=rep(0,(j-1))
  
  
  #if there is no NA value in the variable (i.e Fatal rate of NA), then move to next variable  
  if(is.na(dat$Fatal_Rate[j]))
  {
    rn=which(Quant_DA$FatalVariable%in%dat$FatalVariable)
    next
  }
  #check the difference between the Fatal rate of NA with Fatal rate of each decile
  for(k in 1:(j-1))
  {
    diff[k]=dat$Fatal_Rate[j]-dat$Fatal_Rate[k]
  } 
  a=(min(Mod(diff)))
  #we will take the minimum difference and substitute the mean value in that level
  b=sum(diff%in%a)
  mean=ifelse((b!=0),mean(dat$Mean_Lvl[which(diff%in%a)]), (dat$Mean_Lvl[which(diff%in%(-1*a))]))  
  object[is.na(object[, dat$Col_Num[1]]), dat$Col_Num[1]]=mean
  rn=which(Quant_DA$FatalVariable%in%dat$FatalVariable)
}
object_quant=object[,((ncol(object_factor)+1):ncol(object))]

colSums(is.na(object_quant))


#Keeping only the continuous variables which show some trend / important for
#so we can exclude the following continuous variables from our modelling data-set: rev_Range, age2, da_Mean, da_Range, hnd_Price
i=length(names(object_factor))
object_quant=object_quant[, -c(21-i,49-i,57-i,58-i)]
rn=which(Quant_DA$Col_Num %in% c(21,49, 57,58))
Quant_DA=Quant_DA[-rn,]

names(object_quant)
object=cbind(object_factor, object_quant)

#Updating col. numbers in Quant_DA
for(i in 1:nrow(Quant_DA))
{
  Quant_DA$Col_Num[i]= which(names(object) %in% Quant_DA$FatalVariable[i])
}


#Analysing outliars and doing outliars imputation in continuous variables

x=length(names(object_factor))+1
for(i in x:length(names(object))) 
{
  #outliar treatment in col i of object
  a=boxplot.stats(object[,i] )
  if(a$stats[1]==a$stats[5])
    next  
  # if a$stats[1]=a$stats[5], and if we proceed with outliar imputation the whole
  #    column will reduce into a single value. There is a complete loss in information,
  #    which might be important for us. So, we exclude such variables from outliar imputation process
  
  if(length(a$out)!=0)
  { 
    min=min(object[,i])
    max=max(object[,i])
    mn=min-1
    mx=max+1
    #replacing the bottom outliars with a$stats[1], and top outliars with a$stats[5]
    replacement_value =cut(a$out,breaks=c(mn, a$stats[1], mx), labels=c(a$stats[1], a$stats[5]))
    #converting replacement_value to non-factor - from factor to character
    replacement_value=as.character(replacement_value)
    replacement_value=as.numeric(replacement_value)
    out=data.frame(a$out,replacement_value)
    
    #imputing outliars with min or max values
    out$maindataset_row_numbers=which(object[,i]%in% out$a.out )
    rn=out$maindataset_row_numbers
    object[rn,i]=out$replacement_value
  }
}
rm(out)
summary(object)


#Analyse factor type variables and finding level-wise Fatal rate
factors_all_lvls=data.frame()#will comprise of level-wise Fatal rate
factors_3lvls=data.frame()#will comprise of Fatal-rate when levels are clubbed to 3 levels
decoding_factors=data.frame()#will comprise of decoding of clubbed levels (to new imputed levels)
Levels0FatalRate=data.frame()#will comprise of factor-levels with Fatal rate=0. We will impute these levels with 0.

for(i in 2:ncol(object_factor))#1st column is dependent variable Fatal
  #we have to do analysis of independent variables (col no. 2 onwards)
{
  object[,i]=as.factor(object[,i])
  object%>%count(Fatal,levels=object[,i])%>%filter(Fatal==1)->dat1
  #dat1 will comprise of levels with Fatal=1
  object%>%filter(object[,i]%in%dat1$levels)->datC2
  rn=which(object[,i]%in%dat1$levels)
  dat1$N=unclass(unname(datC2%>%group_by(datC2[,i])%>%summarize(n())))[[2]]
  dat1$Fatal_Rate=round(dat1$n/dat1$N,3)
  dat1$Total_Levels=rep(length(dat1$levels), length(dat1$levels))
  dat1$Fatal=NULL
  dat1$Col_Num=i
  dat1$Var_Name<-rep(names(object)[i],nrow(dat1))
  
  #Missing values imputation for factor variables:
  if(is.na(dat1$levels[nrow(dat1)]))
  {
    dat1$Total_Levels=dat1$Total_Levels-1
    diff=1:(nrow(dat1)-1)
    for(j in 1:(nrow(dat1)-1))
    {
      diff[j]=dat1$Fatal_Rate[nrow(dat1)]-dat1$Fatal_Rate[j]
    }
    a=(min(Mod(diff)))
    b=sum(diff%in%a)#if a exists in diff then b=1 and if -a exsits in diff then b=0
    row_n=ifelse((b!=0), which(diff%in%a), which(diff%in%(-1*a)))
    p=dat1[row_n,"levels"]#so we will impute the value mentioned in the col level of dat1 dataframe 
    object[is.na(object[,i]),i]=p
    dat1=dat1[-nrow(dat1),] #we need not include the NA values for our Fatal analysis
  }
  factors_all_lvls=rbind(factors_all_lvls, dat1) 
  
  #Identify the factor variables that have levels > 3, and clubbing the levels to 3
  if(dat1$Total_Levels[1]>3)
  {
    dat1%>%mutate(levels=ntile(Fatal_Rate,3))%>%group_by(levels)%>%summarize(n=sum(n), N=sum(N))->dat2
    dat2$Fatal_Rate=dat2$n/dat2$N
    dat2$Total_Levels=3
    dat2$Col_Num=i
    dat2$Var_Name<-names(object)[i]
    dat1%>%mutate(imputed_lvl=ntile(Fatal_Rate,3))%>%count(imputed_lvl, levels)->dat4
    dat4$Var_Name=names(object)[i]
    dat4$Col_Num=i
    dat1=dat2
    decoding_factors=rbind(decoding_factors, dat4)
  }  
  factors_3lvls=rbind(factors_3lvls, dat1)#factors_3lvls dataset after clubbing the levels to 3
  #  print(i)
  
  if(length(object[-rn,i])==0) #if there are no rows with 0 Fatal-rate then we skip the further code in for-loop
    next
  
  #if there are rows which have been skipped from above analysis of object[,i], these are the rows with Fatal-rate=0
  #we will append these rows to Level0FatalRate and eventually impute these levels by level0
  levels=unique(object[-rn,i])#to get the levels with 0 Fatal-rate
  imputed_lvl=rep(0, length(levels))
  datC1=data.frame(imputed_lvl=imputed_lvl, levels=levels)
  datC1$Var_Name=dat1$Var_Name[1]
  datC1$Col_Num=dat1$Col_Num[1]
  Levels0FatalRate=rbind(Levels0FatalRate, datC1)#The levels with 0 Fatal rate are included in df Level0FatalRate
  #We will eventually club the Levels0FatalRate DataFrame with dataframe level_decoding
  #All the levels in Levels0FatalRate will be coded to Level 0 in object database
}
rm(dat1)
rm(dat4)
rm(dat2)
rm(datC2)
rm(datC1)
rm(p)

write.csv(factors_all_lvls,"factors_all_lvls.csv")#comprises of Fatal-rate for all levels
write.csv(factors_3lvls,"factors_3lvls.csv")
#analysing factor dataframe, there should be a reasonable difference (atleast 2%) in the even

#further reducing the levels of factor type variables based on event-rate. 
#club together the factors if event rate difference is small (chosing 2% difference)
#making correponding entry in decoding_factors dataset as well
factors_final_clubbed=data.frame()
c=1
factors_3lvls=factors_3lvls[order(factors_3lvls$Col_Num),]
for(i in 1:nrow(factors_3lvls))
{
  if(factors_3lvls$Col_Num[i]==c) {next}
  c=factors_3lvls$Col_Num[i]
  
  dat1=factors_3lvls[factors_3lvls$Col_Num==c,]
  num_rows=nrow(dat1)
  if(num_rows==3)
  {
    x=dat1$Fatal_Rate[1]-dat1$Fatal_Rate[2]
    y=dat1$Fatal_Rate[2]-dat1$Fatal_Rate[3]
    z=dat1$Fatal_Rate[1]-dat1$Fatal_Rate[3]
    
    if((x)^2<.0004 & y^2<.0004)
    {
      dat1$n[1]=dat1$n[2]+dat1$n[3]+dat1$n[1]
      dat1$N[1]=dat1$N[2]+dat1$N[3]+dat1$N[1]
      del=c(2,3)#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=1
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==2]=dat1$levels[1]
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==3]=dat1$levels[1]
      
      #if there is no-existing entry in the combined-factor db, then we can make a new entry
      #as this dataset will be useful for substitution in the main 'object' dataset
      
      #if level reduction is happening for the first time, then we need make fresh entry in decoding_factors
      if(sum(decoding_factors$Col_Num %in% c)==0)
      {
        dat=data.frame(imputed_lvl=dat1$levels[1], levels=levels(object[,c])[c(2,3)], n=1, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
      }
    }
    else if(x^2<.0004)
    {
      dat1$n[1]=dat1$n[1]+dat1$n[2]
      dat1$N[1]=dat1$N[1]+dat1$N[2]
      del=2#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=2
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==2]=dat1$levels[1]
      if(sum(decoding_factors$Col_Num %in% c)==0)
      {
        dat=data.frame(imputed_lvl=dat1$levels[1], levels=levels(object[,c])[2], n=1, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
      }
    }
    else if(y^2<.0004)
    {
      dat1$n[2]=dat1$n[2]+dat1$n[3]
      dat1$N[2]=dat1$N[2]+dat1$N[3]
      del=3#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=2
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==3]=dat1$levels[2]
      if(sum(decoding_factors$Col_Num %in% c)==0)
      {
        dat=data.frame(imputed_lvl=dat1$levels[2], levels=levels(object[,c])[3], n=1, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
      }
    }  
  }
  if((num_rows==2))
  {
    x=dat1$Fatal_Rate[1]-dat1$Fatal_Rate[2]
    if(x^2<.0004)
    {
      dat1$n[1]=dat1$n[1]+dat1$n[2]
      dat1$N[1]=dat1$N[1]+dat1$N[2]
      del=2#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=1
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==2]=dat1$levels[1]
      if(sum(decoding_factors$Col_Num %in% c)==0)
      {
        dat=data.frame(imputed_lvl=dat1$levels[1], levels=levels(object[,c])[2], n=1, Var_Name = dat1$Var_Name, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
      }
    }
  }
  dat1$Fatal_Rate=dat1$n/dat1$N
  factors_final_clubbed=rbind(factors_final_clubbed,dat1)
}        

rm(dat1)

#For the levels with  Fatal-rate = 0 (i.e not even 1 entry in the level has Fatal =1)
#we will impute those values by 0 - and accordingly assign in decoding_factors
#i.e we'll append Levels0FatalRate to decoding_factors
decoding_factors$n=NULL
decoding_factors=rbind(decoding_factors, Levels0FatalRate)
#So, dataframe: decoding_factors contains the decoding of all the clubbed levels to original levels

#Imputing the values in the main object data-set as per the decoding_factors
for(i in 1:nrow(decoding_factors))
{
  rn=which(object[, decoding_factors$Col_Num[i]] %in% decoding_factors$levels[i])
  object[,decoding_factors$Col_Num[i]]=as.character(object[,decoding_factors$Col_Num[i]])
  object[rn,decoding_factors$Col_Num[i]]=decoding_factors$imputed_lvl[i]
}
colSums(is.na(object)) #confirms no missing values


write.csv(decoding_factors, "decoding_factors.csv")
write.csv(factors_final_clubbed,"factors_final_clubbed.csv")


#converting all the factor-type columns into factor class back
for (i in 1:ncol(object_factor))
{
  object[,i]=as.factor(object[,i])
}
summary(object[,(1:ncol(object_factor))])

object_factor=object[,c(1:ncol(object_factor))]
dat=data.frame()

#if number of levels = 1, then we'll not include the variable for modeling
for(i in 1:ncol(object_factor))
{
  if(length((levels(object_factor[,i])))==1)
  {
    dat=rbind(dat,i)
  }
}
names(dat)="col_num"
object=object[,-dat$col_num]
object_factor=object_factor[,-dat$col_num]
summary(object[,1:ncol(object_factor)])
#note: the decoding of the levels is given in decoding_factors


#Creating New Transformation Variables

#Percentage of completed voice calls can be given by following formula:
#Comp_Ptage_VCE=comp_vce_mean/plcd_vce_Mean
object$Comp_Ptage_VCE=ifelse(object$plcd_vce_Mean!=0,object$comp_vce_Mean/object$plcd_vce_Mean,0)
object$Comp_Ptage_VCE=round(object$Comp_Ptage_VCE,2)
object%>%mutate(Lvl=ntile(object$Comp_Ptage_VCE,10))%>%count(Lvl,Fatal)%>%filter(Fatal==1)->dat
dat$FatalVariable="Comp_Ptage_VCE"
dat$Total=unclass(object%>%mutate(Lvl=ntile(object$Comp_Ptage_VCE,10))%>%count(Lvl)%>%unname())[[2]]
dat$Fatal_Rate=dat$n/dat$Total
dat$Col_Num=ncol(object)
dat$DataType="numeric"
dat$Max_Lvl=unclass(object%>%mutate(Lvl=ntile(Comp_Ptage_VCE,10))%>%group_by(Lvl)%>%summarize(max(Comp_Ptage_VCE))%>%unname())[[2]]
dat$Max_Lvl=unclass(object%>%mutate(Lvl=ntile(object$Comp_Ptage_VCE,10))%>%group_by(Lvl)%>%summarize(max(Comp_Ptage_VCE))%>%unname())[[2]]
dat$Min_Lvl=unclass(object%>%mutate(Lvl=ntile(object$Comp_Ptage_VCE,10))%>%group_by(Lvl)%>%summarize(min(Comp_Ptage_VCE))%>%unname())[[2]]
dat$Mean_Lvl=unclass(object%>%mutate(Lvl=ntile(object$Comp_Ptage_VCE,10))%>%group_by(Lvl)%>%summarize(mean(Comp_Ptage_VCE))%>%unname())[[2]]
dat$Fatal=NULL
dat$levels=10
names(dat)=names(Quant_DA)
Quant_DA=rbind(dat, Quant_DA)

a=cbind(object$comp_vce_Mean, object$Comp_Ptage_VCE, object$plcd_vce_Mean)
cor(a)
pairs(a)
#Since cor is not very high, we will keep these columns


plot(x=1:10, y=dat$Fatal_Rate[1:10], type="l", col="red", xlab="Completed Percentage of Voice Calls", ylab="Fatal Rate")
#We observe clear decreasing trend (i.e inverse relationship)
#As we increase the percentage of completed voice calls, the Fatal rate will decrease


#Percentage of dropped voice calls can be given by following formula:
#drop_vce_Mean/plcd_vce_Mean
object$drop_Ptage_VCE=ifelse(object$plcd_vce_Mean!=0, object$drop_vce_Mean/object$plcd_vce_Mean,NA)
object$drop_Ptage_VCE=round(object$drop_Ptage_VCE,2)
obj=object[is.na(object$drop_Ptage_VCE)!=1,]
obj%>%mutate(Lvl=ntile(obj$drop_Ptage_VCE,4))%>%count(Lvl,Fatal)%>%filter(Fatal==1)->dat
dat$FatalVariable="drop_Ptage_VCE"
dat$Total=unclass(obj%>%mutate(Lvl=ntile(obj$drop_Ptage_VCE,4))%>%count(Lvl)%>%unname())[[2]]
dat$Fatal_Rate=dat$n/dat$Total
dat$Col_Num=ncol(obj)
dat$DataType="numeric"
dat$Max_Lvl=unclass(obj%>%mutate(Lvl=ntile(drop_Ptage_VCE,4))%>%group_by(Lvl)%>%summarize(max(drop_Ptage_VCE))%>%unname())[[2]]
dat$Max_Lvl=unclass(obj%>%mutate(Lvl=ntile(obj$drop_Ptage_VCE,4))%>%group_by(Lvl)%>%summarize(max(drop_Ptage_VCE))%>%unname())[[2]]
dat$Min_Lvl=unclass(obj%>%mutate(Lvl=ntile(obj$drop_Ptage_VCE,4))%>%group_by(Lvl)%>%summarize(min(drop_Ptage_VCE))%>%unname())[[2]]
dat$Mean_Lvl=unclass(obj%>%mutate(Lvl=ntile(obj$drop_Ptage_VCE,4))%>%group_by(Lvl)%>%summarize(mean(drop_Ptage_VCE))%>%unname())[[2]]
dat$Fatal=NULL
dat$levels=4
names(dat)=names(Quant_DA)
plot(x=1:4, y=dat$Fatal_Rate[1:4], type="l", col="red", xlab="Percentage of Failed Voice Calls", ylab="Fatal Rate")
#As the percentage of failed calls increases, the Fatal rate decreases. 
#     This is counter-intuitive, hence we drop this variable
#object$drop_Ptage_VCE=NULL

#imputing the na values by median values
object$drop_Ptage_VCE[which(is.na(object$drop_Ptage_VCE))]=median(object$drop_Ptage_VCE,na.rm = TRUE)



#Making a new variable MRC_ASL

#Divide the totMRC_Mean into 2 categories Low and High Cost
object%>%mutate(quantile=ntile(totmrc_Mean,2))%>%filter(Fatal==1)%>%count(quantile)->dat
dat$total=unclass(object%>%mutate(quantile=ntile(totmrc_Mean,2))%>%count(quantile)%>%unname())[[2]]
dat$Fatal_rate=dat$n/dat$total
dat$max=unclass(object%>%mutate(quantile=ntile(totmrc_Mean,2))%>%group_by(quantile)%>%summarize(max(totmrc_Mean))%>%unname())[[2]]
dat$min=unclass(object%>%mutate(quantile=ntile(totmrc_Mean,2))%>%group_by(quantile)%>%summarize(min(totmrc_Mean))%>%unname())[[2]]
#So we notice that Fatal rate reduces with high cost voice plans

#Making a new variable MRC_ASL
object$MRC_ASL=ifelse(object$asl_flag=="N"& object$totmrc_Mean<=44.9, "MRCL_ASLN", ifelse(object$asl_flag=="Y"&object$totmrc_Mean<=44.9, "MRCL_ASLY", ifelse(object$asl_flag=="Y"&object$totmrc_Mean>44.99, "MRCH_ASLY", "MRCH_ASLN")))
object$MRC_ASL=as.factor(object$MRC_ASL)

#finding Fatal rate of MRC_ASL as per the levels
object%>%count(MRC_ASL, Fatal)%>%filter(Fatal==1)->dat
dat$total=unclass(object%>%count(MRC_ASL)%>%unname())[[2]]
dat$Fatal_rate=dat$n/dat$total
#So highest Fatal where cost of calling plan is low and ASL flag is No
#Lowest Fatal is base cost of calling plan is high and ASL flag is Yes
rm(dat)
rm(obj)


#Creating dummies of certain quatitative variables based on Fatal rate in dataframe "Quant_DA"

object$owylis_vce_Range=ifelse(object$owylis_vce_Range<=1,"Low","High")
object$owylis_vce_Range=as.factor(object$owylis_vce_Range)

object$mou_opkv_Range=ifelse(object$mou_opkv_Range<=1.01,"Low","High")
object$mou_opkv_Range=as.factor(object$mou_opkv_Range)

object$iwylis_vce_Mean=ifelse(object$iwylis_vce_Mean<=.33,"Low","High")
object$iwylis_vce_Mean=as.factor(object$iwylis_vce_Mean)

object$ovrrev_Mean=ifelse(object$ovrrev_Mean<=.96,"Low","High")
object$ovrrev_Mean=as.factor(object$ovrrev_Mean)

object$comp_vce_Mean=ifelse(object$comp_vce_Mean<=4, "Low", ifelse(object$comp_vce_Mean<=22, "Med", ifelse(object$comp_vce_Mean<=177.67,"High", "V_High")))
object$comp_vce_Mean=as.factor(object$comp_vce_Mean)

object$plcd_vce_Mean=ifelse(object$plcd_vce_Mean<=5.67, 1, ifelse(object$plcd_vce_Mean<=29.33, 2,3))
object$plcd_vce_Mean=as.factor(object$plcd_vce_Mean)

object$avg6mou=ifelse(object$avg6mou<=137, "Low", ifelse(object$avg6mou<=896,"Med", "High"))
object$avg6mou=as.factor(object$avg6mou)

object$avg3qty=ifelse(object$avg3qty<=21,"Low", ifelse(object$avg3qty<=283, "Med", "High"))
object$avg3qty=as.factor(object$avg3qty)

object$avgqty=ifelse(object$avgqty<=373.83,"Low","High")
object$avgqty=as.factor(object$avgqty)


#We observe a counter-intuitive trend in drop_vce_Range, 
#           so we drop this variable from our modelling
object$drop_vce_Range=NULL

object$callwait_Mean=ifelse(object$callwait_Mean<=.33,"Low", "High")
object$callwait_Mean=as.factor(object$callwait_Mean)

object$ovrmou_Mean=ifelse(object$ovrmou_Mean<=2.75, "Low", "High")
object$ovrmou_Mean=as.factor(object$ovrmou_Mean)


#We observe a counter-intuitive trend in adjmou. 
#     As adjmou increases, Fatal should decrease. Hence we'll not include this variable for modelling
object$adjmou=NULL
#Also excluding the categorical variables which don't seem to impact Fatal behaviour
object$models=NULL
object$area=NULL
#Since drop_blk_mean is also showing counter-intuitive trend, so we drop it from our analysis
object$drop_blk_Mean=NULL


#Dividing data into training and validation sets

set.seed(100)
row_n=sample(1:nrow(object), .7*nrow(object), FALSE)
training=object[row_n,]
test=object[row_n,]

a=grep("Customer_ID", names(training))

#Applying logistic regression model on data:
myresult=glm(Fatal~., data=training[,-a], family=binomial)
summary(myresult)



#Creating more dummies
object$MRCH_ASLY=ifelse(object$MRC_ASL=="MRCH_ASLY", 1,0)
object$MRCL_ASLN=ifelse(object$MRC_ASL=="MRCL_ASLN", 1,0)
object$MRCL_ASLY=ifelse(object$MRC_ASL=="MRCL_ASLY", 1,0)
object$MRC_ASL=NULL
object$aslY_MRC=NULL

object$comp_vce_Mean_Low=ifelse(object$comp_vce_Mean=="Low",1,0)
object$comp_vce_Mean_Med=ifelse(object$comp_vce_Mean=="Med",1,0)
object$comp_vce_Mean_VHigh=ifelse(object$comp_vce_Mean=="V_High",1,0)
object$comp_vce_Mean=NULL

object$avg3qty_Low=ifelse(object$avg3qty=="Low",1,0)
object$avg3qty_Med=ifelse(object$avg3qty=="Med",1,0)
object$avg3qty=NULL

object$hnd_webcap_WC=ifelse(object$hnd_webcap=="WC",1,0)
object$hnd_webcap_WCMB=ifelse(object$hnd_webcap=="WCMB", 1,0)
object$hnd_webcap=NULL


object$adjrev=cut(object$adjrev, breaks= c(0,282.45,389.1,490.39, 605.8,28000), labels = c(1,2,3,4,5))
head(object$adjrev)

#Dividing data into training and validation sets
set.seed(100)
row_n=sample(1:nrow(object), .7*nrow(object), FALSE)
training=object[row_n,]
test=object[row_n,]

a=grep("Customer_ID", names(training))

#Applying logistic regression model on data:
myresult=glm(Fatal~., data=training[,-a], family=binomial)
summary(myresult)

step(myresult, direction="forward")
myresult_1= glm(formula = Fatal ~ asl_flag + ethnic + mou_Mean + totmrc_Mean + mou_Range + 
                  change_mou + owylis_vce_Range + mou_opkv_Range + months + 
                  totcalls + eqpdays + custcare_Mean + callwait_Mean + iwylis_vce_Mean + 
                  callwait_Range + ccrndmou_Range + adjqty + ovrrev_Mean + 
                  rev_Mean + ovrmou_Mean + plcd_vce_Mean + avg3mou + avgmou + 
                  avgqty + avg6mou + avg6qty + age1 + hnd_price + retdays + 
                  roam_Mean + drop_vce_Mean + totrev + adjrev + avgrev + Comp_Ptage_VCE + 
                  drop_Ptage_VCE + MRCH_ASLY + MRCL_ASLN + MRCL_ASLY + comp_vce_Mean_Low + 
                  comp_vce_Mean_Med + comp_vce_Mean_VHigh + avg3qty_Low + avg3qty_Med + 
                  hnd_webcap_WC + hnd_webcap_WCMB, family = binomial, data = training[,-a])
summary(myresult_1)

step(myresult_1, direction="backward")

myresult_2=glm(formula = Fatal ~ asl_flag + ethnic + mou_Mean + totmrc_Mean + 
                 mou_Range + change_mou + owylis_vce_Range + mou_opkv_Range + 
                 months + totcalls + eqpdays + custcare_Mean + iwylis_vce_Mean + 
                 adjqty + rev_Mean + ovrmou_Mean + plcd_vce_Mean + avgmou + 
                 avgqty + avg6mou + age1 + hnd_price + retdays + drop_vce_Mean + 
                 totrev + adjrev + Comp_Ptage_VCE + drop_Ptage_VCE + MRCL_ASLN + 
                 MRCL_ASLY + comp_vce_Mean_Low + avg3qty_Low, family = binomial, 
               data = training[, -a])
summary(myresult_2)

mod=glm(formula = Fatal ~ asl_flag + ethnic + mou_Mean + totmrc_Mean + 
          mou_Range + change_mou + owylis_vce_Range + totcalls+
          months + eqpdays + custcare_Mean + iwylis_vce_Mean + 
          adjqty + ovrrev_Mean  + avgqty + age1 + 
          hnd_price + retdays + drop_vce_Mean + 
          Comp_Ptage_VCE + drop_Ptage_VCE + MRCL_ASLN +
          avg3qty_Low + avgrev, family = binomial, data = training[, -a])
summary(mod)


#We remove avgmou and comp_vce_Mean - as they are giving opposite signs in univariate and multivariate settings

#asl_flagY - Account spending limit - inversely proportional
#   i.e ASL is Y - prob. of Fatal is low
#   i.e where-ever account spending limit is set, Fatal rate is low
#ethnic3 - direct relationship
#   i.e people of enthnicity 3 have higher probability of Fatal
#   According to level decoding given in decoding_factors, 
#   level-3 represents B (Asian - Non Oriental), D(South European), I(Italian), J(Jewish), O (Asian)
#mou_Mean - Mean of minutes of usage - inversely proportional
#   as  MOU increase, Fatal rate decreases
#totmrc_Mean - mean total monthly recurring charges - inversely proportional
#   as Monthly Recurring Charges increase, Fatal decreases
#   Monthly Recurring Charge is the base cost of the calling plan regardless of actual minutes used.
#   So people who have taken higher MRC calling plans have lower probability of Fatal
#mou_Range - directly proportional
#   as the range of MOU increase, Fatal rate increase
#change_mou - %age change in monthly mou - inversely proportional
#   Fatal rate decreases as %age change in monthly mou increases
#months - number of months in service - inversely proportional
#   Fatal rate decreases as number of months in service increases
#eqpdays - age of current equipment - directly proportional
#custcare_Mean - inversely proportional 
#   so more and bigger calls made to customer care, means less Fatal rate
#iwylis_vce_meanLow - Directly proportional - indicates if wireless to wireless calls are low, Fatal is high
#adjqty - Billing adjusted total number of calls over the life of the customer
#     inversely proportional - As the billing adjusted total number of calls increases, Fatal increases
#avgqtyLow - Positive correlation - As the average quantity is low, Fatal rate is high. 
#   As average quantity increases, Fatal rate decreases 
#avg6qty - inversely proportional, As the average number of calls in the past 6 months increases, Fatal decreases
#age1 - inverse relationship - as age increases Fatal decreases
#hnd_price - as the handset price increases, Fatal decreases. 
#   High handset price may lead to better quality hand-set, and hence more comfortable calls, hence reduced Fatal 
#   High handset price affordabilty, might also indicate, more affluent customers have reduced Fatal
#retdays - Number of days since last retention call-positive correlation - More number of days have passed since last retention call, higher the Fatal
#drop_vce_Mean - Positive correlation - As the dropped calls increase, Fatal increases
#ovrrev_Mean - When mean of over-head revenue is low, Fatal rate is low. 
#   Fatal rate increases as overhead revenue increases
#Comp_Ptage_VCE - Negative Correlation - As the percentage of completed voice calls increases, Fatal decreases
#drop_Ptage_VCE - Positive correlation - As the pecentage of dropped voice calls increases, Fatal increases
#MRCL_ASLN - When monthly recurring charges are low, and ASL flag is set to 0, Fatal rate is high
# So, we have to migrate these customers to better (and higher MRC calling plan, and ASL flag set to 1)
#Avg3qty - If average quantity of calls is low in last 3 months, the Fatal rate is high
#Avgrev - As average revenue from a customer increases, the Fatal rate increases. # This comprises of charge of voice, data, roaming, over-head revenue etc.
#   So, if a customer is spending more, he has higher probability of Fatal.

1-pchisq(51068-49345, 46406-46382)#=0

#The above p-value (=0) is exceedingly small, so we can reject the null hypothesis 
#   that the deviance of the model with only constant term and the deviance of the model
#   with independent terms is exactly the same

#Final logistic regression equation:
#log(p/(1-p))=a=-(7.808e-01) -(4.076e-01)*asl_flag +(2.062e-01)*ethnic3 -(6.904e-04)*mou_Mean -(3.802e-03)*totmrc_Mean + 
#   +(6.197e-04)*mou_Range + (6.197e-04)*change_mou - (1.192e-01)*owylis_vce_Range + 
#   -(2.241e-02)*months + 5.875e-04*totcalls - (2.241e-02)*months + (8.8e^-4)*eqpdays - (3.69ed^-2)*custcare_Mean + (9.447e-02)*iwylis_vce_Mean + 
#   -(5.458e^-4)*adjqty - (1.712e-01)*ovrrev_MeanLow  + .144*avgqtyLow - (5.062e-03)*age1 + avg6qty + 
#   - (2.008e-03)*hnd_price + (9.988e-04)*retdays + (1.406e-02)*drop_vce_Mean  + 
#   -.4846*Comp_Ptage_VCE + 1.036*drop_Ptage_VCE + .12*MRCL_ASLN +
#   +.29*avg3qty_Low + (4.136e-03)*avgrev

#p=exp(a)/(1+exp(a))


#Finding confidence interval for the model:
confint(mod)
#Confidence Interval is narrow for all the variables 
#   - indicates that every time we reproduce the modelling process (on different samples) 
#     the MLE coefficients would vary within the confidence interval range. 

#Predict the probability using the above model in training data
train_pred=mod$fitted.values
head(train_pred)

#Predict the probability using the above model in test data
test_pred=predict(mod, type="response", newdata=test)
head(test_pred)

table(object$Fatal)/nrow(object)
#average rate = .239 

training$predicted = ifelse(train_pred>=.239,1,0)
nrow(training)
test$predicted = ifelse(test_pred>=.239,1,0)


#Confusion Matrix to test model efficacy
library(caret)
confusionMatrix(training$predicted, training$Fatal, positive = "1")
confusionMatrix(test$predicted, test$Fatal, positive = "1")
mean(training$predicted!=training$Fatal)#mean error = 41%
mean(test$predicted!=test$Fatal)#Mean error = 41%


#Kappa Metric to test model efficacy
library(irr)
kappa2(data.frame(training$Fatal, training$predicted))
kappa2(data.frame(test$Fatal, test$predicted))


#Lift curve to test model efficacy
library(ROCR)
prediction=prediction(test_pred, test$Fatal)
perf=performance(prediction, "tpr", "fpr")
plot(perf, col="red", xlab="FALSE POSITIVE RATE", ylab="TRUE POSITIVE RATE")
abline(0,1)
#the curve shows a fairly accurate model. 

#to calculate area under the curve
auc=performance(prediction, "auc")
auc
p=unlist(auc@y.values)
p #Value = 62.7% 

#Concordance and discordance ratio measurement on test data-set
dat=data.frame(test_pred, test$Fatal)
head(dat)
#seperating the prob. values where actual values = 1

ones=dat[dat$test.Fatal==1,]
zeroes=dat[dat$test.Fatal==0,]

pairs_tested = 0
concor=0
discor=0
ties=0

for(i in 1:nrow(ones))
{
  for(j in 1:nrow(zeroes))
  {
    pairs_tested=pairs_tested+1
    ifelse((ones[i,1]>zeroes[j,1]), (concor=concor+1), 
           (ifelse((ones[i,1]<zeroes[j,1]), (discor=discor+1),(ties=ties+1))))
  }
}

concor
discor
ties
pairs_tested

concordance_ratio=concor/pairs_tested #=.624
discordance_ratio=discor/pairs_tested#=.376
tie_ratio = ties/pairs_tested#=0

#concordance ratio of .624 indicates a fair model



#Questions

#Q1. Top 5 factors driving Fatal at Mobicom

#1. Minutes of Use - Higher the minutes of use, lower the Fatal. Following significant variables in the equation indicate the same:
#mou_Mean - Mean of minutes of usage - inversely proportional
#   as  MOU increase, Fatal rate decreases
#mou_Range - directly proportional
#   as the range of MOU increase, Fatal rate increase
#change_mou - %age change in monthly mou - inversely proportional
#   Fatal rate decreases as %age change in monthly mou increases

#2. Better the network quality (for voice), lower the Fatal. Following variables indicate the same
#Comp_Ptage_VCE - Negative Correlation - As the percentage of completed voice calls increases, Fatal decreases
#drop_Ptage_VCE - Positive correlation - As the pecentage of dropped voice calls increases, Fatal increases
#drop_vce_Mean - Positive correlation - As the dropped calls increase, Fatal increases
#3. More the number of calls made by the customer, lower the Fatal. The customers who are making less calls,
#are higher prone to Fatal. Following variables confirm to the same.
#adjqty - Billing adjusted total number of calls over the life of the customer
#inversely proportional - As the billing adjusted total number of calls increases, Fatal increases
#Avg3qty - If average quantity of calls is low in last 3 months, the Fatal rate is high
#avg6qty - inversely proportional, As the average number of calls in the past 6 months increases, Fatal decreases
#avgqtyLow - Positive correlation - As the average quantity is low, Fatal rate is high. 
#   As average quantity increases, Fatal rate decreases 
#4. As the customer care mean increases, the Fatal rate decreases. This indicates that the customers
#   who report and discuss their grievances with customer care are less likely to Fatal compared to those who don't 
#5. Calling plan: 
#   If the calling plan has ASL Flag set to yes, the Fatal rate is low
#   If monthly recurring charges are low and ASL flag is set to No, then Fatal rate is high
#   If Monthly recurring charges are high (base calling plan is of high value), then Fatal rate is low. 
#6. As the age of the first house-hold member increases, the Fatal rate decreases.
#   So higher age people are better target group
#   Also the customers of ethnicity B (Asian - Non Oriental), D(South European), 
#   I(Italian), J(Jewish), O (Asian) are less likely to Fatal.


#Q2. Validation of survey findings. 
#a) Whether "cost and billing" and "network and service quality" are 
#   important factors influencing Fatal behaviour.  
#   Cost and billing: 
#     We observe the behavior of variable Total MRC (base cost of the calling plan)
#     Higher cost calling plans are yielding lower Fatal, may be due to additional benefits
#     Hence, we can't conclude that higher cost is increasing Fatal, rather, 
#     higher base cost calling plans providing benefit to customer are helping reduce Fatal

#     plot trend based on totmrc
dat=Quant_DA[Quant_DA$FatalVariable=="totmrc_Mean",]
plot(x=1:10, y=dat$Fatal_Rate[1:10], type="l", col="red", xlab="Total Monthly Recurring Charges", ylab="Fatal_Rate")
#     So this graph shows an that as the Monthly recurring charges increase, Fatal decreases
#     So, higher cost of calls doesn't have an influence on Fatal behavior
#     Rather high MRC calling plans are leading to reduced Fatals because of may-be "other" benefits

#However, high over-head revenue and average revenue in a month from a customer 
#     is leading to high probability of  Fatal. So, we have to keep a tab on 
#     overall costs (which includes voice, data, roaming, sms etc). 


#   Network and Service Quality
#Network Quality
# Better the network quality (for voice), lower the Fatal. Following variables indicate the same
#comp_vce_Mean_Low - When mean number of completed voice calls is low 
#  - Fatal is low. As mean number of completed voice calls increase, Fatal rate decreases
#Comp_Ptage_VCE - Negative Correlation - As the percentage of completed voice calls increases, Fatal decreases
#drop_Ptage_VCE - Positive correlation - As the pecentage of dropped voice calls increases, Fatal increases
#drop_vce_Mean - Positive correlation - As the dropped calls increase, Fatal increases

#Service Quality - Better service leads to lower Fatal
#cust_care_Mean - As the customer care mean increases, the Fatal rate decreases. This indicates that the customers
#   who report and discuss their grievances with customer care are less likely to Fatal compared to those who don't 


#b) Are data usage connectivity issues turning out to be costly?  
#   In other words, is it leading to Fatal?
#Ans. Since none of the data variables are coming significant in our modelling equation, 
#   we can't conclude with certainty that the data issues are turning costly

#Q3.Would you recommend rate plan migration as a proactive retention strategy?
#Variable totmrc_mean (significant as per the regression model) is inversely proportional to Fatal
dat=Quant_DA[Quant_DA$FatalVariable=="Totmrc_Mean",]
plot(x=1:10, y=Quant_DA$Fatal_Rate[1:10], type="l", col="red", xlab="Totmrc_Mean", ylab="Fatal_Rate")
# We observe inverse trend between Total MRC and Fatal rate
# The cost of calling plan reduces, the Fatal rate increases, 
#      so, high MRC calling plans are yielding low Fatal, may be due to some extra benefits / higher duration benefits
#      So we will recommend rate plan migration (of customers having low MRC)

#variable asl_flag (significant as per the regression model)
#     We observe that the Fatal is low when ASL flag is set to yes
#     So, we will recommend rate plan migration (of customers having ASL Flag "No")

#Variable MRCL_ASLN is coming significant as per the regression model, directly proportional to Fatal
#     Customers having Monthly recurring cost low and ASL flag set to No, will have very high probability of Fatal
#     These customers need to focussed for rate plan migration

#variable ovrrev_Mean
#As overrev_Mean (Mean overhead revenue) increases Fatal rate increases
#Hence we need to plan their rate-plan migration 

#variable retdays (number of days since the last retention call was made) is signficant in the model
#   As the retdays increases, the Fatal increases
#   Hence, we should be making more frequent retention calls 
#   providing plan upgrade option


#Q4.What would be your recommendation on how to use this Fatal model for 
#   prioritisation of customers for a proactive retention campaigns in the future?

#Ans. Based on the data analysis, we have identified the Fatal drivers. 
# We will identify customers with high probability of Fatal (>.5) and 
#     we will target them for the retention campaigns. 
# Based on the Fatal drivers following strategies will be recommended for proactive retention campaigns
#     increase the MOU of the customers - increased retention calls providing bundling offers: data / sms etc. which will lead to increased minutes of use
# Rate plan migration - helping the customers migrate from less attractive rate plans to better rate plans
#     customers with high over-head revenue need to be targeted for rate plan migration
# Increased target on higher age customers while customer acquisition (better plans based on age), as they have lower probability of Fatal.
# Increased target on customers of ethnicity 3 while customer acquisition as they have lower probability of Fatal.
# We have to motivate customers to voice their grievances to customer care, so that customer care can help them resolve the queries.
#     The customers who don't seek solution of their grievances from the customer-care, have a higher tendency to Fatal. 
#     May be in our retention calls, we can ask customers to share if they have any grievances / discomfort with the object provider


#Following are the customers in the test data who have high probability of Fatal and need to be targetted:
customers_high_Fatal=test$Customer_ID[test$predicted=1]
#We need to target above customer ids for our retention campaigns. 
#Specifically we'll target the customers with high prob of Fatal and high average revenue

#To make a table of high revenue and high Fatal probability customers in the validation dataset

#Make a table of customers Fataling (Low, Medium, High) with revenue in the validation data-set
test$probability_Fatal=test_pred
test%>%mutate(Prob_level=ntile(test$probability_Fatal,3))->test
test%>%group_by(Prob_level)%>%summarize(Max_Prob=max(probability_Fatal), Min_Prob=min(probability_Fatal))
test$Prob_level=cut(test$Prob_level, breaks=c(0,1,2,3), labels=c("Low(0 -.197)","Med (.197 - .27)", "High (.27 - 1)"))
test%>%mutate(Revenue_Level=ntile(test$avgrev, 3))%>%arrange(Revenue_Level)->test
test%>%group_by(Revenue_Level)%>%summarize(Max_Revenue=max(avgrev), Min_Revenue=min(avgrev))
test$Revenue_Level=cut(test$Revenue_Level, breaks=c(0,1,2,3), labels=c("Low (.94 - 39)", "Med (39-60.94)", "High (60.94-121.3)"))
table(Revenue=test$Revenue_Level, FatalProbability=test$Prob_level)

#We will be targetting customers are in Med & High FatalProbability and Med & High Revenue
Target_Customers=test$Customer_ID[test$probability_Fatal>.197 & test$avgrev>39]

#Q5.What would be the target segments for proactive retention campaigns? 
#   Falling ARPU forecast is also a concern and therefore, 
#   Mobicom would like to save their high revenue customers besides managing 
#   Fatal. Given a budget constraint of a contact list of 20% of the subscriber 
#   pool, which subscribers should prioritized if "revenue saves" is also a 
#   priority besides controlling Fatal. 
#   In other words, controlling Fatal is the primary objective 
#   and revenue saves is the secondary objective.


#Since our budget constraint is only 20% of people, and we want to prioritize people with high revenue;
test$Prob_Revenue = test$predicted*test$avgrev
test%>%mutate(levels=ntile(Prob_Revenue,10))%>%filter(levels==9 | levels==10)%>%count(levels, Customer_ID)->target_customers
#dataframe target_customers contains the customer-ids of our target customers from the test data
target_customers$Customer_ID
nrow(target_customers)/nrow(test) #=20% customers
