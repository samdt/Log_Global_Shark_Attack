#use debug function to debug a function. Following code:
#debug(<function_name>)
#function_name(arguments)#call the function
# to stop the debug click stop on console

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

#function to return col numbers of  factor vars with high number of levels > 30%
delete_factor_high_levels=function(object, nfactor)
{
  df=data.frame()
  i=2
  while(i<=nfactor)
  {
    print(i)
    if(length(unique(object[,i]))>.3*nrow(object))
    {
      df=rbind(df,i)
    }
    i=i+1
  }
  return(df)
}

factor_event_rate=function(object, object_factor)
{
  table(object[,1])
  Levels0FatalRate=data.frame()
  dat2=data.frame()
  for(i in 3:ncol(object_factor))#1st column is dependent variable Fatal
    #we have to do analysis of independent variables (col no. 2 onwards)
  {
    print(i)
    names(object)[i]
    object[,i]=as.factor(object[,i])
    object%>%count(Fatal,levels=object[,i])%>%filter(Fatal==1)->dat1
    #dat1 will comprise of levels with Fatal=1
    object%>%filter(object[,i]%in%dat1$levels)->datC2
    #will comprise of rows in object db where object[,i] has same values as levels selected in dat1 
    dat1$N=unclass(unname(datC2%>%group_by(datC2[,i])%>%summarize(n())))[[2]]
    dat1$Fatal_Rate=round(dat1$n/dat1$N,3)
    dat1$Total_Levels=rep(length(dat1$levels), length(dat1$levels))
    dat1$Fatal=NULL
    dat1$Col_Num=i
    dat1$Var_Name<-names(object)[i]
    dat2=rbind(dat2,dat1)
    rn=which(object[,i]%in%dat1$levels)#which row number have event rate = 1 ; the rest of the rows have levels with event rate=0

  #For levels having 0 event rate, we will impute 0. In temp we will collate where-all we'll impute 0
  #temp will be same format as decoding_factors
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
  
  list=list(dat2, Levels0FatalRate)
  return(list)
}


impute_factor_missing=function(object, object_factor, event_rate)
{
  factors_all_lvls=data.frame()
  l=3
  i=1
  while(i<=nrow(event_rate))
  {  
    print(i)
    dat1=event_rate[event_rate$Col_Num==l,]
    
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
      object[is.na(object[,l]),l]=p
      dat1=dat1[-nrow(dat1),] #we need not include the NA values for our Fatal analysis
      i=i+1 #since we have reduced 1 row (NA) row
    }
  
    factors_all_lvls=rbind(factors_all_lvls, dat1) 
    if(!is.na(dat1$Total_Levels[i]))
      i=i+dat1$Total_Levels[i]#since the na level has reduced
    else break
    l=event_rate$Col_Num[i]
  }
  
  list=list(object, factors_all_lvls)
  return(list)
}

# condensing the levels for each factor col - to max 3

condense_3levels=function(object, event_rate, l=3)
{
  decoding_factors=data.frame()
  factors_3lvls=data.frame()
  
  l=3#start processing from col. no. 3
  i=1
  while(i<=nrow(event_rate))
  {  
    print(i)
    dat1=event_rate[event_rate$Col_Num==l,]
    lev=dat1$Total_Levels[i]
    
    if(dat1$Total_Levels[1]>3)
    {
      dat1%>%mutate(levels=ntile(Fatal_Rate,3))%>%group_by(levels)%>%summarize(n=sum(n), N=sum(N))->dat2
      dat2$Fatal_Rate=dat2$n/dat2$N
      dat2$Total_Levels=3
      dat2$Col_Num=l
      dat2$Var_Name<-names(object)[l]
      dat1%>%mutate(imputed_lvl=ntile(Fatal_Rate,3))%>%count(imputed_lvl, levels)->dat4
      #dat4 will provide which all levels we are imputing by levels 1,2 and 3
      dat4$Var_Name=names(object)[l]
      dat4$Col_Num=l
      dat1=dat2
      decoding_factors=rbind(decoding_factors, dat4)
      #will comprise of the decoding of the factor vars with > 3 levels to upto 3 levels
    }  
    
    factors_3lvls=rbind(factors_3lvls, dat1)#factors_3lvls dataset after clubbing the levels to 3
    print(i)
    if(!is.na(i+lev))
      i=i+lev#since the na level has reduced
    else break
    l=event_rate$Col_Num[i]
  }
  list=list(decoding_factors, factors_3lvls)
  return(list)
}    

clubbing_factors_beyond_3lvls=function(factors_3lvls, decoding_factors)
{
  factors_final_clubbed=data.frame()
  c=1
  i=1
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
  
  list=list(factors_final_clubbed, decoding_factors)
  return(list)
}

impute_factor_code=function(decoding_factors, object)
{
  for(i in 1:nrow(decoding_factors))
  {
    rn=which(object[, decoding_factors$Col_Num[i]] %in% decoding_factors$levels[i])
    object[,decoding_factors$Col_Num[i]]=as.character(object[,decoding_factors$Col_Num[i]])
    object[rn,decoding_factors$Col_Num[i]]=decoding_factors$imputed_lvl[i]
  }
  return (object)
}

#if number of levels = 1, then we'll not include the variable for modeling
exclude_single_level_factorvars=function(object, nfactor)
{
  dat=data.frame()
  
  for(i in 1:nfactor)
  {
    if(length((levels(object[,i])))==1)
    {
      dat=rbind(dat,i)
    }
  }
  if(nrow(dat)!=0)
  {
    names(dat)="col_num"
    object=object[,-dat$col_num]
    nfactor=nfactor-dat$col_num
  }
  list=list(object,nfactor)
  return(list)
}

#decile-wise (/n-quantile-wise) binning of quantitative variables, and finding the corresponding event rate:
ntile_bin_quant_vars=function(object, nfactor)
{
  Quant_DA=data.frame()
  x=nfactor+1
  
  for(i in x:ncol(object))#for each quantitative col
  {
    p=as.name(names(object)[i])#this will remove the "" from the char var
    for(j in 10:1)#divide into j levels; 
      # first we'll try dividing into decile, if max in the 1st decile is 0, then divide into 9 levels, compare and continue
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
    print(i)
    object$Lvl=NULL
  }
  return(Quant_DA)
}

#Analysing outliars and doing outliars imputation in continuous variables
outliar_imputation_quantvars=function(object, nfactor)
{
  x=nfactor+1
  for(i in x:length(names(object))) 
  {
    #outliar treatment in col i of object
    a=boxplot.stats(object[,i] )
    if(a$stats[1]==a$stats[5])
      next  
    #   if a$stats[1]=a$stats[5], and if we proceed with outliar imputation the whole
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
  return(object)
}



#Main Code
library(dplyr)
library(ggplot2)
library(data.table)

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

#Converting target var to 1 or 0
object$Fatal=ifelse(object$Fatal=="Y",1, 0)#converting "Y" and "N" into 1 and 0 respectively


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

#if the number of unique levels is > 50% of no. of rows, then they are not factor type vars
#deleting the vars with levels >30% of nrows 
nfactor=ncol(object)-1
delete_colnum=delete_factor_high_levels(object,nfactor)
names(object)[delete_colnum$X2]
#We will retain the colno. Case.Number - as it might be the primary key
npkeys=1#number of cols for primary key 
object=object[,-delete_colnum$X2[2:nrow(delete_colnum)]]
names(object)  
#update var nfactor also accordingly
nfactor=nfactor-length(delete_colnum$X2)+npkeys

  
object_factor=object[1:(ncol(object)-1)]#specific to this case study
object_quant=object[ncol(object)]#specific to this case study
ls=factor_event_rate(object, object_factor)
event_rate=ls[[1]]
Levels0FatalRate=ls[[2]]

#impute NA values in factor cols
list=impute_factor_missing(object, object_factor,event_rate)
object=list[[1]]
event_rate=list[[2]] #note this dataframe does not comprise NA rows 
#but it also does not update the numbers with NA values (n and N values)
#it can be done if reqd
colSums(is.na(object))#note: all factor vars have 0 missing values

# condensing the levels for each factor col - to max 3
ls=condense_3levels(object, event_rate, 3)#we have to condense levels for columns - col number 3 onwards
decoding_factors=ls[[1]]
factors_3lvls=ls[[2]]
decoding_factors$n=NULL
#decoding_factors=rbind(decoding_factors, Levels0FatalRate)
#we will combine these 2 dataframes, and make imputations in the main database based on these 
#but later - after condensing levels further

#condensing the levels even further if event rate is similar 
ls=clubbing_factors_beyond_3lvls(factors_3lvls, decoding_factors)
factors_final_clubbed=ls[[1]]
decoding_factors=ls[[2]]

decoding_factors=rbind(decoding_factors, Levels0FatalRate)
#So, dataframe: decoding_factors contains the decoding of all the clubbed levels to original levels

#Imputing the values in the main object data-set as per the decoding_factors
object=impute_factor_code(decoding_factors, object)


#converting factor vars back to factor type
for(i in 1:nfactor)
{
  object[,i]=as.factor(object[,i])
}
summary(object)

#if number of levels = 1, then we'll not include the variable for modeling
ls=exclude_single_level_factorvars(object, nfactor)
object=ls[[1]]
nfactor=ls[[2]]
summary(object)


#Once we are done with reducing the levels of factor vars we should be able to create a decision tree
library(rpart)
mod<-rpart(Fatal~.,data=object[,-2],method="class")
summary(mod)
library(rattle)
fancyRpartPlot(mod)
mod
#so the tree shows activity levels 0,1 and area levels: 0,1 are safe from "fatal" accidents 


#Analysing quantitative variables

#decile-wise (/n-quantile-wise) binning of quantitative variables, and finding the corresponding event rate:
Quant_DA=ntile_bin_quant_vars(object, nfactor)

#Missing values imputation of quantitative data. 
# Impute the value of NAs in each col acc. to Fatal rate
object=Missing_Imputation_QuantVars(Quant_DA, object)

Missing_Imputation_QuantVars=function(Quant_DA, object)
{
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
  
  return(object)
}
 
colSums(is.na(object))

#Keeping only the continuous variables which show some trend / important for
#so we can exclude the following continuous variables from our modelling data-set: rev_Range, age2, da_Mean, da_Range, hnd_Price
#i=length(names(object_factor))
#object_quant=object_quant[, -c(21-i,49-i,57-i,58-i)]
#rn=which(Quant_DA$Col_Num %in% c(21,49, 57,58))
#Quant_DA=Quant_DA[-rn,]

#names(object_quant)
#object=cbind(object_factor, object_quant)

#Updating col. numbers in Quant_DA
#for(i in 1:nrow(Quant_DA))
#{
#  Quant_DA$Col_Num[i]= which(names(object) %in% Quant_DA$FatalVariable[i])
#}


#Analysing outliars and doing outliars imputation in continuous variables
object=outliar_imputation_quantvars(object, nfactor)

summary(object)

#Creating transformation variables
# "*" or "/" (percentage transformations)  

summary(object)
#Decision Tree suggests Activity & Area might be critical var
object$Activity_Area=ifelse((object$Activity==0 | object$Activity==1) & (object$Area==0 | object$Area==1), "Activity01Area01",ifelse((object$Activity==0 | object$Activity==1) & object$Area==1, "Activity01Area23", "Activity2"))
#note above statement producing wrong results 
#instead of putting if((A|B)&C) - we should put if((A&C) | (B&C))

object$Activity_M=ifelse((object$Activity==0 | object$Activity==1),"01","2")
object$Area_M=ifelse((object$Area==0 | object$Area==1),"01","23")

table(object$Area)

object$Activity_Area=ifelse(object$Activity_M=="01" & object$Area_M=="01", "Act01Area01", 
                            ifelse(object$Activity_M=="01" & object$Area_M=="23", "Act01Area23", "Act2")) 

with(object,table(Activity_Area, Fatal))
with(object, table(Area,Fatal))

object%>%count(Activity_Area, Fatal)%>%filter(Fatal==1)->dat
dat$total=unclass(object%>%count(Activity_Area)%>%unname())[[2]]
dat$Fatal_rate=dat$n/dat$total
dat
object$Activity_M=NULL
object$Area_M=NULL

#Creating dummies of certain quatitative variables based on Fatal rate in dataframe "Quant_DA"
Quant_DA
object$Year=cut(object$Year, breaks= c(0,1906, 1956, 2013,2016), labels = c(1,2,3,4))
head(object$Year)

#Drop variables where we observe a counter-intuitive trend 
#     As adjmou increases, Fatal should decrease. Hence we'll not include this variable for modelling


#Dividing data into training and validation sets

set.seed(100)
row_n=sample(1:nrow(object), .7*nrow(object), FALSE)
training=object[row_n,]
test=object[row_n,]
a=grep("Case.Number", names(training))

#Applying logistic regression model on data:
myresult=glm(Fatal~., data=training[,-a], family=binomial)
summary(myresult)

#----------------------------------------------------------------------------------------------------------------

#Creating more dummies if required
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
