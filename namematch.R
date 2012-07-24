setwd("C:/My Projects/Zheng - Name Matching")
require(wordcloud)
require(RColorBrewer)
require(R.oo)
require(RecordLinkage)
require(Hmisc)
names<-read.csv("Customer_base.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
names<-read.csv("Customer_base.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=TRUE);
colnames(names)[1]<-"CustomerID"
summary(names)

names$Surname<-tolower(names$Surname)
names$Surname<-trim(names$Surname)
names$Forename<-tolower(names$Forename)
names$Forename<-trim(names$Forename)

namess<-names[1895144:2095143,]
namess<-names[2000000:2095143,]
namess<-namess[!is.na(namess$DOB_Date),]
summary(namess)
namess$Surname<-tolower(namess$Surname)
surname<-table(namess$Surname)
names(surname)<-trim(names(surname))#important to trim off the space.
surname.df<-as.data.frame(surname)
summary(surname.df)
colnames(surname.df)[1]<-"surname"
summary(surname.df)
sfreq<-surname.df$Freq
sfreq<-sort(sfreq,decreasing=TRUE)
length(sfreq)
ind<-seq(1:45232)
plot(log10(ind),log10(sfreq),type="l",col="red",lwd=2,main="Surname distribution")
library(wordcloud)
pal2 <- brewer.pal(8,"Set2")
wordcloud(names(surname), surname, scale=c(9,.1),min.freq=3, max.words=Inf, random.order=F, rot.per=.3, colors=pal2)
surname.df<-surname.df[which(surname.df$Freq>4),]

surname<-surname[nchar(names(surname),"c")==nchar(names(surname),"b")]
surname<-surname[nchar(names(surname),"c")>2]

> names$Forename<-tolower(names$Forename)
> forename<-table(names$Forename)
> forename.df<-as.data.frame(forename)
> ffreq<-forename.df$Freq
> ffreq<-sort(ffreq,decreasing=TRUE)
> plot(log10(ind),log10(ffreq),type="l",col="red",lwd=2,main="Forename distribution")

forename<-table(namess$Forename)
names(forename)<-trim(names(forename))

forename.df<-as.data.frame(forename)
colnames(forename.df)[1]<-"forename"
forename.df$Freq<-ceiling(forename.df$Freq/100)
wordcloud(names(forename), forename, scale=c(9,.1),min.freq=3, max.words=Inf, random.order=F, rot.per=.3, colors=pal2)
wordcloud(forename.df$forename, forename.df$Freq, scale=c(9,.1),min.freq=1, max.words=Inf, random.order=F, rot.per=.3, colors=pal2)

years<-table(names$DOB_Year)
years.df<-as.data.frame(years)
head(years.df,100)
colnames(years.df)[1]<-"year"

years.df<-years.df[as.numeric(as.character(years.df$year))>1900&as.numeric(as.character(years.df$year))<1996,]
years.df$proportion<-years.df$Freq/sum(years.df$Freq)
plot(as.numeric(as.character(years.df$year)),years.df$proportion,type="b",col="red",lwd=2,main="Year of Birth Distribution",xlab="year",ylab="proportion",xaxt="n")
axis(1,at=as.numeric(as.character(years.df$year)),labels=years.df$year,cex.axis=0.7)

months<-table(names$DOB_Month)
months.df<-as.data.frame(months)
colnames(months.df)[1]<-"month"
months.df
months.df<-months.df[as.numeric(months.df$month)>1,]
months.df$proportion<-months.df$Freq/sum(months.df$Freq)
plot(as.numeric(months.df$month),months.df$proportion,type="b",col="red",lwd=2,main="Month of Birth Distribution",xlab="month",ylab="proportion",xaxt="n")
axis(1,at=as.numeric(months.df$month),labels=months.df$month,cex.axis=1)

names$Surname<-tolower(names$Surname)
names$Surname<-trim(names$Surname)
dim(names)[1]/length(unique(names$Surname))

names$Forename<-tolower(names$Forename)
names$Forename<-trim(names$Forename)
dim(names)[1]/length(unique(names$Forename))
dim(names)[1]
length(unique(names$Forename))
names$Fullname<-paste(names$Forename,names$Surname,sep=" ")
fullname<-table(names$Fullname)
fullname.df<-as.data.frame(fullname)
colnames(fullname.df)[1]<-"FullName"
head(fullname.df[with(fullname.df,order(-Freq,FullName)),])
fullname.df<-fullname.df[with(fullname.df,order(-Freq,FullName)),]
wordcloud(fullname.df$FullName, fullname.df$Freq, scale=c(9,.1),min.freq=6, max.words=Inf, random.order=F, rot.per=.3, colors=pal2)
write.csv(fullname.df,file="fullname frequency population.csv")

names1<-names[2000000:2095143,]
head(names1)
names2<-data.frame(names1$CustomerID,names1$Forename,names1$Surname,names1$DOB_Date,names1$DOB_Month,names1$DOB_Year)
colnames(names2)<-c("CustomerID","Forename","Surname","DOB_Date","DOB_Month","DOB_Year")
head(names2)

rpairs<-compare.dedup(names2,blockfld=c(3,5,6),strcmp=TRUE)
rpairs$pairs[1:5,]
martinforename<-names2$Forename[which(names2$Surname=="martin")]
sort(martinforename)
martinforename<-unique(martinforename)
length(martinforename)
similar<-matrix(rep(0,20449),nrow=143,ncol=143)
similar2<-matrix(rep(0,20449),nrow=143,ncol=143)

for(i in 1:143)
  for(j in 1:143)
    similar[i,j]<-levenshteinSim(as.character(martinforename[i]),as.character(martinforename[j]))
    
for(i in 1:143)
  for(j in 1:143)    
    similar[i,j]<-jarowinkler(as.character(martinforename[i]),as.character(martinforename[j]))

similardf<-as.data.frame(similar)
colnames(similardf)<-as.character(martinforename)
rownames(similardf)<-as.character(martinforename)

similardf2<-as.data.frame(similar)
colnames(similardf2)<-as.character(martinforename)
rownames(similardf2)<-as.character(martinforename)
head(similardf2,10)

simvector<-rep(0,10296)

count<-1
for(i in 1:143)
  for(j in i:143)
  {
    simvector[count]<-similar2[i,j]
    count<-count+1
  }
?ecdf
Ecdf(simvector,xlab="similarity")
Ecdf(simvector, xlab="Serum Cholesterol")
scat1d(simvector)                       # add rug plot
histSpike(simvector, add=TRUE, frac=.15)   # add spike histogram

Ecdf(simvector, datadensity='density')

similar3<-matrix(rep(0,100000000),nrow=10000,ncol=10000)
similar4<-matrix(rep(0,100000000),nrow=10000,ncol=10000)
prop<-matrix(rep(0,100000000),nrow=10000,ncol=10000)
length(names$Forename)
forenames<-names$Forename[2085144:2095143]
surnames<-names$Surname[2085144:2095143]
summary(prop)
for(i in 1:10000)
  for(j in 1:10000)
    similar3[i,j]<-jarowinkler(as.character(forenames[i]),as.character(forenames[j]))

for(i in 1:10000)
  for(j in 1:10000)
    similar4[i,j]<-jarowinkler(as.character(surnames[i]),as.character(surnames[j]))

for(i in 1:10000)
  for(j in 1:10000)
    prop[i,j]<-length(surnames[which(similar4[i,]>=similar4[i,j])])

for(i in 1:10000)
  for(j in 1:10000)
    prop[i,j]<-sum(forenames[i,]<=forenames[i,j])

names$fullname<-paste(trim(names[,'Forename']),trim(names[,'Surname']),sep=" ");
write.csv(fullnames,file="fullnames10000.csv")
ind<-read.csv("ind.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=TRUE);
colnames(ind)<-c("index","smallestindex")
ind$origname<-fullnames[ind$index]
ind$matchname<-fullnames[ind$smallestindex]
ind$diff<-ind$index-ind$smallestindex
ind[which(ind$diff!=0),]
fullnames<-names$fullname[2085144:2095143]
fullnames<-tolower(fullnames)


plot(ecdf(as.matrix(prop[1:10000,1:10000])))
Ecdf(as.matrix(prop[1:1000,1:1000]),datadensity='density')

for(i in 1:10000)
  for(j in 1:10000)
    prop[i,j]<-length(forenames[which(similar3[i,]>=similar3[i,j])])

prop<-read.csv("propsurname.csv",head=TRUE,sep=",",na.string="NULL")
prop<-read.csv("propforename.csv",head=TRUE,sep=",",na.string="NULL")
propvec<-as.vector(as.matrix(prop))
Ecdf(propvec, datadensity='density')
Ecdf(propvec[1:1000000], datadensity='density')

propsurname<-read.csv("propsurname.csv",head=TRUE,sep=",",na.string="NULL")
propforename<-read.csv("propforename.csv",head=TRUE,sep=",",na.string="NULL")

wedit100<-read.csv("wedit100.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=TRUE);

for(i in 1:10000)
  for(j in 1:10000)
    prop[i,j]<-length(wedit100[which(wedit[i,]=<wedit[i,j])])

similar3<-as.data.frame(similar3)
colnames(similar3)<-forenames
write.csv(forenames,file="forenames.csv")

prop<-as.data.frame(prop)
colnames(prop)<-forenames

forenames[which(similar3df$jessica>0.86)]

wedit2<-read.csv("wedit1to2.csv",header=FALSE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
uniqwedit2<-unique(wedit2)

props<-read.csv("propsjaro.csv",head=TRUE,sep=",",na.string="NULL")
props<-as.matrix(props)
propsv<-as.vector(props)

firstnames<- seq(1,100)
secondnames<- seq(1,100)
scores<- seq(1,100)
counter <- 1
breakFlag <- FALSE;
for(i in seq(1,10000))
{
  for(j in seq(1,10000))
  {
    if(props[i,j]<0.00001)
    {
      firstnames[counter]<- fullnames[i]
      secondnames[counter]<- fullnames[j]
      scores[counter]<- props[i,j]
      counter<-counter+1
      if(counter>100)
      {
        breakFlag <- TRUE
        break
      }
     }
  }
  if(breakFlag) break
}
dd<-data.frame((firstnames),(secondnames),(scores))

ind<-apply(props,1,function(x) which.min(x))

names10000<-names[2085144:2095143]
names10000$fullname<-tolower(names10000$fullname)
#fill in the DOB and prop value for each pair of ddd
ddd<-read.csv("ddd.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
ddd$count1<-0
ddd$count2<-0
for(i in seq(1:dim(ddd)[1]))
{
  ddd$count1[i]<-sum(names10000$fullname==ddd$Firstname[i])  
  ddd$count2[i]<-sum(names10000$fullname==ddd$Secondname[i])
  ddd$DOB1[i]<-as.character(names10000$DOB[which(names10000$fullname==ddd$Firstname[i])])
  ddd$DOB2[i]<-as.character(names10000$DOB[which(names10000$fullname==ddd$Secondname[i])])  
}

props<-read.csv("propsurname.csv",head=TRUE,sep=",",na.string="NULL")
surname<-names10000$Surname
ddd<-ddd[which(ddd$Firstname!="jason slater morally"),]
ddd<-ddd[which(ddd$Firstname!="conor mac gabhann"),]
ddd<-ddd[which(ddd$Firstname!="emmett mc daid"),]
ddd<-ddd[which(ddd$Firstname!="manal bin abdu"),]
ddd<-ddd[which(ddd$Firstname!="powel francisze jankowski"),]
ddd<-ddd[which(ddd$Secondname!="powel francisze jankowski"),]
ddd<-ddd[which(ddd$Secondname!="julie anne morrissey"),]
ddd<-ddd[which(ddd$Firstname!="julie anne morrissey"),]
ddd<-ddd[which(ddd$Secondname!="william richard morris"),]
ddd<-ddd[which(ddd$Firstname!="william richard morris"),]
ddd<-ddd[which(ddd$Secondname!="christopher pad padgett"),]
ddd<-ddd[which(ddd$Firstname!="christopher pad padgett"),]
ddd<-ddd[which(ddd$Secondname!="md nesser shamim"),]
ddd<-ddd[which(ddd$Firstname!="md nesser shamim"),]
ddd<-ddd[which(ddd$Secondname!="md rajiur rahman"),]
ddd<-ddd[which(ddd$Firstname!="md rajiur rahman"),]
ddd<-ddd[which(ddd$Secondname!="lauren pereira pereira"),]
ddd<-ddd[which(ddd$Firstname!="lauren pereira pereira"),]
ddd<-ddd[which(ddd$Secondname!="ewelina anna partacz"),]
ddd<-ddd[which(ddd$Firstname!="ewelina anna partacz"),]
ddd<-ddd[which(ddd$Secondname!="john mark burnside"),]
ddd<-ddd[which(ddd$Firstname!="john mark burnside"),]
ddd<-ddd[which(ddd$Secondname!="md khalif reza"),]
ddd<-ddd[which(ddd$Firstname!="md khalif reza"),]
ddd<-ddd[which(ddd$Secondname!="jonathan cottam cottam"),]
ddd<-ddd[which(ddd$Firstname!="jonathan cottam cottam"),]
ddd<-ddd[which(ddd$Secondname!="md khalif reza"),]
ddd<-ddd[which(ddd$Firstname!="md khalif reza"),]
ddd<-ddd[which(ddd$Secondname!="md khalif reza"),]
ddd<-ddd[which(ddd$Firstname!="md khalif reza"),]

for(i in seq(1:dim(ddd)[1]))
{
  fname1<-str_split(ddd$Firstname[i]," ")[[1]]
  if(length(fname1)==2)
  {
    sur1<-fname1[2]
  }
  if(length(fname1)==3)
  {
    sur1<-paste(fname1[2],fname1[3],sep=" ");
  }
  
  fname2<-str_split(ddd$Secondname[i]," ")[[1]]
  if(length(fname2)==2)
  {
    sur2<-fname2[2]
  }
  if(length(fname2)==3)
  {
    sur2<-paste(fname2[2],fname2[3],sep=" ")
  }
print(fname1)
print(fname2)
ddd$propsurname[i]<-props[which(surname==sur1)[1],which(surname==sur2)[1]]
}

for(i in seq(1:dim(ddd)[1]))
{
  fname1<-str_split(ddd$Firstname[i]," ")[[1]]
  if(length(fname1)==2)
  {
    sur1<-fname1[2]
  }
  if(length(fname1)==3)
  {
    sur1<-paste(fname1[2],fname1[3],sep=" ");
  }
  
  fname2<-str_split(ddd$Secondname[i]," ")[[1]]
  if(length(fname2)==2)
  {
    sur2<-fname2[2]
  }
  if(length(fname2)==3)
  {
    sur2<-paste(fname2[2],fname2[3],sep=" ")
  }
  print(fname1)
  print(fname2)
  ddd$jarosurname[i]<-jarowinkler(sur1,sur2)
}

rm(props)
propf<-read.csv("propforename.csv",head=TRUE,sep=",",na.string="NULL")
forename<-names10000$Forename
for(i in seq(1:dim(ddd)[1]))
{
  fname1<-str_split(ddd$Firstname[i]," ")[[1]]
  if(length(fname1)==2)
  {
    fore1<-fname1[1]
  }
  if(length(fname1)==3)
  {
    fore1<-fname1[1];
  }
  
  fname2<-str_split(ddd$Secondname[i]," ")[[1]]
  if(length(fname2)==2)
  {
    fore2<-fname2[1]
  }
  if(length(fname2)==3)
  {
    fore2<-fname2[1]
  }
  print(fname1)
  print(fname2)
  ddd$propforename[i]<-propf[which(forename==fore1)[1],which(forename==fore2)[1]]
}

for(i in seq(1:dim(ddd)[1]))
{
  fname1<-str_split(ddd$Firstname[i]," ")[[1]]
  if(length(fname1)==2)
  {
    fore1<-fname1[1]
  }
  if(length(fname1)==3)
  {
    fore1<-fname1[1];
  }
  
  fname2<-str_split(ddd$Secondname[i]," ")[[1]]
  if(length(fname2)==2)
  {
    fore2<-fname2[1]
  }
  if(length(fname2)==3)
  {
    fore2<-fname2[1]
  }
  print(fname1)
  print(fname2)
  ddd$jaroforename[i]<-jarowinkler(fore1,fore2)
}


for(i in seq(1:dim(ddd)[1]))
{
  fname1<-str_split(ddd$Firstname[i]," ")[[1]]
  if(length(fname1)==2)
  {
    fore1<-fname1[1]
  }
  if(length(fname1)==3)
  {
    fore1<-fname1[1];
  }
  
  fname2<-str_split(ddd$Secondname[i]," ")[[1]]
  if(length(fname2)==2)
  {
    fore2<-fname2[1]
  }
  if(length(fname2)==3)
  {
    fore2<-fname2[1]
  }
  print(fname1)
  print(fname2)
  ddd$jaroforename2[i]<-jaroforename[which(forename==fore1)[1],which(forename==fore2)[1]]
}


setwd("C:/My Projects/Zheng - Clustering/Data")
latlon<-read.csv("l0cor5000.csv",header=TRUE,sep=",",na.strings="NA",stringsAsFactors=FALSE);
latlon2<-data.frame(latlon$LoanAmont,latlon$lat,latlon$long)
colnames(latlon2)<-c("LoanAmount","lat","long")
latlon2clean<-latlon2[complete.cases(latlon2),]
agglatlon<-aggregate(latlon2clean,by=list(latlon2clean$lat,latlon2clean$long),FUN=mean,na.rm=TRUE)
agglatlon$LoanAmountScaled<-ceiling(agglatlon$LoanAmount/10)
agglatlon<-transform(agglatlon,lac=ceiling(LoanAmount/10))
UKMap+geom_point(aes(x=long,y=lat,size=LoanAmountScaled,colour=LoanAmountScaled),data=agglatlon)