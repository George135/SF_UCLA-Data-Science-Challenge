#Salesforce/UCLA Data Science Challenge 

setwd("C:/Users/sbuja/Documents/Salesforce Data Challenge/SF_UCLA-Data-Science-Challenge")

#REQUIRED LIBRARIES----
library(ggplot2) #plotting
library(multilevel) #MLM analysis
library(plyr) #data wrangling
library(dplyr) #data wrangling 2
library(tidyr) #data wrangling 3
library(pastecs) #summary stats function stat.desc
library(xlsx) #package to import xls files directly
library(grid) #for multiplot
library(psych) #for alpha (cronbach's alpha function)
library(polycor) #factor analysis related
library(GPArotation) #factor analysis related
library(GGally) #ggpairs plots
library(gridExtra) #tableGrob for adding results table to plots
library(compute.es) #effect size computations
library(gmodels) #CrossTable Function
library(Hmisc) #rcorr
library(stats)
library(Deducer) #for rocplot 
library(pscl) # for mcfadden R2 in logistic regression
library(caret) #for crossvalidation methods
library(ROCR) #For crossvalidation AUC curve
library(scales) #for percent axis


#CUSTOM FUNCTIONS----
#Spencer hates the default describe functions so he wrote his own
Sp.Desc <- function(data)
{
  #If only 1 variable do separately 
  if(dim(data.frame(data))[2]==1){
    if(class(data)=="numeric"){#if R can compute a mean then stat.desc
      print(t(stat.desc(data))[,-c(2,3,6,7,11,14)])
    }
    else{
      #cat(noquote(levels(data)))
      print(table(data))
    }
    return(noquote(""))
  }
  
  #separate categorical from numerical data
  data.num <- data.frame(Dummy=rep(NA,dim(data)[1])) #Make a dummy data.frame
  data.cat <- data.frame(Dummy=rep(NA,dim(data)[1]))
  
  for(i in 1:dim(data)[2]){
    if(!is.na(stat.desc(data[i])["mean",])){#if R can compute a mean then add to data.num
      data.num <- cbind(data.num, data[i])
    }
    else{
      data.cat <- cbind(data.cat, data[i])
    }
  }
  #Delete dummy variable
  data.num$Dummy <- NULL
  data.cat$Dummy <- NULL
  
  #Print Numerical results
  if(dim(data.num)[2]>0) {
    print(t(stat.desc(data.num))[,-c(2,3,6,7,11,14)])
    cat(noquote(""), sep="\n\n")
  }
  
  #Print categorical results
  if(dim(data.cat)[2]>0) {
    for(j in 1:dim(data.cat)[2]){
      cat(noquote(names(data.cat[j])))
      print(table(data.cat[j]))
    }
  }
}

#Custom ggplot theme for vast majority of plots. 
#Basic black and white motif, no grid lines or other such nonsense
Sp.Theme <- function(axis.text.size=12, axis.title.size=12, title.size=16, legend.position="none")
{
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"),
        axis.title.x=element_text(colour = "black", size=axis.title.size), axis.title.y=element_text(colour = "black", size=axis.title.size),
        axis.text.x=element_text(colour = "black", size=axis.text.size), axis.text.y=element_text(colour = "black", size=axis.text.size),
        plot.title=element_text(colour = "black",size=title.size, face="bold", hjust=.5),
        axis.ticks=element_line(colour="black"), legend.position = legend.position, legend.key=element_blank())
}

#function to get qq plot with line from ggplot2
#from http://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2
qqplot.data <- function (vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)
  
}

#Histogram function using ggplot2
Sp.Hist <- function(Data, var, bins=30, save=F)
{
  Data<-Data[var]
  Data<-na.exclude(Data)
  Mean<-mean(Data[,1])
  SD<-sd(Data[,1])
  Histogram <- ggplot(Data, aes(Data[1])) + geom_histogram(aes(y=..density..), colour="white", bins=bins) + 
    stat_function(fun=dnorm, args=list(mean=Mean, sd=SD), size=3) +
    ggtitle(paste(var, "Histogram", "\n", "Mean =", round(Mean,2), "SD =", round(SD,2))) + scale_x_continuous(var) + Sp.Theme()
  Histogram
  if(save){
    ggsave(Histogram, filename=paste(paste(var, " Histogram.png", sep="")), width = 8, height=6.5, dpi=500)
  }
  return (Histogram)
}

#multiplot to grid plot all subject outputs
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}





#EXPLORE DATA----
#Table 1 -- Demographics and Intake
Table1 <- read.csv("C:/Users/sbuja/Documents/Salesforce Data Challenge/cleaned_data-20170810T195849Z-001/cleaned_data/Table 1 - Demographics and Intake.csv", 
                   header=T, na.strings = c(""))

#summary stats
Sp.Desc(Table1$Enrollment.Status)

Table1$Graduate <- ifelse(Table1$Enrollment.Status=="Graduate",1,0)
Table1$Discharged <- ifelse(Table1$Enrollment.Status=="Discharged",1,0)

table(Table1$Graduate)
table(Table1$Discharged)

Sp.Desc(Table1$Race.Categorization.for.Reports) #More reasonable of the racial variables
Sp.Desc(Table1$Self.Identified.Race.and.Ethnicity) #Many Many Levels

#Recode race categorization 
#Categories: Black, Latino, All Other
#For loop coding Black binary
for(i in 1:dim(Table1)[1]){ 
  if(grepl("Black", Table1$Race.Categorization.for.Reports[i])){
    Table1$Race.Bl[i] <- 1
  }
  else{
    Table1$Race.Bl[i] <- 0
  }
}
#For loop coding Latino binary
for(i in 1:dim(Table1)[1]){
  if(grepl("Latino", Table1$Race.Categorization.for.Reports[i])){
    Table1$Race.La[i] <- 1
  }
  else{
    Table1$Race.La[i] <- 0
  }
}
#Test -- Worked 
table(Table1$Race.Bl)
table(Table1$Race.La)
CrossTable(Table1$Race.Bl, Table1$Race.La)


#Gender -- well behaved
Sp.Desc(Table1$Gender)
Table1$Female <- ifelse(Table1$Gender=="Female",1,0)
table(Table1$Female)

#Risk Factors -- For and many ifs coded correctly
#For loop coding risk factors
for(i in 1:dim(Table1)[1]){ 
  #ADHD
  if(grepl("ADHD", Table1$Risk.Factors[i])){
    Table1$Risk.ADHD[i] <- 1
  }
  else{
    Table1$Risk.ADHD[i] <- 0
  }
  
  #Anxiety
  if(grepl("Anxiety", Table1$Risk.Factors[i])){
    Table1$Risk.Anxiety[i] <- 1
  }
  else{
    Table1$Risk.Anxiety[i] <- 0
  }
  
  #Caring for Family  Members
  if(grepl("Caring for Family Members", Table1$Risk.Factors[i])){
    Table1$Risk.Caring[i] <- 1
  }
  else{
    Table1$Risk.Caring[i] <- 0
  }
  
  #Court involvement
  if(grepl("Court involvement", Table1$Risk.Factors[i])){
    Table1$Risk.Court[i] <- 1
  }
  else{
    Table1$Risk.Court[i] <- 0
  }
  
  #Currently Employed
  if(grepl("Currently Employed", Table1$Risk.Factors[i])){
    Table1$Risk.Employed[i] <- 1
  }
  else{
    Table1$Risk.Employed[i] <- 0
  }
  
  #Depression
  if(grepl("Depression", Table1$Risk.Factors[i])){
    Table1$Risk.Depression[i] <- 1
  }
  else{
    Table1$Risk.Depression[i] <- 0
  }
  
  #Domestic Issues
  if(grepl("Domestic Issues", Table1$Risk.Factors[i])){
    Table1$Risk.Domestic[i] <- 1
  }
  else{
    Table1$Risk.Domestic[i] <- 0
  }
  
  #Emotional Abuse
  if(grepl("Emotional Abuse", Table1$Risk.Factors[i])){
    Table1$Risk.EmoteAbuse[i] <- 1
  }
  else{
    Table1$Risk.EmoteAbuse[i] <- 0
  }
  
  #Emotional Abuse
  if(grepl("Exposure to violence", Table1$Risk.Factors[i])){
    Table1$Risk.Violence[i] <- 1
  }
  else{
    Table1$Risk.Violence[i] <- 0
  }
  
  #Food Insecurity
  if(grepl("Food Insecurity", Table1$Risk.Factors[i])){
    Table1$Risk.Food[i] <- 1
  }
  else{
    Table1$Risk.Food[i] <- 0
  }
  
  #Foster Care
  if(grepl("Foster Care", Table1$Risk.Factors[i])){
    Table1$Risk.Foster[i] <- 1
  }
  else{
    Table1$Risk.Foster[i] <- 0
  }
  
  #Gang Affiliation
  if(grepl("Gang Affiliation", Table1$Risk.Factors[i])){
    Table1$Risk.Gang[i] <- 1
  }
  else{
    Table1$Risk.Gang[i] <- 0
  }
  
  #Incarcerated Family Member
  if(grepl("Incarcerated Family Member", Table1$Risk.Factors[i])){
    Table1$Risk.IncFam[i] <- 1
  }
  else{
    Table1$Risk.IncFam[i] <- 0
  }
  
  #Medical Concerns -- Other Mental Health
  if(grepl("Medical Concerns -- Other Mental Health", Table1$Risk.Factors[i])){
    Table1$Risk.OtherMental[i] <- 1
  }
  else{
    Table1$Risk.OtherMental[i] <- 0
  }
  
  #Medical Concerns -- Physical Health
  if(grepl("Medical Concerns -- Physical Health", Table1$Risk.Factors[i])){
    Table1$Risk.PhysHealth[i] <- 1
  }
  else{
    Table1$Risk.PhysHealth[i] <- 0
  }
  
  #Mental Illness in Household
  if(grepl("Mental Illness in Household", Table1$Risk.Factors[i])){
    Table1$Risk.MIHouse[i] <- 1
  }
  else{
    Table1$Risk.MIHouse[i] <- 0
  }
  
  #Other
  if(grepl("Other", Table1$Risk.Factors[i])){
    if(!grepl("Other Mental Health", Table1$Risk.Factors[i])){
      Table1$Risk.Other[i] <- 1
    }
    else{
      Table1$Risk.Other[i] <- 0
    }
  }
  else{
    Table1$Risk.Other[i] <- 0
  }
  
  #Physical Abuse
  if(grepl("Physical Abuse", Table1$Risk.Factors[i])){
    Table1$Risk.PhysAbuse[i] <- 1
  }
  else{
    Table1$Risk.PhysAbuse[i] <- 0
  }
  
  #Pregnant/Parenting
  if(grepl("Pregnant/Parenting", Table1$Risk.Factors[i])){
    Table1$Risk.Parent[i] <- 1
  }
  else{
    Table1$Risk.Parent[i] <- 0
  }
  
  #Previously incarcerated
  if(grepl("Previously incarcerated", Table1$Risk.Factors[i])){
    Table1$Risk.PrevInc[i] <- 1
  }
  else{
    Table1$Risk.PrevInc[i] <- 0
  }
  
  #Receiving Counseling/Therapy
  if(grepl("Receiving Counseling/Therapy", Table1$Risk.Factors[i])){
    Table1$Risk.Counseling[i] <- 1
  }
  else{
    Table1$Risk.Counseling[i] <- 0
  }
  
  #Recent death among family/friends
  if(grepl("Recent death among family/friends", Table1$Risk.Factors[i])){
    Table1$Risk.Death[i] <- 1
  }
  else{
    Table1$Risk.Death[i] <- 0
  }
  
  #Recent Hospitalization
  if(grepl("Recent Hospitalization", Table1$Risk.Factors[i])){
    Table1$Risk.Hospital[i] <- 1
  }
  else{
    Table1$Risk.Hospital[i] <- 0
  }
  
  #Self Harm
  if(grepl("Self Harm", Table1$Risk.Factors[i])){
    Table1$Risk.SelfHarm[i] <- 1
  }
  else{
    Table1$Risk.SelfHarm[i] <- 0
  }
  
  #Social Security/Disability Recipient
  if(grepl("Social Security/Disability Recipient", Table1$Risk.Factors[i])){
    Table1$Risk.Disability[i] <- 1
  }
  else{
    Table1$Risk.Disability[i] <- 0
  }
  
  #Substance Abuse
  if(grepl("Substance Abuse", Table1$Risk.Factors[i])){
    if(grepl("Substance Abuse in Household", Table1$Risk.Factors[i]) && grepl("Substance Abuse;", Table1$Risk.Factors[i])){
      Table1$Risk.Drug[i] <- 1
    }
    else if(!grepl("Substance Abuse in Household", Table1$Risk.Factors[i]) && grepl("Substance Abuse", Table1$Risk.Factors[i])){
      Table1$Risk.Drug[i] <- 1
    }
    else{
      Table1$Risk.Drug[i] <- 0
    }
  }
  else{
    Table1$Risk.Drug[i] <- 0
  }
  
  #Substance Abuse in Household
  if(grepl("Substance Abuse in Household", Table1$Risk.Factors[i])){
    Table1$Risk.DrugHouse[i] <- 1
  }
  else{
    Table1$Risk.DrugHouse[i] <- 0
  }
  
  #Temporary Shelter/Homeless
  if(grepl("Temporary Shelter/Homeless", Table1$Risk.Factors[i])){
    Table1$Risk.Homeless[i] <- 1
  }
  else{
    Table1$Risk.Homeless[i] <- 0
  }
  
  #Victim of Sexual Assault or Exploitation
  if(grepl("Victim of Sexual Assault or Exploitation", Table1$Risk.Factors[i])){
    Table1$Risk.SexAbuse[i] <- 1
  }
  else{
    Table1$Risk.SexAbuse[i] <- 0
  }
  
  #Violence Between Adults
  if(grepl("Violence Between Adults", Table1$Risk.Factors[i])){
    Table1$Risk.DomViolence[i] <- 1
  }
  else{
    Table1$Risk.DomViolence[i] <- 0
  }
  
  
}

table(Table1$Risk.ADHD)
table(Table1$Risk.Anxiety)
table(Table1$Risk.Caring)
table(Table1$Risk.Court)
table(Table1$Risk.Employed)
table(Table1$Risk.Depression)
table(Table1$Risk.Domestic)
table(Table1$Risk.EmoteAbuse)
table(Table1$Risk.Violence)
table(Table1$Risk.Food)
table(Table1$Risk.Foster)
table(Table1$Risk.Gang)
table(Table1$Risk.IncFam)
table(Table1$Risk.OtherMental)
table(Table1$Risk.PhysHealth)
table(Table1$Risk.MIHouse)
table(Table1$Risk.Other)
table(Table1$Risk.PhysAbuse)
table(Table1$Risk.Parent)
table(Table1$Risk.PrevInc)
table(Table1$Risk.Counseling)
table(Table1$Risk.Death)
table(Table1$Risk.Hospital)
table(Table1$Risk.SelfHarm)
table(Table1$Risk.Disability)
table(Table1$Risk.Drug)
table(Table1$Risk.DrugHouse)
table(Table1$Risk.Homeless)
table(Table1$Risk.SexAbuse)
table(Table1$Risk.DomViolence)

Sp.Desc(Table1[c("Risk.ADHD"	, "Risk.Anxiety"	, "Risk.Caring"	, "Risk.Court"	, "Risk.Employed",
                 "Risk.Depression"	, "Risk.Domestic"	, "Risk.EmoteAbuse"	, "Risk.Violence"	, "Risk.Food",
                 "Risk.Foster"	, "Risk.Gang"	, "Risk.IncFam"	, "Risk.OtherMental"	, "Risk.PhysHealth",
                 "Risk.MIHouse"	, "Risk.Other"	, "Risk.PhysAbuse"	, "Risk.Parent"	, "Risk.PrevInc",
                 "Risk.Counseling"	, "Risk.Death"	, "Risk.Hospital"	, "Risk.SelfHarm"	, "Risk.Disability",
                 "Risk.Drug"	, "Risk.DrugHouse"	, "Risk.Homeless"	, "Risk.SexAbuse"	, "Risk.DomViolence")])

#Collapsing into groups

Table1$Risk.MentalHealth <- ifelse(rowSums(Table1[c("Risk.ADHD", "Risk.OtherMental", "Risk.Drug", "Risk.SelfHarm")])>0, 1, 0)
#View(Table1[c("Risk.ADHD", "Risk.OtherMental", "Risk.Drug", "Risk.SelfHarm", "Risk.MentalHealth")])

Table1$Risk.Abuse <- ifelse(rowSums(Table1[c("Risk.EmoteAbuse", "Risk.PhysAbuse", "Risk.SexAbuse", "Risk.Domestic", "Risk.Violence", 
                                            "Risk.DomViolence", "Risk.Foster")])>0, 1, 0)
#View(Table1[c("Risk.EmoteAbuse", "Risk.PhysAbuse", "Risk.SexAbuse", "Risk.Domestic", "Risk.Violence", 
#              "Risk.DomViolence", "Risk.Foster", "Risk.Abuse")])

Table1$Risk.Housing <- ifelse(rowSums(Table1[c("Risk.DrugHouse", "Risk.MIHouse", "Risk.Homeless")])>0, 1, 0)
#View(Table1[c("Risk.DrugHouse", "Risk.MIHouse", "Risk.Homeless", "Risk.Housing")])

Table1$Risk.Legal <- ifelse(rowSums(Table1[c("Risk.Court", "Risk.PrevInc", "Risk.IncFam", "Risk.Gang")])>0, 1, 0)
#View(Table1[c("Risk.Court", "Risk.PrevInc", "Risk.IncFam", "Risk.Gang", "Risk.Legal")])

Table1$Risk.Others <- ifelse(rowSums(Table1[c("Risk.Other", "Risk.Employed", "Risk.Disability", "Risk.Parent",
                                             "Risk.Caring", "Risk.Food")])>0, 1, 0)
#View(Table1[c("Risk.Other", "Risk.Employed", "Risk.Disability", "Risk.Parent",
#              "Risk.Caring", "Risk.Food", "Risk.Others")])


Sp.Desc(Table1[c("Risk.PhysHealth", "Risk.Death", "Risk.Anxiety", "Risk.Counseling", "Risk.Depression", 
                 "Risk.MentalHealth", "Risk.Abuse", "Risk.Housing", "Risk.Legal", "Risk.Others")])

rcorr(as.matrix(Table1[c("Risk.PhysHealth", "Risk.Death", "Risk.Anxiety", "Risk.Counseling", "Risk.Depression", 
               "Risk.MentalHealth", "Risk.Abuse", "Risk.Housing", "Risk.Legal", "Risk.Others")]))



#Services Received -- coded correctly
#For loop coding risk factors
for(i in 1:dim(Table1)[1]){ 
  #DCF
  if(grepl("DCF", Table1$Services.Received[i])){
    Table1$Service.DCF[i] <- 1
  }
  else{
    Table1$Service.DCF[i] <- 0
  }
  
  #Department of Youth Services DYS
  if(grepl("Department of Youth Services", Table1$Services.Received[i])){
    Table1$Service.DYS[i] <- 1
  }
  else{
    Table1$Service.DYS[i] <- 0
  }
  
  #Food Stamps (SNAP)
  if(grepl("Food Stamps", Table1$Services.Received[i])){
    Table1$Service.SNAP[i] <- 1
  }
  else{
    Table1$Service.SNAP[i] <- 0
  }
  
  #Other
  if(grepl("Other", Table1$Services.Received[i])){
    Table1$Service.Other[i] <- 1
  }
  else{
    Table1$Service.Other[i] <- 0
  }
  
  #Other
  if(grepl("Other", Table1$Services.Received[i])){
    Table1$Service.Other[i] <- 1
  }
  else{
    Table1$Service.Other[i] <- 0
  }
  
  #Section 8 Housing
  if(grepl("Section 8 Housing", Table1$Services.Received[i])){
    Table1$Service.Section8[i] <- 1
  }
  else{
    Table1$Service.Section8[i] <- 0
  }
  
  #Social Security/Disability
  if(grepl("Social Security/Disability", Table1$Services.Received[i])){
    Table1$Service.Disability[i] <- 1
  }
  else{
    Table1$Service.Disability[i] <- 0
  }
  
  #Substance Abuse Program
  if(grepl("Substance Abuse Program", Table1$Services.Received[i])){
    Table1$Service.SUDtrx[i] <- 1
  }
  else{
    Table1$Service.SUDtrx[i] <- 0
  }
  
  #Transitional Aid to Families
  if(grepl("Transitional Aid to Families", Table1$Services.Received[i])){
    Table1$Service.FamilyAid[i] <- 1
  }
  else{
    Table1$Service.FamilyAid[i] <- 0
  }
}

table(Table1$Service.DCF)
table(Table1$Service.DYS)
table(Table1$Service.SNAP)
table(Table1$Service.Other)
table(Table1$Service.Section8)
table(Table1$Service.Disability)
table(Table1$Service.SUDtrx)
table(Table1$Service.FamilyAid)

Sp.Desc(Table1[c("Service.DCF", "Service.DYS", "Service.SNAP", "Service.Other", "Service.Section8", "Service.Disability",
                 "Service.SUDtrx", "Service.FamilyAid")])

Table1$Service.Others <- ifelse(rowSums(Table1[c("Service.DCF", "Service.DYS", "Service.Other", "Service.Disability",
                                                 "Service.SUDtrx", "Service.FamilyAid")])>0, 1, 0)
# View(Table1[c("Service.DCF", "Service.DYS", "Service.Other", "Service.Disability",
#               "Service.SUDtrx", "Service.FamilyAid", "Service.Others")])

Sp.Desc(Table1[c("Service.SNAP", "Service.Section8", "Service.Others")])

rcorr(as.matrix(Table1[c("Service.SNAP", "Service.Section8", "Service.Others")]))


#First Language
Sp.Desc(Table1$First.Langauge)

Table1$First.En <- ifelse(Table1$First.Langauge=="English",1,0)
Table1$First.Sp <- ifelse(Table1$First.Langauge=="Spanish",1,0)
Table1$First.Ot <- ifelse(Table1$First.Langauge!="English",ifelse(Table1$First.Langauge!="Spanish",1,0),0)

table(Table1$First.En)
table(Table1$First.Sp)
table(Table1$First.Ot)


#English Language Learner
Sp.Desc(Table1$English.Language.Learner)

Table1$EnglishLearn <- ifelse(Table1$English.Language.Learner=="ELL",1,
                              ifelse(Table1$English.Language.Learner=="FLEP",1,
                                     ifelse(Table1$English.Language.Learner=="",NA,0)))
table(Table1$EnglishLearn)


#Special Education Status
Sp.Desc(Table1$Special.Education.Status)

#Coded binary of none or some
Table1$SpecialEd <- ifelse(Table1$Special.Education.Status=="None",0,1)

table(Table1$SpecialEd)


#Exemptions
Sp.Desc(Table1$Number.of.Exemptions)
Table1$Number.of.Exemptions <- as.double(Table1$Number.of.Exemptions)
Sp.Desc(Table1$Number.of.Exemptions)


#Far/Close
Sp.Desc(Table1$Far.Close)

Table1$FarClose <- ifelse(Table1$Far.Close=="Very Far", 0,
                          ifelse(Table1$Far.Close=="Far", 1,
                                 ifelse(Table1$Far.Close=="Close", 2, 3)))

table(Table1$FarClose)


#MCAS Scores

Sp.Desc(Table1[c("ELA.MCAS.Score", "Math.MCAS.Score", "Science.MCAS.Score")])



View(Table1)

#Write New CSV to share with team
# setwd("C:/Users/sbuja/Documents/Salesforce Data Challenge/cleaned_data-20170810T195849Z-001/cleaned_data/")
# write.csv(Table1, file="Table1 Cleaned.csv", row.names=F)



#PROPENSITY SCORE ANALYSIS----

Table1 <- read.csv("C:/Users/sbuja/Documents/Salesforce Data Challenge/cleaned_data-20170810T195849Z-001/cleaned_data/Table1 Cleaned.csv")

Sp.Desc(Table1[c("Risk.PhysHealth", "Risk.Death", "Risk.Anxiety", "Risk.Counseling", "Risk.Depression", 
                 "Risk.MentalHealth", "Risk.Abuse", "Risk.Housing", "Risk.Legal", "Risk.Others",
                 "Service.SNAP", "Service.Section8", "Service.Others",
                 "Race.Bl", "Race.La",
                 "Female",
                 "SpecialEd",
                 "Number.of.Exemptions",
                 "FarClose")])

Counseling <- read.csv("C:/Users/sbuja/Documents/Salesforce Data Challenge/cleaned_data-20170810T195849Z-001/cleaned_data/CleanedCounseling.csv")

Sp.Desc(Counseling)

#Merge Table 1 with Counseling Data
Table1 <- Table1[order(Table1$New.ID),]
Counseling <- Counseling[order(Counseling$New.ID),]

Table1Counsel <- merge(Table1, Counseling, by="New.ID")

table(Table1$New.ID)

#Check for non-overlapping New.ID
setdiff(Table1$New.ID, Counseling$New.ID)
#None that are non-overlapping

#Check for duplicated IDs
Table1$New.ID[duplicated(Table1$New.ID) | duplicated(Table1$New.ID, fromLast=TRUE)]
Counseling$New.ID[duplicated(Counseling$New.ID) | duplicated(Counseling$New.ID, fromLast=TRUE)]

#NOW NO DUPLICATES AND NO NONOVERLAPPING STUDENTS

Sp.Desc(Table1Counsel[c("Risk.PhysHealth", "Risk.Death", "Risk.Anxiety", "Risk.Counseling", "Risk.Depression", 
                 "Risk.MentalHealth", "Risk.Abuse", "Risk.Housing", "Risk.Legal", "Risk.Others",
                 "Service.SNAP", "Service.Section8", "Service.Others",
                 "Race.Bl", "Race.La",
                 "Female",
                 "SpecialEd",
                 "Number.of.Exemptions",
                 "FarClose",
                 "Counseling")])

#Merged properly after 1zn4g duplicate entries were deleted


Table1Counsel_covs <- c("Risk.PhysHealth", "Risk.Death", "Risk.Anxiety", "Risk.Counseling", "Risk.Depression", 
                        "Risk.MentalHealth", "Risk.Abuse", "Risk.Housing", "Risk.Legal", "Risk.Others",
                        "Service.SNAP", "Service.Section8", "Service.Others",
                        "Race.Bl", "Race.La",
                        "Female",
                        "SpecialEd",
                        "Number.of.Exemptions",
                        "FarClose")

#Do the risk factors and student variables affect who gets counseling?
options(dplyr.width = Inf)

print(Table1Counsel %>%
  group_by(Counseling) %>%
  select(one_of(Table1Counsel_covs)) %>%
  summarize_each(funs(mean(., na.rm = T))), n=40)

lapply(Table1Counsel_covs, function(v) {
  t.test(Table1Counsel[, v] ~ Table1Counsel[, "Counseling"])
})

Model.Pscore <- glm(Counseling ~ Risk.PhysHealth + Risk.Death + Risk.Anxiety + Risk.Depression + 
                      Risk.MentalHealth + Risk.Abuse + Risk.Housing + Risk.Legal + Risk.Others +                        
                      Service.SNAP + Service.Section8 + Service.Others +
                      Race.Bl + Race.La + Female + SpecialEd + Number.of.Exemptions + FarClose,
                    data=Table1Counsel, family=binomial())
summary(Model.Pscore)
#Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          -3.20000    0.54438  -5.878 4.15e-09 ***
# Risk.PhysHealth      -0.01691    0.31186  -0.054   0.9567    
# Risk.Death            0.49507    0.31706   1.561   0.1184    
# Risk.Anxiety          1.25240    0.31118   4.025 5.70e-05 ***
# Risk.Depression       1.31474    0.31799   4.135 3.56e-05 ***
# Risk.MentalHealth     0.10967    0.33468   0.328   0.7432    
# Risk.Abuse            0.23251    0.40960   0.568   0.5703    
# Risk.Housing          0.07012    0.43501   0.161   0.8719    
# Risk.Legal           -0.42983    0.45321  -0.948   0.3429    
# Risk.Others          -0.95991    0.49431  -1.942   0.0521 .  
# Service.SNAP         -0.30558    0.31493  -0.970   0.3319    
# Service.Section8      0.38495    0.31879   1.208   0.2272    
# Service.Others        0.30192    0.33238   0.908   0.3637    
# Race.Bl               0.25721    0.31716   0.811   0.4174    
# Race.La               0.35464    0.30543   1.161   0.2456    
# Female                0.45378    0.31034   1.462   0.1437    
# SpecialEd             0.48214    0.30395   1.586   0.1127    
# Number.of.Exemptions  0.10639    0.07257   1.466   0.1426    
# FarClose             -0.23261    0.38661  -0.602   0.5474
                 
Pscore.df <- data.frame(Pscore = predict(Model.Pscore, type = "response"),
                        Couns = Model.Pscore$model$Counseling)

#View(Pscore.df)

labs <- c("0 - No Counseling", "1 - Counseling")
Propensity.plot <- Pscore.df %>%
  mutate(Couns= ifelse(Couns==0, labs[1], labs[2])) %>%
  ggplot(aes(x = Pscore)) +
  geom_histogram(colour="white") +
  facet_wrap(~Couns) +
  xlab("Probability of receiving counseling") +
  Sp.Theme()
Propensity.plot
ggsave(Propensity.plot, filename="Propensity.plot.png", dpi=200, width=6, height=4)

#Merge Pscore information
#delete person missing counseling data
Table1Counsel <- Table1Counsel[-which(is.na(Table1Counsel$Counseling)), ]
Table1CounselPscore <- cbind(Table1Counsel, Pscore.df)

dim(Table1CounselPscore)

#check that the counseling data lines up for all subjects
table(Table1CounselPscore$Counseling - Table1CounselPscore$Couns)
#All lines up /thumbsup

#Plotting efficacy of propensity matching
Table1CounselPscore$Cstr <- ifelse(Table1CounselPscore$Counseling==0,"No Counseling","Counseling")

Propplot <- function(varname){
  Table1CounselPscore$variable <- Table1CounselPscore[, varname]
  plot <- ggplot(Table1CounselPscore, aes(x=Pscore, y=variable, colour=Cstr)) +
    geom_point()+
    geom_smooth(method="loess", se=F)+
    scale_x_continuous("Propensity Score")+
    scale_y_continuous(varname) + 
    Sp.Theme(legend="right") +
    theme(legend.title=element_blank())
    
  return(plot)
}

Risk.PhysHealth.Propplot <- Propplot("Risk.PhysHealth")
ggsave(Risk.PhysHealth.Propplot, filename="Risk.PhysHealth.Propplot.png", dpi=200, width=6, height=4)

Risk.Death.Propplot <- Propplot("Risk.Death")
ggsave(Risk.Death.Propplot, filename="Risk.Death.Propplot.png", dpi=200, width=6, height=4)

Risk.Anxiety.Propplot <- Propplot("Risk.Anxiety")
ggsave(Risk.Anxiety.Propplot, filename="Risk.Anxiety.Propplot.png", dpi=200, width=6, height=4)

Risk.Depression.Propplot <- Propplot("Risk.Depression")
ggsave(Risk.Depression.Propplot, filename="Risk.Depression.Propplot.png", dpi=200, width=6, height=4)

Risk.MentalHealth.Propplot <- Propplot("Risk.MentalHealth")
ggsave(Risk.MentalHealth.Propplot, filename="Risk.MentalHealth.Propplot.png", dpi=200, width=6, height=4)

Risk.Abuse.Propplot <- Propplot("Risk.Abuse")
ggsave(Risk.Abuse.Propplot, filename="Risk.Abuse.Propplot.png", dpi=200, width=6, height=4)

Risk.Housing.Propplot <- Propplot("Risk.Housing")
ggsave(Risk.Housing.Propplot, filename="Risk.Housing.Propplot.png", dpi=200, width=6, height=4)

Risk.Legal.Propplot <- Propplot("Risk.Legal")
ggsave(Risk.Legal.Propplot, filename="Risk.Legal.Propplot.png", dpi=200, width=6, height=4)

Risk.Others.Propplot <- Propplot("Risk.Others")
ggsave(Risk.Others.Propplot, filename="Risk.Others.Propplot.png", dpi=200, width=6, height=4)

Service.SNAP.Propplot <- Propplot("Service.SNAP")
ggsave(Service.SNAP.Propplot, filename="Service.SNAP.Propplot.png", dpi=200, width=6, height=4)

Service.Section8.Propplot <- Propplot("Service.Section8")
ggsave(Service.Section8.Propplot, filename="Service.Section8.Propplot.png", dpi=200, width=6, height=4)

Service.Others.Propplot <- Propplot("Service.Others")
ggsave(Service.Others.Propplot, filename="Service.Others.Propplot.png", dpi=200, width=6, height=4)

Race.Bl.Propplot <- Propplot("Race.Bl")
ggsave(Race.Bl.Propplot, filename="Race.Bl.Propplot.png", dpi=200, width=6, height=4)

Race.La.Propplot <- Propplot("Race.La")
ggsave(Race.La.Propplot, filename="Race.La.Propplot.png", dpi=200, width=6, height=4)

Female.Propplot <- Propplot("Female")
ggsave(Female.Propplot, filename="Female.Propplot.png", dpi=200, width=6, height=4)

SpecialEd.Propplot <- Propplot("SpecialEd")
ggsave(SpecialEd.Propplot, filename="SpecialEd.Propplot.png", dpi=200, width=6, height=4)

Number.of.Exemptions.Propplot <- Propplot("Number.of.Exemptions")
ggsave(Number.of.Exemptions.Propplot, filename="Number.of.Exemptions.Propplot.png", dpi=200, width=6, height=4)

FarClose.Propplot <- Propplot("FarClose")
ggsave(FarClose.Propplot, filename="FarClose.Propplot.png", dpi=200, width=6, height=4)

#write to share with team
# setwd("C:/Users/sbuja/Documents/Salesforce Data Challenge/cleaned_data-20170810T195849Z-001/cleaned_data/")
# write.csv(Table1CounselPscore, file="Predictor Variables with Propensity Scores.csv", row.names=F)



#GLM and Logistic Analyses----

StLvl <- read.csv("C:/Users/sbuja/Documents/Salesforce Data Challenge/cleaned_data-20170810T195849Z-001/cleaned_data/Predictor Variables with Propensity Scores.csv")

Sp.Desc(StLvl)


#logistic predicting graduation vs. discharged
#current dropped because 
StLvl.NoCurrent <- subset(StLvl, Enrollment.Status.x!="Current")
dim(StLvl.NoCurrent)
table(StLvl.NoCurrent$Graduate)

Model.GD.Log <- glm(Graduate ~ Pscore + Risk.PhysHealth + Risk.Death + Risk.Anxiety + Risk.Depression + 
                    Risk.MentalHealth + Risk.Abuse + Risk.Housing + Risk.Legal + Risk.Employed +  Risk.Others + 
                    Service.SNAP + Service.Section8 + Service.Others +
                    Race.Bl + Race.La + Female + SpecialEd + Number.of.Exemptions + FarClose,
                  data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log)
anova(Model.GD.Log, test="Chisq") #chisq test of logistic regression predictors.
Model.GD.Log.rocplot <- rocplot(Model.GD.Log)
Model.GD.Log.rocplot
ggsave(Model.GD.Log.rocplot, filename="Model.GD.Log.rocplot.png", dpi=200, height=5, width=6)
pR2(Model.GD.Log)
#        llh     llhNull          G2    McFadden        r2ML        r2CU 
#-19.7741674 -56.7899568  74.0315789   0.6518017   0.4197811   0.7414151 

#DONT USE CHISQ TEST. IT TESTS EACH PREDICTOR SEQUESTIALLY (SS TYPE 1) VS. Wald Z which is SS type 3

#Risk.Others and Risk.Employed both predict graduation even though most of the others are there for employed. Recode to separate
# Risk.Employed         1  12.4716       125     78.827 0.0004132 ***
# Risk.Others           1   6.0642       124     72.762 0.0137947 *  
#   
StLvl.NoCurrent$Risk.Others.NoEmp <- ifelse(rowSums(StLvl.NoCurrent[c("Risk.Other", "Risk.Disability", "Risk.Parent", "Risk.Caring", "Risk.Food")])>0, 1, 0)

table(StLvl.NoCurrent$Risk.Others)
table(StLvl.NoCurrent$Risk.Others.NoEmp)
table(StLvl.NoCurrent$Risk.Employed)

Model.GD.Log.NoEmp <- glm(Graduate ~ Pscore + Risk.PhysHealth + Risk.Death + Risk.Anxiety + Risk.Depression + 
                      Risk.MentalHealth + Risk.Abuse + Risk.Housing + Risk.Legal + Risk.Employed +  Risk.Others.NoEmp + 
                      Service.SNAP + Service.Section8 + Service.Others + SpecialEd + 
                      Race.Bl + Race.La + Female + Number.of.Exemptions + FarClose,
                    data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.NoEmp)
pR2(Model.GD.Log.NoEmp)
#        llh     llhNull          G2    McFadden        r2ML        r2CU 
#-20.2681875 -56.7899568  73.0435387   0.6431026   0.4155505   0.7339430
rocplot(Model.GD.Log.NoEmp)

#Compute odds ratios and 95% CIs
exp(cbind(OR = coef(Model.GD.Log.NoEmp), confint(Model.GD.Log.NoEmp)))

#multiply pscores by 100 for better interpretation
StLvl.NoCurrent$Pscore100 <- StLvl.NoCurrent$Pscore * 100

Model.GD.Log.NoEmp <- glm(Graduate ~ Pscore100 + Risk.PhysHealth + Risk.Death + Risk.Anxiety + Risk.Depression + 
                            Risk.MentalHealth + Risk.Abuse + Risk.Housing + Risk.Legal + Risk.Employed +  Risk.Others.NoEmp + 
                            Service.SNAP + Service.Section8 + Service.Others +
                            Race.Bl + Race.La + Female + SpecialEd + Number.of.Exemptions + FarClose,
                          data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.NoEmp)

#Compute odds ratios and 95% CIs
options(scipen = 999, "digits"=4)
exp(cbind(OR = coef(Model.GD.Log.NoEmp), confint(Model.GD.Log.NoEmp)))

#Risk.Legal is misbehaved so drop it. 

Model.GD.Log.NoEmp <- glm(Graduate ~ Pscore100 + Risk.PhysHealth + Risk.Death + Risk.Anxiety + Risk.Depression + 
                            Risk.MentalHealth + Risk.Abuse + Risk.Housing + Risk.Employed +  Risk.Others.NoEmp + 
                            Service.SNAP + Service.Section8 + Service.Others +
                            Race.Bl + Race.La + Female + SpecialEd + Number.of.Exemptions + FarClose,
                          data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.NoEmp)
#Compute odds ratios and 95% CIs
exp(cbind(OR = coef(Model.GD.Log.NoEmp), confint(Model.GD.Log.NoEmp)))

# #                     Estimate Std. Error z value Pr(>|z|)   
# (Intercept)          -9.11789    2.83218  -3.219  0.00128 **
# Pscore100             0.02121    0.10786   0.197  0.84410   
# Risk.PhysHealth       0.46929    1.09298   0.429  0.66766   
# Risk.Death           -0.27844    1.59691  -0.174  0.86158   
# Risk.Anxiety          0.19931    2.95566   0.067  0.94624   
# Risk.Depression      -0.08890    2.75363  -0.032  0.97424   
# Risk.MentalHealth    -0.14163    1.10085  -0.129  0.89763   
# Risk.Abuse           -0.20731    1.35462  -0.153  0.87837   
# Risk.Housing          0.31015    1.49125   0.208  0.83524   
# Risk.Employed         3.00400    1.74496   1.722  0.08516 . 
# Risk.Others.NoEmp    -0.55887    2.37159  -0.236  0.81370   
# Service.SNAP          0.66295    1.22265   0.542  0.58766   
# Service.Section8     -0.03077    1.20954  -0.025  0.97971   
# Service.Others       -1.71287    1.73962  -0.985  0.32481   
# Race.Bl               0.24085    1.27591   0.189  0.85028   
# Race.La               0.79540    1.53096   0.520  0.60338   
# Female                2.10101    1.74599   1.203  0.22885   
# SpecialEd             0.07094    1.49532   0.047  0.96216   
# Number.of.Exemptions  0.45242    0.30000   1.508  0.13153   
# FarClose              0.57312    1.34324   0.427  0.66962   

#Realy cant predict graduation based in large part because of low graduation rates
#Odds Ratio Confidence Intervals are freaking huge. 
#                                 OR            2.5 %         97.5 %
# (Intercept)           0.0001096862 0.00000008491909    0.009022395
# Pscore100             1.0214374935 0.81829607239664    1.274732268
# Risk.PhysHealth       1.5988514511 0.17321633378093   15.243573327
# Risk.Death            0.7569627839 0.02789253797842   17.691926230
# Risk.Anxiety          1.2205556319 0.00370642667211  612.329570162
# Risk.Depression       0.9149341792 0.00261797098248  236.542399771
# Risk.MentalHealth     0.8679429510 0.08431991659538    7.158375176
# Risk.Abuse            0.8127646266 0.04687003562985   11.688809331
# Risk.Housing          1.3636330226 0.06159829330582   25.963119908
# Risk.Employed        20.1661099007 0.69465236883852 1032.392867962
# Risk.Others.NoEmp     0.5718545210 0.00374193758800   63.351891025
# Service.SNAP          1.9405132566 0.18304704673840   26.946815308
# Service.Section8      0.9697025124 0.08284724646953   12.595672079
# Service.Others        0.1803475466 0.00355204929147    3.706201166
# Race.Bl               1.2723289710 0.08362115089143   14.548080491
# Race.La               2.2153168907 0.09562184982235   51.382884345
# Female                8.1743811801 0.39766701046552  625.137947353
# SpecialEd             1.0735156661 0.05255295039186   24.003378781
# Number.of.Exemptions  1.5721139328 0.89453519825733    3.104925386
# FarClose              1.7737842456 0.13113110147153   33.857247765


#More targetted tests of counseling effect
#Throwing everything at the table doesn't seem to work. 
#Try by group

#Risk factors
Model.GD.Log.Risk <- glm(Graduate ~ Pscore100 +  
                                 Risk.PhysHealth +
                                 Risk.Death + 
                                 Risk.Anxiety + 
                                 Risk.Depression + 
                                 Risk.MentalHealth + 
                                 Risk.Abuse + 
                                 Risk.Housing +  
                                 Risk.Employed +  
                                 Risk.Others.NoEmp,
                               data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.Risk)
#Compute odds ratios and 95% CIs
exp(cbind(OR = coef(Model.GD.Log.Risk), confint(Model.GD.Log.Risk)))

#Able to find things this way
#                  Estimate Std. Error z value   Pr(>|z|)    
# (Intercept)       -4.87287    0.93892  -5.190 0.00000021 ***
# Pscore100          0.19635    0.04508   4.355 0.00001329 ***
# Risk.PhysHealth    0.94985    0.66151   1.436   0.151034    
# Risk.Death        -1.78176    0.81397  -2.189   0.028599 *  
# Risk.Anxiety      -3.75514    1.20397  -3.119   0.001815 ** 
# Risk.Depression   -4.74512    1.35770  -3.495   0.000474 ***
# Risk.MentalHealth -0.51576    0.69002  -0.747   0.454791    
# Risk.Abuse        -1.32490    0.92612  -1.431   0.152548    
# Risk.Housing      -1.21514    0.91945  -1.322   0.186300    
# Risk.Employed      3.50221    0.99850   3.507   0.000452 ***
# Risk.Others.NoEmp  2.05935    1.39377   1.478   0.139531   

#plot
#mean center Pscore100 
StLvl.NoCurrent$Pscore100.M <- StLvl.NoCurrent$Pscore100 - 23.3632481
Model.GD.Log.Risk.plot <- glm(Graduate ~ Pscore100.M +  
                           Risk.PhysHealth +
                           Risk.Death + 
                           Risk.Anxiety + 
                           Risk.Depression + 
                           Risk.MentalHealth + 
                           Risk.Abuse + 
                           Risk.Housing +  
                           Risk.Employed +  
                           Risk.Others.NoEmp,
                         data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.Risk.plot)

PredValues <- data.frame(Pscore100.M=0,
                         Risk.PhysHealth = c(0,1,rep(0,16)),
                         Risk.Death = c(rep(0,3),1,rep(0,14)), 
                         Risk.Anxiety = c(rep(0,5),1,rep(0,12)), 
                         Risk.Depression = c(rep(0,7),1,rep(0,10)), 
                         Risk.MentalHealth = c(rep(0,9),1,rep(0,8)), 
                         Risk.Abuse = c(rep(0,11),1,rep(0,6)), 
                         Risk.Housing = c(rep(0,13),1,rep(0,4)),  
                         Risk.Employed = c(rep(0,15),1,rep(0,2)),  
                         Risk.Others.NoEmp = c(rep(0,17),1),
                         Risk.Factor=c(rep("Physical Health",2), rep("Recent Death",2), rep("Anxiety",2), rep("Depression",2),
                                       rep("Other MH",2), rep("Abuse",2), rep("Housing",2), rep("Employed",2),
                                       rep("Other",2)),
                         PresAbs=rep(c("Absent","Present"),9))
PredValues$Risk.Factor <- factor(PredValues$Risk.Factor, levels=c("Employed", "Depression", "Anxiety", "Recent Death",
                                                                  "Other", "Physical Health", "Abuse", "Housing", "Other MH"))
PredValues$pred<- predict(Model.GD.Log.Risk.plot, newdata=PredValues, type="response")

Risk.Grad.plot <- ggplot(data=PredValues, aes(x=Risk.Factor, fill=PresAbs, y=pred)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Graduation Rate by Risk Factors") +
  scale_y_continuous("Graduation Rate", labels=percent) +
  scale_fill_manual(values = c("lightblue", "red")) +
  Sp.Theme(legend.position=c(.8,.8)) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.text = element_text(size=15), 
        axis.text.x = element_text(size=12))
Risk.Grad.plot
ggsave(Risk.Grad.plot, filename="Risk.Grad.plot.png", width = 10, height=5, dpi=200)


#Does counseling work?

table(StLvl.NoCurrent$Counseling, StLvl.NoCurrent$Risk.Employed)
#weirdly no student is both employed and receives counseling (when current students excluded)
#Can't look at counseling effect there)
#Compute odds ratios and 95% CIs
exp(cbind(OR = coef(Model.GD.Log.Risk.Couns), confint(Model.GD.Log.Risk.Couns)))

Model.GD.Log.Risk.Couns <- glm(Graduate ~ Pscore100 + Counseling + 
                                 Risk.PhysHealth + Counseling:Risk.PhysHealth +
                                 Risk.Death + Counseling:Risk.Death + 
                                 Risk.Anxiety + Counseling:Risk.Anxiety + 
                                 Risk.Depression + Counseling:Risk.Depression +
                                 Risk.MentalHealth + Counseling:Risk.MentalHealth + 
                                 Risk.Abuse + Counseling:Risk.Abuse +
                                 Risk.Housing + Counseling:Risk.Housing + 
                                 Risk.Employed +  
                                 Risk.Others.NoEmp + Counseling:Risk.Others.NoEmp,
                               data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.Risk.Couns)

#No evidence that counseling effects these risk factors
# Counseling:Risk.PhysHealth     -18.34148  5716.22509  -0.003   0.997440    
# Counseling:Risk.Death            0.76007     2.09252   0.363   0.716432    
# Counseling:Risk.Anxiety         31.30499  6503.03657   0.005   0.996159    
# Counseling:Risk.Depression      14.55035  4655.04589   0.003   0.997506    
# Counseling:Risk.MentalHealth    -0.71466     2.12039  -0.337   0.736085    
# Counseling:Risk.Abuse          -52.14359  8998.98964  -0.006   0.995377    
# Counseling:Risk.Housing         32.02828  6353.77917   0.005   0.995978    
# Counseling:Risk.Others.NoEmp   107.75616 34964.63266   0.003   0.997541    

#But most of this data is basically incalculable anyways
#Crazy high Standard errors

table(StLvl.NoCurrent$Counseling, StLvl.NoCurrent$Risk.PhysHealth)
#     0  1
#  0 68 39
#  1 19 10

table(StLvl.NoCurrent$Counseling, StLvl.NoCurrent$Risk.Others.NoEmp)
#     0   1
# 0 104   3
# 1  28   1

#Service Utilization
Model.GD.Log.Service <- glm(Graduate ~ Pscore100 +  
                           Service.SNAP + 
                           Service.Section8 + 
                           Service.Others,
                         data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.Service)
#Compute odds ratios and 95% CIs
exp(cbind(OR = coef(Model.GD.Log.Service), confint(Model.GD.Log.Service)))

#overall service utilization didn't predict graduation
#Service.SNAP      0.46471    0.56187   0.827     0.40819    
#Service.Section8 -0.73266    0.58430  -1.254     0.20987    
#Service.Others   -0.55255    0.61408  -0.900     0.36823

Model.GD.Log.ServiceCouns <- glm(Graduate ~ Pscore100 + Counseling +
                              Service.SNAP + Counseling:Service.SNAP +
                              Service.Section8 + Counseling:Service.Section8 +
                              Service.Others + Counseling:Service.Others,
                            data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.ServiceCouns)
#No counseling interaction
#Counseling:Service.SNAP     -0.72487    1.34042  -0.541    0.58866    
#Counseling:Service.Section8 -0.88350    1.44551  -0.611    0.54107    
#Counseling:Service.Others   -0.54196    1.50835  -0.359    0.71936   


#Demographics
Model.GD.Log.Demo<- glm(Graduate ~ Pscore100 +  
                              Race.Bl + 
                              Race.La + 
                              Female,
                            data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.Demo)
#Compute odds ratios and 95% CIs
exp(cbind(OR = coef(Model.GD.Log.Demo), confint(Model.GD.Log.Demo)))
#            Estimate Std. Error z value Pr(>|z|)   
# (Intercept) -3.69142    1.17728  -3.136  0.00172 **
# Pscore100    0.01655    0.01135   1.459  0.14466   
# Race.Bl     -0.92839    0.63529  -1.461  0.14392   
# Race.La     -0.34389    0.64595  -0.532  0.59446   
# Female       2.63274    1.05753   2.490  0.01279 * 

#Females more likely to graduate
#                     OR       2.5 %      97.5 %
# (Intercept)  0.02493646 0.001170662   0.1736112
# Pscore100    1.01668902 0.993904474   1.0397489
# Race.Bl      0.39518783 0.110074409   1.3627133
# Race.La      0.70900519 0.193446314   2.4916606
# Female      13.91188650 2.614794366 257.7287027
Model.GD.Log.Demo.plot <- glm(Graduate ~ Pscore100.M +  
                                Race.Bl + Race.La + Female,
                              data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.Demo.plot)

PredValues <- data.frame(Pscore100.M=0,
                         Race.Bl = c(0,1,rep(0,4)),
                         Race.La = c(rep(0,3),1,rep(0,2)), 
                         Female= c(rep(0,5),1),
                         Demo.Factor=c(rep("Black",2), rep("Latino",2), rep("Female",2)),
                         PresAbs=rep(c("Absent","Present"),3))
PredValues$Demo.Factor <- factor(PredValues$Demo.Factor, levels=c("Female", "Black", "Latino"))
PredValues$pred<- predict(Model.GD.Log.Demo.plot, newdata=PredValues, type="response")

Demo.Grad.plot <- ggplot(data=PredValues, aes(x=Demo.Factor, fill=PresAbs, y=pred)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Graduation Rate by Demographics") +
  scale_y_continuous("Graduation Rate", labels=percent) +
  scale_fill_manual(values = c("lightblue", "red")) +
  Sp.Theme(legend.position=c(.8,.8)) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.text = element_text(size=15), 
        axis.text.x = element_text(size=12))
Demo.Grad.plot
ggsave(Demo.Grad.plot, filename="Demo.Grad.plot.png", width = 5, height=5, dpi=200)




Model.GD.Log.DemoCouns <- glm(Graduate ~ Pscore100 + Counseling +
                                   Race.Bl + Counseling:Race.Bl +
                                   Race.La + Counseling:Race.La +
                                   Female + Counseling:Female,
                                 data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.DemoCouns)
#Counseling:Race.Bl    0.51243    1.43113   0.358  0.72030   
#Counseling:Race.La   -0.41530    1.56102  -0.266  0.79021   
#Counseling:Female    13.49971 1297.40653   0.010  0.99170   

#No counseling effect


#Education Variables
Model.GD.Log.Edu<- glm(Graduate ~ Pscore100 +  
                          SpecialEd + 
                          Number.of.Exemptions + 
                          FarClose,
                        data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.Edu)
#Compute odds ratios and 95% CIs
exp(cbind(OR = coef(Model.GD.Log.Edu), confint(Model.GD.Log.Edu)))

#                     Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)          -5.83188    1.32817  -4.391 0.0000113 ***
# Pscore100             0.01488    0.01560   0.954    0.3400    
# SpecialEd             0.12963    0.86728   0.149    0.8812    
# Number.of.Exemptions  0.33466    0.14251   2.348    0.0189 *  
# FarClose              0.84688    1.01011   0.838    0.4018    

#Exemptions does predict graduation

Model.GD.Log.Edu.plot <- glm(Graduate ~ Pscore100.M +  
                                SpecialEd + Number.of.Exemptions + FarClose,
                              data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.Edu.plot)

PredValues <- data.frame(Pscore100.M=0,
                         SpecialEd = c(0,1,rep(0,6)), 
                         FarClose= c(rep(0,3),1, rep(0,4)),
                         Number.of.Exemptions = c(rep(0,5),5,10,15),
                         Edu.Factor=c(rep("Special Education",2), rep("Far/Close",2), rep("Exemptions",4)),
                         PresAbs=c(rep(c("Absent","Present"),2), c(0,5,10,15)))
PredValues$Edu.Factor <- factor(PredValues$Edu.Factor, levels=c("Exemptions", "Special Education", "Far/Close"))
PredValues$PresAbs <- factor(PredValues$PresAbs, levels=c("0", "5", "10", "15", "Absent", "Present"))
PredValues$pred<- predict(Model.GD.Log.Edu.plot, newdata=PredValues, type="response")

Edu.Grad.plot <- ggplot(data=PredValues, aes(x=Edu.Factor, fill=PresAbs, y=pred)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Graduation Rate by Edugraphics") +
  scale_y_continuous("Graduation Rate", labels=percent) +
  scale_fill_manual(values = c("grey80", "grey60", "grey40", "grey20", "lightblue", "red")) +
  Sp.Theme(legend.position=c(.8,.8)) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.text = element_text(size=15), 
        axis.text.x = element_text(size=12))
Edu.Grad.plot
ggsave(Edu.Grad.plot, filename="Edu.Grad.plot.png", width = 5, height=5, dpi=200)

Model.GD.Log.EduCouns <- glm(Graduate ~ Pscore100 + Counseling +
                                SpecialEd + Counseling:SpecialEd +
                                Number.of.Exemptions + Counseling:Number.of.Exemptions +
                                FarClose + Counseling:FarClose,
                              data=StLvl.NoCurrent, family=binomial())
summary(Model.GD.Log.EduCouns)

#also no evidence for counseling benefit on education outcomes for graduation
# Counseling:SpecialEd               17.189435  3795.482815   0.005    0.9964    
# Counseling:Number.of.Exemptions    -0.337402     0.370111  -0.912    0.3620    
# Counseling:FarClose                33.620798  5083.256706   0.007    0.9947   



#Course Completion and attendance Rates
#Table2
StLvl <- read.csv("C:/Users/sbuja/Documents/Salesforce Data Challenge/cleaned_data-20170810T195849Z-001/cleaned_data/Predictor Variables with Propensity Scores.csv")
Sp.Desc(StLvl)

Table2 <- read.csv("C:/Users/sbuja/Documents/Salesforce Data Challenge/cleaned_data-20170810T195849Z-001/cleaned_data/Table2Summary (1).csv")
Sp.Desc(Table2)


#Merge StLvl with Table2----
#Check for duplicated IDs
StLvl$New.ID[duplicated(StLvl$New.ID) | duplicated(StLvl$New.ID, fromLast=TRUE)]
Table2$New.ID[duplicated(Table2$New.ID) | duplicated(Table2$New.ID, fromLast=TRUE)]

#No duplicates
#Check for non-overlapping New.ID
setdiff(StLvl$New.ID, Table2$New.ID)
#None that are non-overlapping
#"7atyl" "hlhmt" "zuskp"
#7atyl -- Enrolled on 9/8/2015 and discharged 8 days later -- no attendance data
#hlhmt -- Enrolled on 9/8/2015 and discharged 7 days later -- no attendance data
#hlhmt -- Enrolled on 9/8/2015 and discharged 7 days later -- no attendance data

Drop.New.ID <- c("7atyl", "hlhmt", "zuskp")
StLvl.Drop <- subset(StLvl, New.ID %nin% Drop.New.ID)
dim(StLvl.Drop) #should be 386 79
dim(Table2)

setdiff(StLvl.Drop$New.ID, Table2$New.ID)
setdiff(Table2$New.ID, StLvl.Drop$New.ID)
#"m6zdv" in Table2 Attendance data but not StLevel data
Table2.Drop <- subset(Table2, New.ID != "m6zdv")
dim(Table2.Drop) #should be 386

setdiff(StLvl.Drop$New.ID, Table2.Drop$New.ID)
setdiff(Table2.Drop$New.ID, StLvl.Drop$New.ID)
#Now no no matched students


StLvl.Drop <- StLvl.Drop[order(StLvl.Drop$New.ID),]
Table2.Drop <- Table2.Drop[order(Table2.Drop$New.ID),]

StLvl.T2 <- merge(StLvl.Drop, Table2.Drop, by="New.ID")
dim(StLvl.T2)

#Analysis of attendance summary data----
#Variables of interest
# Percentage_Dropclass Percentage_CompletedClass Percentage_DayAttended Percentage_DayAbsent Percentage_BenchmarkEarned

Sp.Hist(StLvl.T2, var="Percentage_Dropclass", bins=50)
table(StLvl.T2$Percentage_Dropclass)  #super ugly distribution

Sp.Hist(StLvl.T2, var="Percentage_CompletedClass")
#How are both the completed class percentages and dropped class percentages crazy high zero inflated?

Sp.Hist(StLvl.T2, var="Percentage_DayAttended") #Looks fine. 

Sp.Hist(StLvl.T2, var="Percentage_DayAbsent") #Looks fine

Sp.Hist(StLvl.T2, var="Percentage_DayTardy") #probably worth sqrt transforming
StLvl.T2$sqrtPercentage_DayTardy <- sqrt(StLvl.T2$Percentage_DayTardy)
Sp.Hist(StLvl.T2, var="sqrtPercentage_DayTardy") 

StLvl.T2$Percentage_WentToClass <- StLvl.T2$Percentage_DayAttended + StLvl.T2$Percentage_DayTardy
Sp.Hist(StLvl.T2, var="Percentage_WentToClass") 
Sp.Desc(StLvl.T2$Percentage_WentToClass)

Sp.Hist(StLvl.T2, var="Percentage_BenchmarkEarned") # quite zero inflated, but workable

Sp.Hist(StLvl.T2, var="Percentage_IncompleteClass")




#Start with Percentage_DayAttended
#couple computations
#multiple pscore *100
StLvl.T2$Pscore100 <- StLvl.T2$Pscore * 100
StLvl.T2$Risk.Others.NoEmp <- ifelse(rowSums(StLvl.T2[c("Risk.Other", "Risk.Disability", "Risk.Parent", "Risk.Caring", "Risk.Food")])>0, 1, 0)


Model.DaysAttend.Risk <- lm(Percentage_DayAttended ~ Pscore100 +  
                           Risk.PhysHealth +
                           Risk.Death + 
                           Risk.Anxiety + 
                           Risk.Depression + 
                           Risk.MentalHealth + 
                           Risk.Abuse + 
                           Risk.Housing +  
                           Risk.Employed +  
                           Risk.Others.NoEmp,
                         data=StLvl.T2)
summary(Model.DaysAttend.Risk)
# (Intercept)        0.307118   0.020295   15.13 < 0.0000000000000002 ***
# Pscore100          0.004316   0.001276    3.38              0.00079 ***
# Risk.PhysHealth    0.000118   0.024203    0.00              0.99613    
# Risk.Death        -0.065421   0.025957   -2.52              0.01214 *  
# Risk.Anxiety      -0.105258   0.041863   -2.51              0.01234 *  
# Risk.Depression   -0.088377   0.041736   -2.12              0.03487 *  
# Risk.MentalHealth -0.027959   0.028065   -1.00              0.31979    
# Risk.Abuse        -0.012462   0.033034   -0.38              0.70620    
# Risk.Housing      -0.014895   0.035003   -0.43              0.67070    
# Risk.Employed      0.033754   0.038981    0.87              0.38709    
# Risk.Others.NoEmp  0.050140   0.058409    0.86              0.39120    

Sp.Hist(data.frame(res=resid(Model.DaysAttend.Risk)), var="res")

Model.DaysAttend.RiskCouns <- lm(Percentage_DayAttended ~ Pscore100 + Counseling +
                              Risk.PhysHealth + Counseling:Risk.PhysHealth +
                              Risk.Death +  Counseling:Risk.Death +
                              Risk.Anxiety +  Counseling:Risk.Anxiety +
                              Risk.Depression +  Counseling:Risk.Depression +
                              Risk.MentalHealth +  Counseling:Risk.MentalHealth +
                              Risk.Abuse +  Counseling:Risk.Abuse +
                              Risk.Housing +   Counseling:Risk.Housing +
                              Risk.Employed +   Counseling:Risk.Employed +
                              Risk.Others.NoEmp + Counseling:Risk.Others.NoEmp,
                            data=StLvl.T2)
summary(Model.DaysAttend.RiskCouns)
# Counseling:Risk.PhysHealth    0.07372    0.05800    1.27              0.2045    
# Counseling:Risk.Death        -0.00371    0.05614   -0.07              0.9474    
# Counseling:Risk.Anxiety       0.05723    0.06329    0.90              0.3664    
# Counseling:Risk.Depression   -0.17300    0.06384   -2.71              0.0070 ** 
# Counseling:Risk.MentalHealth  0.05622    0.05957    0.94              0.3459    
# Counseling:Risk.Abuse        -0.04212    0.06939   -0.61              0.5442    
# Counseling:Risk.Housing       0.05032    0.07386    0.68              0.4961    
# Counseling:Risk.Employed      0.30077    0.13272    2.27              0.0240 *  
# Counseling:Risk.Others.NoEmp  0.16065    0.12029    1.34              0.1825    
Sp.Hist(data.frame(res=resid(Model.DaysAttend.RiskCouns)), var="res")



Model.DaysAttend.Service <- lm(Percentage_DayAttended ~ Pscore100 +  
                                 Service.SNAP +
                                Service.Section8 + 
                                Service.Other,
                            data=StLvl.T2)
summary(Model.DaysAttend.Service)
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.3345133  0.0194980  17.156   <2e-16 ***
# Pscore100         0.0006359  0.0005106   1.245    0.214    
# Service.SNAP     -0.0205250  0.0227183  -0.903    0.367    
# Service.Section8 -0.0169260  0.0242942  -0.697    0.486    
# Service.Other     0.1843255  0.1233643   1.494    0.136  
# 

Model.DaysAttend.ServiceCouns <- lm(Percentage_DayAttended ~ Pscore100 + Counseling +
                                 Service.SNAP + Counseling:Service.SNAP +
                                 Service.Section8 + Counseling:Service.Section8 +
                                 Service.Other + Counseling:Service.Other,
                               data=StLvl.T2)
summary(Model.DaysAttend.ServiceCouns)
#Counseling:Service.SNAP     -0.0350624  0.0530839  -0.661   0.5093    
#Counseling:Service.Section8 -0.0594568  0.0550917  -1.079   0.2812    
#Counseling:Service.Other    -0.0876528  0.2633485  -0.333   0.7394  




Model.DaysAttend.Demo <- lm(Percentage_DayAttended ~ Pscore100 +  
                                 Race.Bl +
                                 Race.La + 
                                 Female,
                               data=StLvl.T2)
summary(Model.DaysAttend.Demo)
# #              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.3355896  0.0273400  12.275   <2e-16 ***
#   Pscore100    0.0005629  0.0005423   1.038   0.3000    
# Race.Bl     -0.0144071  0.0241240  -0.597   0.5507    
# Race.La     -0.0585836  0.0241799  -2.423   0.0159 *  
#   Female       0.0347194  0.0230011   1.509   0.1320    

Model.DaysAttend.DemoCouns <- lm(Percentage_DayAttended ~ Pscore100 + Counseling +
                                      Race.Bl + Counseling:Race.Bl +
                                      Race.La + Counseling:Race.La +
                                      Female + Counseling:Female,
                                    data=StLvl.T2)
summary(Model.DaysAttend.DemoCouns)
#Counseling:Race.Bl -2.761e-02  5.564e-02  -0.496    0.620    
#Counseling:Race.La -3.147e-02  5.513e-02  -0.571    0.568    
#Counseling:Female   5.337e-02  5.512e-02   0.968    0.334   


Model.DaysAttend.Edu <- lm(Percentage_DayAttended ~ Pscore100 +  
                              SpecialEd + 
                              Number.of.Exemptions + 
                              FarClose,
                            data=StLvl.T2)
summary(Model.DaysAttend.Edu)
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           2.599e-01  1.974e-02  13.163  < 2e-16 ***
#   Pscore100            -5.565e-05  5.001e-04  -0.111  0.91146    
# SpecialEd             5.213e-02  2.224e-02   2.343  0.01962 *  
#   Number.of.Exemptions  1.751e-02  5.542e-03   3.159  0.00171 ** 
#   FarClose             -1.345e-02  2.828e-02  -0.476  0.63467    

Model.DaysAttend.EduCouns <- lm(Percentage_DayAttended ~ Pscore100 +  Counseling + 
                             SpecialEd +  Counseling:SpecialEd + 
                             Number.of.Exemptions +  Counseling:Number.of.Exemptions + 
                             FarClose +  Counseling:FarClose,
                           data=StLvl.T2)
summary(Model.DaysAttend.EduCouns)



#Benchmarks Earned
#quasipoisson seems to be better for normalizing ghe 
Model.Benchmarks.Risk <- glm(Percentage_BenchmarkEarned ~ Pscore100 +  
                              Risk.PhysHealth +
                              Risk.Death + 
                              Risk.Anxiety + 
                              Risk.Depression + 
                              Risk.MentalHealth + 
                              Risk.Abuse + 
                              Risk.Housing +  
                              Risk.Employed +  
                              Risk.Others.NoEmp,
                            data=StLvl.T2, family=quasipoisson)
summary(Model.Benchmarks.Risk)
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.38334    0.08623 -16.042  < 2e-16 ***
#   Pscore100          0.02394    0.00525   4.561 6.96e-06 ***
#   Risk.PhysHealth    0.08068    0.09961   0.810  0.41848    
# Risk.Death        -0.35048    0.11524  -3.041  0.00252 ** 
#   Risk.Anxiety      -0.56298    0.18026  -3.123  0.00193 ** 
#   Risk.Depression   -0.50470    0.17630  -2.863  0.00444 ** 
#   Risk.MentalHealth -0.14960    0.12158  -1.230  0.21931    
# Risk.Abuse        -0.21419    0.14508  -1.476  0.14071    
# Risk.Housing      -0.04204    0.15294  -0.275  0.78357    
# Risk.Employed      0.08519    0.18118   0.470  0.63850    
# Risk.Others.NoEmp  0.34998    0.23645   1.480  0.13968   

Sp.Hist(data.frame(res=resid(Model.Benchmarks.Risk)), var="res")

Model.Benchmarks.RiskCouns <- glm(Percentage_BenchmarkEarned ~ Pscore100 + Counseling +
                                   Risk.PhysHealth + Counseling:Risk.PhysHealth +
                                   Risk.Death +  Counseling:Risk.Death +
                                   Risk.Anxiety +  Counseling:Risk.Anxiety +
                                   Risk.Depression +  Counseling:Risk.Depression +
                                   Risk.MentalHealth +  Counseling:Risk.MentalHealth +
                                   Risk.Abuse +  Counseling:Risk.Abuse +
                                   Risk.Housing +   Counseling:Risk.Housing +
                                   Risk.Employed +   Counseling:Risk.Employed +
                                   Risk.Others.NoEmp + Counseling:Risk.Others.NoEmp,
                                 data=StLvl.T2, family=quasipoisson)
summary(Model.Benchmarks.RiskCouns)
#Counseling:Risk.PhysHealth    0.198752   0.226196   0.879  0.38017    
# Counseling:Risk.Death        -0.070573   0.241552  -0.292  0.77033    
# Counseling:Risk.Anxiety       0.228754   0.257460   0.889  0.37487    
# Counseling:Risk.Depression   -0.512565   0.255319  -2.008  0.04544 *  
#   Counseling:Risk.MentalHealth  0.126016   0.244434   0.516  0.60649    
# Counseling:Risk.Abuse         0.063394   0.296942   0.213  0.83107    
# Counseling:Risk.Housing       0.173539   0.310515   0.559  0.57660    
# Counseling:Risk.Employed     -0.105889   0.631632  -0.168  0.86696    
# Counseling:Risk.Others.NoEmp  0.686964   0.468007   1.468  0.14302 
Sp.Hist(data.frame(res=resid(Model.Benchmarks.RiskCouns)), var="res")

summary(glm(Percentage_BenchmarkEarned ~ Pscore100 + Counseling,
                                  data=StLvl.T2, family=quasipoisson))




Model.Benchmarks.Service <- glm(Percentage_BenchmarkEarned ~ Pscore100 +  
                                 Service.SNAP +
                                 Service.Section8 + 
                                 Service.Other,
                               data=StLvl.T2, family=quasipoisson)
summary(Model.Benchmarks.Service)
#(Intercept)      -1.246847   0.083005 -15.021   <2e-16 ***
# Pscore100         0.003767   0.002069   1.821   0.0694 .  
# Service.SNAP     -0.122275   0.097695  -1.252   0.2115    
# Service.Section8  0.050580   0.104107   0.486   0.6274    
# Service.Other     0.406663   0.411407   0.988   0.3236 
Sp.Hist(data.frame(res=resid(Model.Benchmarks.Service)), var="res")

Model.Benchmarks.ServiceCouns <- glm(Percentage_BenchmarkEarned ~ Pscore100 + Counseling +
                                      Service.SNAP + Counseling:Service.SNAP +
                                      Service.Section8 + Counseling:Service.Section8 +
                                      Service.Other + Counseling:Service.Other,
                                    data=StLvl.T2, family=quasipoisson)
summary(Model.Benchmarks.ServiceCouns)
#Counseling:Service.SNAP     -0.012553   0.217803  -0.058   0.9541    
#Counseling:Service.Section8 -0.388558   0.231857  -1.676   0.0946 .  
#Counseling:Service.Other    -0.721171   0.851091  -0.847   0.3973 




Model.Benchmarks.Demo <- glm(Percentage_BenchmarkEarned ~ Pscore100 +  
                              Race.Bl +
                              Race.La + 
                              Female,
                            data=StLvl.T2, family=quasipoisson)
summary(Model.Benchmarks.Demo)
#(Intercept) -1.217957   0.115449 -10.550  < 2e-16 ***
# Pscore100    0.002092   0.002164   0.967 0.334135    
# Race.Bl     -0.223556   0.099145  -2.255 0.024721 *  
#   Race.La     -0.238635   0.102074  -2.338 0.019921 *  
#   Female       0.335927   0.099588   3.373 0.000821 ***

Model.Benchmarks.DemoCouns <- glm(Percentage_BenchmarkEarned ~ Pscore100 + Counseling +
                                   Race.Bl + Counseling:Race.Bl +
                                   Race.La + Counseling:Race.La +
                                   Female + Counseling:Female,
                                 data=StLvl.T2, family=quasipoisson)
summary(Model.Benchmarks.DemoCouns)
#Counseling:Race.Bl -7.479e-02  2.196e-01  -0.341  0.73362    
# Counseling:Race.La  3.366e-02  2.223e-01   0.151  0.87971    
# Counseling:Female   2.004e-01  2.402e-01   0.834  0.40458    


Model.Benchmarks.Edu <- glm(Percentage_BenchmarkEarned ~ Pscore100 +  
                             SpecialEd + 
                             Number.of.Exemptions + 
                             FarClose,
                           data=StLvl.T2, family=quasipoisson)
summary(Model.Benchmarks.Edu)
#(Intercept)          -1.564956   0.089473 -17.491   <2e-16 ***
# Pscore100             0.001462   0.002042   0.716   0.4744    
# SpecialEd             0.067956   0.094644   0.718   0.4732    
# Number.of.Exemptions  0.042358   0.021093   2.008   0.0453 *  
#   FarClose              0.111833   0.117220   0.954   0.3407    

Model.Benchmarks.EduCouns <- glm(Percentage_BenchmarkEarned ~ Pscore100 +  Counseling + 
                                  SpecialEd +  Counseling:SpecialEd + 
                                  Number.of.Exemptions +  Counseling:Number.of.Exemptions + 
                                  FarClose +  Counseling:FarClose,
                                data=StLvl.T2, family=quasipoisson)
summary(Model.Benchmarks.EduCouns)
#Counseling:SpecialEd             4.847e-02  2.059e-01   0.235   0.8140    
# Counseling:Number.of.Exemptions -4.304e-02  4.411e-02  -0.976   0.3298    
# Counseling:FarClose              2.706e-01  2.594e-01   1.043   0.2975    

write.csv

#Went to class
Model.WentToClass.Risk <- lm(Percentage_WentToClass ~ Pscore100 +  
                               Risk.PhysHealth +
                               Risk.Death + 
                               Risk.Anxiety + 
                               Risk.Depression + 
                               Risk.MentalHealth + 
                               Risk.Abuse + 
                               Risk.Housing +  
                               Risk.Employed +  
                               Risk.Others.NoEmp,
                             data=StLvl.T2)
summary(Model.WentToClass.Risk)
#(Intercept)        0.371804   0.022319  16.659  < 2e-16 ***
# Pscore100          0.004101   0.001403   2.923  0.00368 ** 
#   Risk.PhysHealth   -0.007785   0.026617  -0.292  0.77009    
# Risk.Death        -0.063509   0.028546  -2.225  0.02669 *  
#   Risk.Anxiety      -0.104598   0.046039  -2.272  0.02365 *  
#   Risk.Depression   -0.085378   0.045899  -1.860  0.06365 .  
# Risk.MentalHealth -0.034640   0.030865  -1.122  0.26245    
# Risk.Abuse        -0.019950   0.036329  -0.549  0.58322    
# Risk.Housing      -0.024386   0.038495  -0.633  0.52680    
# Risk.Employed      0.011941   0.042870   0.279  0.78075    
# Risk.Others.NoEmp  0.050509   0.064235   0.786  0.43218  
Sp.Hist(data.frame(res=resid(Model.WentToClass.Risk)), var="res")

StLvl.T2$Pscore100 <- StLvl.T2$Pscore*100
StLvl.T2$Pscore100.M <- StLvl.T2$Pscore100 - mean(StLvl.T2$Pscore100)
Model.GD.Risk.plot <- lm(Percentage_WentToClass ~ Pscore100.M +  
                                Risk.PhysHealth +
                                Risk.Death + 
                                Risk.Anxiety + 
                                Risk.Depression + 
                                Risk.MentalHealth + 
                                Risk.Abuse + 
                                Risk.Housing +  
                                Risk.Employed +  
                                Risk.Others.NoEmp,
                              data=StLvl.T2)
summary(Model.GD.Risk.plot)

PredValues <- data.frame(Pscore100.M=0,
                         Risk.PhysHealth = c(0,1,rep(0,16)),
                         Risk.Death = c(rep(0,3),1,rep(0,14)), 
                         Risk.Anxiety = c(rep(0,5),1,rep(0,12)), 
                         Risk.Depression = c(rep(0,7),1,rep(0,10)), 
                         Risk.MentalHealth = c(rep(0,9),1,rep(0,8)), 
                         Risk.Abuse = c(rep(0,11),1,rep(0,6)), 
                         Risk.Housing = c(rep(0,13),1,rep(0,4)),  
                         Risk.Employed = c(rep(0,15),1,rep(0,2)),  
                         Risk.Others.NoEmp = c(rep(0,17),1),
                         Risk.Factor=c(rep("Physical Health",2), rep("Recent Death",2), rep("Anxiety",2), rep("Depression",2),
                                       rep("Other MH",2), rep("Abuse",2), rep("Housing",2), rep("Employed",2),
                                       rep("Other",2)),
                         PresAbs=rep(c("Absent","Present"),9))
PredValues$Risk.Factor <- factor(PredValues$Risk.Factor, levels=c("Anxiety", "Recent Death", "Depression",  "Other MH", "Other",
                                                                   "Housing", "Abuse", "Physical Health", "Employed"))
PredValues$pred<- predict(Model.GD.Risk.plot, newdata=PredValues, type="response")

Risk.WentToClass.plot <- ggplot(data=PredValues, aes(x=Risk.Factor, fill=PresAbs, y=pred)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Percent Went to Class by Risk Factors") +
  scale_y_continuous("Percent Went to Class (Attend + Tardy)", labels=percent) +
  scale_fill_manual(values = c("lightblue", "red")) +
  Sp.Theme(legend.position=c(.826,.4)) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.text = element_text(size=15), 
        axis.text.x = element_text(size=12))
Risk.WentToClass.plot
ggsave(Risk.WentToClass.plot, filename="Risk.WentToClass.plot.png", width = 10.4, height=5, dpi=200)


Model.WentToClass.RiskCouns <- lm(Percentage_WentToClass ~ Pscore100 + Counseling +
                                    Risk.PhysHealth + Counseling:Risk.PhysHealth +
                                    Risk.Death +  Counseling:Risk.Death +
                                    Risk.Anxiety +  Counseling:Risk.Anxiety +
                                    Risk.Depression +  Counseling:Risk.Depression +
                                    Risk.MentalHealth +  Counseling:Risk.MentalHealth +
                                    Risk.Abuse +  Counseling:Risk.Abuse +
                                    Risk.Housing +   Counseling:Risk.Housing +
                                    Risk.Employed +   Counseling:Risk.Employed +
                                    Risk.Others.NoEmp + Counseling:Risk.Others.NoEmp,
                                  data=StLvl.T2)
summary(Model.WentToClass.RiskCouns)

Model.WentToClass.RiskCouns.plot <- lm(Percentage_WentToClass ~ Pscore100.M + Counseling +
                                    Risk.PhysHealth + Counseling:Risk.PhysHealth +
                                    Risk.Death +  Counseling:Risk.Death +
                                    Risk.Anxiety +  Counseling:Risk.Anxiety +
                                    Risk.Depression +  Counseling:Risk.Depression +
                                    Risk.MentalHealth +  Counseling:Risk.MentalHealth +
                                    Risk.Abuse +  Counseling:Risk.Abuse +
                                    Risk.Housing +   Counseling:Risk.Housing +
                                    Risk.Employed +   Counseling:Risk.Employed +
                                    Risk.Others.NoEmp + Counseling:Risk.Others.NoEmp,
                                  data=StLvl.T2)
summary(Model.WentToClass.RiskCouns.plot)

PredValues0 <- data.frame(Pscore100.M=0,
                         Risk.PhysHealth = c(0,1,rep(0,16)),
                         Risk.Death = c(rep(0,3),1,rep(0,14)), 
                         Risk.Anxiety = c(rep(0,5),1,rep(0,12)), 
                         Risk.Depression = c(rep(0,7),1,rep(0,10)), 
                         Risk.MentalHealth = c(rep(0,9),1,rep(0,8)), 
                         Risk.Abuse = c(rep(0,11),1,rep(0,6)), 
                         Risk.Housing = c(rep(0,13),1,rep(0,4)),  
                         Risk.Employed = c(rep(0,15),1,rep(0,2)),  
                         Risk.Others.NoEmp = c(rep(0,17),1),
                         Risk.Factor=c(rep("Physical Health",2), rep("Recent Death",2), rep("Anxiety",2), rep("Depression",2),
                                       rep("Other MH",2), rep("Abuse",2), rep("Housing",2), rep("Employed",2),
                                       rep("Other",2)),
                         PresAbs=rep(c("Absent","Present"),9),
                         Counseling=0)
PredValues1 <- PredValues0
PredValues1$Counseling <- 1
PredValues <- rbind(PredValues0, PredValues1)
PredValues$Couns <- ifelse(PredValues$Counseling==0,"No Counseling", "Counseling")
PredValues$Couns <- factor(PredValues$Couns, levels=c("No Counseling", "Counseling"))
PredValues$Risk.Factor <- factor(PredValues$Risk.Factor, levels=c("Employed", "Depression", "Anxiety", "Recent Death", "Other MH", 
                                                                  "Other", "Housing", "Abuse", "Physical Health"))
PredValues$pred<- predict(Model.WentToClass.RiskCouns.plot, newdata=PredValues)

PredValues <- PredValues[order(PredValues$Risk.Factor),]
#Counseling <- Counseling[order(Counseling$New.ID),]

PredValues <- PredValues[1:16,]


Risk.WentToClass.Couns.plot <- ggplot(data=PredValues, aes(x=PresAbs, fill=Couns, y=pred)) +
  facet_grid(.~Risk.Factor) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Percent Went to Class by Risk Factors") +
  scale_y_continuous("Percent Went to Class (Attend + Tardy)", labels=percent) +
  scale_fill_manual(values = c("green3", "green4")) +
  Sp.Theme(legend.position=c(.8,.9)) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.text = element_text(size=15), 
        axis.text.x = element_text(size=12, angle = 90, hjust = 1),
        panel.background = element_rect(colour = "black", fill=NA),
        strip.text.x=element_text(size=11, face="bold"), strip.background = element_blank())
Risk.WentToClass.Couns.plot
ggsave(Risk.WentToClass.Couns.plot, filename="Risk.WentToClass.Couns.plot.png", width = 7, height=5, dpi=200)



Model.WentToClass.Service <- lm(Percentage_WentToClass ~ Pscore100 +  
                                  Service.SNAP +
                                  Service.Section8 + 
                                  Service.Other,
                                data=StLvl.T2)
summary(Model.WentToClass.Service)
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.3975356  0.0214002  18.576   <2e-16 ***
#   Pscore100         0.0004207  0.0005604   0.751    0.453    
# Service.SNAP     -0.0273551  0.0249347  -1.097    0.273    
# Service.Section8 -0.0225094  0.0266643  -0.844    0.399    
# Service.Other     0.1934088  0.1353997   1.428    0.154

Model.WentToClass.ServiceCouns <- lm(Percentage_WentToClass ~ Pscore100 + Counseling +
                                       Service.SNAP + Counseling:Service.SNAP +
                                       Service.Section8 + Counseling:Service.Section8 +
                                       Service.Other + Counseling:Service.Other,
                                     data=StLvl.T2)
summary(Model.WentToClass.ServiceCouns)
#Counseling:Service.SNAP     -3.794e-02  5.829e-02  -0.651   0.5155    
#Counseling:Service.Section8 -6.431e-02  6.050e-02  -1.063   0.2885    
#Counseling:Service.Other    -8.510e-02  2.892e-01  -0.294   0.7687   


Model.WentToClass.Demo <- lm(Percentage_WentToClass ~ Pscore100 +  
                               Race.Bl +
                               Race.La + 
                               Female,
                             data=StLvl.T2)
summary(Model.WentToClass.Demo)
#(Intercept)  0.3968234  0.0299819  13.235  < 2e-16 ***
# Pscore100    0.0004216  0.0005947   0.709  0.47884    
# Race.Bl     -0.0100861  0.0264551  -0.381  0.70323    
# Race.La     -0.0729743  0.0265164  -2.752  0.00621 ** 
#   Female       0.0313027  0.0252238   1.241  0.21537  

Model.GD.Demo.plot <- lm(Percentage_WentToClass ~ Pscore100.M +  
                           Race.Bl +
                           Race.La + 
                           Female,
                         data=StLvl.T2)
summary(Model.GD.Demo.plot)

PredValues <- data.frame(Pscore100.M=0,
                         Race.Bl = c(0,1,rep(0,4)),
                         Race.La = c(rep(0,3),1,rep(0,2)), 
                         Female= c(rep(0,5),1),
                         Demo.Factor=c(rep("Black",2), rep("Latino",2), rep("Female",2)),
                         PresAbs=rep(c("Absent","Present"),3))
PredValues$Demo.Factor <- factor(PredValues$Demo.Factor, levels=c("Latino", "Female", "Black"))
PredValues$pred<- predict(Model.GD.Demo.plot, newdata=PredValues)

Demo.WentToClass.plot <- ggplot(data=PredValues, aes(x=Demo.Factor, fill=PresAbs, y=pred)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Percent Went to Class by Demographics") +
  scale_y_continuous("Percentage Went to Class", labels=percent, limits=c(0,.5)) +
  scale_fill_manual(values = c("lightblue", "red")) +
  Sp.Theme(legend.position=c(.8,.9)) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.text = element_text(size=15), 
        axis.text.x = element_text(size=12))
Demo.WentToClass.plot
ggsave(Demo.WentToClass.plot, filename="Demo.WentToClass.plot.png", width = 7, height=5, dpi=200)


Model.WentToClass.DemoCouns <- lm(Percentage_WentToClass ~ Pscore100 + Counseling +
                                    Race.Bl + Counseling:Race.Bl +
                                    Race.La + Counseling:Race.La +
                                    Female + Counseling:Female,
                                  data=StLvl.T2)
summary(Model.WentToClass.DemoCouns)
#

Model.WentToClass.Edu <- lm(Percentage_WentToClass ~ Pscore100 +  
                              SpecialEd + 
                              Number.of.Exemptions + 
                              FarClose,
                            data=StLvl.T2)
summary(Model.WentToClass.Edu)
#(Intercept)           0.3165728  0.0218866  14.464  < 2e-16 ***
# Pscore100            -0.0003184  0.0005544  -0.574  0.56608    
# SpecialEd             0.0667475  0.0246557   2.707  0.00709 ** 
#   Number.of.Exemptions  0.0181134  0.0061432   2.949  0.00339 ** 
#   FarClose             -0.0192608  0.0313523  -0.614  0.53936    
# ---

Model.WentToClass.Edu.plot <- lm(Percentage_WentToClass ~ Pscore100.M +  
                                    SpecialEd + Number.of.Exemptions,
                                  data=StLvl.T2)
summary(Model.WentToClass.Edu.plot)

PredValues <- data.frame(Pscore100.M=0,
                         SpecialEd = c(0,1,rep(0,4)), 
                         Number.of.Exemptions = c(rep(0,3),5,10,15),
                         Edu.Factor=c(rep("Special Education",2), rep("Exemptions",4)),
                         PresAbs=c("Absent","Present", 0,5,10,15))
PredValues$Edu.Factor <- factor(PredValues$Edu.Factor, levels=c("Exemptions", "Special Education"))
PredValues$PresAbs <- factor(PredValues$PresAbs, levels=c("0", "5", "10", "15", "Absent", "Present"))
PredValues$pred<- predict(Model.WentToClass.Edu.plot, newdata=PredValues)

Edu.WentToClass.plot <- ggplot(data=PredValues, aes(x=Edu.Factor, fill=PresAbs, y=pred)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Percentage Went to Class by Educational Factors") +
  scale_y_continuous("Percentage Went to Class", labels=percent) +
  scale_fill_manual(values = c("grey80", "grey60", "grey40", "grey20", "lightblue", "red")) +
  Sp.Theme(legend.position="right") +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.text = element_text(size=15), 
        axis.text.x = element_text(size=12))
Edu.WentToClass.plot
setwd("C:/Users/sbuja/Documents/Salesforce Data Challenge/")
ggsave(Edu.WentToClass.plot, filename="Edu.WentToClass.plot.png", width = 7, height=5, dpi=200)

#Sample for interaction

PredValues <- PredValues[1:4,]
PredValues$Edu.Factor <- c("Not Depressed", "Not Depressed", "Depressed", "Depressed") 
PredValues$Edu.Factor <- factor(PredValues$Edu.Factor, levels=c("Not Depressed", "Depressed"))
PredValues$PresAbs <- c("No Counseling", "Counseling", "No Counseling", "Counseling") 
PredValues$PresAbs <- factor(PredValues$PresAbs, levels=c("No Counseling", "Counseling"))


Interaction.sample <- ggplot(data=PredValues, aes(x=Edu.Factor, fill=PresAbs, y=pred)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Percentage Went to Class by Educational Factors") +
  scale_y_continuous("Percentage Went to Class", labels=percent) +
  scale_fill_manual(values = c("green3", "green4")) +
  Sp.Theme(legend.position=c(.8,.9)) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.text = element_text(size=15), 
        axis.text.x = element_text(size=12))
Interaction.sample
ggsave(Interaction.sample, filename="Interaction.sample.png", width = 7, height=5, dpi=200)

Model.WentToClass.EduCouns <- lm(Percentage_WentToClass ~ Pscore100 +  Counseling + 
                                   SpecialEd +  Counseling:SpecialEd + 
                                   Number.of.Exemptions +  Counseling:Number.of.Exemptions + 
                                   FarClose +  Counseling:FarClose,
                                 data=StLvl.T2)
summary(Model.WentToClass.EduCouns)
#Counseling:SpecialEd            -0.0121706  0.0564468  -0.216  0.82941    
# Counseling:Number.of.Exemptions -0.0069904  0.0134002  -0.522  0.60221    
# Counseling:FarClose              0.0816059  0.0729987   1.118  0.26432  


#Competencies

#Competent Plus
Sp.Hist(StLvl.T2, var="Percentage_BasicCompetentClass")
Sp.Hist(StLvl.T2, var="Percentage_CompetentClass")
Sp.Hist(StLvl.T2, var="Percentage_HighlyCompetentClass")
StLvl.T2$Percentage_BasicCompetentPlus <- rowSums(StLvl.T2[c("Percentage_BasicCompetentClass",    "Percentage_CompetentClass",
                                                             "Percentage_HighlyCompetentClass")])
Sp.Desc(StLvl.T2$Percentage_BasicCompetentPlus)
Sp.Hist(StLvl.T2, var="Percentage_BasicCompetentPlus")

#Incompetent
Sp.Hist(StLvl.T2, var="Percentage_IncompleteClass")

Sp.Hist(StLvl.T2, var="Percentage_ExemptClass")
Sp.Hist(StLvl.T2, var="Percentage_InProgressClass")
Sp.Hist(StLvl.T2, var="Percentage_NotRequiredClass")

Sp.Hist(StLvl.T2, var="Percentage_UnratedClass")


StLvl.T2$CompetenciesSum <- rowSums(StLvl.T2[c("Percentage_CompetentClass",    "Percentage_ExemptClass",
                                               "Percentage_HighlyCompetentClass",    "Percentage_InProgressClass",
                                               "Percentage_IncompleteClass",    "Percentage_NotRequiredClass",
                                               "Percentage_UnratedClass")])
Sp.Hist(StLvl.T2, var="CompetenciesSum")



#testing some 70-30 cross validation
#This is incredibly unstable with such small graduation rates
#demonstrate with simulation
sim<- 5000
AUC.Graduation.Risk <- data.frame(McFadden=rep(NA,sim), AUC=rep(NA,sim))
for(i in 1:sim){
  print(noquote(paste("iteration: ",i)))
  #Split into training and test datasets
  OneGrad<-F
  while(!OneGrad){
    Train <- createDataPartition(StLvl.NoCurrent$Graduate, p=0.7, list=FALSE)
    StLvl.NoCurrent.Train <- StLvl.NoCurrent[Train,]
    StLvl.NoCurrent.Test <- StLvl.NoCurrent[-Train,]
    
    if(sum(StLvl.NoCurrent.Test$Graduate)>=1) OneGrad<-T
  }
  
  # dim(StLvl.NoCurrent.Train)
  # dim(StLvl.NoCurrent.Test)
  # 
  # table(StLvl.NoCurrent.Train$Graduate)
  # table(StLvl.NoCurrent.Test$Graduate)
  
  #Logistic regression model with training data
  Model.GD.Log.Risk <- glm(Graduate ~ Pscore100 +
                             Risk.PhysHealth +
                             Risk.Death +
                             Risk.Anxiety +
                             Risk.Depression +
                             Risk.MentalHealth +
                             Risk.Abuse +
                             Risk.Housing +
                             Risk.Employed +
                             Risk.Others.NoEmp,
                           data=StLvl.NoCurrent.Train, family=binomial())
  #summary(Model.GD.Log.Risk)
  # pR2(Model.GD.Log.Risk)
  # rocplot(Model.GD.Log.Risk)
  
  AUC.Graduation.Risk$McFadden[i] <- as.double(pR2(Model.GD.Log.Risk)["McFadden"])
  
  # Pred.GD.Log.Risk <- predict(Model.GD.Log.Risk, data=StLvl.NoCurrent.Test, type="response")
  # confusionMatrix(data=Pred.GD.Log.Risk, StLvl.NoCurrent.Test$Graduate)
  #Doesn't work
  
  prob <- predict(Model.GD.Log.Risk, newdata=StLvl.NoCurrent.Test, type="response")
  pred <- prediction(prob, StLvl.NoCurrent.Test$Graduate)
  # perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  # plot(perf)
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  AUC.Graduation.Risk$AUC[i] <- auc
}

Sp.Desc(AUC.Graduation.Risk)
Sp.Hist(AUC.Graduation.Risk, var="AUC")
Sp.Hist(AUC.Graduation.Risk, var="McFadden")

sim <- 5000
AUC.Graduation.Service <- data.frame(McFadden=rep(NA,sim), AUC=rep(NA,sim))
for(i in 1:sim){
  print(noquote(paste("iteration: ",i)))
  #Split into training and test datasets
  OneGrad<-F
  while(!OneGrad){
    Train <- createDataPartition(StLvl.NoCurrent$Graduate, p=0.7, list=FALSE)
    StLvl.NoCurrent.Train <- StLvl.NoCurrent[Train,]
    StLvl.NoCurrent.Test <- StLvl.NoCurrent[-Train,]
    
    if(sum(StLvl.NoCurrent.Test$Graduate)>=1) OneGrad<-T
  }
  
  
  # dim(StLvl.NoCurrent.Train)
  # dim(StLvl.NoCurrent.Test)
  # 
  # table(StLvl.NoCurrent.Train$Graduate)
  # table(StLvl.NoCurrent.Test$Graduate)
  
  #Logistic regression model with training data
  Model.GD.Log.Service <- glm(Graduate ~ Pscore100 +
                             Service.SNAP +
                             Service.Section8 +
                             Service.Others,
                           data=StLvl.NoCurrent.Train, family=binomial())
  #summary(Model.GD.Log.Service)
  # pR2(Model.GD.Log.Service)
  # rocplot(Model.GD.Log.Service)
  
  AUC.Graduation.Service$McFadden[i] <- as.double(pR2(Model.GD.Log.Service)["McFadden"])
  
  # Pred.GD.Log.Service <- predict(Model.GD.Log.Service, data=StLvl.NoCurrent.Test, type="response")
  # confusionMatrix(data=Pred.GD.Log.Service, StLvl.NoCurrent.Test$Graduate)
  #Doesn't work
  
  prob <- predict(Model.GD.Log.Service, newdata=StLvl.NoCurrent.Test, type="response")
  pred <- prediction(prob, StLvl.NoCurrent.Test$Graduate)
  # perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  # plot(perf)
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  AUC.Graduation.Service$AUC[i] <- auc
}

Sp.Desc(AUC.Graduation.Service)
Sp.Hist(AUC.Graduation.Service, var="AUC")
Sp.Hist(AUC.Graduation.Service, var="McFadden")

sim<- 5000
AUC.Graduation.Demo <- data.frame(McFadden=rep(NA,sim), AUC=rep(NA,sim))
for(i in 1:sim){
  print(noquote(paste("iteration: ",i)))
  #Split into training and test datasets
  OneGrad<-F
  while(!OneGrad){
    Train <- createDataPartition(StLvl.NoCurrent$Graduate, p=0.7, list=FALSE)
    StLvl.NoCurrent.Train <- StLvl.NoCurrent[Train,]
    StLvl.NoCurrent.Test <- StLvl.NoCurrent[-Train,]
    
    if(sum(StLvl.NoCurrent.Test$Graduate)>=1) OneGrad<-T
  }
  
  
  # dim(StLvl.NoCurrent.Train)
  # dim(StLvl.NoCurrent.Test)
  # 
  # table(StLvl.NoCurrent.Train$Graduate)
  # table(StLvl.NoCurrent.Test$Graduate)
  
  #Logistic regression model with training data
  Model.GD.Log.Demo <- glm(Graduate ~ Pscore100 +
                             Race.Bl +
                             Race.La +
                             Female,
                           data=StLvl.NoCurrent.Train, family=binomial())
  #summary(Model.GD.Log.Demo)
  # pR2(Model.GD.Log.Demo)
  # rocplot(Model.GD.Log.Demo)
  
  AUC.Graduation.Demo$McFadden[i] <- as.double(pR2(Model.GD.Log.Demo)["McFadden"])
  
  # Pred.GD.Log.Demo <- predict(Model.GD.Log.Demo, data=StLvl.NoCurrent.Test, type="response")
  # confusionMatrix(data=Pred.GD.Log.Demo, StLvl.NoCurrent.Test$Graduate)
  #Doesn't work
  
  prob <- predict(Model.GD.Log.Demo, newdata=StLvl.NoCurrent.Test, type="response")
  pred <- prediction(prob, StLvl.NoCurrent.Test$Graduate)
  # perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  # plot(perf)
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  AUC.Graduation.Demo$AUC[i] <- auc
}

Sp.Desc(AUC.Graduation.Demo)
Sp.Hist(AUC.Graduation.Demo, var="AUC")
Sp.Hist(AUC.Graduation.Demo, var="McFadden")

sim<- 5000
AUC.Graduation.Edu <- data.frame(McFadden=rep(NA,sim), AUC=rep(NA,sim))
for(i in 1:sim){
  print(noquote(paste("iteration: ",i)))
  #Split into training and test datasets
  OneGrad<-F
  while(!OneGrad){
    Train <- createDataPartition(StLvl.NoCurrent$Graduate, p=0.7, list=FALSE)
    StLvl.NoCurrent.Train <- StLvl.NoCurrent[Train,]
    StLvl.NoCurrent.Test <- StLvl.NoCurrent[-Train,]
    
    if(sum(StLvl.NoCurrent.Test$Graduate)>=1) OneGrad<-T
  }
  
  
  # dim(StLvl.NoCurrent.Train)
  # dim(StLvl.NoCurrent.Test)
  # 
  # table(StLvl.NoCurrent.Train$Graduate)
  # table(StLvl.NoCurrent.Test$Graduate)
  
  #Logistic regression model with training data
  Model.GD.Log.Edu <- glm(Graduate ~ Pscore100 +
                             SpecialEd +
                             Number.of.Exemptions +
                             FarClose,
                           data=StLvl.NoCurrent.Train, family=binomial())
  #summary(Model.GD.Log.Edu)
  # pR2(Model.GD.Log.Edu)
  # rocplot(Model.GD.Log.Edu)
  
  AUC.Graduation.Edu$McFadden[i] <- as.double(pR2(Model.GD.Log.Edu)["McFadden"])
  
  # Pred.GD.Log.Edu <- predict(Model.GD.Log.Edu, data=StLvl.NoCurrent.Test, type="response")
  # confusionMatrix(data=Pred.GD.Log.Edu, StLvl.NoCurrent.Test$Graduate)
  #Doesn't work
  
  prob <- predict(Model.GD.Log.Edu, newdata=StLvl.NoCurrent.Test, type="response")
  pred <- prediction(prob, StLvl.NoCurrent.Test$Graduate)
  # perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  # plot(perf)
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  AUC.Graduation.Edu$AUC[i] <- auc
}

Sp.Desc(AUC.Graduation.Edu)
Sp.Hist(AUC.Graduation.Edu, var="AUC")
Sp.Hist(AUC.Graduation.Edu, var="McFadden")


#Works fine, but seems to be highly variable -- see simulation above
Train <- createDataPartition(StLvl.NoCurrent$Graduate, p=0.7, list=FALSE)
StLvl.NoCurrent.Train <- StLvl.NoCurrent[Train,]
StLvl.NoCurrent.Test <- StLvl.NoCurrent[-Train,]

dim(StLvl.NoCurrent.Train)
dim(StLvl.NoCurrent.Test)

table(StLvl.NoCurrent.Train$Graduate)
table(StLvl.NoCurrent.Test$Graduate)

#Logistic regression model with training data
Model.GD.Log.Risk <- glm(Graduate ~ Pscore100 +
                           Risk.PhysHealth +
                           Risk.Death +
                           Risk.Anxiety +
                           Risk.Depression +
                           Risk.MentalHealth +
                           Risk.Abuse +
                           Risk.Housing +
                           Risk.Employed +
                           Risk.Others.NoEmp,
                         data=StLvl.NoCurrent.Train, family=binomial())
summary(Model.GD.Log.Risk)
pR2(Model.GD.Log.Risk)
rocplot(Model.GD.Log.Risk)

# Pred.GD.Log.Risk <- predict(Model.GD.Log.Risk, data=StLvl.NoCurrent.Test, type="response")
# confusionMatrix(data=Pred.GD.Log.Risk, StLvl.NoCurrent.Test$Graduate)
#Doesn't work

prob <- predict(Model.GD.Log.Risk, newdata=StLvl.NoCurrent.Test, type="response")
pred <- prediction(prob, StLvl.NoCurrent.Test$Graduate)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# ctrl <- trainControl(method = "repeatedcv", number = 5, savePredictions = TRUE)
# 
# mod_fit <- train(as.factor(Graduate) ~ Pscore100 +  
#                    Risk.PhysHealth +
#                    Risk.Death + 
#                    Risk.Anxiety + 
#                    Risk.Depression + 
#                    Risk.MentalHealth + 
#                    Risk.Abuse + 
#                    Risk.Housing +  
#                    Risk.Employed +  
#                    Risk.Others.NoEmp,
#                  data=StLvl.NoCurrent, method="glm", family="binomial",
#                  trControl = ctrl)
# 
# pred = predict(mod_fit, newdata=testing)
# confusionMatrix(data=pred, testing$Class)

