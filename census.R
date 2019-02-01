#Census 

#Data PreProcessing

data=read.table("census-income.data", dec=",",sep=",")
temp=read.table("census-income.data", dec=",",sep=",")

colnames(data)=c('AAGE','ACLSWKR','ADTIND','ADTOCC','AHGA','AHRSPAY',
                 'AHSCOL','AMARITL','AMJIND','AMJOCC','ARACE','AREORGN',
                 'ASEX','AUNMEM','AUNTYPE','AWKSTAT','CAPGAIN','CAPLOSS',
                 'DIVVAL','FILESTAT','GRINREG','GRINST','HHDFMX',
                 'HHDREL','MARSUPWT','MIGMTR1','MIGMTR3','MIGMTR4','MIGSAME',
                 'MIGSUN','NOEMP','PEARNVAL','PEFNTVTY','PEMNTVTY',
                 'PENATVTY','PRCITSHP','PTOTVAL','SEOTR','TAXINC','VETQVA',
                 'VETYN','WKSWORK')


#Data Cleaning

data$ACLSWKR=factor(data$ACLSWKR)
data$Federal_Government=ifelse(data$ACLSWKR==" Federal government",1,0)
data$Local_Government=ifelse(data$ACLSWKR==" Local government",1,0)
data$Never_Worked=ifelse(data$ACLSWKR==" Never worked",1,0)
data$Not_in_universe=ifelse(data$ACLSWKR==" Not in universe",1,0)
data$Private=ifelse(data$ACLSWKR==" Private",1,0)
data$State_government=ifelse(data$ACLSWKR==" State government",1,0)
data$Without_pay=ifelse(data$ACLSWKR==" Without pay",1,0)
data$Self_employed_incorporated=ifelse(data$ACLSWKR==" Self-employed-incorporated" ,1,0)
data$ACLSWKR=NULL



data$AHGA=factor(data$AHGA)
data$School_Grade=factor(data$AHGA,levels=c(" Less than 1st grade"," 1st 2nd 3rd or 4th grade"," 5th or 6th grade"," 7th and 8th grade",
                                            " 9th grade"," 10th grade"," 11th grade"," 12th grade no diploma"),labels = c(1,2,3,4,5,6,7,8))
data$School_Grade=ifelse(is.na(data$School_Grade),0,data$School_Grade)
data$School_Grade=factor(data$School_Grade)
data$High_School=factor(data$AHGA,levels=c(" High school graduate"," Some college but no degree" ),labels=c(1,2))
data$High_School=ifelse(is.na(data$High_School),0,data$High_School)
data$High_School=factor(data$High_School)
data$College=factor(data$AHGA,levels=c(" Bachelors degree(BA AB BS)"," Associates degree-academic program",
                                       " Associates degree-occup /vocational"),labels = c(1,2,3))
data$College=ifelse(is.na(data$College),0,data$College)
data$College=factor(data$College)
data$Masters_Phd=factor(data$AHGA,levels=c(" Masters degree(MA MS MEng MEd MSW MBA)"," Prof school degree (MD DDS DVM LLB JD)",
                                           " Doctorate degree(PhD EdD)"),labels=c(1,2,3))
data$Masters_Phd=ifelse(is.na(data$Masters_Phd),0,data$Masters_Phd)
data$Masters_Phd=factor(data$Masters_Phd)
data$AHGA=NULL



data$AHSCOL=factor(data$AHSCOL,levels=c(" College or university"," High school"," Not in universe"),labels=c(1,2,3))
data$Married=factor(data$AMARITL,levels=c( " Married-A F spouse present"," Married-civilian spouse present"," Married-spouse absent"),
                    labels=c(1,2,3))
data$Married=ifelse(is.na(data$Married),0,1)
data$Widowed=ifelse(data$AMARITL==" Widowed",1,0)
data$AMARITL=NULL


data$Agriculture=ifelse(data$AMJIND==" Agriculture",1,0)
data$Defence=ifelse(data$AMJIND==" Armed  Forces",1,0)
data$Business=ifelse(data$AMJIND==" Business and repair services",1,0)
data$Communications=ifelse(data$AMJIND==" Communications",1,0)
data$Construction=ifelse(data$AMJIND==" Construction",1,0)
data$Education=ifelse(data$AMJIND==" Education",1,0)
data$Entertainment=ifelse(data$AMJIND==" Entertainment",1,0)
data$Agriculture=ifelse(data$AMJIND==" Finance insurance and real estate",1,0)
data$Forest=ifelse(data$AMJIND==" Forestry and fisheries",1,0)
data$Hospital=ifelse(data$AMJIND==" Hospital services",1,0)
data$Manufacturing=ifelse(data$AMJIND==" Manufacturing-durable goods",1,0)
data$Medical=ifelse(data$AMJIND==" Medical except hospital",1,0)
data$Mining=ifelse(data$AMJIND==" Mining",1,0)
data$Minar=ifelse(data$AMJIND==" Not in universe or children",1,0)
data$Other_Services=ifelse(data$AMJIND==" Other professional services",1,0)
data$Personal_Services=ifelse(data$AMJIND==" Personal services except private HH",1,0)
data$Private_Services=ifelse(data$AMJIND==" Private household services",1,0)
data$Administration=ifelse(data$AMJIND==" Public administration",1,0)
data$Retail=ifelse(data$AMJIND==" Retail trade",1,0)
data$Social_Services=ifelse(data$AMJIND==" Social services",1,0)
data$Transportation=ifelse(data$AMJIND==" Transportation",1,0)
data$Sanitary_Services=ifelse(data$AMJIND==" Utility and sanitary services",1,0)
data$Trade=ifelse(data$AMJIND==" Wholesale trade",1,0)
data$AMJIND=NULL


length(levels(data$AMJOCC))
data$AMJOCC=factor(data$AMJOCC,levels=c(" Adm support including clerical",
                                        " Armed Forces"," Executive admin and managerial",
                                        " Farming forestry and fishing"," Handlers equip cleaners etc ",
                                        " Machine operators assmblrs & inspctrs"," Not in universe",
                                        " Other service"," Precision production craft & repair",
                                        " Private household services"," Professional specialty",
                                        " Protective services"," Sales"," Technicians and related support",
                                        " Transportation and material moving"),labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))



data$Mace_Asian=ifelse(data$ARACE==" Asian or Pacific Islander",1,0)
data$Mace_White=ifelse(data$ARACE==" White",1,0)
data$Mace_Black=ifelse(data$ARACE==" Black",1,0)
data$Mace_Indian=ifelse(data$ARACE==" Amer Indian Aleut or Eskimo",1,0)
data$ARACE=NULL

levels(data$AREORGN)
data$Hispanic_American=ifelse(data$AREORGN==" Central or South American",1,0)
data$Hispanic_Chicano=ifelse(data$AREORGN==" Chicano",1,0)
data$Hispanic_Cuban=ifelse(data$AREORGN==" Cuban",1,0)
data$Hispanic_Unknown=ifelse(data$AREORGN==" Do not know",1,0)
data$Hispanic_MexicanAmerican=ifelse(data$AREORGN==" Mexican-American",1,0)
data$Hispanic_Mexican=ifelse(data$AREORGN==" Mexican (Mexicano)",1,0)
data$Hispanic_NA=ifelse(data$AREORGN==" NA",1,0)
data$Hispanic_Spanish=ifelse(data$AREORGN==" Other Spanish",1,0)
data$Hispanic_PuertoRican=ifelse(data$AREORGN==" Puerto Rican",1,0)
data$AREORGN=NULL



data$ASEX=factor(data$ASEX,levels=c(" Female"," Male"),labels=c(1,2))



data$AUNMEM=ifelse(data$AUNMEM==" Yes",1,0)
data$AUNMEM=factor(data$AUNMEM)



data$Unemployeed_JobLeaver=ifelse(data$AUNTYPE==" Job leaver",1,0)
data$Unemployeed_JobLoser=ifelse(data$AUNTYPE==" Job loser - on layoff",1,0)
data$Unemployeed_Entrant=ifelse(data$AUNTYPE==" New entrant"||data$AUNTYPE==" Re-entrant",1,0)
data$AUNTYPE=NULL


levels(data$AWKSTAT)

data$AWKSTAT=factor(data$AWKSTAT,levels=c(" Children or Armed Forces"," Not in labor force",
                                          " Unemployed full-time"," Unemployed part- time",
                                          " PT for econ reasons usually FT"," PT for econ reasons usually PT",
                                          " PT for non-econ reasons usually FT"," Full-time schedules" ),labels=c(1,2,3,4,5,6,7,8))




data$FILESTAT=NULL


data$Previous_Residence_Abroad=ifelse(data$GRINREG==" Abroad",1,0)
data$Previous_Residence_Midwest=ifelse(data$GRINREG==" Midwest",1,0)
data$Previous_Residence_Northeast=ifelse(data$GRINREG==" Northeast",1,0)
data$Previous_Residence_South=ifelse(data$GRINREG==" South",1,0)
data$Previous_Residence_West=ifelse(data$GRINREG==" West",1,0)
data$GRINREG=NULL



data$GRINST=NULL


levels(data$HHDFMX)
data$Children_Less_18=factor(data$HHDFMX,levels=c(" Child <18 ever marr not in subfamily",           
                            " Child <18 ever marr RP of subfamily",
                            " Child <18 never marr not in subfamily",          
                            " Child <18 never marr RP of subfamily",           
                            " Child <18 spouse of subfamily RP"),labels = c(1,2,3,4,5))

data$Children_Less_18=ifelse(is.na(data$Children_Less_18),0,1)
data$Childre_Less_18=NULL

levels(data$HHDFMX)
data$Children_Above_18=factor(data$HHDFMX,levels=c(" Child 18+ ever marr Not in a subfamily",         
                                                   " Child 18+ ever marr RP of subfamily",            
                                                   " Child 18+ never marr Not in a subfamily",        
                                                   " Child 18+ never marr RP of subfamily",           
                                                   " Child 18+ spouse of subfamily RP"),labels = c(1,2,3,4,5))

data$Children_Above_18=ifelse(is.na(data$Children_Above_18),0,1)


data$GrandChildren_Less_18=factor(data$HHDFMX,levels=c(" Grandchild <18 ever marr not in subfamily",      
                                                       " Grandchild <18 never marr child of subfamily RP",
                                                       " Grandchild <18 never marr not in subfamily",     
                                                       " Grandchild <18 never marr RP of subfamily"),labels = c(1,2,3,4))

data$GrandChildren_Less_18=ifelse(is.na(data$GrandChildren_Less_18),0,1)


data$GrandChildren_Above_18=factor(data$HHDFMX,levels=c(" Grandchild 18+ ever marr not in subfamily",      
                                                       " Grandchild 18+ never marr child of subfamily RP",
                                                       " Grandchild 18+ never marr not in subfamily",     
                                                       " Grandchild 18+ never marr RP of subfamily"),labels = c(1,2,3,4))

data$GrandChildren_Above_18=ifelse(is.na(data$GrandChildren_Above_18),0,1)


data$Rel_Less_18=factor(data$HHDFMX,levels=c(" Other Rel <18 ever marr not in subfamily",       
                                                       " Other Rel <18 ever marr RP of subfamily",        
                                                       " Other Rel <18 never marr child of subfamily RP", 
                                                       " Other Rel <18 never marr not in subfamily",      
                                                       " Other Rel <18 never married RP of subfamily",    
                                                       " Other Rel <18 spouse of subfamily RP"),labels = c(1,2,3,4,5,6))

data$Rel_Less_18=ifelse(is.na(data$Rel_Less_18),0,1)


data$Rel_Above_18=factor(data$HHDFMX,levels=c(" Other Rel 18+ ever marr not in subfamily",       
                                             " Other Rel 18+ ever marr RP of subfamily",        
                                             " Other Rel 18+ never marr child of subfamily RP", 
                                             " Other Rel 18+ never marr not in subfamily",      
                                             " Other Rel 18+ never married RP of subfamily",    
                                             " Other Rel 18+ spouse of subfamily RP"),labels = c(1,2,3,4,5,6))

data$Rel_Above_18=ifelse(is.na(data$Rel_Above_18),0,1)
data$HHDFMX=NULL




data$HHDREL=NULL


levels(data$MIGMTR1)
data$Migration_Aborad_To_MSA=ifelse(data$MIGMTR1==" Abroad to MSA",1,0)
data$Migration_Aborad_To_NON_MSA=ifelse(data$MIGMTR1==" Abroad to nonMSA",1,0)
data$Migration_MSA_To_MSA=ifelse(data$MIGMTR1==" MSA to MSA",1,0)
data$Migration_MSA_To_NON_MSA=ifelse(data$MIGMTR1==" MSA to nonMSA",1,0)
data$Migration_Non_Mover=ifelse(data$MIGMTR1==" Nonmover",1,0)
data$Migration_Non_MSA_To_MSA=ifelse(data$MIGMTR1==" NonMSA to MSA",1,0)
data$Migration_Non_MSA_To_NON_MSA=ifelse(data$MIGMTR1==" NonMSA to nonMSA",1,0)
data$MIGMTR1=NULL



data$Migration_Aborad=ifelse(data$MIGMTR3==" Abroad",1,0)
data$Migration_Different_Country_Same_State=ifelse(data$MIGMTR3==" Different country same state",1,0)
data$Migration_Different_Division_Same_Region=ifelse(data$MIGMTR3==" Different division same region",1,0)
data$Migration_Different_Region=ifelse(data$MIGMTR3==" Different region",1,0)
data$Migration_Different_State_Same_Region=ifelse(data$MIGMTR3==" Different state same division",1,0)
data$Migration_Same_County=ifelse(data$MIGMTR3==" Same county",1,0)
data$MIGMTR3=NULL



data$Migration_Different_State_In_Midwest=ifelse(data$MIGMTR4==" Different state in Midwest",1,0)
data$Migration_Different_State_In_Northwest=ifelse(data$MIGMTR4==" Different state in Northweast",1,0)
data$Migration_Different_State_In_South=ifelse(data$MIGMTR4==" Different state in South",1,0)
data$Migration_Different_State_In_West=ifelse(data$MIGMTR4==" Different state in West",1,0)
data$MIGMTR4=NULL




data$MIGSAME=ifelse(data$MIGSAME==" Yes",1,0)


data$MIGSUN=ifelse(data$MIGSUN==" Yes",1,0)

levels(data$PEARNVAL)
data$Both_Parent_Present=ifelse(data$PEARNVAL==" Both parents present",1,0)
data$Father_Only_Present=ifelse(data$PEARNVAL==" Father only present",1,0)
data$Mother_Only_Present=ifelse(data$PEARNVAL==" Mother only present",1,0)
data$PEARNVAL=NULL


levels(data$PEFNTVTY)
data$PEFNTVTY=NULL
data$PEMNTVTY=NULL
data$PENATVTY=NULL

levels(data$PRCITSHP)
data$PRCITSHP=factor(data$PRCITSHP,levels=c(" Foreign born- Not a citizen of U S ",        
                                             " Foreign born- U S citizen by naturalization",
                                             " Native- Born abroad of American Parent(s)",  
                                             " Native- Born in Puerto Rico or U S Outlying",
                                             " Native- Born in the United States"),labels=c(1,2,3,4,5))          

data$PRCITSHP=as.numeric(as.character(data$PRCITSHP))
data$PRCITSHP=ifelse(data$PRCITSHP>2,1,0)
data$PRCITSHP=factor(data$PRCITSHP)


data$SEOTR=ifelse(data$SEOTR==" Yes",1,0)



data$Worked_More_Than_50000_Weeks=ifelse(data$WKSWORK==" 50000+.",1,0)
data$Worked_Less_Than_50000_Weeks=ifelse(data$WKSWORK==" - 50000.",1,0)
data$WKSWORK=NULL


colnames(data)[1]="Age"
colnames(data)[2]="Industry_Code"
colnames(data)[3]="Occupation_Code"
colnames(data)[5]="Enrolled_In_Education_Last_Week"
colnames(data)[6]="Major_Indusrty_Code"
colnames(data)[7]="Sex"
colnames(data)[4]="Wage_Per_Hour"
colnames(data)[8]="Member_Of_Labor_Union"
colnames(data)[9]="Full_Part_Time_Employement_Stat"
colnames(data)[10]="Capital_Gain"
colnames(data)[11]="Capital_Loss"
colnames(data)[12]="Divedends_From_Stock"
colnames(data)[13]="Instance_Weight"
colnames(data)[14]="Live_In_This_House_1_Year_Ago"
colnames(data)[15]="Migration_Previous_In_Sunbelt"
colnames(data)[16]="Number_Of_Persons_Worked_Employeer"
colnames(data)[17]="Citzenship"
colnames(data)[18]="Total_Person_Income"
colnames(data)[19]="Own_Business_Or_Self_Employeed"
colnames(data)[20]="Taxable_Income_Amount"
colnames(data)[21]="Fill_Inc_Questionnaire_For_Veteran's_Admin"
colnames(data)[22]="Veterans_Benefits"


for(i in 1:107){
  data[,i]=as.double(data[,i])
}



# Using the elbow method to find the optimal number of clusters

set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(data, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')


# Fitting K-Means to the dataset

set.seed(29)
kmeans = kmeans(x = data, centers = 6)
y_kmeans = kmeans$cluster




#Writing Of data
df=data.frame(y_kmeans)
df$Age=data$Age
write.csv(df,"census_plot.csv")



