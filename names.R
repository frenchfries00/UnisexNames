#We shall fill the data frame using the National data from https://www.ssa.gov/oact/babynames/limits.html
library(ggplot2)
library(maps)
library(openintro)
OurData=data.frame(Year=1880:2014,TotalNames=NA,TotalBabies=NA,TotalGirls=NA,TotalBoys=NA,
                   UnisexNames=NA,UnisexGirls=NA,UnisexBoys=NA,TrueUnisexNames=NA,TrueUGirls=NA,
                   TrueUBoys=NA,FrequentUName=NA)

for (yr in 1880:2014){
  YearNames<-read.table(paste0("AllNames/yob",as.character(yr),".txt"),sep=",")
  names(YearNames)<-c("FirstName","Gender","Babies")
  YearNames$FirstName<-as.character(YearNames$FirstName)
  GirlsNames<-YearNames$FirstName[YearNames$Gender=="F"]
  BoysNames<-YearNames$FirstName[YearNames$Gender=="M"]
  UnisexNames<-data.frame(FirstName=intersect(GirlsNames, BoysNames))
  for (i in 1:nrow(UnisexNames)){
    UnisexNames$Girls[i]<-YearNames$Babies[YearNames$FirstName==UnisexNames$FirstName[i]&YearNames$Gender=="F"]
    UnisexNames$Boys[i]<-YearNames$Babies[YearNames$FirstName==UnisexNames$FirstName[i]&YearNames$Gender=="M"]
  }
  UnisexNames$Ratio<-UnisexNames$Boys/UnisexNames$Girls
  UnisexNames<-UnisexNames[UnisexNames$Ratio>0.1&UnisexNames$Ratio<10,]
  UnisexNames$FirstName<-as.character(UnisexNames$FirstName)
  TrueUnisexNames<-UnisexNames[UnisexNames$Girls+UnisexNames$Boys>100,]
  print(yr)
  print(head(UnisexNames))
  c<-yr-1879
  OurData$TotalNames[c]<-nrow(YearNames)
  OurData$TotalBabies[c]<-sum(YearNames$Babies)
  OurData$TotalGirls[c]<-sum(YearNames$Babies[YearNames$Gender=="F"])
  OurData$TotalBoys[c]<-sum(YearNames$Babies[YearNames$Gender=="M"])
  OurData$UnisexNames[c]<-nrow(UnisexNames)
  OurData$UnisexGirls[c]<-sum(UnisexNames$Girls)
  OurData$UnisexBoys[c]<-sum(UnisexNames$Boys)
  OurData$TrueUnisexNames[c]<-nrow(TrueUnisexNames)
  OurData$TrueUGirls[c]<-sum(TrueUnisexNames$Girls)
  OurData$TrueUBoys[c]<-sum(TrueUnisexNames$Boys)
  OurData$FrequentUName[c]<-TrueUnisexNames$FirstName[which.max(TrueUnisexNames$Girls+TrueUnisexNames$Boys)]
}
UNames<-UnisexNames$FirstName
write.csv(OurData,"UnisexNames.csv")

png(filename = "UnisexYear.png", width = 800, height = 480, units = "px")
p<-ggplot(OurData, aes(Year)) + 
  labs(title = "People with gender-neutral first names")+labs(y="Percentage")+
  geom_line(aes(y = UnisexGirls/TotalGirls, colour = "Girls"))+
  geom_line(aes(y = UnisexBoys/TotalBoys, colour = "Boys"))+
  geom_line(aes(y = (UnisexGirls+UnisexBoys)/TotalBabies, colour = "Total"),size=2 )+ 
  scale_colour_manual(values = c("green","blue", "red"))+
  theme(text = element_text(size=20))+
  theme(legend.title=element_blank())
print(p)
dev.off()


DataByState=data.frame(State=c(state.abb,"DC"),TotalNames=0,TotalBabies=0,
                       TotalGirls=0,TotalBoys=0,
                   UnisexNames=0,UnisexGirls=0,UnisexBoys=0,
                   TrueUnisexNames=0,TrueUGirls=0,
                   TrueUBoys=0,FrequentUName=NA)


for (st in c(state.abb,"DC")){
  StateNames<-read.table(paste0("NamesByState/",as.character(st),".TXT"),sep=",")
  names(StateNames)<-c("State","Gender","Year","FirstName","Babies")
  StateNames<-StateNames[StateNames$Year==2014,]
  StateNames$FirstName<-as.character(StateNames$FirstName)
  GirlsNames<-StateNames$FirstName[StateNames$Gender=="F"&StateNames$Year==2014]
  BoysNames<-StateNames$FirstName[StateNames$Gender=="M" &StateNames$Year==2014]
  UnisexNames<-data.frame(FirstName=UNames,Girls=0,Boys=0)
  for (i in 1:nrow(UnisexNames)){
    #print(UnisexNames$FirstName[i])
    if (sum(StateNames$Babies[StateNames$FirstName==UnisexNames$FirstName[i]&StateNames$Gender=="F"])>0)
        UnisexNames$Girls[i]<-StateNames$Babies[StateNames$FirstName==UnisexNames$FirstName[i]&StateNames$Gender=="F"]
    if (sum(StateNames$Babies[StateNames$FirstName==UnisexNames$FirstName[i]&StateNames$Gender=="M"])>0)
        UnisexNames$Boys[i]<-StateNames$Babies[StateNames$FirstName==UnisexNames$FirstName[i]&StateNames$Gender=="M"]
  }
  UnisexNames$Ratio<-UnisexNames$Boys/UnisexNames$Girls
  #UnisexNames<-UnisexNames[UnisexNames$Ratio>0.1&UnisexNames$Ratio<10,]
  UnisexNames$FirstName<-as.character(UnisexNames$FirstName)
  TrueUnisexNames<-UnisexNames[UnisexNames$Girls+UnisexNames$Boys>100,]
  print(st)
  print(head(UnisexNames))
  c<-which(DataByState$State==st)
  DataByState$TotalNames[c]<-nrow(StateNames)
  DataByState$TotalBabies[c]<-sum(StateNames$Babies)
  DataByState$TotalGirls[c]<-sum(StateNames$Babies[StateNames$Gender=="F"])
  DataByState$TotalBoys[c]<-sum(StateNames$Babies[StateNames$Gender=="M"])
  DataByState$UnisexNames[c]<-nrow(UnisexNames)
  DataByState$UnisexGirls[c]<-sum(UnisexNames$Girls)
  DataByState$UnisexBoys[c]<-sum(UnisexNames$Boys)
  DataByState$TrueUnisexNames[c]<-nrow(TrueUnisexNames)
  DataByState$TrueUGirls[c]<-sum(TrueUnisexNames$Girls)
  DataByState$TrueUBoys[c]<-sum(TrueUnisexNames$Boys)
  DataByState$FrequentUName[c]<-UnisexNames$FirstName[which.max(UnisexNames$Girls+UnisexNames$Boys)]
}  
DataByState$UnisexPercentage<-(DataByState$UnisexBoys+DataByState$UnisexGirls)/DataByState$TotalBabies
DataByState$region<-tolower(abbr2state(DataByState$State))

states_map <- map_data("state")
png(filename = "UnisexMap.png", width = 800, height = 480, units = "px")
p<-ggplot(DataByState, aes(map_id = region)) + 
  geom_map(aes(fill = UnisexPercentage), map = states_map) +
  scale_fill_gradientn(colours=c("blue","yellow")) + 
  expand_limits(x = states_map$long, y = states_map$lat)
print(p)
dev.off()
