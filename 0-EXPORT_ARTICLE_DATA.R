# library(jsonlite)
# 
# fullDf=fromJSON("D:/MTG/Meta analysis/mtgo_data_2021_02_28.json")[[1]]
# View(fullDf)
# fullDf[1,]$Mainboard[[1]][1]

#REMEMBER 
# TO
#  PULL
#   THE
#    DECKLISTS

#1st file to execute
#Contains the parameters of the programs, such as the files to be used, the
#period to cover, the type of events, how to aggregate results in graphics...

###############################################################################
#PARAMETERS - CHANGE THEM TO ANALYSE DIFFERENT DATA SETS AND ARCHETYPES, OR
#TUNE THE METRICS. YOU CAN ALSO UPDATE THEM IN THE CONSOLE AFTER RUNNING THE
#PROGRAM, BEFORE USING THE FUNCTIONS AT THE END TO GENERATE GRAPHS AND RESULTS

#Directory of the file
#Requires restarting Data Treatment if updated
DirectoryFile="D:/MTG/Meta analysis/r_mtgo_modern_analysis/MTGO_Data"

#Name of the file
#Requires restarting Data Treatment if updated
RawFile="mtgo_data_2021_02_28.csv"

#Path of the repertory containing the multiple subdirectories of the results
#converted to JSON
#Repertory of the data imported from: https://github.com/Badaro/MTGODecklistCache 
#MTGODataPath="D:/MTG/Meta analysis/ManaTradersDecklistCache/Tournaments"
MTGODataPath="D:/MTG/Meta analysis/MTGODecklistCache/Tournaments"

#Format of the wanted events: NA if all
#NA,"Sealed","Standard","Pioneer","Modern","Legacy", "Vintage", "Pauper
MTGFormat="Modern"

#Earliest date - if NA, starts from the beginning of the data
#Requires restarting Data Treatment if updated
Beginning="2021-02-17"
#If you want to know the minimum date in the data, use:
#min(rawData$DATE)
#after you executed the 2nd file paragraph

#Latest date - if NA, goes up to the end of the data
#Requires restarting Data Treatment if updated
#End="2021-01-18"
End="2021-03-01"

#If you want to know the maximum date in the data, use:
#max(rawData$DATE)
#after you executed the 2nd file paragraph

#Event type:
#"Official Competitions" = Major Official Events + Prelims
#"Challenges" = Challenges
#"Preliminaries" = Prelims
#"All Events" = everything available including leagues
#"Everything but Leagues" = Major Events, official or unofficial, and Prelims
#"Major Events Top32" = Major Events, official or unofficial, only top32
#"ManaTraders Series" = ManaTraders Series
#"NRG Series" = NRG Series
#"Major Official Events" = Challenge,Champ,Showcase,Premier,Qualifier,MOCS
#Requires restarting Data Treatment if updated
EventType="Official Competitions"

#Type of deck classification - "Super" or "Exact"
#Requires restarting Data Treatment if updated
Classification="Exact"

#Required metagame share to appear on pie chart (numeric, gets converted to %)
PieShare=2

#Required metagame share to appear on histogram (numeric, gets converted to %)
HistShare=2

#WEIGHT OF THE METRICS POINTS FOR THE COMPILATION REQUIRED FOR RANKING
Presence_Weight=1

#WEIGHT OF THE METRICS AVERAGE FOR THE COMPILATION REQUIRED FOR RANKING
PPR_Weight=1

#CODE OF THE LAST SET
lastSetCode="KHM"

###############################################################################

#2nd file to execute
#Imports all the archetypes that appeared in MTGO results in a specific period
#on specific events, and add another layer of archetypes if needed

#Execute this file whenever you update a parameter that tells you to do so

#DATA SHARED BY PHELPS-SAN @ TRON DISCORD, HIS WORK CAN BE FOUND HERE:
#https://github.com/Badaro/MTGODecklistCache
#https://github.com/Badaro/MTGOArchetypeParser

#IMPORT DATA
setwd(DirectoryFile)
rawData=read.csv(RawFile,sep=",",header=T)
rawData=subset(rawData,select = -12)

#NAMES AND DATE DON'T ALLOW THE IDENTIFICATION OF AN EVENT, BUT THE COMBINATION
#OF BOTH CAN, HENCE THE ADDITION OF ANOTHER COLUMN FOR THIS IDENTIFICATION
event_names=rep(NA,length(rawData$EVENT))
for (i in 1:length(rawData$EVENT)){
  event_names[i]=paste(rawData$EVENT[i],as.character(rawData$DATE[i]),sep=" ")
}
rawData$EVENT_NAME=event_names
#View(rawData) 

if(is.na(Beginning)){
  Beginning=min(rawData$DATE)
}
if(is.na(End)){
  End=max(rawData$DATE)
}

#View(periodData)
#length(periodData$PLAYER)

#install.packages("readr")
library(readr)

generate_Prelim_Data = function(periodData) {
  
  #COLLECT PRELIMINARIES ONLY FOR SPECIFIC TREATMENT
  PrelimData=periodData[grep("Preliminary", periodData$EVENT), ]
  PrelimData$POINTS=as.numeric(PrelimData$POINTS)
  #View(PrelimData) 
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE PRELIMINARIES
  if(length(PrelimData$EVENT)>=1){
    for (i in 1:length(PrelimData)){
      if (PrelimData$RESULT[i]=="5-0" | PrelimData$RESULT[i]=="4-1" | 
          PrelimData$RESULT[i]=="3-2"){
        #before prelim structure changes
        PrelimData$NB_ROUNDS[i]=5
      }else if (PrelimData$RESULT[i]=="4-0" | PrelimData$RESULT[i]=="3-1"){
        #after prelim structure changes
        PrelimData$NB_ROUNDS[i]=4
      }
    }
  }
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN PRELIMINARIES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  PrelimData$NB_DEFEATS=PrelimData$NB_ROUNDS - PrelimData$POINTS/3
  
  #ADD TOP8 COLUMNS FOR MERGE WITH CHALLENGES
  PrelimData$TOP8_PTS=rep(0,length(PrelimData$POINTS))
  PrelimData$TOP8_DEF=rep(0,length(PrelimData$NB_DEFEATS))
  PrelimData$TOP8_MATCHES=rep(0,length(PrelimData$NB_ROUNDS))
  
  return(PrelimData)
  
}

generate_Challenge_Data = function(periodData) {
  
  #COLLECT CHALLENGES ONLY FOR SPECIFIC TREATMENT
  challEvents = "Challenge"
  ChallData=periodData[grep(paste(challEvents,collapse="|"), periodData$EVENT), ]
  ChallData$POINTS=as.numeric(ChallData$POINTS)
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE CHALLENGES
  #DIVIDE THE MAXIMUM NUMBER OF POINTS IN SWISS TO GET THE RESULT
  #IF MORE THAN 1 PLAYER HAS THE MAXIMUM OF POINTS, THEN IT IS LIKELY THAT 
  #THERE IS NOT ANY PLAYER AT X-0, SO YOU ADD 1 TO THE RESULT
  listEventsChall=unique(ChallData$EVENT_NAME)
  nbRoundsVec=c()
  if(length(listEventsChall)>=1){
    for (i in 1:length(listEventsChall)){
      periodChallEventData=subset(ChallData, EVENT_NAME == listEventsChall[i])
      maxPoints=max(periodChallEventData$POINTS)
      nbPlayMaxPts=length(which(periodChallEventData$POINTS == maxPoints))
      if(nbPlayMaxPts==1){
        nbRounds=maxPoints/3
      }else{
        nbRounds=1+maxPoints/3
      }
      nbRoundsEvent=rep(nbRounds,length(periodChallEventData$EVENT))
      nbRoundsVec=c(nbRoundsVec,nbRoundsEvent)
    }
  }
  ChallData$NB_ROUNDS=nbRoundsVec
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN CHALLENGES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  ChallData$NB_DEFEATS=ChallData$NB_ROUNDS - ChallData$POINTS/3
  
  #ADD TOP8 POINTS: 3*3 to 1st, 3*2 to 2nd, 3*1 to 3rd and 4th, none to others
  ChallData$TOP8_PTS=rep(0,length(ChallData$POINTS))
  if(length(listEventsChall)>=1){
    for (i in 1:length(ChallData$RESULT)){
      if (ChallData$RESULT[i] == "1st Place"){
        ChallData$TOP8_PTS[i] = 9
      }else if (ChallData$RESULT[i] == "2nd Place"){
        ChallData$TOP8_PTS[i] = 6
      }else if (ChallData$RESULT[i] == "3rd Place" | 
                ChallData$RESULT[i] == "4th Place"){
        ChallData$TOP8_PTS[i] = 3
      }
    }
  }
  
  #ADD TOP8 DEFEATS: 0 FOR THE WINNER, 1 FOR THE OTHERS
  ChallData$TOP8_DEF=ChallData$NB_DEFEATS
  if(length(listEventsChall)>=1){
    for (i in 1:length(ChallData$TOP8_DEF)){
      if (ChallData$RESULT[i] == "2nd Place" | ChallData$RESULT[i] == "3rd Place"| 
          ChallData$RESULT[i] == "4th Place" | ChallData$RESULT[i] == "5th Place"| 
          ChallData$RESULT[i] == "6th Place" | ChallData$RESULT[i] == "7th Place"| 
          ChallData$RESULT[i] == "8th Place"){
        ChallData$TOP8_DEF[i] = 1 + ChallData$NB_DEFEATS[i]
      }
    }
  }
  
  if(length(listEventsChall)>=1){
    ChallData$TOP8_MATCHES=rep(0,length(ChallData$TOP8_PTS))
    for (i in 1:length(ChallData$TOP8_MATCHES)){
      if (ChallData$RESULT[i] == "1st Place" | 
          ChallData$RESULT[i] == "2nd Place"){
        ChallData$TOP8_MATCHES[i] = 3
      }else if (ChallData$RESULT[i] == "3rd Place" | 
                ChallData$RESULT[i] == "4th Place"){
        ChallData$TOP8_MATCHES[i] = 2 
      }else if (ChallData$RESULT[i] == "5th Place"| 
                ChallData$RESULT[i] == "6th Place" | 
                ChallData$RESULT[i] == "7th Place"| 
                ChallData$RESULT[i] == "8th Place"){
        ChallData$TOP8_MATCHES[i] = 1 
      }
    }
  }
  
  #REMOVE ALL THE RESULTS WITH 3 DEFEATS (BECAUSE WE ONLY HAVE PART OF THEM, 
  #WHEREAS WE HAVE ALL THE X-0, X-1 AND X-2 RESULTS)
  #ChallDataWO3Def=ChallData[ChallData$NB_DEFEATS != 3, ]
  #NO NEED FOR THAT FEATURE ANYMORE
  
  return(ChallData)
  
}

generate_Major_Data = function(periodData) {
  
  #COLLECT CHALLENGES ONLY FOR SPECIFIC TREATMENT
  MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
  MajData=periodData[grep(paste(MajEvents,collapse="|"), periodData$EVENT), ]
  MajData$POINTS=as.numeric(MajData$POINTS)
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE CHALLENGES
  #DIVIDE THE MAXIMUM NUMBER OF POINTS IN SWISS TO GET THE RESULT
  #IF MORE THAN 1 PLAYER HAS THE MAXIMUM OF POINTS, THEN IT IS LIKELY THAT 
  #THERE IS NOT ANY PLAYER AT X-0, SO YOU ADD 1 TO THE RESULT
  listEventsChall=unique(MajData$EVENT_NAME)
  nbRoundsVec=c()
  if(length(listEventsChall)>=1){
    for (i in 1:length(listEventsChall)){
      periodChallEventData=subset(MajData, EVENT_NAME == listEventsChall[i])
      maxPoints=max(periodChallEventData$POINTS)
      nbPlayMaxPts=length(which(periodChallEventData$POINTS == maxPoints))
      if(nbPlayMaxPts==1){
        nbRounds=maxPoints/3
      }else{
        nbRounds=1+maxPoints/3
      }
      nbRoundsEvent=rep(nbRounds,length(periodChallEventData$EVENT))
      nbRoundsVec=c(nbRoundsVec,nbRoundsEvent)
    }
  }
  MajData$NB_ROUNDS=nbRoundsVec
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN CHALLENGES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  MajData$NB_DEFEATS=MajData$NB_ROUNDS - MajData$POINTS/3
  
  #ADD TOP8 POINTS: 3*3 to 1st, 3*2 to 2nd, 3*1 to 3rd and 4th, none to others
  MajData$TOP8_PTS=rep(0,length(MajData$POINTS))
  if(length(listEventsChall)>=1){
    for (i in 1:length(MajData$RESULT)){
      if (MajData$RESULT[i] == "1st Place"){
        MajData$TOP8_PTS[i] = 9
      }else if (MajData$RESULT[i] == "2nd Place"){
        MajData$TOP8_PTS[i] = 6
      }else if (MajData$RESULT[i] == "3rd Place" | 
                MajData$RESULT[i] == "4th Place"){
        MajData$TOP8_PTS[i] = 3
      }
    }
  }
  
  #ADD TOP8 DEFEATS: 0 FOR THE WINNER, 1 FOR THE OTHERS
  MajData$TOP8_DEF=MajData$NB_DEFEATS
  if(length(listEventsChall)>=1){
    for (i in 1:length(MajData$TOP8_DEF)){
      if (MajData$RESULT[i] == "2nd Place" | MajData$RESULT[i] == "3rd Place"| 
          MajData$RESULT[i] == "4th Place" | MajData$RESULT[i] == "5th Place"| 
          MajData$RESULT[i] == "6th Place" | MajData$RESULT[i] == "7th Place"| 
          MajData$RESULT[i] == "8th Place"){
        MajData$TOP8_DEF[i] = 1 + MajData$NB_DEFEATS[i]
      }
    }
  }
  
  if(length(listEventsChall)>=1){
    MajData$TOP8_MATCHES=rep(0,length(MajData$TOP8_PTS))
    for (i in 1:length(MajData$TOP8_MATCHES)){
      if (MajData$RESULT[i] == "1st Place" | 
          MajData$RESULT[i] == "2nd Place"){
        MajData$TOP8_MATCHES[i] = 3
      }else if (MajData$RESULT[i] == "3rd Place" | 
                MajData$RESULT[i] == "4th Place"){
        MajData$TOP8_MATCHES[i] = 2 
      }else if (MajData$RESULT[i] == "5th Place"| 
                MajData$RESULT[i] == "6th Place" | 
                MajData$RESULT[i] == "7th Place"| 
                MajData$RESULT[i] == "8th Place"){
        MajData$TOP8_MATCHES[i] = 1 
      }
    }
  }
  
  #REMOVE ALL THE RESULTS WITH 3 DEFEATS (BECAUSE WE ONLY HAVE PART OF THEM, 
  #WHEREAS WE HAVE ALL THE X-0, X-1 AND X-2 RESULTS)
  #MajDataWO3Def=MajData[MajData$NB_DEFEATS != 3, ]
  #NO NEED FOR THAT FEATURE ANYMORE
  
  return(MajData)
  
}

generate_League_Data = function(periodData) {
  
  #COLLECT PRELIMINARIES ONLY FOR SPECIFIC TREATMENT
  LeagueData=periodData[grep("League", periodData$EVENT), ]
  #View(LeagueData) 
  
  for (i in 1:length(LeagueData)){
    LeagueData$NB_ROUNDS[i]=5
  }
  
  #NUMBER OF ROUNDS IN EACH EVENT FOR THE LEAGUES - ALWAYS 5
  LeagueData$NB_ROUNDS=rep(5,length(LeagueData$EVENT))
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN PRELIMINARIES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  LeagueData$NB_DEFEATS=rep(0,length(LeagueData$EVENT))
  
  #ADD TOP8 COLUMNS FOR MERGE WITH CHALLENGES
  LeagueData$TOP8_PTS=rep(0,length(LeagueData$POINTS))
  LeagueData$TOP8_DEF=rep(0,length(LeagueData$NB_DEFEATS))
  LeagueData$TOP8_MATCHES=rep(0,length(LeagueData$NB_ROUNDS))
  
  return(LeagueData)
  
}

generate_ManaTraders_Data = function(periodData) {
  
  #COLLECT CHALLENGES ONLY FOR SPECIFIC TREATMENT
  MTData=periodData[grep("ManaTraders", periodData$EVENT), ]
  MTData$POINTS=as.numeric(MTData$POINTS)
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE CHALLENGES
  #DIVIDE THE MAXIMUM NUMBER OF POINTS IN SWISS TO GET THE RESULT
  #IF MORE THAN 1 PLAYER HAS THE MAXIMUM OF POINTS, THEN IT IS LIKELY THAT 
  #THERE IS NOT ANY PLAYER AT X-0, SO YOU ADD 1 TO THE RESULT
  listEventsMT=unique(MTData$EVENT_NAME)
  nbRoundsVec=c()
  if(length(listEventsMT)>=1){
    for (i in 1:length(listEventsMT)){
      periodMTEventData=subset(MTData, EVENT_NAME == listEventsMT[i])
      maxPoints=max(periodMTEventData$POINTS)
      nbPlayMaxPts=length(which(periodMTEventData$POINTS == maxPoints))
      if(nbPlayMaxPts==1){
        nbRounds=maxPoints/3
      }else{
        nbRounds=1+maxPoints/3
      }
      nbRoundsEvent=rep(nbRounds,length(periodMTEventData$EVENT))
      nbRoundsVec=c(nbRoundsVec,nbRoundsEvent)
    }
  }
  MTData$NB_ROUNDS=nbRoundsVec
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN CHALLENGES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  MTData$NB_DEFEATS=MTData$NB_ROUNDS - MTData$POINTS/3
  
  #ADD TOP8 COLUMNS FOR MERGE WITH CHALLENGES
  MTData$TOP8_PTS=rep(0,length(MTData$POINTS))
  MTData$TOP8_DEF=rep(0,length(MTData$NB_DEFEATS))
  MTData$TOP8_MATCHES=rep(0,length(MTData$NB_ROUNDS))
  
  return(MTData)
  
}

generate_NRG_Data = function(periodData) {
  
  #COLLECT CHALLENGES ONLY FOR SPECIFIC TREATMENT
  NRGData=periodData[grep("NRG", periodData$EVENT), ]
  NRGData$POINTS=as.numeric(NRGData$POINTS)
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE CHALLENGES
  #DIVIDE THE MAXIMUM NUMBER OF POINTS IN SWISS TO GET THE RESULT
  #IF MORE THAN 1 PLAYER HAS THE MAXIMUM OF POINTS, THEN IT IS LIKELY THAT 
  #THERE IS NOT ANY PLAYER AT X-0, SO YOU ADD 1 TO THE RESULT
  listEventsMT=unique(NRGData$EVENT_NAME)
  nbRoundsVec=c()
  if(length(listEventsMT)>=1){
    for (i in 1:length(listEventsMT)){
      periodMTEventData=subset(NRGData, EVENT_NAME == listEventsMT[i])
      maxPoints=max(periodMTEventData$POINTS)
      nbPlayMaxPts=length(which(periodMTEventData$POINTS == maxPoints))
      if(nbPlayMaxPts==1){
        nbRounds=maxPoints/3
      }else{
        nbRounds=1+maxPoints/3
      }
      nbRoundsEvent=rep(nbRounds,length(periodMTEventData$EVENT))
      nbRoundsVec=c(nbRoundsVec,nbRoundsEvent)
    }
  }
  NRGData$NB_ROUNDS=nbRoundsVec
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN CHALLENGES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  NRGData$NB_DEFEATS=NRGData$NB_ROUNDS - NRGData$POINTS/3
  
  #ADD TOP8 COLUMNS FOR MERGE WITH CHALLENGES
  NRGData$TOP8_PTS=rep(0,length(NRGData$POINTS))
  NRGData$TOP8_DEF=rep(0,length(NRGData$NB_DEFEATS))
  NRGData$TOP8_MATCHES=rep(0,length(NRGData$NB_ROUNDS))
  
  return(NRGData)
  
}

#Event type:
#"Official Competitions" = Major Official Events + Prelims
#"Challenges" = Challenges
#"Preliminaries" = Prelims
#"All Events" = everything available including leagues
#"Everything but Leagues" = Major Events, official or unofficial, and Prelims
#"Major Events Top32" = Major Events, official or unofficial, only top32
#"ManaTraders Series" = ManaTraders Series
#"NRG Series" = NRG Series
#"Major Official Events" = Challenge,Champ,Showcase,Premier,Qualifier,MOCS
generate_df = function(rawData,EventType,mtgFormat){
  
  #SELECT DATA FOR A SPECIFIC PERIOD
  rawData$DATE = as.Date(rawData$DATE)
  rawData$POINTS = as.numeric(rawData$POINTS)
  periodData=subset(rawData, DATE >= as.Date(Beginning) & DATE < as.Date(End))
  periodData=periodData[grep(MTGFormat,periodData$EVENT),]
  
  if (EventType=="Official Competitions"){
    #FUSE THE DATA BACK TO GET ALL THE COMPETITIVE RESULTS IN THE SAME FILE
    df=rbind(generate_Major_Data(periodData),generate_Prelim_Data(periodData))
  }else if (EventType=="Challenges"){
    df=generate_Challenge_Data(periodData)
  }else if (EventType=="Preliminaries"){
    df=generate_Prelim_Data(periodData)
  }else if (EventType=="All Events"){
    df=rbind(generate_Major_Data(periodData),generate_Prelim_Data(periodData))
    df=rbind(df,generate_League_Data(periodData))
    df=rbind(df,generate_ManaTraders_Data(periodData))
    df=rbind(df,generate_NRG_Data(periodData))
  }else if (EventType=="Everything but Leagues"){
    df=rbind(generate_Major_Data(periodData),generate_Prelim_Data(periodData))
    df=rbind(df,generate_ManaTraders_Data(periodData))
    df=rbind(df,generate_NRG_Data(periodData))
  }else if (EventType=="Major Events Top32"){
    df=generate_Major_Data(periodData)
    dfMT=generate_ManaTraders_Data(periodData)
    dfMT$RESULT=parse_number(dfMT$RESULT)
    dfMT=dfMT[dfMT$RESULT<=32,]
    df=rbind(df,dfMT)
    dfNRG=generate_NRG_Data(periodData)
    dfNRG$RESULT=parse_number(dfNRG$RESULT)
    dfNRG=dfNRG[dfNRG$RESULT<=32,]
    df=rbind(df,dfNRG)
  }else if (EventType=="ManaTraders Series"){
    df=generate_ManaTraders_Data(periodData)
  }else if (EventType=="NRG Series"){
    df=generate_NRG_Data(periodData)
  }else if (EventType=="Major Official Events"){
    df=generate_Major_Data(periodData)
  }
  return(df)
}

df=generate_df(rawData,EventType,MTGFormat)

#POSSIBLE QUICKFIX FOR A BETTER ACCURACY IN THE DATA
#THE Shadow Prowess STRATEGY PLAYING BLACK, RED AND GREEN CARDS (A COLOR 
#COMBINATION CALLED JUND) HAS QUITE A DIFFERENT STRUCTURE FROM THE OTHER 
#SHADOW PROWESS DECKS
# for (i in 1:length(df$ARCHETYPE)){
#   if(df$ARCHETYPE[i]=="Shadow Prowess" && df$COLOR[i]=="BRG"){
#     df$ARCHETYPE[i]="Jund Shadow"
#   }
# }

add_super_archetypes = function(df){
  
  df$SUPER_ARCH=df$ARCHETYPE
  for (i in 1:length(df$ARCHETYPE)){
    if(df$ARCHETYPE[i]=="WURG Control" |
       df$ARCHETYPE[i]=="WUBG Control" |
       df$ARCHETYPE[i]=="Bant Midrange"| 
       df$ARCHETYPE[i]=="Scapeshift"| 
       df$ARCHETYPE[i]=="UBRG Control"| 
       df$ARCHETYPE[i]=="Niv To Light"| 
       df$ARCHETYPE[i]=="Omnath Saheeli"| 
       df$ARCHETYPE[i]=="Temur Control"| 
       df$ARCHETYPE[i]=="Sultai Control"| 
       df$ARCHETYPE[i]=="Bant Control"|
       df$ARCHETYPE[i]=="Simic Control"){
      
      df$SUPER_ARCH[i]="UGx Control"
      
    }
    
    if(df$ARCHETYPE[i]=="Jund Prowess" |
       df$ARCHETYPE[i]=="Gruul Prowess" |
       df$ARCHETYPE[i]=="Izzet Prowess"| 
       df$ARCHETYPE[i]=="Boros Prowess"|
       df$ARCHETYPE[i]=="Mardu Prowess"|
       df$ARCHETYPE[i]=="Obosh Aggro"| 
       df$ARCHETYPE[i]=="Mono Red Prowess"| 
       df$ARCHETYPE[i]=="Rakdos Prowess"| 
       df$ARCHETYPE[i]=="Jeskai Prowess"| 
       df$ARCHETYPE[i]=="Naya Prowess"){
      
      df$SUPER_ARCH[i]="Rx Prowess"
      
    }
    
    if(df$ARCHETYPE[i]=="Jeskai Blink" |
       df$ARCHETYPE[i]=="Bant Blink"| 
       df$ARCHETYPE[i]=="WURG Blink" |
       df$ARCHETYPE[i]=="Abzan Blink"
    ){
      
      df$SUPER_ARCH[i]="Blink"
      
    }
    
    if(df$ARCHETYPE[i]=="Jeskai Control" |
       df$ARCHETYPE[i]=="Izzet Control"| 
       df$ARCHETYPE[i]=="Dimir Control"| 
       df$ARCHETYPE[i]=="Azorius Control"|
       df$ARCHETYPE[i]=="Azorius Midrange"|
       df$ARCHETYPE[i]=="Esper Control"| 
       df$ARCHETYPE[i]=="Grixis Control"|
       df$ARCHETYPE[i]=="UW Miracles"|
       df$ARCHETYPE[i]=="UR Kiki Boilproof"){
      
      df$SUPER_ARCH[i]="Non UGx Control"
      
    }
    
    if(df$ARCHETYPE[i]=="Amulet Titan" | 
       df$ARCHETYPE[i]=="KGC Amulet Titan"| 
       df$ARCHETYPE[i]=="Primeval Titan"| 
       df$ARCHETYPE[i]=="Reclaimer Titan"|
       df$ARCHETYPE[i]=="Valakut Field"){
      
      df$SUPER_ARCH[i]="P.Titan"
      
    }
    
    if(df$ARCHETYPE[i]=="UBRG Shadow" | 
       df$ARCHETYPE[i]=="Grixis Shadow"|
       df$ARCHETYPE[i]=="Shadow Prowess"|
       df$ARCHETYPE[i]=="BR Shadow Prowess"|
       df$ARCHETYPE[i]=="WBR Shadow Prowess"| 
       df$ARCHETYPE[i]=="BRG Shadow Prowess"|
       df$ARCHETYPE[i]=="Jund Shadow"|
       df$ARCHETYPE[i]=="Rakdos Shadow"|
       df$ARCHETYPE[i]=="Sultai Shadow"|
       df$ARCHETYPE[i]=="Esper Shadow"|
       df$ARCHETYPE[i]=="Orzhov Shadow"|
       df$ARCHETYPE[i]=="Dimir Shadow"|
       df$ARCHETYPE[i]=="Mardu Shadow"|
       df$ARCHETYPE[i]=="RB Shadow Lurrus"|
       df$ARCHETYPE[i]=="Mardu Shadow Lurrus"){
      
      df$SUPER_ARCH[i]="Shadow"
      
    }
    
    if(df$ARCHETYPE[i]=="Stoneforge Eldrazi" |
       df$ARCHETYPE[i]=="Obligator Eldrazi" |
       df$ARCHETYPE[i]=="Eldrazi Tron" |
       df$ARCHETYPE[i]=="Green Eldrazi"){
      
      df$SUPER_ARCH[i]="Eldrazi"
      
    }
    
    if(df$ARCHETYPE[i]=="Azorius Taxes" |
       df$ARCHETYPE[i]=="Esper Taxes" |
       df$ARCHETYPE[i]=="Abzan Taxes" |
       df$ARCHETYPE[i]=="Mono White Taxes"| 
       df$ARCHETYPE[i]=="Selenya Taxes"|
       df$ARCHETYPE[i]=="Boros Taxes"|
       df$ARCHETYPE[i]=="Orzhov Taxes"|
       df$ARCHETYPE[i]=="Jeskai Taxes"|
       df$ARCHETYPE[i]=="BW Eldrazi & Taxes"){
      
      df$SUPER_ARCH[i]="D&T"
      
    }
    
    if(df$ARCHETYPE[i]=="Abzan Company" | 
       df$ARCHETYPE[i]=="Selesnya Midrange"|
       df$ARCHETYPE[i]=="Naya Midrange"  |
       df$ARCHETYPE[i]=="Badzan"){
      
      df$SUPER_ARCH[i]="GWx Midrange"
      
    }
    
    if(df$ARCHETYPE[i]=="Mardu Midrange" | 
       df$ARCHETYPE[i]=="Rakdos Midrange"){
      
      df$SUPER_ARCH[i]="RBx Midrange"
      
    }
    
    if(df$ARCHETYPE[i]=="Bant Spirits" | 
       df$ARCHETYPE[i]=="Spirits"){
      
      df$SUPER_ARCH[i]="Spirits"
      
    }
    
    if(df$ARCHETYPE[i]=="Belcher"|
       df$ARCHETYPE[i]=="UW Belcher"|
       df$ARCHETYPE[i]=="RG Belcher"){
      
      df$SUPER_ARCH[i]="Belcher"
      
    }
    
    if(df$ARCHETYPE[i]=="Gifts Storm"|
       df$ARCHETYPE[i]=="Twiddle Storm"|
       df$ARCHETYPE[i]=="Song Storm"){
      
      df$SUPER_ARCH[i]="Storm"
      
    }
    
    if(df$ARCHETYPE[i]=="Izzet Restore Balance" |
       df$ARCHETYPE[i]=="Temur Foretold Balance" |
       df$ARCHETYPE[i]=="Izzet Living End"){
      
      df$SUPER_ARCH[i]="URx Balance"
      
    }
    
    if(df$ARCHETYPE[i]=="Slivers"){
      
      df$SUPER_ARCH[i]="Slivers"
      
    }
    
    if(df$ARCHETYPE[i]=="E Tron"){
      
      df$SUPER_ARCH[i]="Eldrazi"
      
    }
    
    if(df$ARCHETYPE[i]=="U Tron" |
       df$ARCHETYPE[i]=="Dice Factory Tron"){
      
      df$SUPER_ARCH[i]="Other Tron"
      
    }
    
    if(df$ARCHETYPE[i]=="Elementals"){
      
      df$SUPER_ARCH[i]="Elementals"
      
    }
    
    if(df$ARCHETYPE[i]=="Humans"){
      
      df$SUPER_ARCH[i]="Humans"
      
    }
    
    if(df$ARCHETYPE[i]=="Merfolks"){
      
      df$SUPER_ARCH[i]="Merfolks"
      
    }
    
    if(df$ARCHETYPE[i]=="Heliod Combo" |
       df$ARCHETYPE[i]=="GW Heliod"|
       df$ARCHETYPE[i]=="Mono White Heliod"){
      
      df$SUPER_ARCH[i]="Heliod"
      
    }
    
    if(df$ARCHETYPE[i]=="Jund Midrange" |
       df$ARCHETYPE[i]=="Jund Lurrus Midrange"|
       df$ARCHETYPE[i]=="Sultai Midrange"|
       df$ARCHETYPE[i]=="UBRG Midrange"|
       df$ARCHETYPE[i]=="Abzan Midrange"|
       df$ARCHETYPE[i]=="Golgari Midrange"){
      
      df$SUPER_ARCH[i]="BGx Midrange"
      
    }
    
    if(df$ARCHETYPE[i]=="Gruul Midrange" |
       df$ARCHETYPE[i]=="Naya Midrange"){
      
      df$SUPER_ARCH[i]="RGx Midrange"
      
    }
    
    if(df$ARCHETYPE[i]=="Dredge"){
      
      df$SUPER_ARCH[i]="Dredge"
      
    }
    
    if(df$ARCHETYPE[i]=="Burn" |
       df$ARCHETYPE[i]=="RW Burn"|
       df$ARCHETYPE[i]=="RG Burn"|
       df$ARCHETYPE[i]=="RB Burn"){
      
      df$SUPER_ARCH[i]="Rx Burn"
      
    }
    
    if(df$ARCHETYPE[i]=="Crabvine"){
      
      df$SUPER_ARCH[i]="Crabvine"
      
    }
    
    if(df$ARCHETYPE[i]=="Orzhov Midrange"){
      
      df$SUPER_ARCH[i]="BWx Midrange"
      
    }
    
    if(df$ARCHETYPE[i]=="Thopter Urza" |
       df$ARCHETYPE[i]=="Uroza" |
       df$ARCHETYPE[i]=="Grixis Whirza" |
       df$ARCHETYPE[i]=="Paradoxical Urza" |
       df$ARCHETYPE[i]=="Urza Oko"){
      
      df$SUPER_ARCH[i]="Urza"
      
    }
    
    if(df$ARCHETYPE[i]=="Bant Stoneblade" |
       df$ARCHETYPE[i]=="UW Stoneblade"){
      
      df$SUPER_ARCH[i]="UWx Stoneblade"
      
    }
    
    if(df$ARCHETYPE[i]=="Infect"){
      
      df$SUPER_ARCH[i]="Infect"
      
    }
    
    if(df$ARCHETYPE[i]=="UB Inverter"){
      
      df$SUPER_ARCH[i]="Inverter"
      
    }
    
    if(df$ARCHETYPE[i]=="MonoG Tron" |
       df$ARCHETYPE[i]=="KGC Tron"){
      
      df$SUPER_ARCH[i]="Gx Tron"
      
    }
    
    if(df$ARCHETYPE[i]=="Bogles"){
      
      df$SUPER_ARCH[i]="Bogles"
      
    }
    
    if(df$ARCHETYPE[i]=="Devoted"|
       df$ARCHETYPE[i]=="GW Devoted Lurrus"){
      
      df$SUPER_ARCH[i]="Devoted"
      
    }
    
    if(df$ARCHETYPE[i]=="Ad Nauseam"){
      
      df$SUPER_ARCH[i]="Ad Nauseam"
      
    }
    
    if(df$ARCHETYPE[i]=="Rogues"){
      
      df$SUPER_ARCH[i]="Rogues"
      
    }
    
    if(df$ARCHETYPE[i]=="Mill" |
       df$ARCHETYPE[i]=="UB Mill"){
      
      df$SUPER_ARCH[i]="Mill"
      
    }
    
    if(df$ARCHETYPE[i]=="Hammer Time"){
      
      df$SUPER_ARCH[i]="Hammer Time"
      
    }
    
    if(df$ARCHETYPE[i]=="Enduring Ideal"){
      
      df$SUPER_ARCH[i]="Enduring Ideal"
      
    }
    
    if(df$ARCHETYPE[i]=="Polymorph"){
      
      df$SUPER_ARCH[i]="Polymorph"
      
    }
    
    if(df$ARCHETYPE[i]=="Red Prison" | 
       df$ARCHETYPE[i]=="Boros Land Destruction"|
       df$ARCHETYPE[i]=="Pyro Prison" ){
      
      df$SUPER_ARCH[i]="Red Prison"
      
    }
    
    if(df$ARCHETYPE[i]=="Domain Zoo"|
       df$ARCHETYPE[i]=="Bushwhacker Zoo"){
      
      df$SUPER_ARCH[i]="Zoo"
    }
  }
  
  return(df)
  
}

#/!\ to be updated when you change data, at least check if there isn't any new 
#archetype that should be considered - worst case the simple archetype name is
#reused

#ADD SUPER ARCHETYPES DEPENDING ON EXACT ARCHETYPE

#TO SEE WHICH DECKLISTS CORRESPONDS TO A LABEL, FOR INSTANCE "Bant Midrange"
#df[grep("Bant Midrange", df$ARCHETYPE), ]$URL

df=add_super_archetypes(df)

#THE CODE DOES NOT CHECK WHETHER WE WANT TO USE SUPER ARCHETYPES HERE, BECAUSE 
#IT IS STILL VERY FAST TO ADD THAT COLUMN, AND IT IS EASIER FOR USE WHEN YOU 
#CHANGE THE TYPE OF ARCHETYPES YOU WANT TO USE IN THE CONSOLE ONCE YOU EXECUTED
#THE ENTIRE CODE ONCE TO GENERATE VARIOUS GRAPHS

#TO SEE WHICH EXACT ARCHETYPES ARE CONTAINED IN A SUPER ARCHETYPE, for instance 
#"UGx Control"
#unique(df[grep("UGx Control", df$SUPER_ARCH), ]$ARCHETYPE)

#3rd file to execute
#provides functions to determine metagame share and winrates, as well as graphs
#to display those results

#also imports all the libraries that can be useful here or in following files

#Execute this file only once, unless you directly edit it, it doesn't treat data
#only provides some functions to analyse it

################################################################################
#LIBRARIES
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("ggrepel")
#install.packages("jsonlite")
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("purrr")
#install.packages("jsonify")
#install.packages("plyr")
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("corrplot")
#install.packages("ggpubr")
#install.packages("ClustMAPDP")
#install.packages("expm")
#install.packages("matrixStats")
#devtools::install_github('thomasp85/gganimate')
#install.packages('gganimate')
#install.packages("gifski")
#install.packages("raster")
#install.packages("conflicted")
#install.packages("xlsx") 
library(ggplot2)
library(dplyr)
library(ggrepel)
library(jsonlite)
library(tidyverse)
library(data.table)
library(purrr)
library(jsonify)
library(plyr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggpubr)
library(ClustMAPDP)
library(expm)
library(matrixStats)
library(gganimate)
library(gifski)
library(raster)
library(conflicted)
library(xlsx)

#IF YOU WANT TO REMOVE THE conflicted PACKAGE TO USE "View(df)"
#unloadNamespace("conflicted")

conflict_prefer("select", "dplyr")
conflict_prefer("arrange", "dplyr")

################################################################################

#VARIABLE FOR THE ACCURACY ON THE NAMING OF THE ARCHETYPES
#PROVIDES THE NAME OF THE COLUMN OF THE DATAFRAME TO BE USED
archetype_acc=NA
if(Classification=="Super"){
  archetype_acc="SUPER_ARCH"
}else if(Classification=="Exact"){
  archetype_acc="ARCHETYPE"
}

#LIST ALL THE DIFFERENT ARCHETYPES IN THE DATA
generate_archetype_list = function(df,beginning,end){
  #CREATE A DATAFRAME CONTAINING THE LIST OF ARCHETYPES
  periodDf=subset(df, DATE >= as.Date(beginning) & DATE < as.Date(end))
  arch_list=data.frame(unique(df[[archetype_acc]]))
  names(arch_list)[1] = c("ARCHETYPES")
  return(arch_list)
}

#COMPUTES THE SHARE OF EACH ARCHETYPE IN THE DATA
#presence CAN BE EITHER "Copies", "Players", "Matches" or "Ratio M/P"
generate_metagame_data = function(df,graph_share,presence,beginning,end){
  
  arch_list=generate_archetype_list(df,beginning,end)
  
  #ADD THE PRESENCE OF EACH ARCHETYPE IN THE DATA
  arch_list$PRESENCE=rep(0,length(arch_list$ARCHETYPES))
  for (i in 1:length(arch_list$PRESENCE)){
    arch_id=which(df[[archetype_acc]]==arch_list$ARCHETYPES[i])
    if (presence=="Copies"){
      #NUMBER OF COPIES
      arch_list$PRESENCE[i]=length(arch_id)
    }else if (presence=="Players"){
      #NUMBER OF PLAYERS
      arch_list$PRESENCE[i]=length(unique(df[arch_id,]$PLAYER))
    }else if (presence=="Matches"){
      #NUMBER OF ROUNDS PLAYED
      arch_list$PRESENCE[i]=sum(df[arch_id,]$NB_ROUNDS,df[arch_id,]$TOP8_MATCHES)
    }else if (presence=="Ratio M/P"){
      #NUMBER OF ROUNDS PLAYED
      arch_list$PRESENCE[i]=sum(df[arch_id,]$NB_ROUNDS,df[arch_id,]$TOP8_MATCHES)/
        length(unique(df[arch_id,]$PLAYER))
    }
  }
  
  #FOR EASIER READING OF THE GRAPHS, AGGREGATE ALL THE ARCHETYPES ACCOUNTING FOR 
  #LESS THAN graph_share% OF THE DATA
  graph_perc=graph_share/100*sum(arch_list$PRESENCE)
  arch_list_vis=arch_list[arch_list$PRESENCE >= graph_perc, ]
  #arch_list_vis=arch_list_vis[order(arch_list_vis$ARCHETYPES, decreasing = TRUE),]
  arch_list_vis=arrange(arch_list_vis,desc(PRESENCE))
  
  #ADD AN "OTHER" CATEGORY CONTAINING THE SUM OF COPIES OF ALL ARCHETYPES UNDER X%
  sum_others=sum(arch_list[arch_list$PRESENCE < graph_perc, ]$PRESENCE)
  otherName=paste("Other (each <",graph_share,"%)",sep="")
  arch_list_vis=rbind(arch_list_vis,c(otherName, sum_others))
  #arch_list_vis=arch_list_vis[order(arch_list_vis$ARCHETYPES),]
  
  arch_list_vis$PRESENCE=as.numeric(arch_list_vis$PRESENCE)
  arch_list_vis$SHARE=as.numeric(format(round(arch_list_vis$PRESENCE/
                                                sum(arch_list_vis$PRESENCE)*100,
                                              1), nsmall = 1))
  
  arch_list_vis$ARCHETYPES = reorder(arch_list_vis$ARCHETYPES, 
                                     as.numeric(arch_list_vis$PRESENCE))
  arch_list_vis$ARCHETYPES = relevel(arch_list_vis$ARCHETYPES, otherName)
  
  arch_list_vis$ARCHETYPES=fct_rev(arch_list_vis$ARCHETYPES)
  
  return(arch_list_vis)
}

#COMPUTES A NAME FOR THE HISTOGRAM AND THE PIE CHART
#presence CAN BE EITHER "Copies", "Players" or "Matches"
generate_metagame_graph_title = function(presence,beginning,end,EventType){
  MetaGraphTitle=paste("Proportion of", Classification,"archetypes in MTGO", 
                       EventType,"between\n", beginning, "and", end,
                       "based on number of", presence,sep = " ")
  return(MetaGraphTitle)
}

#GENERATE A PIE CHART BASED ON DATA IN DF
#presence CAN BE EITHER "Copies", "Players" or "Matches"
metagame_pie_chart = function(df,presence,beginning,end,EventType){
  
  #CHANGE THE NUMBER FOR THE PROPORTION OF THE "OTHERS" CATEGORY HERE
  df_gen=generate_metagame_data(df,PieShare,presence,beginning,end)
  
  ggplot(df_gen, aes(x="", -SHARE, fill = ARCHETYPES)) + 
    geom_bar(width = 1, size = 1, color = "white", stat = "identity") + 
    coord_polar("y", start=0) + 
    geom_text(aes(label = paste0(SHARE, "%")), 
              position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = NULL, fill = NULL, subtitle = "by Anael Yahi",
         title = generate_metagame_graph_title(presence,beginning,end,EventType)) + 
    guides(color = FALSE, size = FALSE) +
    scale_color_gradient(low="red", high="blue") +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "#111111",size = 20),
          plot.subtitle = element_text(hjust = 0.5,size = 18),
          legend.text = element_text(size = 17))
  
}

#GENERATE A BOX PLOT BASED ON DATA IN DF
#presence CAN BE EITHER "Copies", "Players" or "Matches"
metagame_box_plot = function(df,presence,beginning,end,EventType,histShare){
  
  #GET THE DATA FOR ALL ARCHETYPES HAVING A META SHARE ABOVE HistShare
  df_gen=generate_metagame_data(df,histShare,presence,beginning,end)
  
  #GENERATE A TITLE FOR THE BOXPLOT
  boxplot_title=paste(generate_metagame_graph_title(presence,beginning,end,EventType),"-",
                      df_gen[grep("Other",df_gen$ARCHETYPES),]$ARCHETYPES, sep=" ")
  
  #THIS GRAPH DOESN'T DISPLAY THE "Other" CATEGORY
  df_gen=df_gen[!grepl("Other",df_gen$ARCHETYPES),]
  
  #REORDER ARCHETYPES BY ASCENDING PRESENCE
  df_gen$ARCHETYPES = reorder(df_gen$ARCHETYPES, as.numeric(df_gen$PRESENCE))
  
  #plot is much clearer
  ggplot(df_gen, aes(x=ARCHETYPES, y=as.numeric(SHARE), fill=ARCHETYPES)) + 
    geom_bar(stat="identity") + theme_minimal() + guides( fill = FALSE) +
    labs(x = NULL, y = "Presence (%)", fill = NULL, 
         title = boxplot_title, subtitle = "by Anael Yahi") + 
    scale_color_gradient(low="blue", high="red") +
    #scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
    theme(axis.text.x  = element_text(size=12)) + coord_flip()
}

#PROVIDE A GRAPH FOR A METRIC DATAFRAME DISPLAYING WINRATES DEPENDING ON
#PRESENCE, WHICH IS HIGHLIGHTED BY THE DIAMETERS OF ANOTHER TYPE OF PRESENCE
#presence AND diameters CAN BE EITHER "Copies", "Players" or "Matches"
#tiers CAN BE EITHER "Win+Pres","Pres M+SD" or "Pres %"
#isLog is a boolean
#only_best is a boolean
metric_graph = function(metric_df,presence,diameters,diam_ratio,beginning,end,
                        tiers,isLog,only_best,EventType) {
  if(only_best){
    metric_df=metric_df[metric_df$TOTAL_NB_MATCHES>mean(metric_df$TOTAL_NB_MATCHES),]
  }
  
  #COMPUTES THE PARAMETERS OF THE LINES TO APPEAR ON THE GRAPH
  if (presence=="Copies"){
    coeffdir=-max(metric_df$WINRATE_AVERAGE)/max(metric_df$NB_COPIES/
                                                   sum(metric_df$NB_COPIES)*100)
  }else if (presence=="Players"){
    coeffdir=-max(metric_df$WINRATE_AVERAGE)/max(metric_df$NB_PLAYERS/
                                                   sum(metric_df$NB_PLAYERS)*100)
  }else if (presence=="Matches"){
    coeffdir=-max(metric_df$WINRATE_AVERAGE)/max(metric_df$TOTAL_NB_MATCHES/
                                                   sum(metric_df$TOTAL_NB_MATCHES)*100)
  }
  average=mean(metric_df$WINRATE_AVERAGE)
  sdeviation=sd(metric_df$WINRATE_AVERAGE)
  
  #GENERATES THE LABELS
  if (presence=="Copies"){
    x_label="Total number of copies of each archetype (%)"
  }else if (presence=="Players"){
    x_label="Total number of different players for each archetype (%)"
  }else if (presence=="Matches"){
    x_label="Total number of matches played by each archetype (%)"
  }
  y_label="Average winrate of each archetype (%)"
  graph_title=paste("Winrates depending on presence:", Classification,"archetypes ", 
                    "between", beginning, "and", end, "in MTGO", EventType,sep = " ")
  graph_subtitle=paste("Circle diameters depending on",diameters,"\nby Anael Yahi",sep=" ")
  if(tiers=="Win+Pres"){
    graph_subtitle=paste("Separated by mean +/- n standard deviations (n={0,1,2,3}) 
Circle diameters depending on",diameters,"\nby Anael Yahi",sep=" ")
  }
  
  #GENERATES THE GRAPH
  if (presence=="Copies"){
    metric_df$NB_COPIES=metric_df$NB_COPIES/sum(metric_df$NB_COPIES)*100
    metric_plot=ggplot(metric_df, aes(NB_COPIES, WINRATE_AVERAGE*100))
    avg_presence=mean(metric_df$NB_COPIES)
    std_presence=sd(metric_df$NB_COPIES)
  }else if (presence=="Players"){
    metric_df$NB_PLAYERS=metric_df$NB_PLAYERS/sum(metric_df$NB_PLAYERS)*100
    metric_plot=ggplot(metric_df, aes(NB_PLAYERS, WINRATE_AVERAGE*100))
    avg_presence=mean(metric_df$NB_PLAYERS)
    std_presence=sd(metric_df$NB_PLAYERS)
  }else if (presence=="Matches"){
    metric_df$TOTAL_NB_MATCHES=metric_df$TOTAL_NB_MATCHES/
      sum(metric_df$TOTAL_NB_MATCHES)*100
    metric_plot=ggplot(metric_df, aes(TOTAL_NB_MATCHES, WINRATE_AVERAGE*100))
    avg_presence=mean(metric_df$TOTAL_NB_MATCHES)
    std_presence=sd(metric_df$TOTAL_NB_MATCHES)
  }
  
  if (diameters=="Copies"){
    metric_plot=metric_plot + 
      geom_point(aes(color = ARCHETYPES), size=metric_df$NB_COPIES*diam_ratio,
                 show.legend = FALSE)
  }else if (diameters=="Players"){
    metric_plot=metric_plot + 
      geom_point(aes(color = ARCHETYPES), size=metric_df$NB_PLAYERS*diam_ratio,
                 show.legend = FALSE)
  }else if (diameters=="Matches"){
    metric_plot=metric_plot + 
      geom_point(aes(color = ARCHETYPES), size=metric_df$TOTAL_NB_MATCHES*
                   diam_ratio,show.legend = FALSE)
  }
  
  metric_plot=metric_plot + coord_cartesian() + theme_bw() + 
    labs(x=x_label, y=y_label, title=graph_title, subtitle=graph_subtitle) + 
    geom_text_repel(aes(label=ARCHETYPES),hjust=0, vjust=0,point.padding = NA) 
  
  #tiers CAN BE EITHER "Win+Pres","Pres M+SD" or "Pres %"
  if (tiers=="Win+Pres"){
    #TIERS BASED ON COMBINATION OF MEAN AND STANDARD DEVIATION OF PRESENCE AND WINRATE
    metric_plot=metric_plot + geom_abline(intercept = average, slope = coeffdir,
                                          color="red", size=1.5) +
      geom_abline(intercept = average+1*sdeviation, slope = coeffdir,
                  color="red", linetype="dotted", size=1.5) +
      geom_abline(intercept = average+2*sdeviation, slope = coeffdir,
                  color="red", linetype="dotted", size=1.5) +
      geom_abline(intercept = average+3*sdeviation, slope = coeffdir,
                  color="red", linetype="dashed", size=1.5) +
      geom_abline(intercept = average-1*sdeviation, slope = coeffdir,
                  color="red", linetype="dotted", size=1.5) +
      geom_abline(intercept = average-2*sdeviation, slope = coeffdir,
                  color="red", linetype="dotted", size=1.5) +
      geom_abline(intercept = average-3*sdeviation, slope = coeffdir,
                  color="red", linetype="dashed", size=1.5)
    
  }else if (tiers=="Pres M+SD"){
    #TIERS BASED ON MEAN + N * STANDARD DEVIATION OF PRESENCE, N={0,1,2,3}
    metric_plot=metric_plot + geom_vline(xintercept = avg_presence, linetype="dotted",
                                         color = "purple", size=2) +
      geom_text(aes(x=avg_presence, label="Tiers 2.5\n", y=
                      max(WINRATE_AVERAGE*100)), colour="purple",
                angle=0, size=8) +
      geom_text(aes(x=avg_presence, label="\nPresence mean", y=
                      max(WINRATE_AVERAGE*100)), colour="grey",
                angle=0, size=4) +
      geom_vline(xintercept = avg_presence + std_presence, linetype="dotted",
                 color = "blue", size=1.5) +
      geom_text(aes(x=avg_presence + std_presence, label="Tiers 2\n",
                    y=max(WINRATE_AVERAGE*100)), colour="blue",
                angle=0, size=8) +
      geom_text(aes(x=avg_presence + std_presence, label="\nMean + 1*sd",
                    y=max(WINRATE_AVERAGE*100)), colour="grey",
                angle=0, size=4) +
      geom_vline(xintercept = avg_presence + 2*std_presence, linetype="dotted",
                 color = "dark green", size=1.5) +
      geom_text(aes(x=avg_presence + 2*std_presence, label="Tiers 1.5\n",
                    y=max(WINRATE_AVERAGE*100)), colour="dark green",
                angle=0, size=8) +
      geom_text(aes(x=avg_presence + 2*std_presence, label="\nMean + 2*sd",
                    y=max(WINRATE_AVERAGE*100)), colour="grey",
                angle=0, size=4) +
      geom_vline(xintercept = avg_presence + 3*std_presence, linetype="dotted",
                 color = "orange", size=1.5) +
      geom_text(aes(x=avg_presence + 3*std_presence, label="Tiers 1\n",
                    y=max(WINRATE_AVERAGE*100)), colour="orange",
                angle=0, size=8) +
      geom_text(aes(x=avg_presence + 3*std_presence, label="\nMean + 3*sd",
                    y=max(WINRATE_AVERAGE*100)), colour="grey",
                angle=0, size=4) +
      geom_vline(xintercept = avg_presence + 4*std_presence, linetype="dotted",
                 color = "red", size=1.5) +
      geom_text(aes(x=avg_presence + 4*std_presence, label="Tiers 0\n",
                    y=max(WINRATE_AVERAGE*100)), colour="red",
                angle=0, size=8) +
      geom_text(aes(x=avg_presence + 4*std_presence, label="\nMean + 4*sd",
                    y=max(WINRATE_AVERAGE*100)), colour="grey",
                angle=0, size=4)
    
  }else if (tiers=="Pres %"){
    #TIERS BASED ON ARBITRARY % OF PRESENCE: 2,4,6,8,10
    metric_plot=metric_plot + geom_vline(xintercept = 10, linetype="dashed",
                                         color = "blue", size=2) +
      geom_vline(xintercept = 8, linetype="dotted",
                 color = "blue", size=1.5) +
      geom_vline(xintercept = 6, linetype="dotted",
                 color = "blue", size=1.5) +
      geom_vline(xintercept = 4, linetype="dotted",
                 color = "blue", size=1.5) +
      geom_vline(xintercept = 2, linetype="dashed",
                 color = "blue", size=2) +
      geom_abline(intercept = average, slope = 0,
                  color="red", linetype="dashed", size=1.5) +
      geom_abline(intercept = average+sdeviation, slope = 0,
                  color="red", linetype="dashed", size=1.5) +
      geom_abline(intercept = average-sdeviation, slope = 0,
                  color="red", linetype="dashed", size=1.5) +
      geom_abline(intercept = average+0.5*sdeviation, slope = 0,
                  color="red", linetype="dotted", size=1.5) +
      geom_abline(intercept = average-0.5*sdeviation, slope = 0,
                  color="red", linetype="dotted", size=1.5)
  }
  
  if (isLog){
    metric_plot=metric_plot + scale_x_continuous(trans = 'log10')
  }
  
  return(metric_plot)
  
}

#FILL IN METRIC POINTS IN AN ARCHETYPES DATA FRAME
metric_points_archetypes = function(df,beginning,end){
  df2=subset(df, DATE >= as.Date(beginning) & DATE < as.Date(end))
  #GET THE LIST OF THE DIFFERENT ARCHETYPES IN THE DATA
  metric_df=generate_archetype_list(df2,beginning,end)
  players_df=metric_points_players(df,beginning,end)
  
  metric_df$NB_COPIES=rep(0,length(metric_df$ARCHETYPES))
  metric_df$NB_PLAYERS=rep(0,length(metric_df$ARCHETYPES))
  metric_df$TOTAL_NB_MATCHES=rep(0,length(metric_df$ARCHETYPES))
  metric_df$WINRATE_AVERAGE=rep(0,length(metric_df$ARCHETYPES))
  metric_df$WINRATE_95_MIN=rep(0,length(metric_df$ARCHETYPES))
  metric_df$WINRATE_95_MAX=rep(0,length(metric_df$ARCHETYPES))
  metric_df$MATCHES_PER_PLAYER=rep(0,length(metric_df$ARCHETYPES))
  metric_df$WINRATE_ARCH_OUT_PLAYERS=rep(0,length(metric_df$ARCHETYPES))
  
  for (i in 1:length(metric_df$ARCHETYPES)){
    #POSITION OF THE CORRESPONDING EXACT OR SUPER ARCHETYPE IN THE DATA
    arch_identification=which(df2[[archetype_acc]]==metric_df$ARCHETYPES[i])
    #NUMBER OF APPEARANCES IN THE DATA OF THE CORRESPONDING ARCHETYPE
    metric_df$NB_COPIES[i]=length(arch_identification)
    #NUMBER OF DIFFERENT PLAYERS PLAYING THAT DECK
    metric_df$NB_PLAYERS[i]=length(unique(df2[arch_identification,]$PLAYER))
    #NUMBER OF MATCHES PLAYED BY THAT ARCHETYPE IN THE DATA
    metric_df$TOTAL_NB_MATCHES[i]=sum(df2[arch_identification,]$NB_ROUNDS,
                                      df2[arch_identification,]$TOP8_MATCHES)
    #NUMBER OF MATCHES PER PLAYER - THE HIGHER, THE MORE A PLAYER wiTH THAT DECK
    #APPEARED IN THE RESULTS
    metric_df$MATCHES_PER_PLAYER[i]=metric_df$TOTAL_NB_MATCHES[i]/
      metric_df$NB_PLAYERS[i]
    
    #NUMBER OF WINS OF THAT ARCHETYPE
    total_wins_arch=sum((df2$POINTS[arch_identification] + 
                           df2$TOP8_PTS[arch_identification])/3)
    #NUMBER OF MATCHES OF THAT ARCHETYPE
    total_matches_arch=sum(df2$NB_ROUNDS[arch_identification] + 
                             df2$TOP8_MATCHES[arch_identification])
    
    #95% CONFIDENCE INTERVALS OF THE WINRATE
    #EFFECTIVE WINRATE IN THE DATA
    metric_df$WINRATE_AVERAGE[i]=binom.test(total_wins_arch, total_matches_arch, 
                                            p=0.5,alternative="two.sided", 
                                            conf.level=0.95)$estimate
    #LOWER BOUND OF THE "TRUE" WINRATE             
    metric_df$WINRATE_95_MIN[i]=binom.test(total_wins_arch, total_matches_arch, 
                                           p=0.5,alternative="two.sided", 
                                           conf.level=0.95)$conf.int[1]
    #UPPER BOUND OF THE "TRUE" WINRATE 
    metric_df$WINRATE_95_MAX[i]=binom.test(total_wins_arch, total_matches_arch, 
                                           p=0.5,alternative="two.sided", 
                                           conf.level=0.95)$conf.int[2]
    
    #WINRATE OF THAT ARCHETYPE DIVIDED BY THE AVERAGE WINRATE OF ITS PILOTS
    metric_df$WINRATE_ARCH_OUT_PLAYERS[i]=metric_df$WINRATE_AVERAGE[i]/
      mean(players_df[grep(metric_df$ARCHETYPES[i],
                           players_df$ARCHETYPE_NAMES),]$WINRATE_AVERAGE)
  }
  
  return(metric_df)
}

#COMBINES THE RATIOS OF POINTS PER ROUND AND NUMBER OF COPIES FOR EACH
#ARCHETYPE, THEN PROVIDES A RANK BASED ON THAT
#THE NEW METRIC OBTAINED THAT WAY IS NORMALIZED TO BE BETWEEN 0 AND 1
#ALSO IMPLEMENTS THE VS META SCORE 
#https://www.vicioussyndicate.com/vs-meta-score-new-metric-measuring-archetypes-standing-meta/
archetypes_ranking = function(metric_df,beginning,end){
  
  metric_df$METRIC_COMB=metric_df$WINRATE_AVERAGE
  for (i in 1:length(metric_df$METRIC_COMB)){
    metric_df$METRIC_COMB[i] = 
      (Presence_Weight * (metric_df$TOTAL_NB_MATCHES[i]-
                            min(metric_df$TOTAL_NB_MATCHES)) /
         max(metric_df$TOTAL_NB_MATCHES) +
         (PPR_Weight * metric_df$WINRATE_AVERAGE[i]-
            min(metric_df$WINRATE_AVERAGE)) /
         max(metric_df$WINRATE_AVERAGE )) /
      (Presence_Weight+PPR_Weight)
  }
  
  metric_df = metric_df[order(-metric_df$METRIC_COMB),]
  
  metric_df$RANK=metric_df$METRIC_COMB
  for (i in 1:length(metric_df$RANK)){
    metric_df$RANK[i]=i
  }
  
  metric_df$VS_META_SCORE=metric_df$METRIC_COMB
  MetaPeak=c(max(metric_df$TOTAL_NB_MATCHES) - min(metric_df$TOTAL_NB_MATCHES) /
               max(metric_df$TOTAL_NB_MATCHES),
             max(metric_df$WINRATE_AVERAGE) - min(metric_df$WINRATE_AVERAGE) /
               max(metric_df$WINRATE_AVERAGE))
  
  for (i in 1:length(metric_df$VS_META_SCORE)){
    ArchCoord=c((metric_df$TOTAL_NB_MATCHES[i]-min(metric_df$TOTAL_NB_MATCHES)) /
                  max(metric_df$TOTAL_NB_MATCHES),
                (metric_df$WINRATE_AVERAGE[i]-min(metric_df$WINRATE_AVERAGE)) /
                  max(metric_df$WINRATE_AVERAGE))
    
    metric_df$VS_META_SCORE[i]=pointDistance(MetaPeak, ArchCoord, lonlat=FALSE)
  }
  return(metric_df)
}

#PLOT OF THE AVERAGE WINRATE FOR THE MOST POPULAR ARCHETYPES
#presence CAN BE EITHER "Copies", "Players" or "Matches"
winrates_graph = function(df,arch_ranked,presence,beginning,end,EventType){
  
  #GET ONLY THE DECKS APPEARING THE MOST IN THE DATA
  if (presence=="Copies"){
    #KEEP ONLY THE DECK WITH THE MOST COPIES
    presence_min=HistShare/100*length(df$ARCHETYPE)
    arch_most_played=arch_ranked[arch_ranked$NB_COPIES>=presence_min,]
  }else if (presence=="Players"){
    #KEEP ONLY THE DECK WITH THE MOST PLAYERS
    presence_min=HistShare/100*length(unique(df$PLAYER))
    arch_most_played=arch_ranked[arch_ranked$NB_PLAYERS>=presence_min,]
  }else if (presence=="Matches"){
    #KEEP ONLY THE DECK WITH THE MOST MATCHES
    presence_min=HistShare/100*(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))
    arch_most_played=arch_ranked[arch_ranked$TOTAL_NB_MATCHES>=presence_min,]
  }
  
  #REORDER ARCHETYPES BY ASCENDING AVERAGE WINRATE
  arch_most_played$ARCHETYPES = reorder(arch_most_played$ARCHETYPES, 
                                        as.numeric(arch_most_played$WINRATE_AVERAGE))
  #PLOT THE AVERAGE WINRATE AND THE CONFIDENCE INTERVALS
  y_label_winrate="Winrates of the most popular archetypes (%)"
  graph_title_winrate=paste(
    "Confidence intervals on the winrates of the most present archetypes ", 
    "( at least ",HistShare,"% of the ",presence,") between ", beginning, 
    " and ", end, " in MTGO ", EventType,sep="")
  
  ggplot(arch_most_played, aes(x=ARCHETYPES, y=WINRATE_AVERAGE*100)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(WINRATE_AVERAGE*100,1), nsmall = 1)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y=y_label_winrate, title=graph_title_winrate,
         subtitle="Red lines for the average of the bounds of the CI
Green line for the average of the computed winrate
by Anael Yahi")+
    geom_errorbar(aes(ymax = WINRATE_95_MAX*100, ymin = WINRATE_95_MIN*100)) + 
    geom_hline(yintercept = mean(arch_most_played$WINRATE_AVERAGE*100), 
               color="green", linetype="dashed", size=1)+ 
    geom_hline(yintercept = mean(arch_most_played$WINRATE_95_MIN*100), 
               color="red", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = mean(arch_most_played$WINRATE_95_MAX*100), 
               color="red", linetype="dashed", size=0.5) + 
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
}

#PLOT THE REPARTITION FOR THE META SCORE OF THE PRESENCE AND WINRATES
#FOR THE MOST POPULAR ARCHETYPES
#PRESENCE: NUMBER OF MATCHES
meta_score_graph = function(df,arch_ranked_sub,beginning,end,EventType){
  
  presence_min=HistShare/100*(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))
  arch_ranked_sub_2=arch_ranked[arch_ranked$TOTAL_NB_MATCHES>=log(presence_min),]
  
  meanData=mean(arch_ranked_sub_2$VS_META_SCORE)
  sdData=sd(arch_ranked_sub_2$VS_META_SCORE)
  meanPlusSd=meanData+sdData
  meanMinusSd=meanData-sdData
  
  arch_ranked_sub_2$ARCHETYPES=reorder(arch_ranked_sub_2$ARCHETYPES,
                                       arch_ranked_sub_2$VS_META_SCORE)
  
  titleLinearComb=paste("VS Meta Score for the most popular archetypes
At least ",HistShare,"% of presence
Presence Weight = ",Presence_Weight, " / Winrate weight = ",PPR_Weight, "
Between ",beginning," and ",end," in MTGO ",EventType,sep="")
  
  ggplot(arch_ranked_sub_2, aes(x=ARCHETYPES, y=VS_META_SCORE)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(VS_META_SCORE,2), nsmall = 2)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y="Value of the meta score metric", title=titleLinearComb,
         subtitle="Green line for the average of the meta score
Red lines for the average +/- a standard deviation
by Anael Yahi")+
    geom_hline(yintercept = meanData, color="green", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanPlusSd, color="red", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanMinusSd, color="red", linetype="dashed", size=0.5) + 
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
  
}

#PLOT THE REPARTITION FOR THE LINEAR COMBINATION OF THE PRESENCE AND WINRATES
#FOR THE MOST POPULAR ARCHETYPES
#PRESENCE: NUMBER OF MATCHES
linear_comb_graph = function(df,arch_ranked,beginning,end,EventType){
  
  presence_min=HistShare/100*(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))
  arch_ranked_sub_2=arch_ranked[arch_ranked$TOTAL_NB_MATCHES>=presence_min,]
  
  meanData=mean(arch_ranked_sub_2$METRIC_COMB*100)
  sdData=sd(arch_ranked_sub_2$METRIC_COMB*100)
  meanPlusSd=meanData+sdData
  meanMinusSd=meanData-sdData
  
  arch_ranked_sub_2$ARCHETYPES=reorder(arch_ranked_sub_2$ARCHETYPES,
                                       arch_ranked_sub_2$METRIC_COMB)
  
  titleLinearComb=paste("Linear combination of the metrics for the most popular archetypes
At least ",HistShare,"% of presence
Presence Weight = ",Presence_Weight, " / Winrate weight = ",PPR_Weight, "
Between ",beginning," and ",end," in MTGO ",EventType,sep="")
  
  ggplot(arch_ranked_sub_2, aes(x=ARCHETYPES, y=METRIC_COMB*100)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(METRIC_COMB*100,1), nsmall = 1)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y="Value of the linear combination metric", title=titleLinearComb,
         subtitle="Green line for the average of the metrics linear combination
Red lines for the average +/- a standard deviation
by Anael Yahi")+
    geom_hline(yintercept = meanData, color="green", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanPlusSd, color="red", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanMinusSd, color="red", linetype="dashed", size=0.5) + 
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
  
}

#PLOT THE REPARTITION FOR THE A LOGARITHMIC COMBINATION OF THE PRESENCE AND 
#WINRATES FOR THE MOST POPULAR ARCHETYPES
#PRESENCE: NUMBER OF MATCHES
log_comb_graph = function(df,arch_ranked,beginning,end,EventType){
  
  presence_min=HistShare/100*(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))
  arch_ranked_sub_2=arch_ranked[arch_ranked$TOTAL_NB_MATCHES>=log(presence_min),]
  
  meanData=mean(arch_ranked_sub_2$METRIC_COMB*100)
  sdData=sd(arch_ranked_sub_2$METRIC_COMB*100)
  meanPlusSd=meanData+sdData
  meanMinusSd=meanData-sdData
  
  arch_ranked_sub_2$ARCHETYPES=reorder(arch_ranked_sub_2$ARCHETYPES,
                                       arch_ranked_sub_2$METRIC_COMB)
  
  titleLinearComb=paste("Combination of the metrics for the most popular archetypes
At least ",HistShare,"% of presence - Linear winrate, logarithmic presence
Presence Weight = ",Presence_Weight, " / Winrate weight = ",PPR_Weight,  "
Between ",beginning," and ",end," in MTGO ",EventType,sep="")
  
  ggplot(arch_ranked_sub_2, aes(x=ARCHETYPES, y=METRIC_COMB*100)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(METRIC_COMB*100,1), nsmall = 1)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y="Value of the linear combination metric", title=titleLinearComb,
         subtitle="Green line for the average of the metrics linear combination
Red lines for the average +/- a standard deviation
by Anael Yahi")+
    geom_hline(yintercept = meanData, color="green", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanPlusSd, color="red", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanMinusSd, color="red", linetype="dashed", size=0.5) + 
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
  
}

#PLOT THE REPARTITION FOR THE LINEAR COMBINATION OF THE PRESENCE AND WINRATES
#FOR THE MOST POPULAR ARCHETYPES
#PRESENCE: NUMBER OF MATCHES
lower_bound_ci_winrate_graph = function(df,arch_ranked,beginning,end,EventType){
  
  presence_min=HistShare/100*(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))
  arch_ranked_sub_2=arch_ranked[arch_ranked$TOTAL_NB_MATCHES>=presence_min,]
  
  meanData=mean(arch_ranked_sub_2$WINRATE_95_MIN*100)
  sdData=sd(arch_ranked_sub_2$WINRATE_95_MIN*100)
  meanPlusSd=meanData+sdData
  meanMinusSd=meanData-sdData
  
  arch_ranked_sub_2$ARCHETYPES=reorder(arch_ranked_sub_2$ARCHETYPES,
                                       arch_ranked_sub_2$WINRATE_95_MIN)
  
  titleLinearComb=paste("Lower bound of the confidence intervals for the winrates of the most popular decks
At least ",HistShare,"% of presence
Presence Weight = ",Presence_Weight, " / Winrate weight = ",PPR_Weight,   "
Between ",beginning," and ",end," in MTGO ",EventType,sep="")
  
  ggplot(arch_ranked_sub_2, aes(x=ARCHETYPES, y=WINRATE_95_MIN*100)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(WINRATE_95_MIN*100,1), nsmall = 1)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y="Lower estimation of the winrate (%)", title=titleLinearComb,
         subtitle="Green line for the average of the lower estimation of winrates
Red lines for the average +/- a standard deviation
by Anael Yahi")+
    geom_hline(yintercept = meanData, color="green", linetype="dashed", size=1)+ 
    geom_hline(yintercept = meanPlusSd, color="red", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanMinusSd, color="red", linetype="dashed", size=0.5) + 
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
}

#PLOT THE REPARTITION FOR THE WINRATE RATIO OF ARCHETYPE OUT OF PLAYERS
#FOR THE MOST POPULAR ARCHETYPES
#PRESENCE: NUMBER OF MATCHES
winrate_ratio_arch_out_player_graph = function(df,metric_df,beginning,end,EventType){
  
  presence_min=HistShare/100*(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))
  metric_df_2=metric_df[metric_df$TOTAL_NB_MATCHES>=presence_min,]
  
  metric_df_2$ARCHETYPES=reorder(metric_df_2$ARCHETYPES,
                                 metric_df_2$WINRATE_ARCH_OUT_PLAYERS)
  
  titleRatioWin=paste("Ratio of the winrate of the archetypes out of the winrate ", 
                      "of their pilotes for the most popular decks
At least ",HistShare,"% of presence",
                      "\nBetween ",beginning," and ",end," in MTGO ",EventType,sep="")
  
  ggplot(metric_df_2, aes(x=ARCHETYPES, y=WINRATE_ARCH_OUT_PLAYERS)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(WINRATE_ARCH_OUT_PLAYERS,3), nsmall = 3)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y="Ratio", title=titleRatioWin,
         subtitle="Ratio>1 means that the archetype winrate is over its pilots' winrate
         \nby Anael Yahi")+
    geom_hline(yintercept = 1, color="green", linetype="dotted", size=1)+
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
}

#SORT THE ARCHETYPES IN CLUSTERS BASED ON PRESENCE AND WINRATE
kmeans_arch = function (metric_df,k,iter,init,algo,beginning,end,
                        count_wr,only_best,EventType){
  df_elim=select(metric_df, TOTAL_NB_MATCHES, WINRATE_AVERAGE, ARCHETYPES,NB_PLAYERS)
  if(only_best){
    df_elim=df_elim[df_elim$TOTAL_NB_MATCHES>mean(df_elim$TOTAL_NB_MATCHES),]
  }
  df_elim$PRESENCE=df_elim$TOTAL_NB_MATCHES/(sum(df_elim$TOTAL_NB_MATCHES))*100
  if (count_wr){
    df_kde=select(df_elim, PRESENCE, WINRATE_AVERAGE)
  }else{
    df_kde=select(df_elim, PRESENCE)
  }
  
  set.seed(123)
  res.km=kmeans(scale(df_kde), k, iter.max = iter, nstart = init, 
                algorithm = algo)
  df_kde$CLUSTER=factor(res.km$cluster)
  df_kde$ARCHETYPES=df_elim$ARCHETYPES
  df_kde$NB_PLAYERS=df_elim$NB_PLAYERS
  df_kde$WINRATE_AVERAGE=df_elim$WINRATE_AVERAGE
  
  x_label="Presence"
  y_label="Winrate"
  graph_title=paste("Winrates depending on presence:", Classification,"archetypes ", 
                    "between", beginning, "and", end, "in MTGO", EventType,sep = " ")
  graph_subtitle=paste("Clustered in",k,"categories with",algo,"algorithm
by Anael Yahi",sep = " ")
  ggplot(data = df_kde,  mapping = aes(x = PRESENCE, y = WINRATE_AVERAGE, 
                                       colour = CLUSTER)) + 
    coord_cartesian() + theme_bw() + scale_x_continuous(trans = 'log10') + 
    labs(x=x_label, y=y_label, title=graph_title, subtitle=graph_subtitle) +
    geom_text_repel(aes(label=ARCHETYPES),hjust=-1.5, vjust=-0.5,point.padding = NA) + 
    geom_point(aes(size=NB_PLAYERS),show.legend = FALSE)
}

#EronRelentless AND Kanonenfutter ARE THE SAME PERSON
#LIST ALL THE DIFFERENT PLAYERS IN THE DATA
generate_player_list = function(df,beginning,end){
  #CREATE A DATAFRAME CONTAINING THE LIST OF ARCHETYPES
  periodDf=subset(df, DATE >= as.Date(beginning) & DATE < as.Date(end))
  play_list=data.frame(unique(df$PLAYER))
  names(play_list)[1] = c("PLAYERS")
  return(play_list)
}

#EronRelentless AND Kanonenfutter ARE THE SAME PERSON
#FILL IN METRIC POINTS IN A PLAYER DATA FRAME
metric_points_players = function(df,beginning,end){
  df2=subset(df, DATE >= as.Date(beginning) & DATE < as.Date(end))
  #GET THE LIST OF THE DIFFERENT PLAYERS IN THE DATA
  metric_df_players=generate_player_list(df2,beginning,end)
  
  metric_df_players$NO_APPEARANCES=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$TOTAL_NB_MATCHES=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$WINRATE_AVERAGE=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$WINRATE_95_MIN=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$WINRATE_95_MAX=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$TOTAL_POINTS=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$ARCHETYPE_NAMES=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$ARCHETYPE_COUNT=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$URL=rep(0,length(metric_df_players$PLAYERS))
  for (i in 1:length(metric_df_players$PLAYERS)){
    #POSITION OF THE CORRESPONDING EXACT OR SUPER ARCHETYPE IN THE DATA
    play_identification=which(df2$PLAYER==metric_df_players$PLAYERS[i])
    df3=df2[play_identification,]
    #NUMBER OF APPEARANCES IN THE DATA OF THE CORRESPONDING ARCHETYPE
    metric_df_players$NO_APPEARANCES[i]=length(play_identification)
    #LIST OF DIFFERENT ARCHETYPES THAT PLAYER PLAYED
    metric_df_players$ARCHETYPE_NAMES[i]=list(unique(df3$ARCHETYPE))
    #paste(unique(df3$ARCHETYPE),collapse=",")
    arch_count=c()
    for(j in 1:length(metric_df_players$ARCHETYPE_NAMES[[i]])){
      arch_count[j]=length(df3[df3$ARCHETYPE==metric_df_players$
                                 ARCHETYPE_NAMES[[i]][[j]],]$URL)
    }
    metric_df_players$ARCHETYPE_COUNT[i]=list(arch_count)
    
    metric_df_players$URL[i]=list(unique(df3$URL))
    #paste(df3$URL,collapse=",")
    
    #NUMBER OF MATCHES PLAYED BY THAT ARCHETYPE IN THE DATA
    metric_df_players$TOTAL_NB_MATCHES[i]=sum(df3$NB_ROUNDS,
                                              df3$TOP8_MATCHES)
    #NUMBER OF WINS OF THAT ARCHETYPE
    total_wins_arch=sum((df3$POINTS + df3$TOP8_PTS)/3)
    #NUMBER OF MATCHES OF THAT ARCHETYPE
    total_matches_arch=sum(df3$NB_ROUNDS + df3$TOP8_MATCHES)
    metric_df_players$TOTAL_POINTS[i]=total_wins_arch*3
    
    #95% CONFIDENCE INTERVALS OF THE WINRATE
    #EFFECTIVE WINRATE IN THE DATA
    metric_df_players$WINRATE_AVERAGE[i]=binom.test(total_wins_arch, total_matches_arch, 
                                                    p=0.5,alternative="two.sided", 
                                                    conf.level=0.95)$estimate
    #LOWER BOUND OF THE MEASURED WINRATE             
    metric_df_players$WINRATE_95_MIN[i]=binom.test(total_wins_arch, total_matches_arch, 
                                                   p=0.5,alternative="two.sided", 
                                                   conf.level=0.95)$conf.int[1]
    #UPPER BOUND OF THE MEASURED WINRATE 
    metric_df_players$WINRATE_95_MAX[i]=binom.test(total_wins_arch, total_matches_arch, 
                                                   p=0.5,alternative="two.sided", 
                                                   conf.level=0.95)$conf.int[2]
  }
  metric_df_players=arrange(metric_df_players,desc(TOTAL_POINTS))
  
  return(metric_df_players)
}

#GENERATE A DF CONTAINING TIERS LISTS DEPENDING ON VARIOUS METRICS
generate_tiers_lists = function(arch_ranked){
  #TIERS LIST BASED ON METRIC SCORE
  arch_metric_score=arch_ranked[c("ARCHETYPES","METRIC_COMB")]
  arch_metric_score=arrange(arch_metric_score,desc(METRIC_COMB))
  head(arch_metric_score)
  mmc=mean(arch_metric_score$METRIC_COMB)
  sdmc=sd(arch_metric_score$METRIC_COMB)
  arch_metric_score_tiers1=arch_metric_score[arch_metric_score$METRIC_COMB>mmc+sdmc,]
  arch_metric_score_tiers1.5=arch_metric_score[arch_metric_score$METRIC_COMB>mmc &
                                                 arch_metric_score$METRIC_COMB<=mmc+sdmc,]
  arch_metric_score_tiers2=arch_metric_score[arch_metric_score$METRIC_COMB<=mmc &
                                               arch_metric_score$METRIC_COMB>=mmc-sdmc,]
  arch_metric_score_tiers2.5=arch_metric_score[arch_metric_score$METRIC_COMB<mmc-sdmc,]
  arch_metric_score_tiers1
  arch_metric_score_tiers1.5
  arch_metric_score_tiers2
  arch_metric_score_tiers2.5
  
  #TIERS LIST BASED ON VS META SCORE
  arch_meta_score=arch_ranked[c("ARCHETYPES","VS_META_SCORE")]
  arch_meta_score=arrange(arch_meta_score,VS_META_SCORE)
  head(arch_meta_score)
  mms=mean(arch_meta_score$VS_META_SCORE)
  sdms=sd(arch_meta_score$VS_META_SCORE)
  arch_meta_score_tiers2.5=arch_meta_score[arch_meta_score$VS_META_SCORE>mms+sdms,]
  arch_meta_score_tiers2=arch_meta_score[arch_meta_score$VS_META_SCORE>mms &
                                           arch_meta_score$VS_META_SCORE<=mms+sdms,]
  arch_meta_score_tiers1.5=arch_meta_score[arch_meta_score$VS_META_SCORE<=mms &
                                             arch_meta_score$VS_META_SCORE>=mms-sdms,]
  arch_meta_score_tiers1=arch_meta_score[arch_meta_score$VS_META_SCORE<mms-sdms,]
  arch_meta_score_tiers1
  arch_meta_score_tiers1.5
  arch_meta_score_tiers2
  arch_meta_score_tiers2.5
  
  #TIERS LIST BASED ON PRESENCE
  arch_presence_score=arch_ranked[c("ARCHETYPES","TOTAL_NB_MATCHES")]
  arch_presence_score=arrange(arch_presence_score,desc(TOTAL_NB_MATCHES))
  head(arch_presence_score)
  mps=mean(arch_presence_score$TOTAL_NB_MATCHES)
  sdps=sd(arch_presence_score$TOTAL_NB_MATCHES)
  arch_presence_score_tiers1=arch_presence_score[arch_presence_score$TOTAL_NB_MATCHES>mps+sdps,]
  arch_presence_score_tiers1.5=arch_presence_score[arch_presence_score$TOTAL_NB_MATCHES>mps &
                                                     arch_presence_score$TOTAL_NB_MATCHES<=mps+sdps,]
  arch_presence_score_tiers2=arch_presence_score[arch_presence_score$TOTAL_NB_MATCHES<=mps &
                                                   arch_presence_score$TOTAL_NB_MATCHES>=mps-sdps,]
  arch_presence_score_tiers2.5=arch_presence_score[arch_presence_score$TOTAL_NB_MATCHES<mps-sdps,]
  arch_presence_score_tiers1
  arch_presence_score_tiers1.5
  arch_presence_score_tiers2
  arch_presence_score_tiers2.5
  
  #TIERS LIST BASED ON AVERAGE WINRATE
  arch_winrate_score=arch_ranked[c("ARCHETYPES","WINRATE_AVERAGE")]
  arch_winrate_score=arrange(arch_winrate_score,desc(WINRATE_AVERAGE))
  head(arch_winrate_score)
  mws=mean(arch_winrate_score$WINRATE_AVERAGE)
  sdws=sd(arch_winrate_score$WINRATE_AVERAGE)
  arch_winrate_score_tiers1=arch_winrate_score[arch_winrate_score$WINRATE_AVERAGE>mws+sdws,]
  arch_winrate_score_tiers1.5=arch_winrate_score[arch_winrate_score$WINRATE_AVERAGE>mws &
                                                   arch_winrate_score$WINRATE_AVERAGE<=mws+sdws,]
  arch_winrate_score_tiers2=arch_winrate_score[arch_winrate_score$WINRATE_AVERAGE<=mws &
                                                 arch_winrate_score$WINRATE_AVERAGE>=mws-sdws,]
  arch_winrate_score_tiers2.5=arch_winrate_score[arch_winrate_score$WINRATE_AVERAGE<mws-sdws,]
  arch_winrate_score_tiers1
  arch_winrate_score_tiers1.5
  arch_winrate_score_tiers2
  arch_winrate_score_tiers2.5
  
  #TIERS LIST BASED ON LOWER ESTIMATION OF WINRATE
  arch_low_winrate_score=arch_ranked[c("ARCHETYPES","WINRATE_95_MIN")]
  arch_low_winrate_score=arrange(arch_low_winrate_score,desc(WINRATE_95_MIN))
  head(arch_low_winrate_score)
  mlws=mean(arch_low_winrate_score$WINRATE_95_MIN)
  sdlws=sd(arch_low_winrate_score$WINRATE_95_MIN)
  arch_low_winrate_score_tiers1=arch_low_winrate_score[arch_low_winrate_score$WINRATE_95_MIN>mlws+sdlws,]
  arch_low_winrate_score_tiers1.5=arch_low_winrate_score[arch_low_winrate_score$WINRATE_95_MIN>mlws &
                                                           arch_low_winrate_score$WINRATE_95_MIN<=mlws+sdlws,]
  arch_low_winrate_score_tiers2=arch_low_winrate_score[arch_low_winrate_score$WINRATE_95_MIN<=mlws &
                                                         arch_low_winrate_score$WINRATE_95_MIN>=mlws-sdlws,]
  arch_low_winrate_score_tiers2.5=arch_low_winrate_score[arch_low_winrate_score$WINRATE_95_MIN<mlws-sdlws,]
  arch_low_winrate_score_tiers1
  arch_low_winrate_score_tiers1.5
  arch_low_winrate_score_tiers2
  arch_low_winrate_score_tiers2.5
  
  #EXPORT ALL THE TIERS LISTS TO CSV
  low_winrate_score_tiers_list=list(paste(arch_low_winrate_score_tiers1$ARCHETYPES,collapse=" - "),
                                    paste(arch_low_winrate_score_tiers1.5$ARCHETYPES,collapse=" - "),
                                    paste(arch_low_winrate_score_tiers2$ARCHETYPES,collapse=" - "),
                                    paste(arch_low_winrate_score_tiers2.5$ARCHETYPES,collapse=" - "))
  winrate_score_tiers_list=list(paste(arch_winrate_score_tiers1$ARCHETYPES,collapse=" - "),
                                paste(arch_winrate_score_tiers1.5$ARCHETYPES,collapse=" - "),
                                paste(arch_winrate_score_tiers2$ARCHETYPES,collapse=" - "),
                                paste(arch_winrate_score_tiers2.5$ARCHETYPES,collapse=" - "))
  presence_score_tiers_list=list(paste(arch_presence_score_tiers1$ARCHETYPES,collapse=" - "),
                                 paste(arch_presence_score_tiers1.5$ARCHETYPES,collapse=" - "),
                                 paste(arch_presence_score_tiers2$ARCHETYPES,collapse=" - "),
                                 paste(arch_presence_score_tiers2.5$ARCHETYPES,collapse=" - "))
  meta_score_tiers_list=list(paste(arch_meta_score_tiers1$ARCHETYPES,collapse=" - "),
                             paste(arch_meta_score_tiers1.5$ARCHETYPES,collapse=" - "),
                             paste(arch_meta_score_tiers2$ARCHETYPES,collapse=" - "),
                             paste(arch_meta_score_tiers2.5$ARCHETYPES,collapse=" - "))
  metric_score_tiers_list=list(paste(arch_metric_score_tiers1$ARCHETYPES,collapse=" - "),
                               paste(arch_metric_score_tiers1.5$ARCHETYPES,collapse=" - "),
                               paste(arch_metric_score_tiers2$ARCHETYPES,collapse=" - "),
                               paste(arch_metric_score_tiers2.5$ARCHETYPES,collapse=" - "))
  
  df_tiers_list=data.frame(presence=I(presence_score_tiers_list),
                           winrate=I(winrate_score_tiers_list),
                           low_winrate=I(low_winrate_score_tiers_list),
                           comb_presence_winrate=I(metric_score_tiers_list),
                           meta_score=I(metric_score_tiers_list)
  )
  
  rownames(df_tiers_list)=c("Tiers 1","Tiers 1.5","Tiers 2","Tiers 2.5")
  
  return(df_tiers_list)
}

#EronRelentless AND Kanonenfutter ARE THE SAME PERSON
#COUNT THE NUMBER OF TOP8 FOR EACH PLAYER
players_top8 = function(df,beginning,end) {
  
  #REMOVES ALL THE PLAYERS WHOSE RESULT 
  top8results = c("1st Place","2nd Place", "3rd Place", "4th Place", "5th Place", 
                  "6th Place", "7th Place", "8th Place")
  dftop8=df[df$RESULT %in% top8results,]
  top8players=generate_player_list(dftop8,beginning,end)
  top8players$NO_APPEARANCES=rep(1,length(top8players$PLAYERS))
  for (i in 1:length(top8players$NO_APPEARANCES)){
    play_identification=which(dftop8$PLAYER==top8players$PLAYERS[i])
    top8players_sub=dftop8[play_identification,]
    #NUMBER OF APPEARANCES IN THE DATA OF THE CORRESPONDING ARCHETYPE
    top8players$NO_APPEARANCES[i]=length(play_identification)
    #LIST OF DIFFERENT ARCHETYPES THAT PLAYER PLAYED
    top8players$ARCHETYPE_NAMES[i]=list(unique(top8players_sub$ARCHETYPE))
    arch_count=c()
    for(j in 1:length(top8players$ARCHETYPE_NAMES[[i]])){
      arch_count[j]=length(top8players_sub[top8players_sub$ARCHETYPE==top8players$
                                             ARCHETYPE_NAMES[[i]][[j]],]$URL)
    }
    top8players$ARCHETYPE_COUNT[i]=list(arch_count)
    
    top8players$URL[i]=list(unique(top8players_sub$URL))
  }
  top8players=arrange(top8players,desc(NO_APPEARANCES))
  return(top8players)
}


#4th file to execute
#Import the decklists of all events in the chosen period

#Data cloned from: https://github.com/Badaro/MTGODecklistCache 

#("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")

#CREATE DF TO CONTAIN THE LIST OF PLAYED CARDS: OVERALL, MD AND SB
FormatDataGetter = function(){
  
  DaysList=seq(as.Date(Beginning), as.Date(End), by="days")
  formatName=tolower(MTGFormat)
  FormatResultsPaths=c()
  #Event type:
  #"Official Competitions" = Major Official Events + Prelims
  #"Challenges" = Challenges
  #"Preliminaries" = Prelims
  #"All Events" = everything available including leagues
  #"Everything but Leagues" = Major Events, official or unofficial, and Prelims
  #"Major Events Top32" = Major Events, official or unofficial, only top32
  #"Major Events" = Major Events, official or unofficial
  #"ManaTraders Series" = ManaTraders Series
  #"NRG Series" = NRG Series
  #"Major Official Events" = Challenge,Champ,Showcase,Premier,Qualifier,MOCS
  #PATTERNS OF THE FILE NAMES DEPENDING ON THE EVENT TYPES WE USE
  if (EventType=="Everything but Leagues"){
    eventPattern=paste(formatName,"-[^league].*?-\\d{4}-\\d{1,2}-\\d{1,2}(-\\d)?\\.json")
  }else if (EventType=="Challenges"){
    eventPattern=paste(formatName,"-.*?[challenge].*?-\\d{4}-\\d{1,2}-\\d{1,2}(-\\d)?\\.json")
  }else if (EventType=="Major Events"){
    eventPattern=paste(formatName,"-[^league|preliminary].*?-\\d{4}-\\d{1,2}-\\d{1,2}(-\\d)?\\.json")
  }else if (EventType=="Major Events Top32"){
    eventPattern=paste(formatName,"-[^league|preliminary].*?-\\d{4}-\\d{1,2}-\\d{1,2}(-\\d)?\\.json")
  }else if (EventType=="Preliminaries"){
    eventPattern=paste(formatName,"-preliminary.*?-\\d{4}-\\d{1,2}-\\d{1,2}(-\\d)?\\.json")
  }else if (EventType=="All Events"){
    eventPattern=paste(formatName,"-*?-\\d{4}-\\d{1,2}-\\d{1,2}(-\\d)?\\.json")
  }
  
  #LIST THE PATHS OF ALL THE RELEVANT FILES
  for (i in 1:length(DaysList)){
    yeari=substr(x = DaysList[i], start = 1, stop = 4)
    monthi=substr(x = DaysList[i], start = 6, stop = 7)
    dayi=substr(x = DaysList[i], start = 9, stop = 10)
    MTGODataPathDate=paste(MTGODataPath,yeari,monthi,dayi,sep = "/")
    FormatResultsPaths=c(FormatResultsPaths,dir(
      MTGODataPathDate, recursive=TRUE, full.names=TRUE, pattern=eventPattern))
  }
  
  FormatResults=data.frame()
  #IMPORT ALL THE DECKS DATA FROM EACH EVENT FOR EACH DATE
  #THE IMPORTANT DATA IS CONTAINED IN $Deck, USING ONLY IT MAKES THE MANIPULATION 
  #MUCH EASIER
  #jsonlite APPEARS AS THE BEST LIBRARY FOR THE IMPORT HERE (BETTER NAMES AND RBIND)
  for (i in 1:length(FormatResultsPaths)){
    FormatResults=rbind(FormatResults, jsonlite::fromJSON(FormatResultsPaths[i])$Deck)
  }
  
  # USE unique(substring(FormatResults$AnchorUri,1,80)) TO CHECK WHETHER THE EVENTS
  # ARE CORRECTLY FILTERED
  
  #GET THE MD DATA
  FormatMDList=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                        c("CardNames", "CardCount"))
  for (i in 1:length(FormatResults$Mainboard)){
    for (j in 1:length(FormatResults$Mainboard[[i]]$CardName)){
      FormatMDList = rbind(FormatMDList, c(FormatResults$Mainboard[[i]]$CardName[[j]],
                                           (FormatResults$Mainboard[[i]]$Count[[j]])))
      names(FormatMDList)=c("CardNames","CardCount")
    }
  }
  #GET THE SB DATA
  FormatSBList=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                        c("CardNames", "CardCount"))
  for (i in 1:length(FormatResults$Sideboard)){
    for (j in 1:length(FormatResults$Sideboard[[i]]$CardName)){
      FormatSBList = rbind(FormatSBList, c(FormatResults$Sideboard[[i]]$CardName[[j]],
                                           (FormatResults$Sideboard[[i]]$Count[[j]])))
      names(FormatSBList)=c("CardNames","CardCount")
    }
  }
  
  #AGGREGATES THE DATA
  FormatAllResults=rbind(FormatMDList,FormatSBList)
  
  return(list(Raw = FormatResults,MD = FormatMDList,SB = FormatSBList, 
              All = FormatAllResults))
}

#CAN TAKE SOME TIME TO EXECUTE, LEAVE IT A MINUTE OR TWO EVENTUALLY
FormatData=FormatDataGetter()

#ADD THE DECKLISTS OF EACH DECK TO THE MAIN DATAFRAME
DecklistsAdd = function(FormatData,df){
  df$MD_TEMP=rep(NA,length(df$URL))
  df$SB_TEMP=rep(NA,length(df$URL))
  df$DL_TEMP=rep(NA,length(df$URL))
  df$MDCounts=rep(NA,length(df$URL))
  df$MDCards=rep(NA,length(df$URL))
  df$SBCounts=rep(NA,length(df$URL))
  df$SBCards=rep(NA,length(df$URL))
  df$DLCountsList=rep(NA,length(df$URL))
  df$DLCards=rep(NA,length(df$URL))
  for (i in 1:length(df$URL)){
    #LIST OF MD CARDS
    if(!is.na(df$URL[i])){
      df$MD_TEMP[i]=FormatData$Raw[FormatData$Raw$AnchorUri==df$URL[i],]$Mainboard
      #LIST OF SB CARDS
      df$SB_TEMP[i]=FormatData$Raw[FormatData$Raw$AnchorUri==df$URL[i],]$Sideboard
    }else{ #FOR A SINGLE EVENT FILE
      df$MD_TEMP[i]=FormatData$Raw$Mainboard[i]
      #LIST OF SB CARDS
      df$SB_TEMP[i]=FormatData$Raw$Sideboard[i]
    }
    
    #COMBINE BOTH LISTS WHILE SUMMING DUPLICATES
    tempDl=merge(df$MD_TEMP[i][[1]], df$SB_TEMP[i][[1]], by="CardName", all = T)
    tempDl[is.na(tempDl)] = 0
    tempDl$Count=tempDl$Count.x+tempDl$Count.y
    df$DL_TEMP[i]=list(tempDl[c("CardName","Count")])
    
    df$MDCounts[i]=list(df$MD_TEMP[i][[1]]$Count)
    df$MDCards[i]=list(df$MD_TEMP[i][[1]]$CardName)
    
    df$SBCounts[i]=list(df$SB_TEMP[i][[1]]$Count)
    df$SBCards[i]=list(df$SB_TEMP[i][[1]]$CardName)
    
    df$DLCountsList[i]=list(df$DL_TEMP[i][[1]]$Count)
    df$DLCards[i]=list(df$DL_TEMP[i][[1]]$CardName)
  }
  #THE 3 COLUMNS BELOW WERE USED AS TEMPORARY SAVING SPACES
  df$MD_TEMP=NULL
  df$SB_TEMP=NULL
  df$DL_TEMP=NULL
  
  return(df)
}

df=DecklistsAdd(FormatData,df)

#FormatCardsList = dataframe containing the list of cards you want to analyse
#board = "All","MD or "SB
#returns a dataframe containing statistics for each card in the data: names, 
#number of copies per deck/overall/on average, number of decks, presence  
FormatCardsStatsGetter = function(FormatCardsList,board){
  
  #GET THE NAMES OF EACH DIFFERENT CARD
  FormatCardsNames=unique(FormatCardsList[[board]]$CardNames)
  #NUMBER OF DIFFERENT CARDS
  Nb_diff_cards=length(FormatCardsNames)
  #GET THE NUMBER OF COPIES OF EACH DIFFERENT CARD
  FormatCardsCounts=rep(0,Nb_diff_cards)
  for (i in 1:Nb_diff_cards){
    FormatCardsCounts[i]=sum(as.numeric(FormatCardsList[[board]][which(
      FormatCardsList[[board]]$CardNames==FormatCardsNames[i]),]$CardCount))
  }
  
  #GET THE NUMBER OF DECKS WHERE EACH CARD WAS PLAYED
  FormatDecksCounts=rep(0,Nb_diff_cards)
  for (i in 1:Nb_diff_cards){
    FormatDecksCounts[i]=length(FormatCardsList[[board]][which(
      FormatCardsList[[board]]$CardNames==FormatCardsNames[i]),]$CardCount)
  }
  
  #ASSOCIATE THE NAMES OF EACH DIFFERENT CARD WITH THEIR TOTAL NUMBER OF COPIES AND
  #THE NUMBER OF DECKS PLAYING THEM
  FormatCards=setNames(data.frame(FormatCardsNames,FormatCardsCounts,FormatDecksCounts), 
                       c("CardNames", "CardCount", "DeckCount"))
  #TOTAL NUMBER OF CARDS PLAYED IN THE EVENTS
  NbTotalCards=sum(FormatCards$CardCount)
  
  #GET THE % PRESENCE OF EACH CARD OUT OF ALL THE CARDS PLAYED
  FormatCards$CardCountOutTotalCards=as.numeric(format(round(
    FormatCards$CardCount*100/NbTotalCards,1), nsmall = 1))
  
  #TOTAL NUMBER OF DECKS PLAYED IN THE EVENTS
  NbTotalDecks=length(FormatCardsList$Raw$AnchorUri)
  
  #GET THE % OF DECKS PLAYING EACH CARD
  FormatCards$DeckCountOutTotalDecks=as.numeric(format(round(
    FormatCards$DeckCount*100/NbTotalDecks,1), nsmall = 1))
  
  #GET THE AVERAGE NUMBER OF COPIES OF EACH CARD
  FormatCards$AverageCopies=as.numeric(format(round(
    FormatCards$CardCount/FormatDecksCounts,1), nsmall = 1))
  
  return(FormatCards)
}

#STATS OF CARDS OVERALL
FormatCardsStats=FormatCardsStatsGetter(FormatData,"All")
#STATS OF MD CARDS
FormatMDStats=FormatCardsStatsGetter(FormatData,"MD")
#STATS OF SB CARDS
FormatSBStats=FormatCardsStatsGetter(FormatData,"SB")


#5th file to execute
#Imports all the data for each existing card: name, color, sets, mana costs,
#types...

#DATA FROM: https://mtgjson.com/downloads/all-files/
setwd(DirectoryFile)
#IMPORT ALL THE DATA FOR ALL THE CARDS IN THE GAME
cardData=read.csv("cards.csv",sep=",",header=T)
#KEEP ONLY RELEVANT INFORMATION AND REMOVE DUPLICATES - CARDS PRINTED 
#MULTIPLE TIMES
cardDataSub=unique(subset(cardData,select=c(
  artist,colors,convertedManaCost,faceName,isReprint,layout,manaCost,name,
  setCode,subtypes,supertypes,type,types)))

#filtrer pour ne garder que les cartes présentes dans les résultats au lieu de 
#toutes les cartes du jeu ? 

#ADD A COLUMN WITH THE AVERAGE CONVERTED MANA COST OF EACH DECK (WITH OR WITHOUT
#CONSIDERING LANDS, WHICH ALWAYS HAVE A CMC OF 0)
addCMC = function(df){
  for (i in 1:length(df$URL)){
    
    list_cmc_lands=rep(0,length(df$MDCards[[i]]))
    for (j in 1:length(df$MDCards[[i]])){
      if (df$MDCards[[i]][j] %in% cardDataSub$name){
        list_cmc_lands[j]=cardDataSub[cardDataSub$name==df$MDCards[[i]][j],]$
          convertedManaCost
      }else if (df$MDCards[[i]][j] %in% cardDataSub$faceName){
        list_cmc_lands[j]=cardDataSub[cardDataSub$faceName==df$MDCards[[i]][j],]$
          convertedManaCost
      }
    }
    
    df$CMC_LANDS[i]=sum(list_cmc_lands*df$MDCounts[[i]])/sum(df$MDCounts[[i]])
    
    list_cmc=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("CMC", "COUNT"))
    
    for (j in 1:length(df$MDCards[[i]])){
      if (df$MDCards[[i]][j] %in% cardDataSub$name){
        if(!cardDataSub[cardDataSub$name==df$MDCards[[i]][j],]$types=="Land"){
          list_cmc_temp=data.frame(cardDataSub[cardDataSub$name==df$MDCards[[i]][j],]$
                                     convertedManaCost,df$MDCounts[[i]][j])
          names(list_cmc_temp)=c("CMC", "COUNT")
          list_cmc=rbind(list_cmc, list_cmc_temp)
        }
      }else if (df$MDCards[[i]][j] %in% cardDataSub$faceName){
        if(!cardDataSub[cardDataSub$faceName==df$MDCards[[i]][j],]$types=="Land"){
          list_cmc_temp=data.frame(cardDataSub[cardDataSub$faceName==df$MDCards[[i]][j],]$
                                     convertedManaCost,df$MDCounts[[i]][j])
          names(list_cmc_temp)=c("CMC", "COUNT")
          list_cmc=rbind(list_cmc, list_cmc_temp)
        }
      }
    }
    
    df$CMC[i]=sum(list_cmc$CMC*list_cmc$COUNT)/sum(list_cmc$COUNT)
  }
  
  return(df)
}

#IT CAN BE A BIT LONG TO EXECUTE, LEAVE IT A MINUTE OR TWO
#TODO: REPAIR THE FUNCTION:
# Error in if (df$MDCards[[i]][j] %in% cardDataSub$name) { : 
#     l'argument est de longueur nulle
df=addCMC(df)


#6th file to execute
#Selects a specific archetype in the data for which we try to optimize the 
#decklist through an analysis of the number of different cards

#TO BE MANUALLY CHOSEN AFTER ANALYSIS OF ALL THE METRICS
#KEEP ONLY THE DATA OF A SINGLE ARCHETYPE
#bestArchetype="Urza Oko"

#ADD THE NUMBER OF CARDS HAVING AT LEAST A SPECIFIC TYPE OF EACH DECK
#USE THE FOLLOWING COMMAND TO FIND ALL THE TYPES YOU CAN USE:
#unique(cardDataSub$types)
addRatioType = function(df,type){
  #GENERATE THE NAME OF THE COLUMN BASED ON THE TYPE
  rowNameType=toupper(paste("RATIO",type,sep="_"))
  #CREATE THE COLUMN
  df[[rowNameType]]=rep(0,length(df$URL))
  #FILL THE COLUMN WITH THE NUMBER OF CARDS OF THE CHOSEN TYPE IN EACH DECK
  for (i in 1:length(df$URL)){
    landcount=0
    for (j in 1:length(df$MDCards[[i]])){
      if (df$MDCards[[i]][j] %in% cardDataSub$name){
        if((length(grep(type,cardDataSub[cardDataSub$name==df$MDCards[[i]][j],]$types))==1)&&
           (grep(type,cardDataSub[cardDataSub$name==df$MDCards[[i]][j],]$types)==1)){
          landcount=landcount+df$MDCounts[[i]][j]
        }
      }else if (df$MDCards[[i]][j] %in% cardDataSub$faceName){
        if((length(grep(type,cardDataSub[cardDataSub$faceName==df$MDCards[[i]][j],]$types))==1)&&
           (grep(type,cardDataSub[cardDataSub$faceName==df$MDCards[[i]][j],]$types)==1)){
          landcount=landcount+df$MDCounts[[i]][j]
        }
      }
    }
    df[[rowNameType]][i]=landcount/df$TOTAL_MD[i]
  }
  return(df)
}

infos_best_arch=function(df,bestArchetype){
  df_best_arch=df[df$ARCHETYPE==bestArchetype,]
  
  #ADD THE NUMBER OF MAINDECK CARDS FOR EACH DECK - DEFAULT IS 60
  df_best_arch$TOTAL_MD=rep(60,length(df_best_arch$URL))
  for (i in 1:length(df_best_arch$TOTAL_MD)){
    df_best_arch$TOTAL_MD[i]=sum(df_best_arch$MDCounts[[i]])
  }
  
  #ADD THE WINRATE OF EACH DECK
  for (i in 1:length(df_best_arch$URL)){
    #NUMBER OF WINS OF EACH DECK
    total_wins_deck=(df_best_arch$POINTS[i] + df_best_arch$TOP8_PTS[i])/3
    #NUMBER OF MATCHES OF EACH DECK
    total_matches_deck=df_best_arch$NB_ROUNDS[i] + df_best_arch$TOP8_MATCHES[i]
    #WINRATE OF EACH DECK
    df_best_arch$WINRATE=total_wins_deck/total_matches_deck
  }
  #IT CAN BE A BIT LONG TO EXECUTE, LEAVE IT A MINUTE OR TWO
  df_best_arch=addRatioType(df_best_arch,"Land")
  df_best_arch=addRatioType(df_best_arch,"Creature")
  df_best_arch=addRatioType(df_best_arch,"Planeswalker")
  df_best_arch=addRatioType(df_best_arch,"Instant")
  df_best_arch=addRatioType(df_best_arch,"Sorcery")
  df_best_arch=addRatioType(df_best_arch,"Artifact")
  df_best_arch=addRatioType(df_best_arch,"Enchantment")
  
  sbCards=unique(unlist(df_best_arch$SBCards))
  for (i in 1:length(sbCards)){
    df_best_arch=isPresentSBCard(df_best_arch,sbCards[i])
  }
  
  mdCards=unique(unlist(df_best_arch$MDCards))
  for (i in 1:length(mdCards)){
    df_best_arch=isPresentMDCard(df_best_arch,mdCards[i])
  }
  
  return(df_best_arch)
}

#names(df_best_arch)

#unique(unlist(df_best_arch$SBCards))
#table(unlist(df_best_arch$SBCards))
#length(df_best_arch$URL)

#ADD A VARIABLE TELLING WHETHER cardName IS IN THE SB (1) OR NOT (0)
isPresentSBCard = function(df_best_arch,cardName){
  #GENERATE THE NAME OF THE COLUMN BASED ON THE TYPE
  rowName=gsub('[[:punct:] ]+',' ',cardName)
  rowName=chartr(" ", "_", rowName)
  rowNameCard=toupper(paste("SB_PRESENCE",rowName,sep="_"))
  
  #CREATE THE COLUMN
  df_best_arch[[rowNameCard]]=rep(0,length(df_best_arch$URL))
  #FILL THE COLUMN WITH THE NUMBER OF CARDS OF THE CHOSEN TYPE IN EACH DECK
  for (i in 1:length(df_best_arch$URL)){
    if(cardName %in% df_best_arch$SBCards[[i]]){
      df_best_arch[[rowNameCard]][i]=1
    }
  }
  return(df_best_arch)
}

#ADD A VARIABLE TELLING WHETHER cardName IS IN THE MD (1) OR NOT (0)
isPresentMDCard = function(df_best_arch,cardName){
  #GENERATE THE NAME OF THE COLUMN BASED ON THE TYPE
  rowName=gsub('[[:punct:] ]+',' ',cardName)
  rowName=chartr(" ", "_", rowName)
  rowNameCard=toupper(paste("MD_PRESENCE",rowName,sep="_"))
  
  #CREATE THE COLUMN
  df_best_arch[[rowNameCard]]=rep(0,length(df_best_arch$URL))
  #FILL THE COLUMN WITH THE NUMBER OF CARDS OF THE CHOSEN TYPE IN EACH DECK
  for (i in 1:length(df_best_arch$URL)){
    if(cardName %in% df_best_arch$MDCards[[i]]){
      df_best_arch[[rowNameCard]][i]=1
    }
  }
  return(df_best_arch)
}

#COMPUTES THE AVERAGE CHARACTERISTICS OF THE ARCHETYPE
#AVERAGE RATIO OF EACH TYPE OF CARD, AVERAGE NUMBER OF EACH CARD IN MD/SB...
archAverageData = function(df,bestArchetype){
  df_best_arch=df[df$ARCHETYPE==bestArchetype,]
  cardsList=unique(c(unlist(df_best_arch$SBCards),unlist(df_best_arch$MDCards)))
  averageCards=setNames(data.frame(matrix(ncol = 6, nrow = 0)), 
                        c("CardName","Types","AvgMdCount", "AvgSbCount",
                          "AlwaysMDPresent","AlwaysSBPresent"))
  
  for (i in 1:length(cardsList)){
    cardNamei=cardsList[i]
    
    if (cardNamei %in% cardDataSub$name){
      cardTypei=cardDataSub[cardDataSub$name==cardNamei,]$types
    }else if (cardNamei %in% cardDataSub$faceName){
      cardTypei=cardDataSub[cardDataSub$faceName==cardNamei,]$types
    }
    
    AvgMdCounti=0
    AvgSbCounti=0
    AlwaysMDPresenti=TRUE
    AlwaysSBPresenti=TRUE
    
    for (j in 1:length(df_best_arch$URL)){
      #1 == TRUE
      #0 == FALSE
      if (!is.na(match(cardNamei,df_best_arch$MDCards[[j]]))){
        AvgMdCounti=AvgMdCounti+df_best_arch$MDCounts[[j]][match(
          cardNamei, df_best_arch$MDCards[[j]])]
      }else{
        AlwaysMDPresenti=FALSE
      }
      if (!is.na(match(cardNamei,df_best_arch$SBCards[[j]]))){
        AvgSbCounti=AvgSbCounti+df_best_arch$SBCounts[[j]][match(
          cardNamei, df_best_arch$SBCards[[j]])]
      }else{
        AlwaysSBPresenti=FALSE
      }
    }
    
    AvgMdCounti=as.numeric(format(round(AvgMdCounti/length(df_best_arch$URL),2),
                                  nsmall = 2))
    
    AvgSbCounti=as.numeric(format(round(AvgSbCounti/length(df_best_arch$URL),2),
                                  nsmall = 2))
    
    averageCardsTmpi=data.frame(cardNamei,cardTypei,AvgMdCounti,AvgSbCounti,
                                AlwaysMDPresenti,AlwaysSBPresenti)
    
    
    names(averageCardsTmpi)=c("CardName","Types","AvgMdCount", "AvgSbCount",
                              "AlwaysMDPresent","AlwaysSBPresent")
    averageCards=rbind(averageCards, averageCardsTmpi)
  }
  return(averageCards)
}

#GENERATE AN AVERAGE DECKLIST ROUNDING DOWN THE NUMBER OF CARDS
Average_decklist_round_down= function(df_avg_cards,avgCardRatio){
  MDCardsByType=list(arrange(df_avg_cards[df_avg_cards$Types=="Land",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Creature",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Planeswalker",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Instant",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Sorcery",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Artifact",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Enchantment",],
                             desc(AvgMdCount)))
  averageMD=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                     c("CardName","CardCount"))
  for (i in 1:length(MDCardsByType)){
    k=0
    j=0
    notEnoughCards=TRUE
    cardList=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                      c("CardName","CardCount"))
    while (notEnoughCards){
      j=j+1
      cardNameij=MDCardsByType[[i]][j,]$CardName
      cardCountij=round(MDCardsByType[[i]][j,]$AvgMdCount)
      
      k=sum(cardList$CardCount)+cardCountij
      
      if(k<avgCardRatioRound[[i]] && avgCardRatioRound[[i]]!=0 && cardCountij !=0){
        cardij=data.frame(cardNameij,cardCountij)
        names(cardij)=c("CardName","CardCount")
        cardList=rbind(cardList,cardij)
      }else{
        notEnoughCards=FALSE
      }
    }
    averageMD=rbind(averageMD,cardList)
  }
  return(averageMD)
}

#GENERATE AN AVERAGE DECKLIST ROUNDING UP THE NUMBER OF CARDS
Average_decklist_round_up= function(df_avg_cards,avgCardRatio){
  MDCardsByType=list(arrange(df_avg_cards[df_avg_cards$Types=="Land",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Creature",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Planeswalker",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Instant",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Sorcery",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Artifact",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Enchantment",],
                             desc(AvgMdCount)))
  averageMD=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                     c("CardName","CardCount"))
  for (i in 1:length(MDCardsByType)){
    k=0
    j=0
    notEnoughCards=TRUE
    cardList=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                      c("CardName","CardCount"))
    while (notEnoughCards){
      j=j+1
      cardNameij=MDCardsByType[[i]][j,]$CardName
      cardCountij=round(MDCardsByType[[i]][j,]$AvgMdCount)
      
      if(cardCountij!=0){
        k=sum(cardList$CardCount)+cardCountij
        cardij=data.frame(cardNameij,cardCountij)
        names(cardij)=c("CardName","CardCount")
        cardList=rbind(cardList,cardij)
      }
      if(k>=avgCardRatioRound[[i]] | avgCardRatioRound[[i]]==0 | cardCountij==0){
        notEnoughCards=FALSE
      }
    }
    averageMD=rbind(averageMD,cardList)
  }
  return(averageMD)
}

#Generate the basic structure of an average SB
Average_SB= function(df_avg_cards){
  averageSB=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                     c("CardName","CardCount"))
  SBCardsSorted=arrange(df_avg_cards, desc(AvgSbCount ))
  k=0
  j=0
  notEnoughCards=TRUE
  cardList=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                    c("CardName","CardCount"))
  while (notEnoughCards){
    j=j+1
    cardNameij=SBCardsSorted[j,]$CardName
    cardCountij=round(SBCardsSorted[j,]$AvgSbCount)
    
    if(k<15 && cardCountij !=0){
      cardij=data.frame(cardNameij,cardCountij)
      names(cardij)=c("CardName","CardCount")
      cardList=rbind(cardList,cardij)
    }else{
      notEnoughCards=FALSE
    }
  }
  averageSB=rbind(averageSB,cardList)
  return(averageSB)
}

#8TH FILE
#STATS OF CARDS OVERALL
FormatCardsList=unique(unlist(df$DLCards))

#LIST OF DIFFERENT CARDS
CardResults=setNames(data.frame(matrix(ncol = 1, nrow = length(FormatCardsList))), 
                     c("CardNames"))

#A BIT LONG TO EXECUTE, LEAVE IT A FEW MINUTES
for (i in 1:length(FormatCardsList)){
  CardResults$CardNames[i]=FormatCardsList[i]
  
  #DOES NOT HANDLE WEAR//TEAR (SPLIT CARDS) VERY WELL!
  if (CardResults$CardNames[i] %in% cardDataSub$name){
    cardDataSubi=cardDataSub[cardDataSub$name==CardResults$CardNames[i],]
  }else if (CardResults$CardNames[i] %in% cardDataSub$faceName){
    cardDataSubi=cardDataSub[cardDataSub$faceName==CardResults$CardNames[i],]
  }
  cardDataSubi=cardDataSubi[cardDataSubi$isReprint==0,][1,]
  
  if (length(cardDataSubi$faceName)>1){print(i)}
  
  CardResults$Artists[i]=cardDataSubi$artist
  
  CardResults$Colors[i]=cardDataSubi$colors
  
  CardResults$CMC[i]=cardDataSubi$convertedManaCost
  
  CardResults$Types[i]=cardDataSubi$types
  
  CardResults$FirstSet[i]=cardDataSubi$setCode 
  
  dfCardi=df[grep(CardResults$CardNames[i],df$DLCards),]
  
  CardResults$NbDecks[i]=length(dfCardi$URL)
  
  CardResults$PercDecks[i]=CardResults$NbDecks[i]/length(df$URL)*100
  
  CardResults$CardWins[i]=sum((dfCardi$POINTS + 
                                 dfCardi$TOP8_PTS)/3)
  CardResults$CardMatches[i]=sum(dfCardi$NB_ROUNDS + 
                                   dfCardi$TOP8_MATCHES)
  
  CardResults$Presence[i]=as.numeric(format(round(
    CardResults$CardMatches[i]/sum(df$NB_ROUNDS + df$TOP8_MATCHES)*100,1), 
    nsmall = 1))
  
  AvgMdCounti=0
  AvgSbCounti=0
  
  for (j in 1:length(dfCardi$URL)){
    #1 == TRUE
    #0 == FALSE
    if (!is.na(match(CardResults$CardNames[i],dfCardi$MDCards[[j]]))){
      AvgMdCounti=AvgMdCounti+dfCardi$MDCounts[[j]][match(
        CardResults$CardNames[i], dfCardi$MDCards[[j]])]
    }
    if (!is.na(match(CardResults$CardNames[i],dfCardi$SBCards[[j]]))){
      AvgSbCounti=AvgSbCounti+dfCardi$SBCounts[[j]][match(
        CardResults$CardNames[i], dfCardi$SBCards[[j]])]
    }
  }
  
  AvgMdCounti=as.numeric(format(round(AvgMdCounti/length(dfCardi$URL),2),
                                nsmall = 2))
  CardResults$AvgMdCount[i]=AvgMdCounti
  
  AvgSbCounti=as.numeric(format(round(AvgSbCounti/length(dfCardi$URL),2),
                                nsmall = 2))
  CardResults$AvgSbCount[i]=AvgSbCounti
  
  CardResults$PercCopies[i]=CardResults$PercDecks[i]*(
    CardResults$AvgMdCount[i] + CardResults$AvgSbCount[i])/4
  
  #CI WITH BINOMIAL TEST
  CardResults$WINRATE_AVERAGE[i]=as.numeric(format(round(
    binom.test(CardResults$CardWins[i], CardResults$CardMatches[i], p=0.5,
               alternative="two.sided", conf.level=0.95)$estimate*100,1), 
    nsmall = 1))
  
  
  CardResults$WINRATE_95_MIN[i]=as.numeric(format(round(
    binom.test(CardResults$CardWins[i], CardResults$CardMatches[i], p=0.5,
               alternative="two.sided", conf.level=0.95)$conf.int[1]*100,1), 
    nsmall = 1))
  
  CardResults$WINRATE_95_MAX [i]=as.numeric(format(round(
    binom.test(CardResults$CardWins[i], CardResults$CardMatches[i], p=0.5,
               alternative="two.sided", conf.level=0.95)$conf.int[2]*100,1), 
    nsmall = 1))
  
  CardResults$Archetypes[i]=list(unique(df[
    grep(CardResults$CardNames[i],df$DLCards),]$ARCHETYPE))
  
  CardResults$URL[i]=list(df[
    grep(CardResults$CardNames[i],df$DLCards),]$URL)
}

#CREATE THE DIRECTORY WHERE TO SAVE THE PICTURES
setwd(DirectoryFile)
# eventList=c("Challenges","Competitions","Preliminaries")
# EventType=eventList[1]
dir.create(file.path(DirectoryFile,MTGFormat))
dir.create(file.path(paste(DirectoryFile,MTGFormat,sep="/"), 
                     paste(Beginning,End,sep="_")))
dir.create(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                           sep="/"), EventType))

dir.create(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                           EventType,sep="/"), "Results_as_pictures"))
dir.create(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                           EventType,sep="/"), "Results_as_csv"))
dir.create(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                           EventType,sep="/"), "Results_as_txt"))


#LOOKS LIKE A "FOR" WON'T WORK TO EXPORT THE GRAPHS

#####################
# df=generate_df(rawData,EventType,MTGFormat)
# df=add_super_archetypes(df)

#GRAPHIC ANALYSIS
metric_df=metric_points_archetypes(df,Beginning,End)
metric_df_log_matches=metric_df
metric_df_log_matches$TOTAL_NB_MATCHES=
  log(metric_df_log_matches$TOTAL_NB_MATCHES)

#################################################################################

#WORKING DIRECTORY FOR THE CSV
setwd(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                      EventType,sep="/"), "Results_as_csv"))

# write.csv(metric_df,paste(Beginning,'-',End,'_DF_Archetypes_Results.csv',sep=''), 
#           row.names = TRUE)

#PRINT WINRATES OF ALL THE PLAYED CARDS
CardResults = CardResults[order(-CardResults$NbDecks),]

CardResults2CSV=CardResults
CardResults2CSV[,"Archetypes"]=NA
CardResults2CSV[,"URL"]=NA
for (i in 1:length(CardResults2CSV$CardName)){
  CardResults2CSV$Archetypes[i]=paste(unlist(
    CardResults$Archetypes[i]),collapse = "; ")
  
  CardResults2CSV$URL[i]=paste(unlist(CardResults$URL[i]),
                                     collapse = "; ")
}

CardResults=CardResults2CSV

write.csv(arrange(CardResults,CardResults$CardNames), paste(
  Beginning,'-',End,'_DF_All_Cards.csv',sep=''),row.names = TRUE)


#SELECTS ONLY NONLAND CARDS
CardResultsNonLand=CardResults[-grep("Land",CardResults$Types),]
CardResultsNonLand = CardResultsNonLand[order(-CardResultsNonLand$NbDecks),]
write.csv(arrange(CardResultsNonLand,CardNames), paste(
  Beginning,'-',End,'_DF_Nonland_Cards.csv',sep=''),row.names = TRUE)

#PRINT WINRATES OF THE MOST PLAYED CARDS (WITH THE MOST MATCHES)
mostPlayedCards=subset(CardResults[1:50,],select = c(CardNames,NbDecks,
                                                     WINRATE_AVERAGE))
mostPlayedCards=arrange(mostPlayedCards,desc(NbDecks))
write.csv(mostPlayedCards, paste(Beginning,'-',End,'_DF_Most_Played_Cards.csv',
                                 sep=''),row.names = TRUE)

#PRINT WINRATES OF THE TOP CARDS WITH THE BEST WINRATES
CardResults = CardResults[order(-CardResults$WINRATE_AVERAGE),]
highestWinrateCards=subset(CardResults[1:50,],select = c(CardNames,NbDecks,
                                                         WINRATE_AVERAGE))
highestWinrateCards=arrange(highestWinrateCards,desc(WINRATE_AVERAGE))
write.csv(highestWinrateCards, paste(
  Beginning,'-',End,'_DF_Highest_Winrate_Cards.csv',sep=''),row.names = TRUE)

#INSTEAD WE TAKE THE LOWER BOUND OF THE WINRATE CONFIDENCE INTERVAL NOW
CardResults = CardResults[order(-CardResults$WINRATE_95_MIN),]
highestLowerWinrateBound=subset(CardResults[1:50,],select = c(CardNames,NbDecks,
                                                              WINRATE_95_MIN))
highestLowerWinrateBound=arrange(highestLowerWinrateBound,desc(WINRATE_95_MIN))
write.csv(highestLowerWinrateBound, paste(
  Beginning,'-',End,'_DF_Highest_Lower_Winrate_Bound_Cards.csv',sep=''),
  row.names = TRUE)

#ARTIST RESULTS
artistsList=unique(unlist(CardResults$Artists))
someZeros=rep(0,length(artistsList))
artistsResults = setNames(data.frame(artistsList,someZeros,someZeros),
                          c("ArtistName","Winrate","Presence"))

for (i in 1:length(artistsList)){
  artistsResults$Winrate[i]=sum((CardResults[CardResults$Artists==artistsResults$
                                               ArtistName[i],]$WINRATE_AVERAGE*
                                   CardResults[CardResults$Artists==artistsResults$
                                                 ArtistName[i],]$Presence))/
    sum(CardResults[CardResults$Artists==artistsResults$ArtistName[i],]$Presence)
  
  artistsResults$Winrate[i]=as.numeric(format(round(artistsResults$Winrate[i],1), 
                                              nsmall = 1))
  
  artistsResults$Presence[i]=sum(CardResults[CardResults$Artists==artistsResults$
                                               ArtistName[i],]$Presence)
  artistsResults$Presence[i]=as.numeric(format(round(artistsResults$Presence[i],1), 
                                               nsmall = 1))
}

artistsResults=arrange(artistsResults,desc(Winrate))

write.csv(artistsResults, paste(
  Beginning,'-',End,'_DF_Artist_Results.csv',sep=''),row.names = TRUE)

#SET RESULTS
setList=unique(unlist(CardResults$FirstSet))
someZeros=rep(0,length(setList))
setResults = setNames(data.frame(setList,someZeros,someZeros),
                          c("SetName","Winrate","Presence"))

for (i in 1:length(setList)){
  setResults$Winrate[i]=sum((CardResults[
    CardResults$FirstSet==setResults$SetName[i],]$
      WINRATE_AVERAGE*CardResults[
        CardResults$FirstSet==setResults$SetName[i],]$Presence))/
    sum(CardResults[CardResults$FirstSet==setResults$SetName[i],]$Presence)
  
  setResults$Winrate[i]=as.numeric(format(round(setResults$Winrate[i],1), 
                                              nsmall = 1))
  
  setResults$Presence[i]=sum(CardResults[CardResults$FirstSet==setResults$
                                           SetName[i],]$Presence)
  setResults$Presence[i]=as.numeric(format(round(setResults$Presence[i],1), 
                                               nsmall = 1))
}

setResults=arrange(setResults,desc(Winrate))

write.csv(setResults, paste(
  Beginning,'-',End,'_DF_Set_Results.csv',sep=''),row.names = TRUE)

#GET THE DATA FOR ALL PLAYERS
metric_df_play=metric_points_players(df,Beginning,End)
metric_df_play_sub=metric_df_play[c("PLAYERS","NO_APPEARANCES","TOTAL_NB_MATCHES",
                                    "WINRATE_AVERAGE","WINRATE_95_MIN",
                                    "WINRATE_95_MAX","TOTAL_POINTS")]
metric_df_play_sub[,"ARCHETYPE_NAMES"]=NA
metric_df_play_sub[,"ARCHETYPE_COUNT"]=NA
metric_df_play_sub[,"URL"]=NA
for (i in 1:length(metric_df_play_sub$URL)){
  metric_df_play_sub$ARCHETYPE_NAMES[i]=paste(unlist(
    metric_df_play$ARCHETYPE_NAMES[i]),collapse = "; ")
  metric_df_play_sub$ARCHETYPE_COUNT[i]=paste(unlist(
    metric_df_play$ARCHETYPE_COUNT[i]),collapse = "; ")
  metric_df_play_sub$URL[i]=paste(unlist(metric_df_play$URL[i]),collapse = "; ")
}
write.csv(metric_df_play_sub,paste(
  Beginning,'-',End,'_DF_Players_Overall_Results.csv',sep=''), row.names = TRUE)

#COUNT THE NUMBER OF TOP8 FOR EACH PLAYER
top8_df_play=players_top8(df,Beginning,End)
top8_df_play_cop=top8_df_play
top8_df_play_cop[,"ARCHETYPE_NAMES"]=NA
top8_df_play_cop[,"ARCHETYPE_COUNT"]=NA
top8_df_play_cop[,"URL"]=NA
for (i in 1:length(top8_df_play_cop$URL)){
  top8_df_play_cop$ARCHETYPE_NAMES[i]=paste(
    unlist(top8_df_play$ARCHETYPE_NAMES[i]),collapse = "; ")
  top8_df_play_cop$ARCHETYPE_COUNT[i]=paste(
    unlist(top8_df_play$ARCHETYPE_COUNT[i]),collapse = "; ")
  top8_df_play_cop$URL[i]=paste(unlist(top8_df_play$URL[i]),collapse = "; ")
}
write.csv(top8_df_play_cop,paste(
  Beginning,'-',End,'_DF_Top8_Players_',MTGFormat,'_Results.csv',sep=''), 
  row.names = TRUE)

# DATA OF THE CARDS FROM THE LAST SET

#SELECT ALL THE CARDS FROM THE LAST SET
lastSetCards=cardDataSub[cardDataSub$setCode==lastSetCode,]
#KEEP ONLY THE FIRST PRINTED CARDS
lastSetOCards=lastSetCards[lastSetCards$isReprint==0,]

lastSetPlayedCards=c()
for (i in 1:length(FormatCardsList)){
  if (FormatCardsList[i] %in% lastSetOCards$name){
    lastSetPlayedCards[length(lastSetPlayedCards)+1]=FormatCardsList[i]
  }else if (FormatCardsList[i] %in% lastSetOCards$faceName){
    lastSetPlayedCards[length(lastSetPlayedCards)+1]=FormatCardsList[i]
  }
}

lastSetOCardsData = CardResults[CardResults$CardNames %in% lastSetPlayedCards, ]

lastSetOCardsData=arrange(lastSetOCardsData,lastSetOCardsData$CardName)

lastSetOCardsData2CSV=lastSetOCardsData

lastSetOCardsData2CSV[,"Archetypes"]=NA
lastSetOCardsData2CSV[,"URL"]=NA

for (i in 1:length(lastSetOCardsData2CSV$CardName)){
  lastSetOCardsData2CSV$Archetypes[i]=paste(unlist(
    lastSetOCardsData$Archetypes[i]),collapse = "; ")
  
  lastSetOCardsData2CSV$URL[i]=paste(unlist(lastSetOCardsData$URL[i]),
                                     collapse = "; ")
}

write.csv(lastSetOCardsData2CSV,paste(
  Beginning,'-',End,'_',lastSetCode,'_cardData.csv',sep=''), row.names = FALSE)

################################################################################

#WORKING DIRECTORY FOR THE GRAPHS
setwd(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                      EventType,sep="/"), "Results_as_pictures"))

#PRESENCE
PieShare=2
# 1. Open jpeg file
jpeg(paste("Pieplot_Copies_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
metagame_pie_chart(df,"Copies",Beginning,End,EventType)
dev.off()

jpeg(paste("Pieplot_Matches_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
metagame_pie_chart(df,"Matches",Beginning,End,EventType)
dev.off()

jpeg(paste("Pieplot_Players_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
metagame_pie_chart(df,"Players",Beginning,End,EventType)
dev.off()

jpeg(paste("Barplot_Matches_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 7000, height = 3500,res=300)
metagame_box_plot(df,"Matches",Beginning,End,EventType,0)
dev.off()

#WINRATES
arch_ranked=archetypes_ranking(metric_df,Beginning,End)
jpeg(paste("0.95 CI on winrate between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
winrates_graph(df,arch_ranked,"Matches",Beginning,End,EventType)
dev.off()

jpeg(paste("Low winrate estimation in 0.95 CI between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
lower_bound_ci_winrate_graph(df,arch_ranked,Beginning,End,EventType)
dev.off()

jpeg(paste("Ratio of archetype winrates out of pilot winrates between", Beginning, 
           "and", End, "in MTGO", EventType,".jpg"), width = 8000, height = 3000, 
     res=400)
winrate_ratio_arch_out_player_graph(df,metric_df,Beginning,End,EventType)
dev.off()

#presence AND diameters CAN BE EITHER "Copies", "Players" or "Matches"
#tiers CAN BE EITHER "Win+Pres","Pres M+SD" or "Pres %"
#function(metric_df,presence,diameters,diam_ratio,beginning,end,tiers,isLog,
#only_best,EventType)
jpeg(paste("Winrates and presence based on Matches between", Beginning, "and", 
           End,"in MTGO", EventType,".jpg"), width = 6000, height = 3000,
     res=300)
metric_graph(metric_df,"Matches","Players",1,Beginning,End,"Pres M+SD",TRUE,
             FALSE,EventType)
dev.off()

#ARCHETYPE CLUSTERING
#function (metric_df,k,iter,init,algo,beginning,end,count_wr,
#only_best,EventType)
jpeg(paste("Best archetypes clustering between", Beginning, "and", End,
           "in MTGO", EventType,".jpg"), width = 5000, height = 3000,res=300)
kmeans_arch(metric_df,4,30,50,"Hartigan-Wong",Beginning,End,TRUE,TRUE,EventType)
dev.off()

#WITH LOGARITHM OF PRESENCE
arch_ranked=archetypes_ranking(metric_df_log_matches,Beginning,End)

metric_df_log_matches_sub=metric_df_log_matches[
  metric_df_log_matches$TOTAL_NB_MATCHES>mean(
    metric_df_log_matches$TOTAL_NB_MATCHES),]
arch_ranked_sub=archetypes_ranking(metric_df_log_matches_sub,Beginning,End)

#RANKING THE DECKS BASED ON COMBINATION OF WINRATE AND LOG(PRESENCE)
jpeg(paste("Deck score based on sum of winrate and log(presence) between", 
           Beginning, "and", End, "in MTGO", EventType,".jpg"), width = 8000, 
     height = 3000,res=400)
log_comb_graph(df,arch_ranked_sub,Beginning,End,EventType)
dev.off()

#RANKING THE DECKS BASED ON META SCORE OF WINRATE AND LOG(PRESENCE)
jpeg(paste("Deck score based on VS Meta Score between", 
           Beginning, "and", End, "in MTGO", EventType,".jpg"), width = 8000, 
     height = 3000,res=400)
meta_score_graph(df,arch_ranked_sub,Beginning,End,EventType)
dev.off()

df_tiers_list=generate_tiers_lists(arch_ranked_sub)

#View(df_tiers_list)

setwd(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                      EventType,sep="/"), "Results_as_csv"))
write.csv(df_tiers_list,paste(Beginning,'-',End,'_DF_Tiers_Lists.csv',sep=''), 
          row.names = TRUE)


################################################################################
#CONVERT ALL CSV TO XLSX

csvNames = list.files(paste(DirectoryFile,MTGFormat,paste(
  Beginning,End,sep="_"),EventType,"Results_as_csv",sep="/"), pattern="*.csv", 
  full.names=TRUE)

for(i in csvNames) {
  csvi = read.csv(i)
  xlsxiName = sub('.csv', '.xlsx', i, fixed = TRUE)
  write.xlsx(csvi, xlsxiName, row.names = FALSE)
}

################################################################################

#EXPORT A QUICK ANALYSIS OF THE DATA IN .TXT
setwd(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                      EventType,sep="/"), "Results_as_txt"))

nbDecks=length(df$URL)
nbDiffPlayers=length(unique(df$PLAYER))
nbDiffCards=length(unique(FormatCardsStats$CardNames))
nbExactArch=length(unique(df$ARCHETYPE))
nbSuperArch=length(unique(df$SUPER_ARCH))
totNbRounds=sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES)
# avgNbRoundsWTop8=as.numeric(format(round(
#   (sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))/length(df$URL),2), nsmall = 2))
avgNbRounds=as.numeric(format(round(sum(df$NB_ROUNDS)/length(df$URL),2),nsmall=2))
minNbRounds=min(df$NB_ROUNDS)
maxNbRounds=max(df$NB_ROUNDS)
nbEvents=length(unique(df$EVENT_NAME))

eventInfo=paste("QUICK ANALYSIS OF THE DATA", 
                "\nBeginning: ", Beginning,
                "\nEnd: ", End,
                "\nType of MTGO events: ", EventType,
                "\nNumber of decks in the data: ",nbDecks,
                "\nNumber of different players in the data: ",nbDiffPlayers,
                "\nNumber of different cards in the data: ",nbDiffCards,
                "\nNumber of exact archetypes in the data: ",nbExactArch,
                "\nNumber of super archetypes in the data: ",nbSuperArch,
                "\nNumber of rounds played in the data (with top8): ",totNbRounds,
                # "\nAverage number of rounds in the data (with top8):",
                # avgNbRoundsWTop8,
                "\nAverage number of rounds in the data (w/o top8): ",avgNbRounds,
                "\nMinimum number of rounds in the data (w/o top8): ",minNbRounds,
                "\nMaximum number of rounds in the data (w/o top8): ",maxNbRounds,
                "\nNumber of events in the data: ", nbEvents,sep="")

cat(eventInfo,file=paste(Beginning,'-',End,'_Quick_Analysis.txt'))


#EXPORT THE LIST OF COVERED EVENTS WITH THEIR URL
coveredEvents=setNames(data.frame(matrix(ncol=2, nrow=nbEvents)), 
                       c("EVENT_NAME", "URL"))
coveredEvents$EVENT_NAME=unique(df$EVENT_NAME)
for (i in 1:nbEvents){
  coveredEvents$URL[i]=df[df$EVENT_NAME==coveredEvents$EVENT_NAME[i],]$URL[1]
}

titleEventFile=paste("List of MTGO", EventType,"between",Beginning, "and", End, 
                     "\n", sep=" ")
cat(titleEventFile,file=paste(Beginning,'-',End,'_List_of_',EventType,'.txt'))
write.table(coveredEvents, paste(Beginning,'-',End,'_List_of_',EventType,'.txt'), 
            append=TRUE, row.names = FALSE, col.names = FALSE, sep = " - ")

################################################################################

#COUNT THE PROPORTION OF RED AND BLUE SPELLS IN THE DATA

#COUNT THE PROPORTION OF DECKS PLAYING RED OR BLUE SPELLS