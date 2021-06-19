#2nd file to execute
#Imports all the archetypes that appeared in MTGO results in a specific period
#on specific events, and add another layer of archetypes if needed

#Execute this file whenever you update a parameter that tells you to do so

#DATA SHARED BY PHELPS-SAN @ TRON DISCORD, HIS WORK CAN BE FOUND HERE:
#https://github.com/Badaro/MTGODecklistCache
#https://github.com/Badaro/MTGOArchetypeParser

#install.packages("jsonlite")
library("jsonlite")
#install.packages("readr")
library("readr")

generate_League_Data = function(periodData) {
  
  #COLLECT PRELIMINARIES ONLY FOR SPECIFIC TREATMENT
  LeagueData=periodData[grep("League", periodData$Tournament), ]
  #View(LeagueData) 
  
  #NUMBER OF ROUNDS IN EACH EVENT FOR THE LEAGUES - ALWAYS 5
  LeagueData$NRounds=rep(5,length(LeagueData$Tournament))
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN PRELIMINARIES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  LeagueData$NDefeats=rep(0,length(LeagueData$Tournament))
  
  #ADD TOP8 COLUMNS FOR MERGE WITH CHALLENGES
  LeagueData$T8Points=rep(0,length(LeagueData$Points))
  LeagueData$T8Defeats=rep(0,length(LeagueData$NDefeats))
  LeagueData$T8Matches=rep(0,length(LeagueData$NRounds))
  
  return(LeagueData)
  
}

generate_Prelim_Data = function(periodData) {
  
  #COLLECT PRELIMINARIES ONLY FOR SPECIFIC TREATMENT
  PrelimData=periodData[grep("Preliminary", periodData$Tournament), ]
  #View(PrelimData) 
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE PRELIMINARIES
  if(nrow(PrelimData)>=1){
    for (i in 1:nrow(PrelimData)){
      if (PrelimData$Result[i]=="5-0" | PrelimData$Result[i]=="4-1" | 
          PrelimData$Result[i]=="3-2"){
        #before prelim structure changes
        PrelimData$NRounds[i]=5
      }else if (PrelimData$Result[i]=="4-0" | PrelimData$Result[i]=="3-1"){
        #after prelim structure changes
        PrelimData$NRounds[i]=4
      }
    }
  }
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN PRELIMINARIES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  PrelimData$NDefeats=PrelimData$NRounds - PrelimData$Points/3
  
  #ADD TOP8 COLUMNS FOR MERGE WITH CHALLENGES
  PrelimData$T8Points=rep(0,length(PrelimData$Points))
  PrelimData$T8Defeats=rep(0,length(PrelimData$NDefeats))
  PrelimData$T8Matches=rep(0,length(PrelimData$NRounds))
  
  return(PrelimData)
  
}

generate_Challenge_Data = function(periodData) {
  
  #COLLECT CHALLENGES ONLY FOR SPECIFIC TREATMENT
  challEvents = "Challenge"
  ChallData=periodData[grep(paste(challEvents,collapse="|"), 
                            periodData$Tournament), ]
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE CHALLENGES
  #DIVIDE THE MAXIMUM NUMBER OF POINTS IN SWISS TO GET THE RESULT
  #IF MORE THAN 1 PLAYER HAS THE MAXIMUM OF POINTS, THEN IT IS LIKELY THAT 
  #THERE IS NOT ANY PLAYER AT X-0, SO YOU ADD 1 TO THE RESULT
  listEventsChall=unique(ChallData$TournamentName)
  nbRoundsVec=c()
  if(length(listEventsChall)>=1){
    for (i in 1:length(listEventsChall)){
      periodChallEventData=subset(ChallData, TournamentName == listEventsChall[i])
      maxPoints=max(periodChallEventData$Points)
      nbPlayMaxPts=length(which(periodChallEventData$Points == maxPoints))
      if(nbPlayMaxPts==1){
        nbRounds=maxPoints/3
      }else{
        nbRounds=1+maxPoints/3
      }
      nbRoundsEvent=rep(nbRounds,nrow(periodChallEventData))
      nbRoundsVec=c(nbRoundsVec,nbRoundsEvent)
    }
  }
  ChallData$NRounds=nbRoundsVec
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN CHALLENGES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  ChallData$NDefeats=ChallData$NRounds - ChallData$Points/3
  
  #ADD TOP8 POINTS: 3*3 to 1st, 3*2 to 2nd, 3*1 to 3rd and 4th, none to others
  ChallData$T8Points=rep(0,nrow(ChallData))
  if(length(listEventsChall)>=1){
    for (i in 1:length(ChallData$Result)){
      if (ChallData$Result[i] == "1st Place"){
        ChallData$T8Points[i] = 9
      }else if (ChallData$Result[i] == "2nd Place"){
        ChallData$T8Points[i] = 6
      }else if (ChallData$Result[i] == "3rd Place" | 
                ChallData$Result[i] == "4th Place"){
        ChallData$T8Points[i] = 3
      }
    }
  }
  
  #ADD TOP8 DEFEATS: 0 FOR THE WINNER, 1 FOR THE OTHERS
  ChallData$T8Defeats=ChallData$NDefeats
  if(length(listEventsChall)>=1){
    for (i in 1:length(ChallData$T8Defeats)){
      if (ChallData$Result[i] == "2nd Place" | ChallData$Result[i] == "3rd Place"| 
          ChallData$Result[i] == "4th Place" | ChallData$Result[i] == "5th Place"| 
          ChallData$Result[i] == "6th Place" | ChallData$Result[i] == "7th Place"| 
          ChallData$Result[i] == "8th Place"){
        ChallData$T8Defeats[i] = 1 + ChallData$NDefeats[i]
      }
    }
  }
  
  if(length(listEventsChall)>=1){
    ChallData$T8Matches=rep(0,length(ChallData$T8Points))
    for (i in 1:length(ChallData$T8Matches)){
      if (ChallData$Result[i] == "1st Place" | 
          ChallData$Result[i] == "2nd Place"){
        ChallData$T8Matches[i] = 3
      }else if (ChallData$Result[i] == "3rd Place" | 
                ChallData$Result[i] == "4th Place"){
        ChallData$T8Matches[i] = 2 
      }else if (ChallData$Result[i] == "5th Place"| 
                ChallData$Result[i] == "6th Place" | 
                ChallData$Result[i] == "7th Place"| 
                ChallData$Result[i] == "8th Place"){
        ChallData$T8Matches[i] = 1 
      }
    }
  }
  
  #REMOVE ALL THE RESULTS WITH 3 DEFEATS (BECAUSE WE ONLY HAVE PART OF THEM, 
  #WHEREAS WE HAVE ALL THE X-0, X-1 AND X-2 RESULTS)
  #ChallDataWO3Def=ChallData[ChallData$NDefeats != 3, ]
  #NO NEED FOR THAT FEATURE ANYMORE
  
  return(ChallData)
  
}

generate_Major_Data = function(periodData) {
  
  #COLLECT CHALLENGES ONLY FOR SPECIFIC TREATMENT
  MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
  MajData=periodData[grep(paste(MajEvents,collapse="|"), periodData$Tournament), ]
  MajData$Points=as.numeric(MajData$Points)
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE CHALLENGES
  #DIVIDE THE MAXIMUM NUMBER OF POINTS IN SWISS TO GET THE RESULT
  #IF MORE THAN 1 PLAYER HAS THE MAXIMUM OF POINTS, THEN IT IS LIKELY THAT 
  #THERE IS NOT ANY PLAYER AT X-0, SO YOU ADD 1 TO THE RESULT
  listEventsChall=unique(MajData$TournamentName)
  nbRoundsVec=c()
  if(length(listEventsChall)>=1){
    for (i in 1:length(listEventsChall)){
      periodChallEventData=subset(MajData, TournamentName == listEventsChall[i])
      maxPoints=max(periodChallEventData$Points)
      nbPlayMaxPts=length(which(periodChallEventData$Points == maxPoints))
      if(nbPlayMaxPts==1){
        nbRounds=maxPoints/3
      }else{
        nbRounds=1+maxPoints/3
      }
      nbRoundsEvent=rep(nbRounds,nrow(periodChallEventData))
      nbRoundsVec=c(nbRoundsVec,nbRoundsEvent)
    }
  }
  MajData$NRounds=nbRoundsVec
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN CHALLENGES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  MajData$NDefeats=MajData$NRounds - MajData$Points/3
  
  #ADD TOP8 POINTS: 3*3 to 1st, 3*2 to 2nd, 3*1 to 3rd and 4th, none to others
  MajData$T8Points=rep(0,length(MajData$Points))
  if(length(listEventsChall)>=1){
    for (i in 1:length(MajData$Result)){
      if (MajData$Result[i] == "1st Place"){
        MajData$T8Points[i] = 9
      }else if (MajData$Result[i] == "2nd Place"){
        MajData$T8Points[i] = 6
      }else if (MajData$Result[i] == "3rd Place" | 
                MajData$Result[i] == "4th Place"){
        MajData$T8Points[i] = 3
      }
    }
  }
  
  #ADD TOP8 DEFEATS: 0 FOR THE WINNER, 1 FOR THE OTHERS
  MajData$T8Defeats=MajData$NDefeats
  if(length(listEventsChall)>=1){
    for (i in 1:length(MajData$T8Defeats)){
      if (MajData$Result[i] == "2nd Place" | MajData$Result[i] == "3rd Place"| 
          MajData$Result[i] == "4th Place" | MajData$Result[i] == "5th Place"| 
          MajData$Result[i] == "6th Place" | MajData$Result[i] == "7th Place"| 
          MajData$Result[i] == "8th Place"){
        MajData$T8Defeats[i] = 1 + MajData$NDefeats[i]
      }
    }
  }
  
  if(length(listEventsChall)>=1){
    MajData$T8Matches=rep(0,length(MajData$T8Points))
    for (i in 1:length(MajData$T8Matches)){
      if (MajData$Result[i] == "1st Place" | 
          MajData$Result[i] == "2nd Place"){
        MajData$T8Matches[i] = 3
      }else if (MajData$Result[i] == "3rd Place" | 
                MajData$Result[i] == "4th Place"){
        MajData$T8Matches[i] = 2 
      }else if (MajData$Result[i] == "5th Place"| 
                MajData$Result[i] == "6th Place" | 
                MajData$Result[i] == "7th Place"| 
                MajData$Result[i] == "8th Place"){
        MajData$T8Matches[i] = 1 
      }
    }
  }
  
  #REMOVE ALL THE RESULTS WITH 3 DEFEATS (BECAUSE WE ONLY HAVE PART OF THEM, 
  #WHEREAS WE HAVE ALL THE X-0, X-1 AND X-2 RESULTS)
  #MajDataWO3Def=MajData[MajData$NDefeats != 3, ]
  #NO NEED FOR THAT FEATURE ANYMORE
  
  return(MajData)
  
}

generate_ManaTraders_Data = function(periodData) {
  
  #COLLECT CHALLENGES ONLY FOR SPECIFIC TREATMENT
  MTData=periodData[grep("ManaTraders", periodData$Tournament), ]
  MTData$Points=as.numeric(MTData$Points)
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE CHALLENGES
  #DIVIDE THE MAXIMUM NUMBER OF POINTS IN SWISS TO GET THE RESULT
  #IF MORE THAN 1 PLAYER HAS THE MAXIMUM OF POINTS, THEN IT IS LIKELY THAT 
  #THERE IS NOT ANY PLAYER AT X-0, SO YOU ADD 1 TO THE RESULT
  listEventsMT=unique(MTData$TournamentName)
  nbRoundsVec=c()
  if(length(listEventsMT)>=1){
    for (i in 1:length(listEventsMT)){
      periodMTEventData=subset(MTData, TournamentName == listEventsMT[i])
      maxPoints=max(periodMTEventData$Points)
      nbPlayMaxPts=length(which(periodMTEventData$Points == maxPoints))
      if(nbPlayMaxPts==1){
        nbRounds=maxPoints/3
      }else{
        nbRounds=1+maxPoints/3
      }
      nbRoundsEvent=rep(nbRounds,length(periodMTEventData$Tournament))
      nbRoundsVec=c(nbRoundsVec,nbRoundsEvent)
    }
  }
  MTData$NRounds=nbRoundsVec
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN CHALLENGES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  MTData$NDefeats=MTData$NRounds - MTData$Points/3
  
  #ADD TOP8 COLUMNS FOR MERGE WITH CHALLENGES
  MTData$T8Points=rep(0,length(MTData$Points))
  MTData$T8Defeats=rep(0,length(MTData$NDefeats))
  MTData$T8Matches=rep(0,length(MTData$NRounds))
  
  return(MTData)
  
}

generate_NRG_Data = function(periodData) {
  
  #COLLECT CHALLENGES ONLY FOR SPECIFIC TREATMENT
  NRGData=periodData[grep("NRG", periodData$Tournament), ]
  NRGData$Points=as.numeric(NRGData$Points)
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE CHALLENGES
  #DIVIDE THE MAXIMUM NUMBER OF POINTS IN SWISS TO GET THE RESULT
  #IF MORE THAN 1 PLAYER HAS THE MAXIMUM OF POINTS, THEN IT IS LIKELY THAT 
  #THERE IS NOT ANY PLAYER AT X-0, SO YOU ADD 1 TO THE RESULT
  listEventsMT=unique(NRGData$TournamentName)
  nbRoundsVec=c()
  if(length(listEventsMT)>=1){
    for (i in 1:length(listEventsMT)){
      periodMTEventData=subset(NRGData, TournamentName == listEventsMT[i])
      maxPoints=max(periodMTEventData$Points)
      nbPlayMaxPts=length(which(periodMTEventData$Points == maxPoints))
      if(nbPlayMaxPts==1){
        nbRounds=maxPoints/3
      }else{
        nbRounds=1+maxPoints/3
      }
      nbRoundsEvent=rep(nbRounds,length(periodMTEventData$Tournament))
      nbRoundsVec=c(nbRoundsVec,nbRoundsEvent)
    }
  }
  NRGData$NRounds=nbRoundsVec
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN CHALLENGES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  NRGData$NDefeats=NRGData$NRounds - NRGData$Points/3
  
  #ADD TOP8 COLUMNS FOR MERGE WITH CHALLENGES
  NRGData$T8Points=rep(0,length(NRGData$Points))
  NRGData$T8Defeats=rep(0,length(NRGData$NDefeats))
  NRGData$T8Matches=rep(0,length(NRGData$NRounds))
  
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
generate_df = function(EventType,mtgFormat,RawFile){
  
  #IMPORT DATA
  # setwd(DirectoryFile)
  # rawData=fromJSON(RawFile)[[1]]
  rawData=fromJSON(paste(DirectoryFile,RawFile,sep="/"))[[1]]
  rawData=rawData[grep(pattern = MTGFormat,x=rawData$Tournament),]
  rawData$Date = as.Date(rawData$Date)
  rawData$Points = as.numeric(rawData$Points)
  # View(rawData)
  # rawData[1,]$Mainboard[[1]][1]
  # rawData[1,]$Mainboard[[1]][2]
  
  if(is.na(Beginning)){
    Beginning=min(rawData$Date)
  }
  if(is.na(End)){
    End=max(rawData$Date)
  }
  
  #SELECT DATA FOR A SPECIFIC PERIOD
  periodData=subset(rawData, Date >= as.Date(Beginning) & Date < as.Date(End))
  periodData=periodData[grep(MTGFormat,periodData$Tournament),]
  
  #NAMES AND DATE DON'T ALLOW THE IDENTIFICATION OF AN EVENT, BUT THE COMBINATION
  #OF BOTH CAN, HENCE THE ADDITION OF ANOTHER COLUMN FOR THIS IDENTIFICATION
  periodData$TournamentName=rep(NA,nrow(periodData))
  for (i in 1:nrow(periodData)){
    periodData$TournamentName[i]=paste(periodData$Tournament[i],
                        as.character(periodData$Date[i]),sep=" ")
  }
  #View(periodData) 
  
  
  if (EventType=="All Events"){
    #everything available including leagues
    df=rbind(generate_Major_Data(periodData),generate_Prelim_Data(periodData))
    df=rbind(df,generate_League_Data(periodData))
    df=rbind(df,generate_ManaTraders_Data(periodData))
    df=rbind(df,generate_NRG_Data(periodData))
  } else if (EventType=="Everything but Leagues"){
    #Major Events, official or not, and Preliminaries
    df=rbind(generate_Major_Data(periodData),generate_Prelim_Data(periodData))
    df=rbind(df,generate_ManaTraders_Data(periodData))
    df=rbind(df,generate_NRG_Data(periodData))
  } else if (EventType=="Official Competitions"){
    #Major Official Events + Preliminaries
    df=rbind(generate_Major_Data(periodData),generate_Prelim_Data(periodData))
  } else if (EventType=="Major Official Events"){
    #Challenge,Champ,Showcase,Premier,Qualifier,MOCS
    df=generate_Major_Data(periodData)
  } else if (EventType=="Major Events Top32"){
    #Major Events, official or unofficial, only top32
    df=generate_Major_Data(periodData)
    #No need to filter only the top32, since this is everything we have
    
    dfMT=generate_ManaTraders_Data(periodData)
    dfMT$Result=parse_number(dfMT$Result) 
    #we can parse here because a ranking is given, contrary to Prelims where 
    #the score is given instead
    dfMT=dfMT[dfMT$Result<=32,]
    df=rbind(df,dfMT)
    
    dfNRG=generate_NRG_Data(periodData)
    dfNRG$Result=parse_number(dfNRG$Result)
    dfNRG=dfNRG[dfNRG$Result<=32,]
    df=rbind(df,dfNRG)
  } else if (EventType=="Challenges"){
    df=generate_Challenge_Data(periodData)
  } else if (EventType=="Preliminaries"){
    df=generate_Prelim_Data(periodData)
  } else if (EventType=="ManaTraders Series"){
    df=generate_ManaTraders_Data(periodData)
  } else if (EventType=="NRG Series"){
    df=generate_NRG_Data(periodData)
  }
  
  #ADD SUPER ARCHETYPES DEPENDING ON EXACT ARCHETYPE
  
  #TO SEE WHICH DECKLISTS CORRESPONDS TO A LABEL, FOR INSTANCE "Bant Midrange"
  #df[grep("Bant Midrange", df$Archetype$Archetype), ]$URL
  
  df=add_super_archetypes(df)
  
  #THE CODE DOES NOT CHECK WHETHER WE WANT TO USE SUPER ARCHETYPES HERE, BECAUSE 
  #IT IS STILL VERY FAST TO ADD THAT COLUMN, AND IT IS EASIER FOR USE WHEN YOU 
  #CHANGE THE TYPE OF ARCHETYPES YOU WANT TO USE IN THE CONSOLE ONCE YOU EXECUTED
  #THE ENTIRE CODE ONCE TO GENERATE VARIOUS GRAPHS
  
  #TO SEE WHICH EXACT ARCHETYPES ARE CONTAINED IN A SUPER ARCHETYPE, for instance 
  #"UGx Control"
  #unique(df[grep("UGx Control", df$Archetype$SuperArchetype), ]$Archetype$Archetype)
  # 
  # df$Allboards=list(setNames(data.frame(rep(NA,75),rep(NA,75)), 
  #                       c("Count", "CardName")),nrow(df))
  # for (i in 1:nrow(df)){
  #   # df$Allboards[[i]]$CardName=list(unique(c(df$Mainboard[[i]]$CardName,
  #   #                                     df$Sideboard[[i]]$CardName)))
  #   cardNamesi=unique(c(df$Mainboard[[i]]$CardName,
  #                       df$Sideboard[[i]]$CardName))
  #   df$Allboards[[i]]$CardName[1:length(cardNamesi)]=cardNamesi
  #   listCounts=rep(0,length(df$Allboards[[i]]$CardName[[1]]))
  #   for (j in 1:length(df$Allboards[[i]]$CardName[[1]])){
  #     indexMD=which(df$Mainboard[[i]]$CardName==df$Allboards[[i]]$CardName[j])
  #     countMD=0
  #     if(length(indexMD)!=0){
  #       countMD=df$Mainboard[[i]]$Count[indexMD]
  #     }
  #     
  #     indexSB=which(df$Sideboard[[i]]$CardName==df$Allboards[[i]]$CardName[j])
  #     countSB=0
  #     if(length(indexSB)!=0){
  #       countSB=df$Sideboard[[i]]$Count[indexSB]
  #     }
  #     
  #     listCounts[j]= countMD + countSB
  #   }
  #   #df$Allb4oards[[i]]$Count=list(listCounts)
  #   df$Allboards[[i]]$Count[1:length(listCounts)]=listCounts
  # }
  
  return(df)
}

#POSSIBLE QUICKFIX FOR A BETTER ACCURACY IN THE DATA
#THE Shadow Prowess STRATEGY PLAYING BLACK, RED AND GREEN CARDS (A COLOR 
#COMBINATION CALLED JUND) HAS QUITE A DIFFERENT STRUCTURE FROM THE OTHER 
#SHADOW PROWESS DECKS
# for (i in 1:length(df$Archetype$Archetype)){
#   if(df$Archetype$Archetype[i]=="Shadow Prowess" && df$COLOR[i]=="BRG"){
#     df$Archetype$Archetype[i]="Jund Shadow"
#   }
# }

add_super_archetypes = function(df){
  
  df$Archetype$SuperArchetype=df$Archetype$Archetype
  for (i in 1:length(df$Archetype$Archetype)){
    if(df$Archetype$Archetype[i]=="WURG Control" |
       df$Archetype$Archetype[i]=="WUBG Control" |
       df$Archetype$Archetype[i]=="Bant Midrange"| 
       df$Archetype$Archetype[i]=="Scapeshift"| 
       df$Archetype$Archetype[i]=="UBRG Control"| 
       df$Archetype$Archetype[i]=="Niv To Light"| 
       df$Archetype$Archetype[i]=="Omnath Saheeli"| 
       df$Archetype$Archetype[i]=="Temur Control"| 
       df$Archetype$Archetype[i]=="Sultai Control"| 
       df$Archetype$Archetype[i]=="Bant Control"|
       df$Archetype$Archetype[i]=="Simic Control"){
      
      df$Archetype$SuperArchetype[i]="UGx Control"
      
    }
    
    if(df$Archetype$Archetype[i]=="Jund Prowess" |
       df$Archetype$Archetype[i]=="Gruul Prowess" |
       df$Archetype$Archetype[i]=="Izzet Prowess"| 
       df$Archetype$Archetype[i]=="Boros Prowess"|
       df$Archetype$Archetype[i]=="Mardu Prowess"|
       df$Archetype$Archetype[i]=="Obosh Aggro"| 
       df$Archetype$Archetype[i]=="Mono Red Prowess"| 
       df$Archetype$Archetype[i]=="Rakdos Prowess"| 
       df$Archetype$Archetype[i]=="Jeskai Prowess"| 
       df$Archetype$Archetype[i]=="Naya Prowess"){
      
      df$Archetype$SuperArchetype[i]="Rx Prowess"
      
    }
    
    if(df$Archetype$Archetype[i]=="Jeskai Blink" |
       df$Archetype$Archetype[i]=="Bant Blink"| 
       df$Archetype$Archetype[i]=="WURG Blink" |
       df$Archetype$Archetype[i]=="Abzan Blink"
       ){
      
      df$Archetype$SuperArchetype[i]="Blink"
      
    }
    
    if(df$Archetype$Archetype[i]=="Jeskai Control" |
       df$Archetype$Archetype[i]=="Izzet Control"| 
       df$Archetype$Archetype[i]=="Dimir Control"| 
       df$Archetype$Archetype[i]=="Azorius Control"|
       df$Archetype$Archetype[i]=="Azorius Midrange"|
       df$Archetype$Archetype[i]=="Esper Control"| 
       df$Archetype$Archetype[i]=="Grixis Control"|
       df$Archetype$Archetype[i]=="UW Miracles"|
       df$Archetype$Archetype[i]=="UR Kiki Boilproof"){
      
      df$Archetype$SuperArchetype[i]="Non UGx Control"
      
    }
    
    if(df$Archetype$Archetype[i]=="Amulet Titan" | 
       df$Archetype$Archetype[i]=="KGC Amulet Titan"| 
       df$Archetype$Archetype[i]=="Primeval Titan"| 
       df$Archetype$Archetype[i]=="Reclaimer Titan"|
       df$Archetype$Archetype[i]=="Valakut Field"){
      
      df$Archetype$SuperArchetype[i]="P.Titan"
      
    }
    
    if(df$Archetype$Archetype[i]=="UBRG Shadow" | 
       df$Archetype$Archetype[i]=="Grixis Shadow"|
       df$Archetype$Archetype[i]=="Shadow Prowess"|
       df$Archetype$Archetype[i]=="BR Shadow Prowess"|
       df$Archetype$Archetype[i]=="WBR Shadow Prowess"| 
       df$Archetype$Archetype[i]=="BRG Shadow Prowess"|
       df$Archetype$Archetype[i]=="Jund Shadow"|
       df$Archetype$Archetype[i]=="Rakdos Shadow"|
       df$Archetype$Archetype[i]=="Sultai Shadow"|
       df$Archetype$Archetype[i]=="Esper Shadow"|
       df$Archetype$Archetype[i]=="Orzhov Shadow"|
       df$Archetype$Archetype[i]=="Dimir Shadow"|
       df$Archetype$Archetype[i]=="Mardu Shadow"|
       df$Archetype$Archetype[i]=="RB Shadow Lurrus"|
       df$Archetype$Archetype[i]=="Mardu Shadow Lurrus"){
      
      df$Archetype$SuperArchetype[i]="Shadow"
      
    }
    
    if(df$Archetype$Archetype[i]=="Stoneforge Eldrazi" |
       df$Archetype$Archetype[i]=="Obligator Eldrazi" |
       df$Archetype$Archetype[i]=="Eldrazi Tron" |
       df$Archetype$Archetype[i]=="Green Eldrazi"){
      
      df$Archetype$SuperArchetype[i]="Eldrazi"
      
    }
    
    if(df$Archetype$Archetype[i]=="Azorius Taxes" |
       df$Archetype$Archetype[i]=="Esper Taxes" |
       df$Archetype$Archetype[i]=="Abzan Taxes" |
       df$Archetype$Archetype[i]=="Mono White Taxes"| 
       df$Archetype$Archetype[i]=="Selenya Taxes"|
       df$Archetype$Archetype[i]=="Boros Taxes"|
       df$Archetype$Archetype[i]=="Orzhov Taxes"|
       df$Archetype$Archetype[i]=="Jeskai Taxes"|
       df$Archetype$Archetype[i]=="BW Eldrazi & Taxes"){
      
      df$Archetype$SuperArchetype[i]="D&T"
      
    }
    
    if(df$Archetype$Archetype[i]=="Abzan Company" | 
       df$Archetype$Archetype[i]=="Selesnya Midrange"|
       df$Archetype$Archetype[i]=="Naya Midrange"  |
       df$Archetype$Archetype[i]=="Badzan"){
      
      df$Archetype$SuperArchetype[i]="GWx Midrange"
      
    }
    
    if(df$Archetype$Archetype[i]=="Mardu Midrange" | 
       df$Archetype$Archetype[i]=="Rakdos Midrange"){
      
      df$Archetype$SuperArchetype[i]="RBx Midrange"
      
    }
    
    if(df$Archetype$Archetype[i]=="Bant Spirits" | 
       df$Archetype$Archetype[i]=="Spirits"){
      
      df$Archetype$SuperArchetype[i]="Spirits"
      
    }
    
    if(df$Archetype$Archetype[i]=="Belcher"|
       df$Archetype$Archetype[i]=="UW Belcher"|
       df$Archetype$Archetype[i]=="RG Belcher"){
      
      df$Archetype$SuperArchetype[i]="Belcher"
      
    }
    
    if(df$Archetype$Archetype[i]=="Gifts Storm"|
       df$Archetype$Archetype[i]=="Twiddle Storm"|
       df$Archetype$Archetype[i]=="Song Storm"){
      
      df$Archetype$SuperArchetype[i]="Storm"
      
    }
    
    if(df$Archetype$Archetype[i]=="Izzet Restore Balance" |
       df$Archetype$Archetype[i]=="Temur Foretold Balance" |
       df$Archetype$Archetype[i]=="Izzet Living End"){
      
      df$Archetype$SuperArchetype[i]="URx Balance"
      
    }
    
    if(df$Archetype$Archetype[i]=="Slivers"){
      
      df$Archetype$SuperArchetype[i]="Slivers"
      
    }
    
    if(df$Archetype$Archetype[i]=="E Tron"){
      
      df$Archetype$SuperArchetype[i]="Eldrazi"
      
    }
    
    if(df$Archetype$Archetype[i]=="U Tron" |
       df$Archetype$Archetype[i]=="Dice Factory Tron"){
      
      df$Archetype$SuperArchetype[i]="Other Tron"
      
    }
    
    if(df$Archetype$Archetype[i]=="Elementals"){
      
      df$Archetype$SuperArchetype[i]="Elementals"
      
    }
    
    if(df$Archetype$Archetype[i]=="Humans"){
      
      df$Archetype$SuperArchetype[i]="Humans"
      
    }
    
    if(df$Archetype$Archetype[i]=="Merfolks"){
      
      df$Archetype$SuperArchetype[i]="Merfolks"
      
    }
    
    if(df$Archetype$Archetype[i]=="Heliod Combo" |
       df$Archetype$Archetype[i]=="GW Heliod"|
       df$Archetype$Archetype[i]=="Mono White Heliod"){
      
      df$Archetype$SuperArchetype[i]="Heliod"
      
    }
    
    if(df$Archetype$Archetype[i]=="Jund Midrange" |
       df$Archetype$Archetype[i]=="Jund Lurrus Midrange"|
       df$Archetype$Archetype[i]=="Sultai Midrange"|
       df$Archetype$Archetype[i]=="UBRG Midrange"|
       df$Archetype$Archetype[i]=="Abzan Midrange"|
       df$Archetype$Archetype[i]=="Golgari Midrange"){
      
      df$Archetype$SuperArchetype[i]="BGx Midrange"
      
    }
    
    if(df$Archetype$Archetype[i]=="Gruul Midrange" |
       df$Archetype$Archetype[i]=="Naya Midrange"){
      
      df$Archetype$SuperArchetype[i]="RGx Midrange"
      
    }
    
    if(df$Archetype$Archetype[i]=="Dredge"){
      
      df$Archetype$SuperArchetype[i]="Dredge"
      
    }
    
    if(df$Archetype$Archetype[i]=="Burn" |
       df$Archetype$Archetype[i]=="RW Burn"|
       df$Archetype$Archetype[i]=="RG Burn"|
       df$Archetype$Archetype[i]=="RB Burn"){
      
      df$Archetype$SuperArchetype[i]="Rx Burn"
      
    }
    
    if(df$Archetype$Archetype[i]=="Crabvine"){
      
      df$Archetype$SuperArchetype[i]="Crabvine"
      
    }
    
    if(df$Archetype$Archetype[i]=="Orzhov Midrange"){
      
      df$Archetype$SuperArchetype[i]="BWx Midrange"
      
    }
    
    if(df$Archetype$Archetype[i]=="Thopter Urza" |
       df$Archetype$Archetype[i]=="Uroza" |
       df$Archetype$Archetype[i]=="Grixis Whirza" |
       df$Archetype$Archetype[i]=="Paradoxical Urza" |
       df$Archetype$Archetype[i]=="Urza Oko"){
      
      df$Archetype$SuperArchetype[i]="Urza"
      
    }
    
    if(df$Archetype$Archetype[i]=="Bant Stoneblade" |
       df$Archetype$Archetype[i]=="UW Stoneblade"){
      
      df$Archetype$SuperArchetype[i]="UWx Stoneblade"
      
    }
    
    if(df$Archetype$Archetype[i]=="Infect"){
      
      df$Archetype$SuperArchetype[i]="Infect"
      
    }
    
    if(df$Archetype$Archetype[i]=="UB Inverter"){
      
      df$Archetype$SuperArchetype[i]="Inverter"
      
    }
    
    if(df$Archetype$Archetype[i]=="MonoG Tron" |
       df$Archetype$Archetype[i]=="KGC Tron"){
      
      df$Archetype$SuperArchetype[i]="Gx Tron"
      
    }
    
    if(df$Archetype$Archetype[i]=="Bogles"){
      
      df$Archetype$SuperArchetype[i]="Bogles"
      
    }
    
    if(df$Archetype$Archetype[i]=="Devoted"|
       df$Archetype$Archetype[i]=="GW Devoted Lurrus"){
      
      df$Archetype$SuperArchetype[i]="Devoted"
      
    }
    
    if(df$Archetype$Archetype[i]=="Ad Nauseam"){
      
      df$Archetype$SuperArchetype[i]="Ad Nauseam"
      
    }
    
    if(df$Archetype$Archetype[i]=="Rogues"){
      
      df$Archetype$SuperArchetype[i]="Rogues"
      
    }
    
    if(df$Archetype$Archetype[i]=="Mill" |
       df$Archetype$Archetype[i]=="UB Mill"){
      
      df$Archetype$SuperArchetype[i]="Mill"
      
    }
    
    if(df$Archetype$Archetype[i]=="Hammer Time"){
      
      df$Archetype$SuperArchetype[i]="Hammer Time"
      
    }
    
    if(df$Archetype$Archetype[i]=="Enduring Ideal"){
      
      df$Archetype$SuperArchetype[i]="Enduring Ideal"
      
    }
    
    if(df$Archetype$Archetype[i]=="Polymorph"){
      
      df$Archetype$SuperArchetype[i]="Polymorph"
      
    }
    
    if(df$Archetype$Archetype[i]=="Red Prison" | 
       df$Archetype$Archetype[i]=="Boros Land Destruction"|
       df$Archetype$Archetype[i]=="Pyro Prison" ){
      
      df$Archetype$SuperArchetype[i]="Red Prison"
      
    }
    
    if(df$Archetype$Archetype[i]=="Domain Zoo"|
       df$Archetype$Archetype[i]=="Bushwhacker Zoo"){
      
      df$Archetype$SuperArchetype[i]="Zoo"
    }
  }
  
  return(df)
  
}

#/!\ to be updated when you change data, at least check if there isn't any new 
#archetype that should be considered - worst case the simple archetype name is
#reused

#UNCOMMENT IF YOU EXECUTE THIS FILE ALONE
# df=generate_df(EventType,MTGFormat,RawFile)
