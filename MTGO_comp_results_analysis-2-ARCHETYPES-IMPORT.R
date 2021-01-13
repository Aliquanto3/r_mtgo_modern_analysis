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

#SELECT DATA FOR A SPECIFIC PERIOD
rawData$DATE = as.Date(rawData$DATE)
rawData$POINTS = as.numeric(rawData$POINTS)
periodData=subset(rawData, DATE >= as.Date(Beginning) & DATE < as.Date(End))

#View(periodData)
#length(periodData$PLAYER)

generate_Prelim_Data = function() {
  
  #COLLECT PRELIMINARIES ONLY FOR SPECIFIC TREATMENT
  PrelimData=periodData[grep("Preliminary", periodData$EVENT), ]
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
  
  if(EventType=="Competitions" || EventType=="Events"){
    #ADD TOP8 COLUMNS FOR MERGE WITH CHALLENGES
    PrelimData$TOP8_PTS=rep(0,length(PrelimData$POINTS))
    PrelimData$TOP8_DEF=rep(0,length(PrelimData$NB_DEFEATS))
    PrelimData$TOP8_MATCHES=rep(0,length(PrelimData$NB_ROUNDS))
  }
  
  return(PrelimData)
  
}

generate_Challenge_Data = function() {
  
  #COLLECT CHALLENGES ONLY FOR SPECIFIC TREATMENT
  challEvents = c("Challenge","Champ","Showcase")
  ChallData=periodData[grep(paste(challEvents,collapse="|"), periodData$EVENT), ]
  
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

generate_League_Data = function() {
  
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

generate_ManaTraders_Data = function() {
  
  #COLLECT CHALLENGES ONLY FOR SPECIFIC TREATMENT
  MTData=periodData[grep("ManaTraders", periodData$EVENT), ]
  
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

df=rawData

if (EventType=="Competitions"){
  #FUSE THE DATA BACK TO GET ALL THE COMPETITIVE RESULTS IN THE SAME FILE
  df=rbind(generate_Challenge_Data(),generate_Prelim_Data())
}else if (EventType=="Challenges"){
  df=generate_Challenge_Data()
}else if (EventType=="Preliminaries"){
  df=generate_Prelim_Data()
}else if (EventType=="Events"){
  df=rbind(generate_Challenge_Data(),generate_Prelim_Data())
  df=rbind(df,generate_League_Data())
  df=rbind(df,generate_ManaTraders_Data())
}

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
       df$ARCHETYPE[i]=="Bant Blink"| 
       df$ARCHETYPE[i]=="Temur Control"| 
       df$ARCHETYPE[i]=="Sultai Control"| 
       df$ARCHETYPE[i]=="Bant Control"|
       df$ARCHETYPE[i]=="Simic Control"|
       df$ARCHETYPE[i]=="WURG Blink"){
      
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
    
    if(df$ARCHETYPE[i]=="Jeskai Control" |
       df$ARCHETYPE[i]=="Jeskai Blink" |
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
       df$ARCHETYPE[i]=="Reclaimer Titan"){
      
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
       df$ARCHETYPE[i]=="Eldrazi Tron"){
      
      df$SUPER_ARCH[i]="Eldrazi"
      
    }
    
    if(df$ARCHETYPE[i]=="Azorius Taxes" |
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
       df$ARCHETYPE[i]=="Selenya Midrange"|
       df$ARCHETYPE[i]=="Naya Midrange"){
      
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
       df$ARCHETYPE[i]=="Grixis Whirza"){
      
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