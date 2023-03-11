# 2nd file to execute
# Imports all the archetypes that appeared in MTGO results in a specific period
# on specific events, and add another layer of archetypes if needed

# DATA SHARED BY PHELPS-SAN @ TRON DISCORD, HIS WORK CAN BE FOUND HERE:
# https://github.com/Badaro/MTGODecklistCache
# https://github.com/Badaro/MTGOArchetypeParser

#install.packages("jsonlite")
library("jsonlite")
#install.packages("readr")
library("readr")

generate_Prelim_Data = function(PrelimData) {
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE PRELIMINARIES
  if (nrow(PrelimData) >= 1) {
    PrelimData$NRounds = rep(0,nrow(PrelimData))
    conditionOldPrelims = PrelimData$Result == "5-0" |
      PrelimData$Result == "4-1" |
      PrelimData$Result == "3-2"
    
    PrelimData$NRounds = ifelse(conditionOldPrelims,5,4)
  }
  
  #CALCULATE THE NUMBER OF DEFEATS OF EACH DECK IN PRELIMINARIES -
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN)
  PrelimData$NWins = PrelimData$Points / 3
  PrelimData$NDraws = 0 # for merge with Full meta events
  PrelimData$NDefeats = PrelimData$NRounds - PrelimData$Points / 3
  
  #ADD TOP8 COLUMNS FOR MERGE WITH CHALLENGES
  PrelimData$T8Points = rep(0, nrow(PrelimData))
  PrelimData$T8Defeats = rep(0, nrow(PrelimData))
  PrelimData$T8Matches = rep(0, nrow(PrelimData))
  
  return(PrelimData)
}

generate_Tournament_Data = function(TournamentData) {
  
  # CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE CHALLENGES
  # DIVIDE THE MAXIMUM NUMBER OF POINTS IN SWISS TO GET THE RESULT
  # IF MORE THAN 1 PLAYER HAS THE MAXIMUM OF POINTS, THEN IT IS LIKELY THAT
  # THERE IS NOT ANY PLAYER AT X-0, SO YOU ADD 1 TO THE RESULT
  listEvents = unique(TournamentData$TournamentName)
  nRoundsVec = c()
  if (length(listEvents) >= 1) {
    for (event in listEvents) {
      eventData = subset(TournamentData, TournamentName == event)
      maxPoints = max(eventData$Points, na.rm = TRUE)
      nPlayMaxPts = length(which(eventData$Points == maxPoints))
      nRounds = ifelse(nPlayMaxPts == 1,
                        maxPoints %/% 3,
                        1 + maxPoints %/% 3)
      nRoundsEvent = rep(nRounds, nrow(eventData))
      nRoundsVec = c(nRoundsVec, nRoundsEvent)
    }
  }
  TournamentData$NRounds = nRoundsVec
  
  # CALCULATE THE RECORD OF EACH DECK IN TOURNAMENTS -
  # NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN)
  TournamentData$Points[is.na(TournamentData$Points)] = 0
  TournamentData$NWins = TournamentData$Points %/% 3
  TournamentData$NDraws = TournamentData$Points %% 3
  TournamentData$NDefeats = TournamentData$NRounds - TournamentData$NWins - TournamentData$NDraws
  # Doesn't take draws in account if they are a multiple of 3

  conditionMUNotNull = !sapply(TournamentData$Matchups, function(x) length(x) == 0 )
  
  for (i in (1:nrow(TournamentData))[conditionMUNotNull]) {
    
    TournamentDataI = TournamentData[i,]
    WinsI = TournamentDataI$Matchups[[1]]$Wins 
    DefeatsI = TournamentDataI$Matchups[[1]]$Losses 
    
    TournamentDataI$NWins = sum(WinsI > DefeatsI)
    
    TournamentDataI$NDefeats = sum(WinsI < DefeatsI)
    
    TournamentDataI$NDraws = sum(WinsI == DefeatsI)
    
    TournamentData[i,] = TournamentDataI
  }
  # TODO : vérifier la pertinence des résultats
  
  # Review the relevance of the computation of the number of rounds
  conditionRoundsDifferentFromScore = TournamentData$NRounds !=
    TournamentData$NWins + TournamentData$NDraws + TournamentData$NDefeats
  
  TournamentData[conditionRoundsDifferentFromScore,]$NRounds =
    TournamentData[conditionRoundsDifferentFromScore,]$NWins + 
    TournamentData[conditionRoundsDifferentFromScore,]$NDraws + 
    TournamentData[conditionRoundsDifferentFromScore,]$NDefeats
  
    #ADD TOP8 POINTS: 3*3 to 1st, 3*2 to 2nd, 3*1 to 3rd and 4th, none to others
    
    ConditionTop1 = TournamentData$NumericResult == 1
    ConditionTop2 = TournamentData$NumericResult == 2
    ConditionTop4 = TournamentData$NumericResult == 3 | 
      TournamentData$NumericResult == 4
    
    TournamentData$T8Points = ifelse(ConditionTop1,9,
                                     ifelse(ConditionTop2,6,
                                            ifelse(ConditionTop4,3,0)))
    
    #ADD TOP8 DEFEATS: 0 FOR THE WINNER, 1 FOR THE OTHERS
    TournamentData$T8Defeats = rep(0, nrow(TournamentData))
    
    if (length(listEvents) >= 1) {
      conditionTop8Not1 = TournamentData$NumericResult >= 2 &
        TournamentData$NumericResult <= 8
      TournamentData$T8Defeats = ifelse(conditionTop8Not1,
                                        TournamentData$T8Defeats+1,
                                        TournamentData$T8Defeats)
      
      ConditionTop2 = TournamentData$NumericResult <= 2
      ConditionTop4 = TournamentData$NumericResult == 3 | 
        TournamentData$NumericResult ==4
      ConditionTop8 = TournamentData$NumericResult <= 8 & 
        TournamentData$NumericResult >=4
      TournamentData$T8Matches = ifelse(ConditionTop2,3,
                                        ifelse(ConditionTop4,2,
                                               ifelse(ConditionTop8,1,0)))
    }
  
  return(TournamentData)
  
}

# Event type:
# All sources = Everything (except MTGO Leagues - for any filter)
# Top32 Only = Only events with a top32 (aka not MTGO Preliminaries)
# Full Meta = Only events with the full metagame available (not MTGO Official results)
# ManaTraders = ManaTraders Series results
# Paper = Results from MTG Melee
# MTGO Official Competitions = Results from the MTGO website
# MTGO Events with a Top32 = MTGO results with a top32 (so not Preliminaries)
# MTGO Preliminaries = As per name
generate_df = function(EventType, MTGFormat, RawFile, Date.autoupdate) {
  #IMPORT DATA
  rawData = fromJSON(paste(DirectoryFile, RawFile, sep = "/"))[[1]]
  if (!MTGFormat == "All_Formats") {
    # probably filtered before with correct use of the parser and import of a
    # correctly named file
    rawData = rawData[grep(pattern = MTGFormat, x = rawData$Tournament), ]
  }
  rawData$Date = as.Date(rawData$Date)
  rawData$Points = as.numeric(rawData$Points)
  # View(rawData)
  
  if(Date.autoupdate){
    Beginning = max(rawData$Date) - 28
    End = max(rawData$Date)
  }
  
  if (is.na(Beginning)) {
    Beginning = min(rawData$Date)
  }
  if (is.na(End)) {
    End = max(rawData$Date)
  }
  
  #SELECT DATA FOR A SPECIFIC PERIOD
  periodData = subset(rawData, Date >= as.Date(Beginning) &
                        Date < as.Date(End))
  
  # NAMES AND DATE DON'T ALLOW THE IDENTIFICATION OF AN EVENT, BUT THE COMBINATION
  # OF BOTH CAN, HENCE THE ADDITION OF ANOTHER COLUMN FOR THIS IDENTIFICATION,
  # WITH A MORE READABLE NAME THAN THE FILE NAME COLUMN
  periodData$TournamentName = rep(NA, nrow(periodData))
  periodData$TournamentName = paste(periodData$Tournament,
                                    as.character(periodData$Date), sep =
                                      " ")
  periodData$Result[is.na(periodData$Result)] = 0
  #View(periodData)
  
  # Remove the noise from leagues in case it wasn't done by the parser
  periodData = periodData[!grepl("League", periodData$Tournament),]
  # Make the final position easier to manipulate as a number (doesn't work for Prelims)
  periodData$NumericResult = parse_number(periodData$Result)
  
  # /!\ Some events only have a top32, or don't even have one (Prelims)
  if (EventType == "All sources") {
    Top32Data = periodData[!grepl("Preliminary", periodData$Tournament),]
    PrelimData = periodData[grep("Preliminary", periodData$Tournament),]
    df = rbind(generate_Tournament_Data(Top32Data),
               generate_Prelim_Data(PrelimData))
    
  } else if (EventType == "Top32 Only") {
    Top32Data = periodData[!grepl("Preliminary", periodData$Tournament),]
    df = generate_Tournament_Data(Top32Data)
    # Keep only the top32 of all those events
    df = df[df$NumericResult <= 32, ]
    
  } else if (EventType == "Full Meta") {
    # Use data from Manatraders and MTG Melee, not the partial MTGO website
    MTData = periodData[grep("https://www.manatraders.com/webshop/personal/",
                             periodData$AnchorUri),]
    PaperData = periodData[grep("https://mtgmelee.com/Decklist/View/",
                                periodData$AnchorUri),]
    df = rbind(generate_Tournament_Data(MTData),
               generate_Tournament_Data(PaperData))
    
  } else if (EventType == "ManaTraders") {
    # Use data from Manatraders
    MTData = periodData[grep("https://www.manatraders.com/webshop/personal/",
                             periodData$AnchorUri),]
    df = generate_Tournament_Data(MTData)
    
  } else if (EventType == "Paper") {
    # Use data from MTG Melee
    PaperData = periodData[grep("https://mtgmelee.com/Decklist/View/",
                                periodData$AnchorUri),]
    df = generate_Tournament_Data(PaperData)
    
  } else{
    MTGOData = periodData[grep("https://www.mtgo.com/en/mtgo/decklist/",
                               periodData$AnchorUri),]
    
    if (EventType == "MTGO Official Competitions") {
      MTGOTop32Data = periodData[!grepl("Preliminary", periodData$Tournament),]
      PrelimData = periodData[grep("Preliminary", periodData$Tournament),]
      df = rbind(generate_Tournament_Data(MTGOTop32Data),
                 generate_Prelim_Data(PrelimData))
      
    } else if (EventType == "MTGO Events with a Top32") {
      # MTGO tournaments with a top32, so not Preliminaries (nor Leagues, already filtered)
      MTGOTop32Data = periodData[!grepl("Preliminary", periodData$Tournament),]
      df = generate_Tournament_Data(MTGOTop32Data)
      
    } else if (EventType == "MTGO Preliminaries") {
      # Preliminaries only
      PrelimData = periodData[grep("Preliminary", periodData$Tournament),]
      df = generate_Prelim_Data(PrelimData)
    }
  }
  
  return(df)
}

#UNCOMMENT IF YOU EXECUTE THIS FILE ALONE
# df=generate_df(EventType,MTGFormat,RawFile)

# Return the list of URL of archetypes with Conflicts
getConflictURL = function(df){
  return(df[startsWith(df$Archetype$Archetype,"Conflict")==TRUE,]$AnchorUri)
}
# Return the list of names of archetypes with Conflicts
getConflictArchetype = function(df){
  return(df[startsWith(df$Archetype$Archetype,"Conflict")==TRUE,]$Archetype$Archetype)
}
# Return the list of URL of archetypes with Unknowns
getUnknown = function(df){
  return(df[df$Archetype$Archetype=="Unknown",]$AnchorUri)
}
# Get all the URL of a deck running a specific card
getURLofCard=function(cardName){
  for (i in 1:nrow(df)){
    if (length(grep(cardName,df$Mainboard[[i]]$CardName)) + 
        length(grep(cardName,df$Sideboard[[i]]$CardName))>0){
      print(df$AnchorUri[[i]])
    }
  }
}
# Get all the URL of the decks of a specific archetype
getURLofDeck=function(deckName){
  print(df[df$Archetype$Archetype==deckName,]$AnchorUri)
}
