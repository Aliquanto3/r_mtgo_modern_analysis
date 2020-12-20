#4th file to execute
#Import the decklists of all events in the chosen period

#Data cloned from: https://github.com/Badaro/MTGODecklistCache 

#CREATE DF TO CONTAIN THE LIST OF PLAYED CARDS: OVERALL, MD AND SB
ModernDataGetter = function(){
  
  DaysList=seq(as.Date(Beginning), as.Date(End), by="days")
  
  ModernResultsPaths=c()
  
  #PATTERNS OF THE FILE NAMES DEPENDING ON THE EVENT TYPES WE USE
  eventPattern=""
  if (EventType=="Competitions"){
    eventPattern="modern-[^league].*?-\\d{4}-\\d{1,2}-\\d{1,2}(-\\d)?\\.json"
  }else if (EventType=="Challenges"){
    eventPattern="modern-.*?challenge.*?-\\d{4}-\\d{1,2}-\\d{1,2}(-\\d)?\\.json"
  }else if (EventType=="Preliminaries"){
    eventPattern="modern-preliminary.*?-\\d{4}-\\d{1,2}-\\d{1,2}(-\\d)?\\.json"
  }
  
  #LIST THE PATHS OF ALL THE RELEVANT FILES
  for (i in 1:length(DaysList)){
    yeari=substr(x = DaysList[i], start = 1, stop = 4)
    monthi=substr(x = DaysList[i], start = 6, stop = 7)
    dayi=substr(x = DaysList[i], start = 9, stop = 10)
    MTGODataPathDate=paste(MTGODataPath,yeari,monthi,dayi,sep = "/")
    ModernResultsPaths=c(ModernResultsPaths,dir(MTGODataPathDate, 
                           recursive=TRUE, full.names=TRUE, pattern=eventPattern))
  }
  
  ModernResults=data.frame()
  #IMPORT ALL THE DECKS DATA FROM EACH EVENT FOR EACH DATE
  #THE IMPORTANT DATA IS CONTAINED IN $Deck, USING ONLY IT MAKES THE MANIPULATION 
  #MUCH EASIER
  #jsonlite APPEARS AS THE BEST LIBRARY FOR THE IMPORT HERE (BETTER NAMES AND RBIND)
  for (i in 1:length(ModernResultsPaths)){
    ModernResults=rbind(ModernResults, jsonlite::fromJSON(ModernResultsPaths[i])$Deck)
  }
  
  #GET THE MD DATA
  ModernMDList=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                        c("CardNames", "CardCount"))
  for (i in 1:length(ModernResults$Mainboard)){
    for (j in 1:length(ModernResults$Mainboard[[i]]$CardName)){
      ModernMDList = rbind(ModernMDList, c(ModernResults$Mainboard[[i]]$CardName[[j]],
                                           (ModernResults$Mainboard[[i]]$Count[[j]])))
      names(ModernMDList)=c("CardNames","CardCount")
    }
  }
  #GET THE SB DATA
  ModernSBList=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                        c("CardNames", "CardCount"))
  for (i in 1:length(ModernResults$Sideboard)){
    for (j in 1:length(ModernResults$Sideboard[[i]]$CardName)){
      ModernSBList = rbind(ModernSBList, c(ModernResults$Sideboard[[i]]$CardName[[j]],
                                           (ModernResults$Sideboard[[i]]$Count[[j]])))
      names(ModernSBList)=c("CardNames","CardCount")
    }
  }
  
  #AGGREGATES THE DATA
  ModernAllResults=rbind(ModernMDList,ModernSBList)
  
  return(list(Raw = ModernResults,MD = ModernMDList,SB = ModernSBList, 
              All = ModernAllResults))
}

#CAN TAKE SOME TIME TO EXECUTE, LEAVE IT A MINUTE OR TWO EVENTUALLY
ModernData=ModernDataGetter()

#ADD THE DECKLISTS OF EACH DECK TO THE MAIN DATAFRAME
DecklistsAdd = function(ModernData,df){
  for (i in 1:length(df$URL)){
    #LIST OF MD CARDS
    df$MD_TEMP[i]=ModernData$Raw[ModernData$Raw$AnchorUri==df$URL[i],]$Mainboard
    #LIST OF SB CARDS
    df$SB_TEMP[i]=ModernData$Raw[ModernData$Raw$AnchorUri==df$URL[i],]$Sideboard
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

df=DecklistsAdd(ModernData,df)

#ModernCardsList = dataframe containing the list of cards you want to analyse
#board = "All","MD or "SB
#returns a dataframe containing statistics for each card in the data: names, 
#number of copies per deck/overall/on average, number of decks, presence  
ModernCardsStatsGetter = function(ModernCardsList,board){
  
  #GET THE NAMES OF EACH DIFFERENT CARD
  ModernCardsNames=unique(ModernCardsList[[board]]$CardNames)
  #NUMBER OF DIFFERENT CARDS
  Nb_diff_cards=length(ModernCardsNames)
  #GET THE NUMBER OF COPIES OF EACH DIFFERENT CARD
  ModernCardsCounts=rep(0,Nb_diff_cards)
  for (i in 1:Nb_diff_cards){
    ModernCardsCounts[i]=sum(as.numeric(ModernCardsList[[board]][which(
      ModernCardsList[[board]]$CardNames==ModernCardsNames[i]),]$CardCount))
  }
  
  #GET THE NUMBER OF DECKS WHERE EACH CARD WAS PLAYED
  ModernDecksCounts=rep(0,Nb_diff_cards)
  for (i in 1:Nb_diff_cards){
    ModernDecksCounts[i]=length(ModernCardsList[[board]][which(
      ModernCardsList[[board]]$CardNames==ModernCardsNames[i]),]$CardCount)
  }
  
  #ASSOCIATE THE NAMES OF EACH DIFFERENT CARD WITH THEIR TOTAL NUMBER OF COPIES AND
  #THE NUMBER OF DECKS PLAYING THEM
  ModernCards=setNames(data.frame(ModernCardsNames,ModernCardsCounts,ModernDecksCounts), 
                       c("CardNames", "CardCount", "DeckCount"))
  #TOTAL NUMBER OF CARDS PLAYED IN THE EVENTS
  NbTotalCards=sum(ModernCards$CardCount)
  
  #GET THE % PRESENCE OF EACH CARD OUT OF ALL THE CARDS PLAYED
  ModernCards$CardCountOutTotalCards=as.numeric(format(round(
    ModernCards$CardCount*100/NbTotalCards,1), nsmall = 1))
  
  #TOTAL NUMBER OF DECKS PLAYED IN THE EVENTS
  NbTotalDecks=length(ModernCardsList$Raw$AnchorUri)
  
  #GET THE % OF DECKS PLAYING EACH CARD
  ModernCards$DeckCountOutTotalDecks=as.numeric(format(round(
    ModernCards$DeckCount*100/NbTotalDecks,1), nsmall = 1))
  
  #GET THE AVERAGE NUMBER OF COPIES OF EACH CARD
  ModernCards$AverageCopies=as.numeric(format(round(
    ModernCards$CardCount/ModernDecksCounts,1), nsmall = 1))
  
  return(ModernCards)
}

#STATS OF CARDS OVERALL
ModernCardsStats=ModernCardsStatsGetter(ModernData,"All")
#STATS OF MD CARDS
ModernMDStats=ModernCardsStatsGetter(ModernData,"MD")
#STATS OF SB CARDS
ModernSBStats=ModernCardsStatsGetter(ModernData,"SB")

