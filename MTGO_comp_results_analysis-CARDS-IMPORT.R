library(jsonify)

#CREATE DF TO CONTAIN THE LIST OF PLAYED CARDS: OVERALL, MD AND SB
ModernDataGetter = function(){
  
  ModernResultsPaths=dir(MTGODataPath, 
                         recursive=TRUE, full.names=TRUE, pattern=
                           "modern-[^league].*?-\\d{4}-\\d{1,2}-\\d{1,2}(-\\d)?\\.json")
  
  ModernResults=jsonlite::fromJSON(ModernResultsPaths[1])$Deck
  #THE IMPORTANT DATA IS CONTAINED IN $Deck, USING ONLY IT MAKES THE MANIPULATION 
  #MUCH EASIER
  #jsonlite APPEARS AS THE BEST LIBRARY FOR THE IMPORT HERE (BETTER NAMES AND RBIND)
  for (i in 2:length(ModernResultsPaths)){
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

ModernData=ModernDataGetter()

specify_decimal = function(x, k) trimws(format(round(x, k), nsmall=k))

#ModernCardsList = dataframe containing the list of cards you want to analyse
#board = "All","MD or "SB
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
  ModernCards$CardCountOutTotalCards=as.numeric(specify_decimal(
    ModernCards$CardCount*100/NbTotalCards,2))
  
  #TOTAL NUMBER OF DECKS PLAYED IN THE EVENTS
  NbTotalDecks=length(ModernCardsList$Raw$AnchorUri)
  
  #GET THE % OF DECKS PLAYING EACH CARD
  ModernCards$DeckCountOutTotalDecks=as.numeric(specify_decimal(
    ModernCards$DeckCount*100/NbTotalDecks,2))
  
  #GET THE AVERAGE NUMBER OF COPIES OF EACH CARD
  ModernCards$AverageCopies=as.numeric(specify_decimal(
    ModernCards$CardCount/ModernDecksCounts,2))
  
  return(ModernCards)
}

#DATA FROM: https://mtgjson.com/downloads/all-files/
setwd(DirectoryFile)
cardData=read.csv("cards.csv",sep=",",header=T)
#View(cardData)
names(cardData)
cardDataSub=subset(cardData,select=c(colors,convertedManaCost,faceName,layout,
                                     manaCost,name,setCode,subtypes,supertypes,
                                     type,types))
names(cardDataSub)
# unique(cardDataSub$type)
# unique(cardDataSub$types)
# unique(cardDataSub$subtypes)
# unique(cardDataSub$supertypes)
# unique(cardDataSub$setCode)

#SELECT FIRST LINE CONTAINING THE NAME OF A CARD - EXAMPLE: "Lightning Bolt
boltData0=cardDataSub[cardDataSub$name=="Lightning Bolt",]
boltData=t(cardDataSub[grep("Lightning Bolt", cardDataSub$name),])[
  1:length(names(cardDataSub))]
mode(boltData0)

#TO BE USED FOR DFC
# if (card.layout == "transform" | card.layout == "flip" | 
#     card.layout == "adventure" | card.layout == "meld" | card.layout == "modal_dfc") {
#   cardName = card.faceName
# }else{
#   cardName = card.name;
# }