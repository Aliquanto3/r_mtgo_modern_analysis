#5th file to execute
#Get stats for each card

#returns a dataframe containing statistics for each card in the data: names, 
#number of copies per deck/overall/on average, number of decks, presence  
#board = "Mainboard" or "Sideboard" or "Allboards"
#TODO: merge MD and SB ("All")
CardsStatsGetter = function(df,board){
  
  #GET THE NAMES OF EACH DIFFERENT CARD
  cardList=list()
  for (i in 1:nrow(df)){
    cardList[i]=list(df[[board]][[i]]$CardName)
  }
  CardsNames=unique(unlist(cardList))
  
  #NUMBER OF DIFFERENT CARDS
  NCards=length(CardsNames)
  
  CardsCounts=rep(0,NCards)
  DecksCounts=rep(0,NCards)
  WinsCounts=rep(0,NCards)
  MatchesCounts=rep(0,NCards)
  WinrateAverage=rep(0,NCards)
  Winrate95Min=rep(0,NCards)
  Winrate95Max=rep(0,NCards)
  Artists=rep(0,NCards)
  Colors=rep(0,NCards)
  CMC=rep(0,NCards)
  Types=rep(0,NCards)
  FirstSet=rep(0,NCards)
  Archetypes=rep(0,NCards)
  URL=rep(0,NCards)
  for (i in 1:NCards){
    
    #DOES NOT HANDLE WEAR//TEAR (SPLIT CARDS) VERY WELL!
    if (CardsNames[i] %in% cardDataSub$name){
      cardDataSubi=cardDataSub[cardDataSub$name==CardsNames[i],]
    }else if (CardsNames[i] %in% cardDataSub$faceName){
      cardDataSubi=cardDataSub[cardDataSub$faceName==CardsNames[i],]
    }
    cardDataSubi=cardDataSubi[cardDataSubi$isReprint==0,][1,]
    
    Artists[i]=cardDataSubi$artist
    
    Colors[i]=cardDataSubi$colors
    
    CMC[i]=cardDataSubi$convertedManaCost
    
    Types[i]=cardDataSubi$types
    
    FirstSet[i]=cardDataSubi$setCode
    
    #GET THE NUMBER OF COPIES OF EACH DIFFERENT CARD
    cardCountsi=rep(0,nrow(df))
    for (j in 1:nrow(df)){
      if(length(which(df[[board]][[j]]$CardName==CardsNames[i]))>0){
        
        cardCountsi[j]=sum(df[[board]][[j]][which(
          df[[board]][[j]]$CardName==CardsNames[i]),]$Count)
      }
    }
    CardsCounts[i]=sum(cardCountsi)
    
    #GET THE NUMBER OF DECKS WHERE EACH CARD WAS PLAYED
    deckCountsi=rep(0,nrow(df))
    for (j in 1:nrow(df)){
      if(CardsNames[i] %in% df[[board]][[j]]$CardName){
        deckCountsi[j]=1
      }
    }
    DecksCounts[i]=sum(deckCountsi)
    
    #GET THE NUMBER OF WINS OF THE CARD
    winCountsi=rep(0,nrow(df))
    for (j in 1:nrow(df)){
      if(CardsNames[i] %in% df[[board]][[j]]$CardName){
        winCountsi[j]=sum((df$Points[j] + df$T8Points[j])/3)
      }
    }
    WinsCounts[i]=sum(winCountsi)
    
    #GET THE NUMBER OF MATCHES OF THE CARD
    matchCountsi=rep(0,nrow(df))
    for (j in 1:nrow(df)){
      if(CardsNames[i] %in% df[[board]][[j]]$CardName){
        matchCountsi[j]=sum((df$NRounds[j] + df$T8Matches[j]))
      }
    }
    MatchesCounts[i]=sum(matchCountsi)
    
    #CI WITH BINOMIAL TEST
    WinrateAverage[i]=as.numeric(format(round(
      binom.test(WinsCounts[i], MatchesCounts[i], p=0.5,
                 alternative="two.sided", conf.level=0.95)$estimate*100,1), 
      nsmall = 1))
    
    Winrate95Min[i]=as.numeric(format(round(
      binom.test(WinsCounts[i], MatchesCounts[i], p=0.5,
                 alternative="two.sided", conf.level=0.95)$conf.int[1]*100,1), 
      nsmall = 1))
    
    Winrate95Max[i]=as.numeric(format(round(
      binom.test(WinsCounts[i], MatchesCounts[i], p=0.5,
                 alternative="two.sided", conf.level=0.95)$conf.int[2]*100,1), 
      nsmall = 1))
    
    #GET THE LIST OF ARCHETYPES WHERE THE CARD IS PLAYED
    archetypesi=list()
    for (j in 1:nrow(df)){
      if(CardsNames[i] %in% df[[board]][[j]]$CardName){
        archetypesi[j]=df$Archetype$Archetype[j]
      }
    }
    archetypesiNames=names(table(unlist(archetypesi), exclude = NULL))
    archetypesiCount=as.vector(table(unlist(archetypesi), exclude = NULL))
    archetypesiString=""
    for (k in 1:length(archetypesiNames)){
      if(!is.null(archetypesiNames[k])){
        archetypesiString=paste(archetypesiString,archetypesiCount[k],
                                archetypesiNames[k],"\n",sep=" ")
      }
    }
    Archetypes[i]=archetypesiString
    
    #GET THE LIST OF URL OF DECKS WHERE THE CARD IS PLAYED
    urli=list()
    for (j in 1:nrow(df)){
      if(CardsNames[i] %in% df[[board]][[j]]$CardName){
        urli[j]=df$AnchorUri[j]
      }
    }
    URLiString=""
    for (k in 1:length(urli)){
      if(!is.null(urli[k][[1]])){
        URLiString=paste(URLiString,urli[k],"\n",sep=" ")
      }
    }
    URL[i]=URLiString
  }
  
  #ASSOCIATE THE NAMES OF EACH DIFFERENT CARD WITH THEIR TOTAL NUMBER OF COPIES 
  #AND THE NUMBER OF DECKS PLAYING THEM
  Cards=setNames(data.frame(CardsNames,CardsCounts,DecksCounts,WinsCounts,
                            MatchesCounts,WinrateAverage,Winrate95Min,
                            Winrate95Max,Archetypes,URL,Artists,Colors,
                            CMC,Types,FirstSet), 
                       c("CardNames", "CardCount", "DeckCount", "WinCount", 
                         "MatchCount","WinrateAverage","Winrate95Min",
                         "Winrate95Max","Archetypes","URL","Artists","Colors",
                         "CMC","Types","FirstSet"))
  
  #TOTAL NUMBER OF CARDS PLAYED IN THE EVENTS
  NbTotalCards=sum(Cards$CardCount)
  
  #GET THE % PRESENCE OF EACH CARD OUT OF ALL THE CARDS PLAYED
  Cards$CardCountOutTotalCards=as.numeric(format(round(
    Cards$CardCount*100/NbTotalCards,1), nsmall = 1))
  
  #GET THE % OF DECKS PLAYING EACH CARD
  Cards$DeckCountOutTotalDecks=as.numeric(format(round(
    Cards$DeckCount*100/nrow(df),1), nsmall = 1))
  
  #GET THE AVERAGE NUMBER OF COPIES OF EACH CARD
  Cards$AverageCopies=as.numeric(format(round(
    Cards$CardCount/DecksCounts,1), nsmall = 1))
  
  return(Cards)
}

#UNCOMMENT IF YOU EXECUTE THIS FILE ALONE
# # #STATS OF CARDS OVERALL - TODO
# # CardsStats=CardsStatsGetter(df,"All")
# #STATS OF MD CARDS
# MDStats=CardsStatsGetter(df,"Mainboard")
# #STATS OF SB CARDS
# SBStats=CardsStatsGetter(df,"Sideboard")

# ################################################################################
# #CMC depending on land ratio
# # ggplot(df[df$NB_LAND!=0,], aes(MD_LAND_RATIO*60, CMC))+ 
# #   geom_point() + 
# #   coord_cartesian() + theme_bw()+ 
# #   labs(title="Average CMC (without lands) of each deck depending on land ratio
# #        (Number of lands / Number of cards in the MD)")
