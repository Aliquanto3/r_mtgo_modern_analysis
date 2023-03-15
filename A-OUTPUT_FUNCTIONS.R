createDirectories = function(DirectoryFile,MTGFormat,Beginning,End,EventType){
  dir.create(file.path(DirectoryFile,MTGFormat))
  dir.create(file.path(paste(DirectoryFile,MTGFormat,sep="/"), 
                       paste(Beginning,End,sep="_")))
  dir.create(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                             sep="/"), EventType))
  
  pathToLastDirs = paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                         EventType,sep="/")
  dir.create(file.path(pathToLastDirs, "Results_as_pictures"))
  dir.create(file.path(pathToLastDirs, "Results_as_csv"))
  dir.create(file.path(pathToLastDirs, "Results_as_txt"))
}

updatedShare = function(metric_df){
  round(mean(metric_df$TotalMatches)/sum(metric_df$TotalMatches)*100,2)
}

writePlayerResults = function(df){
  #GET THE DATA FOR ALL PLAYERS
  metric_df_play=metric_points_players(df)
  metric_df_play_sub=metric_df_play[c("Players","NAppearances","TotalMatches",
                                      "WinrateAverage","Winrate95Min",
                                      "Winrate95Max","TotalPoints")]
  metric_df_play_sub[,"ArchetypeNames"]=NA
  metric_df_play_sub[,"ArchetypeCounts"]=NA
  metric_df_play_sub[,"URL"]=NA
  for (i in 1:length(metric_df_play_sub$URL)){
    metric_df_play_sub$ArchetypeNames[i]=paste(unlist(
      metric_df_play$ArchetypeNames[i]),collapse = "; ")
    metric_df_play_sub$ArchetypeCounts[i]=paste(unlist(
      metric_df_play$ArchetypeCounts[i]),collapse = "; ")
    metric_df_play_sub$URL[i]=paste(unlist(metric_df_play$URL[i]),collapse = "; ")
  }
  write.csv(metric_df_play_sub,paste(
    Beginning,'-',End,'_DF_Players_Overall',MTGFormat,'_Results.csv',sep=''), row.names = TRUE)
  
  #COUNT THE NUMBER OF TOP8 FOR EACH PLAYER
  top8_df_play=players_top8(df,Beginning,End)
  top8_df_play_cop=top8_df_play
  top8_df_play_cop[,"ArchetypeNames"]=NA
  top8_df_play_cop[,"ArchetypeCounts"]=NA
  top8_df_play_cop[,"URL"]=NA
  for (i in 1:length(top8_df_play_cop$URL)){
    top8_df_play_cop$ArchetypeNames[i]=paste(
      unlist(top8_df_play$ArchetypeNames[i]),collapse = "; ")
    top8_df_play_cop$ArchetypeCounts[i]=paste(
      unlist(top8_df_play$ArchetypeCounts[i]),collapse = "; ")
    top8_df_play_cop$URL[i]=paste(unlist(top8_df_play$URL[i]),collapse = "; ")
  }
  write.csv(top8_df_play_cop,paste(
    Beginning,'-',End,'_DF_Top8_Players_',MTGFormat,'_Results.csv',sep=''), 
    row.names = TRUE)
}

writeCardResults = function(MDStats,SBStats){
  #PRINT WINRATES OF ALL THE MD CARDS
  CardResultsMD = MDStats[order(-MDStats$DeckCount),]
  
  CardResults2CSV=CardResultsMD
  CardResults2CSV[,"Archetypes"]=NA
  CardResults2CSV[,"URL"]=NA
  for (i in 1:length(CardResults2CSV$CardName)){
    CardResults2CSV$Archetypes[i]=paste(unlist(
      CardResultsMD$Archetypes[i]),collapse = "; ")
    
    CardResults2CSV$URL[i]=paste(unlist(CardResultsMD$URL[i]),
                                 collapse = "; ")
  }
  
  CardResultsMD=CardResults2CSV
  
  write.csv(arrange(CardResultsMD,CardResultsMD$CardNames), paste(
    Beginning,'-',End,'_DF_MD_Cards.csv',sep=''),row.names = TRUE)
  
  #PRINT WINRATES OF ALL THE SB CARDS
  CardResultsSB = SBStats[order(-SBStats$DeckCount),]
  
  CardResults2CSV=CardResultsSB
  CardResults2CSV[,"Archetypes"]=NA
  CardResults2CSV[,"URL"]=NA
  for (i in 1:length(CardResults2CSV$CardName)){
    CardResults2CSV$Archetypes[i]=paste(unlist(
      CardResultsSB$Archetypes[i]),collapse = "; ")
    
    CardResults2CSV$URL[i]=paste(unlist(CardResultsSB$URL[i]),
                                 collapse = "; ")
  }
  
  CardResultsSB=CardResults2CSV
  
  write.csv(arrange(CardResultsSB,CardResultsSB$CardNames), paste(
    Beginning,'-',End,'_DF_SB_Cards.csv',sep=''),row.names = TRUE)
  
  #SELECTS ONLY NONLAND CARDS in MD
  CardResultsNonLand=CardResultsMD[-grep("Land",CardResultsSB$Types),]
  CardResultsNonLand = CardResultsNonLand[order(-CardResultsNonLand$DeckCount),]
  write.csv(arrange(CardResultsNonLand,CardNames), paste(
    Beginning,'-',End,'_DF_MD_Nonland_Cards.csv',sep=''),row.names = TRUE)
  
  #SELECTS ONLY NONLAND CARDS IN SB
  CardResultsNonLand=CardResultsSB[-grep("Land",CardResultsSB$Types),]
  CardResultsNonLand = CardResultsNonLand[order(-CardResultsNonLand$DeckCount),]
  write.csv(arrange(CardResultsNonLand,CardNames), paste(
    Beginning,'-',End,'_DF_SB_Nonland_Cards.csv',sep=''),row.names = TRUE)
  
  #PRINT WINRATES OF THE MOST PLAYED CARDS (WITH THE MOST MATCHES) IN MD
  mostPlayedCardsMD=subset(CardResultsMD[1:50,],select = c(CardNames,DeckCount,
                                                           WinrateAverage))
  mostPlayedCardsMD=arrange(mostPlayedCardsMD,desc(DeckCount))
  write.csv(mostPlayedCardsMD, paste(Beginning,'-',End,'_DF_MD_Most_Played_Cards.csv',
                                     sep=''),row.names = TRUE)
  
  #PRINT WINRATES OF THE MOST PLAYED CARDS (WITH THE MOST MATCHES) IN SB
  mostPlayedCardsSB=subset(CardResultsSB[1:50,],select = c(CardNames,DeckCount,
                                                           WinrateAverage))
  mostPlayedCardsSB=arrange(mostPlayedCardsSB,desc(DeckCount))
  write.csv(mostPlayedCardsSB, paste(Beginning,'-',End,'_DF_SB_Most_Played_Cards.csv',
                                     sep=''),row.names = TRUE)
  
  #PRINT WINRATES OF THE TOP CARDS WITH THE BEST WINRATES IN MD
  CardResultsMD = CardResultsMD[order(-CardResultsMD$WinrateAverage),]
  highestWinrateCards=subset(CardResultsMD[1:50,],select = c(CardNames,DeckCount,
                                                             WinrateAverage))
  highestWinrateCards=arrange(highestWinrateCards,desc(WinrateAverage))
  write.csv(highestWinrateCards, paste(
    Beginning,'-',End,'_DF_MD_Highest_Winrate_Cards.csv',sep=''),row.names = TRUE)
  
  #PRINT WINRATES OF THE TOP CARDS WITH THE BEST WINRATES IN SB
  CardResultsSB = CardResultsSB[order(-CardResultsSB$WinrateAverage),]
  highestWinrateCards=subset(CardResultsSB[1:50,],select = c(CardNames,DeckCount,
                                                             WinrateAverage))
  highestWinrateCards=arrange(highestWinrateCards,desc(WinrateAverage))
  write.csv(highestWinrateCards, paste(
    Beginning,'-',End,'_DF_SB_Highest_Winrate_Cards.csv',sep=''),row.names = TRUE)
  
  #INSTEAD WE TAKE THE LOWER BOUND OF THE WINRATE CONFIDENCE INTERVAL NOW IN MD
  CardResultsMD = CardResultsMD[order(-CardResultsMD$Winrate95Min),]
  highestLowerWinrateBound=subset(CardResultsMD[1:50,],select = c(CardNames,DeckCount,
                                                                  Winrate95Min))
  highestLowerWinrateBound=arrange(highestLowerWinrateBound,desc(Winrate95Min))
  write.csv(highestLowerWinrateBound, paste(
    Beginning,'-',End,'_DF_MD_Highest_Lower_Winrate_Bound_Cards.csv',sep=''),
    row.names = TRUE)
  
  #INSTEAD WE TAKE THE LOWER BOUND OF THE WINRATE CONFIDENCE INTERVAL NOW IN SB
  CardResultsSB = CardResultsSB[order(-CardResultsSB$Winrate95Min),]
  highestLowerWinrateBound=subset(CardResultsSB[1:50,],select = c(CardNames,DeckCount,
                                                                  Winrate95Min))
  highestLowerWinrateBound=arrange(highestLowerWinrateBound,desc(Winrate95Min))
  write.csv(highestLowerWinrateBound, paste(
    Beginning,'-',End,'_DF_SB_Highest_Lower_Winrate_Bound_Cards.csv',sep=''),
    row.names = TRUE)
  
  #ARTIST RESULTS
  artistsResults=artistStatsGetter(CardResultsMD,CardResultsSB)
  
  write.csv(artistsResults, paste(
    Beginning,'-',End,'_DF_Artist_Results.csv',sep=''),row.names = TRUE)
  
  #SET RESULTS
  setResults=setStatsGetter(CardResultsMD,CardResultsSB)
  
  write.csv(setResults, paste(
    Beginning,'-',End,'_DF_Set_Results.csv',sep=''),row.names = TRUE)
  
  # DATA OF THE CARDS FROM THE LAST SET IN MD
  #SELECT ALL THE CARDS FROM THE LAST SET
  lastSetCards=cardDataSub[cardDataSub$setCode==lastSetCode,]
  #KEEP ONLY THE FIRST PRINTED CARDS
  lastSetOCards=lastSetCards[lastSetCards$isReprint==0,]
  
  FormatCardsListMD=unique(MDStats$CardNames)
  lastSetPlayedCards=c()
  for (i in 1:length(FormatCardsListMD)){
    if (FormatCardsListMD[i] %in% lastSetOCards$name){
      lastSetPlayedCards[length(lastSetPlayedCards)+1]=FormatCardsListMD[i]
    }else if (FormatCardsListMD[i] %in% lastSetOCards$faceName){
      lastSetPlayedCards[length(lastSetPlayedCards)+1]=FormatCardsListMD[i]
    }
  }
  
  lastSetOCardsData = CardResultsMD[CardResultsMD$CardNames %in% lastSetPlayedCards, ]
  
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
    Beginning,'-',End,'_',lastSetCode,'_MD_cardData.csv',sep=''), row.names = FALSE)
  
  # DATA OF THE CARDS FROM THE LAST SET IN SB
  
  #SELECT ALL THE CARDS FROM THE LAST SET
  lastSetCards=cardDataSub[cardDataSub$setCode==lastSetCode,]
  #KEEP ONLY THE FIRST PRINTED CARDS
  lastSetOCards=lastSetCards[lastSetCards$isReprint==0,]
  
  FormatCardsListSB=unique(SBStats$CardNames)
  lastSetPlayedCards=c()
  for (i in 1:length(FormatCardsListSB)){
    if (FormatCardsListSB[i] %in% lastSetOCards$name){
      lastSetPlayedCards[length(lastSetPlayedCards)+1]=FormatCardsListSB[i]
    }else if (FormatCardsListSB[i] %in% lastSetOCards$faceName){
      lastSetPlayedCards[length(lastSetPlayedCards)+1]=FormatCardsListSB[i]
    }
  }
  
  lastSetOCardsData = CardResultsSB[CardResultsSB$CardNames %in% lastSetPlayedCards, ]
  
  lastSetOCardsData=arrange(lastSetOCardsData,lastSetOCardsData$CardName)
  
  lastSetOCardsData2CSV=lastSetOCardsData
  
  if(nrow(lastSetOCardsData2CSV)>0){
    lastSetOCardsData2CSV[,"Archetypes"]=NA
    lastSetOCardsData2CSV[,"URL"]=NA
    
    for (i in 1:length(lastSetOCardsData2CSV$CardName)){
      lastSetOCardsData2CSV$Archetypes[i]=paste(unlist(
        lastSetOCardsData$Archetypes[i]),collapse = "; ")
      
      lastSetOCardsData2CSV$URL[i]=paste(unlist(lastSetOCardsData$URL[i]),
                                         collapse = "; ")
    }
    
    write.csv(lastSetOCardsData2CSV,paste(
      Beginning,'-',End,'_',lastSetCode,'_SB_cardData.csv',sep=''), row.names = FALSE)
  }else{
    write.csv(NULL,paste(
      Beginning,'-',End,'_',lastSetCode,'_SB_cardData.csv',sep=''), row.names = FALSE)
  }
}

drawPresenceGraphs = function(df,Beginning,End,EventType){
  
}

drawWinRateGraphs = function(df,Beginning,End,EventType){
  
}

bidimensionGraphs = function(df,Beginning,End,EventType){
  
}

scoreGraphs = function(df,Beginning,End,EventType){
  
}

convertCSV2XLSX = function(DirectoryFile,MTGFormat,Beginning,End,EventType){
  csvNames = list.files(paste(DirectoryFile,MTGFormat,paste(
    Beginning,End,sep="_"),EventType,"Results_as_csv",sep="/"), pattern="*.csv", 
    full.names=TRUE)
  
  for(i in csvNames) {
    csvi = read.csv(i)
    xlsxiName = sub('.csv', '.xlsx', i, fixed = TRUE)
    write.xlsx(csvi, xlsxiName, row.names = FALSE)
  }
}

exportTextAnalysis = function(){
  
  nbDecks=length(df$AnchorUri)
  nbDiffPlayers="N/A"
  if(PlayerResults){
    nbDiffPlayers=length(unique(df$Player))
  }
  nbDiffCards="N/A"
  if(CardResults){
    nbDiffCards=length(unique(c(MDStats$CardNames,SBStats$CardNames)))
  }
  nbExactArch=length(unique(df$Archetype$Archetype))
  nbSuperArch=length(unique(df$Archetype$SuperArchetype))
  totNbRounds=sum(df$NRounds)+sum(df$T8Matches)
  # avgNbRoundsWTop8=as.numeric(format(round(
  #   (sum(df$NRounds)+sum(df$T8Matches))/length(df$AnchorUri),2), nsmall = 2))
  avgNbRounds=as.numeric(format(round(sum(df$NRounds)/length(df$AnchorUri),2),nsmall=2))
  minNbRounds=min(df$NRounds)
  maxNbRounds=max(df$NRounds)
  nbEvents=length(unique(df$TournamentName))
  
  eventInfo=paste("QUICK ANALYSIS OF THE DATA", 
                  "\nBeginning: ", Beginning,
                  "\nEnd: ", End,
                  "\nType of MTGO events: ", EventType,
                  "\nNumber of decks in the data: ",nbDecks,
                  "\nCut to be displayed: ", PieShare, "%",
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
                         c("TournamentName", "URL"))
  coveredEvents$TournamentName=unique(df$TournamentName)
  for (i in 1:nbEvents){
    coveredEvents$URL[i]=df[df$TournamentName==coveredEvents$TournamentName[i],]$AnchorUri[1]
    coveredEvents$URL[i]<-gsub("#.*","",coveredEvents$URL[i])
  }
  
  titleEventFile=paste("List of MTGO", EventType,"between",Beginning, "and", End, 
                       "\n", sep=" ")
  cat(titleEventFile,file=paste(Beginning,'-',End,'_List_of_',EventType,'.txt'))
  write.table(coveredEvents, paste(Beginning,'-',End,'_List_of_',EventType,'.txt'), 
              append=TRUE, row.names = FALSE, col.names = FALSE, sep = " - ")
}

companionAnalysis = function(){
  companionAnalysis()
  dfChall=df
  unique(dfChall$TournamentName)
  
  nrow(dfChall)
  
  dfComp=dfChall[!dfChall$Archetype$Companion=="",]
  dfWoComp=dfChall[dfChall$Archetype$Companion=="",]
  
  nrow(dfComp)
  nrow(dfWoComp)
  
  #3 points per win
  CompWins=sum(dfComp$Points + dfComp$T8Points)/3
  CompMatches=sum(dfComp$NRounds + dfComp$T8Matches)
  CompWR=CompWins/CompMatches*100
  
  #3 points per win
  woCompWins=sum(dfWoComp$Points + dfWoComp$T8Points)/3
  woCompMatches=sum(dfWoComp$NRounds + dfWoComp$T8Matches)
  woCompWR=woCompWins/woCompMatches*100
  
  print(paste("The win rate of decks with a companion is: ",round(CompWR,2),
              "% between ",Beginning," and ",End," in Modern Challenges.",sep=""))
  print(paste("The win rate of decks without a companion is: ",round(woCompWR,2),
              "% between ",Beginning," and ",End," in Modern Challenges.",sep=""))
  
  library(e1071)  
  
  #Compare each companion
  wrCompanions=c()
  skewCompanions=c()
  varCompanions=c()
  noCompanions=c()
  wrList=list()
  quantile0=c()
  quantile025=c()
  quantile05=c()
  quantile075=c()
  quantile100=c()
  companions=unique(dfChall$Archetype$Companion)
  
  for (companionIndex in 1:length(companions)){
    
    dfCompanion=dfChall[dfChall$Archetype$Companion==companions[companionIndex],]
    #3 points per win
    companionWins=(dfCompanion$Points + dfCompanion$T8Points)/3
    companionMatches=dfCompanion$NRounds + dfCompanion$T8Matches
    wr=companionWins/companionMatches
    
    wrCompanions[companionIndex]=mean(wr)*100
    noCompanions[companionIndex]=nrow(dfCompanion)
    skewCompanions[companionIndex]=skewness(wr)
    varCompanions[companionIndex]=var(wr)
    wrList[[companionIndex]]=wr
    quantile0[companionIndex]=quantile(wr,probs=0)
    quantile025[companionIndex]=quantile(wr,probs=0.25)
    quantile05[companionIndex]=quantile(wr,probs=0.5)
    quantile075[companionIndex]=quantile(wr,probs=0.75)
    quantile100[companionIndex]=quantile(wr,probs=1)
    
    # if(companions[companionIndex]==""){
    #   print(paste("The average win rate of ", noCompanions[companionIndex]," decks without a companion is ",
    #               round(wrCompanions[companionIndex],2),"% with a between ",Beginning,
    #               " and ",End," in Modern Challenges.",sep=""))
    # }else{
    #   print(paste("The average win rate of ", noCompanions[companionIndex]," decks with ", 
    #               companions[companionIndex]," as companion is ",
    #               round(wrCompanions[companionIndex],2),"% between ",Beginning,
    #               " and ",End," in Modern Challenges.",sep=""))
    # }
  }
  
  resComps=data.frame(companions,noCompanions,signif(wrCompanions,4),
                      signif(skewCompanions,3),signif(varCompanions,3),quantile0,
                      quantile025,quantile05,quantile075,quantile100)
  colnames(resComps)=c("Companion","No decks","Win rate","WR Skewness","WR Variance",
                       "Quantile 0","Quantile 0.25","Quantile 0.5","Quantile 0.75",
                       "Quantile 1")
  resComps
  
  max.len=max(lengths(wrList))
  wrListNA=list()
  for (i in 1:length(companions)){
    wrListNA[[i]] = c(wrList[[i]], rep(NA, max.len - length(wrList[[i]])))
  }
  wrDf=as.data.frame(do.call(cbind, wrListNA))*100
  names(wrDf)=companions
  
  # boxplot(wrDf,main=
  #           paste("Boxplot of the win rates of each companion between ",Beginning,"
  #                 and ",End," in ", MTGFormat, " ", EventType, sep=""))
  
}

cardComparison = function(Cards){
  wrCards=c()
  skewCards=c()
  varCards=c()
  noCards=c()
  wrList=list()
  quantile0=c()
  quantile025=c()
  quantile05=c()
  quantile075=c()
  quantile100=c()
  
  for (CardIndex in 1:length(Cards)){
    
    deckIndex=c()
    for(i in 1:nrow(dfChall)){
      if(length(grep(pattern=Cards[CardIndex],x=dfChall$Mainboard[[i]]$CardName))>0){
        deckIndex=c(deckIndex,i)
      }
    }
    
    dfCard=dfChall[deckIndex,]
    
    #3 points per win
    CardWins=(dfCard$Points + dfCard$T8Points)/3
    CardMatches=dfCard$NRounds + dfCard$T8Matches
    wr=CardWins/CardMatches
    
    wrCards[CardIndex]=mean(wr)*100
    noCards[CardIndex]=nrow(dfCard)
    skewCards[CardIndex]=skewness(wr)
    varCards[CardIndex]=var(wr)
    wrList[[CardIndex]]=wr
    quantile0[CardIndex]=quantile(wr,probs=0)
    quantile025[CardIndex]=quantile(wr,probs=0.25)
    quantile05[CardIndex]=quantile(wr,probs=0.5)
    quantile075[CardIndex]=quantile(wr,probs=0.75)
    quantile100[CardIndex]=quantile(wr,probs=1)
  }
  
  resComps=data.frame(Cards,noCards,signif(wrCards,4),
                      signif(skewCards,3),signif(varCards,3),quantile0,
                      quantile025,quantile05,quantile075,quantile100)
  colnames(resComps)=c("Card","No decks","Win rate","WR Skewness","WR Variance",
                       "Quantile 0","Quantile 0.25","Quantile 0.5","Quantile 0.75",
                       "Quantile 1")
  resComps
  
  max.len=max(lengths(wrList))
  wrListNA=list()
  for (i in 1:length(Cards)){
    wrListNA[[i]] = c(wrList[[i]], rep(NA, max.len - length(wrList[[i]])))
  }
  wrDf=as.data.frame(do.call(cbind, wrListNA))*100
  names(wrDf)=Cards
  
  # boxplot(wrDf,main=
  #           paste("Boxplot of the win rates of each Card between ",Beginning,"
  #                 and ",End," in ", MTGFormat, " ", EventType, sep=""))
}