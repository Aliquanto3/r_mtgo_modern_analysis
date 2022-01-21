library(rprojroot)

setwd(rprojroot::find_rstudio_root_file())
getwd()

source(file.path(paste(rprojroot::find_rstudio_root_file(),"1-PARAMETERS.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"2-ARCHETYPES-IMPORT.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"3-METAGAME_FUNCTIONS.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"4-CARD_DATA-IMPORT.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"5-CARD_STATS.R",sep="/")))

#TO BE USED LATER WITH A LOOP ON EACH ARCHETYPE?
#WHAT IS THE DIFFERENCE BETWEEN BOTH? CAN THEY BE IN A SINGLE FILE?
# source("D:/MTG/Meta analysis/r_mtgo_modern_analysis/6-DECKLISTS_ANALYSIS.R")
# source("D:/MTG/Meta analysis/r_mtgo_modern_analysis/7_ARCHETYPE_ANALYSIS.R")

# df=generate_df(EventType,MTGFormat,RawFile)
# archetype_acc=getArchetypeAcc(Classification)
# #STILL HAS WARNINGS, WHERE DO THEY COME FROM? - NOT REALLY AN EMERGENCY
# cardDataSub=getCardData(DirectoryFile)
# df=addCMC(df)
# MDStats=CardsStatsGetter(df,"Mainboard")
# SBStats=CardsStatsGetter(df,"Sideboard")
# #CardResults=CardsStatsGetter(df,"Allboards")

#source("D:/MTG/Meta analysis/r_mtgo_modern_analysis/EXPORT_GRAPHS_AND_TXT.R")

#LAST FILE TO EXECUTE FOR ARTICLE DATA

df=generate_df(EventType,MTGFormat,RawFile)
archetype_acc=getArchetypeAcc(Classification)
#STILL HAS WARNINGS, WHERE DO THEY COME FROM? - NOT REALLY AN EMERGENCY
cardDataSub=getCardData(DirectoryFile)
#df=addCMC(df) #Unnecessary atm and long to run
if(!PlayerResultsOnly){
  MDStats=CardsStatsGetter(df,"Mainboard")
  SBStats=CardsStatsGetter(df,"Sideboard")
}
#CardResults=CardsStatsGetter(df,"Allboards")

#CREATE THE DIRECTORY WHERE TO SAVE THE PICTURES
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

#GRAPHIC ANALYSIS
metric_df=metric_points_archetypes(df,Beginning,End)
metric_df_log_matches=metric_df
metric_df_log_matches$TotalMatches=
  log(metric_df_log_matches$TotalMatches)

#################################################################################

#WORKING DIRECTORY FOR THE CSV
setwd(file.path(paste(rprojroot::find_rstudio_root_file(),
                      DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                      EventType,sep="/"), "Results_as_csv"))

# write.csv(metric_df,paste(Beginning,'-',End,'_DF_Archetypes_Results.csv',sep=''), 
#           row.names = TRUE)

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

#GET THE DATA FOR ALL PLAYERS
metric_df_play=metric_points_players(df,Beginning,End)
metric_df_play_sub=metric_df_play[c("PlayerS","NAppearances","TotalMatches",
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

################################################################################

#WORKING DIRECTORY FOR THE GRAPHS
setwd(file.path(paste(rprojroot::find_rstudio_root_file(),
                      DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                      EventType,sep="/"), "Results_as_pictures"))

#PRESENCE
PieShare=2
# 1. Open jpeg and svg file
graph=metagame_pie_chart(df,"Copies",Beginning,End,EventType)
ggsave(file=paste("Pieplot_Copies_Presence_",Beginning,"_",End,"_",EventType,".svg",
                  sep=""), plot=graph, width=10, height=10)
jpeg(paste("Pieplot_Copies_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
graph
dev.off()

graph=metagame_pie_chart(df,"Matches",Beginning,End,EventType)
ggsave(file=paste("Pieplot_Matches_Presence_",Beginning,"_",End,"_",EventType,".svg",
                  sep=""), plot=graph, width=10, height=10)
jpeg(paste("Pieplot_Matches_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
graph
dev.off()

graph=metagame_pie_chart(df,"Players",Beginning,End,EventType)
ggsave(file=paste("Pieplot_Players_Presence_",Beginning,"_",End,"_",EventType,".svg",
                  sep=""), plot=graph, width=10, height=10)
jpeg(paste("Pieplot_Players_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
graph
dev.off()

graph=metagame_box_plot(df,"Matches",Beginning,End,EventType,0)
ggsave(file=paste("Barplot_Matches_Presence_",Beginning,"_",End,"_",EventType,".svg",
                  sep=""), plot=graph, width=10, height=10)
jpeg(paste("Barplot_Matches_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
graph
dev.off()

#WINRATES
arch_ranked=archetypes_ranking(metric_df,Beginning,End)

graph=winrates_graph(df,arch_ranked,"Matches",Beginning,End,EventType)
ggsave(file=paste("0.95 CI on winrate between", Beginning, "and", End, 
                  "in MTGO", EventType,".svg"), plot=graph, width=24, height=9)
jpeg(paste("0.95 CI on winrate between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
graph
dev.off()

graph=winrates_graph(df,arch_ranked,"Matches",Beginning,End,EventType)
ggsave(file=paste("Low winrate estimation in 0.95 CI between", Beginning, "and", End, 
                  "in MTGO", EventType,".svg"), plot=graph, width=24, height=9)
jpeg(paste("Low winrate estimation in 0.95 CI between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
graph
dev.off()

graph=winrate_ratio_arch_out_player_graph(df,metric_df,Beginning,End,EventType)
ggsave(file=paste("Ratio of archetype winrates out of pilot winrates between", Beginning, 
                  "and", End, "in MTGO", EventType,".svg"), plot=graph, width=24, height=9)
jpeg(paste("Ratio of archetype winrates out of pilot winrates between", Beginning, 
           "and", End, "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
graph
dev.off()

#presence AND diameters CAN BE EITHER "Copies", "Players" or "Matches"
#tiers CAN BE EITHER "Win+Pres","Pres M+SD" or "Pres %"
#function(metric_df,presence,diameters,diam_ratio,beginning,end,tiers,isLog,
#only_best,EventType)
graph=metric_graph(metric_df,"Matches","Players",1,Beginning,End,"Pres M+SD",TRUE,
                   FALSE,EventType)
ggsave(file=paste("Winrates and presence based on Matches between", Beginning, "and", 
                  End,"in MTGO", EventType,".svg"), plot=graph, width=18, height=9)
jpeg(paste("Winrates and presence based on Matches between", Beginning, "and", 
           End,"in MTGO", EventType,".jpg"), width = 6000, height = 3000, res=400)
graph
dev.off()

#ARCHETYPE CLUSTERING
#function (metric_df,k,iter,init,algo,beginning,end,count_wr,
#only_best,EventType)
graph=kmeans_arch(metric_df,4,30,50,"Hartigan-Wong",Beginning,End,TRUE,TRUE,EventType)
ggsave(file=paste("Best archetypes clustering between", Beginning, "and", End,
                  "in MTGO", EventType,".svg"), plot=graph, width=15, height=9)
jpeg(paste("Best archetypes clustering between", Beginning, "and", End,
           "in MTGO", EventType,".jpg"), width = 5000, height = 3000, res=300)
graph
dev.off()


#WITH LOGARITHM OF PRESENCE
arch_ranked=archetypes_ranking(metric_df_log_matches,Beginning,End)

metric_df_log_matches_sub=metric_df_log_matches[
  metric_df_log_matches$TotalMatches>mean(
    metric_df_log_matches$TotalMatches),]
arch_ranked_sub=archetypes_ranking(metric_df_log_matches_sub,Beginning,End)

#RANKING THE DECKS BASED ON COMBINATION OF WINRATE AND LOG(PRESENCE)
graph=log_comb_graph(df,arch_ranked_sub,Beginning,End,EventType)
ggsave(file=paste("Deck score based on sum of winrate and log(presence) between", 
                  Beginning, "and", End, "in MTGO", EventType,".svg"), plot=graph, width=24, height=9)
jpeg(paste("Deck score based on sum of winrate and log(presence) between", 
           Beginning, "and", End, "in MTGO", EventType,".jpg"), width = 8000, height = 3000, res=400)
graph
dev.off()

#RANKING THE DECKS BASED ON META SCORE OF WINRATE AND LOG(PRESENCE)
graph=meta_score_graph(df,arch_ranked_sub,Beginning,End,EventType)
ggsave(file=paste("Deck score based on VS Meta Score between", 
                  Beginning, "and", End, "in MTGO", EventType,".svg"), plot=graph, width=24, height=9)
jpeg(paste("Deck score based on VS Meta Score between", 
           Beginning, "and", End, "in MTGO", EventType,".jpg"), width = 8000, height = 3000, res=400)
graph
dev.off()

df_tiers_list=generate_tiers_lists(arch_ranked_sub)

#View(df_tiers_list)

setwd(file.path(paste(rprojroot::find_rstudio_root_file(),
                      DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
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
setwd(file.path(paste(rprojroot::find_rstudio_root_file(),
                      DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                      EventType,sep="/"), "Results_as_txt"))

nbDecks=length(df$AnchorUri)
nbDiffPlayers=length(unique(df$Player))
nbDiffCards=length(unique(c(MDStats$CardNames,SBStats$CardNames)))
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

################################################################################

#COUNT THE PROPORTION OF RED AND BLUE SPELLS IN THE DATA

#COUNT THE PROPORTION OF DECKS PLAYING RED OR BLUE SPELLS

dfChall=df[!grepl(pattern = "Showcase", df$TournamentName),]
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
  wrList[companionIndex]=wr
  
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

resComps=data.frame(companions,noCompanions,signif(wrCompanions,4),signif(skewCompanions,3),signif(varCompanions,3))
colnames(resComps)=c("Companion","No decks","Win rate","WR Skewness","WR Variance")
resComps
