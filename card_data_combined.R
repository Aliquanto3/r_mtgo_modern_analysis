Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_181') 
library(xlsx)
mdCardsDf=read.csv("D:/MTG/Meta analysis/r_mtgo_modern_analysis/MTGO_Data/Modern/2021-04-15_2021-05-17/Official Competitions/Results_as_csv/2021-04-15-2021-05-17_STX_MD_cardData.csv")
sbCardsDf=read.csv("D:/MTG/Meta analysis/r_mtgo_modern_analysis/MTGO_Data/Modern/2021-04-15_2021-05-17/Official Competitions/Results_as_csv/2021-04-15-2021-05-17_STX_SB_cardData.csv")

#View(sbCardsDf)

names(mdCardsDf)
names(sbCardsDf)

cardsDf = data.frame(matrix(ncol = length(names(mdCardsDf)), nrow = 0))
colnames(cardsDf) = names(mdCardsDf)

for (i in 1:nrow(mdCardsDf)){
  cardsDf[i,]=mdCardsDf[i,]
  for (j in 1:nrow(sbCardsDf)){
    if (cardsDf[i,]$CardNames==sbCardsDf[j,]$CardNames){
      cardsDf[i,]$CardCount = cardsDf[i,]$CardCount + sbCardsDf[j,]$CardCount
      #WRONG, duplicates
      cardsDf[i,]$DeckCount = cardsDf[i,]$DeckCount + sbCardsDf[j,]$DeckCount
      cardsDf[i,]$WinCount = cardsDf[i,]$WinCount + sbCardsDf[j,]$WinCount
      cardsDf[i,]$MatchCount = cardsDf[i,]$MatchCount + sbCardsDf[j,]$MatchCount
      cardsDf[i,]$WinrateAverage = binom.test(cardsDf[i,]$WinCount, cardsDf[i,]$MatchCount, 
                 p=0.5,alternative="two.sided", conf.level=0.95)$estimate
      cardsDf[i,]$WinrateAverage = binom.test(cardsDf[i,]$WinCount, cardsDf[i,]$MatchCount, 
                                              p=0.5,alternative="two.sided", conf.level=0.95)$conf.int[1]
      cardsDf[i,]$WinrateAverage = binom.test(cardsDf[i,]$WinCount, cardsDf[i,]$MatchCount, 
                                              p=0.5,alternative="two.sided", conf.level=0.95)$conf.int[2]
      cardsDf[i,]$Archetypes = paste(cardsDf[i,]$Archetypes,sbCardsDf[j,]$Archetypes)
      cardsDf[i,]$URL = paste(cardsDf[i,]$URL,sbCardsDf[j,]$URL)
      cardsDf[i,]$CardCountOutTotalCards = cardsDf[i,]$CardCountOutTotalCards + sbCardsDf[j,]$CardCountOutTotalCards
      cardsDf[i,]$DeckCountOutTotalDecks = cardsDf[i,]$DeckCountOutTotalDecks + sbCardsDf[j,]$DeckCountOutTotalDecks
      cardsDf[i,]$AverageCopies = (mdCardsDf[i,]$CardCount*mdCardsDf[i,]$AverageCopies + 
        sbCardsDf[j,]$CardCount*sbCardsDf[j,]$AverageCopies)/(mdCardsDf[i,]$CardCount+sbCardsDf[j,]$CardCount)
    }
  }
}
for (i in 1:nrow(sbCardsDf)){
  if (!sbCardsDf[i,]$CardNames %in% cardsDf$CardNames){
    cardsDf=rbind(cardsDf,sbCardsDf[i,])
  }
}

#View(cardsDf)
#head(cardsDf)
write.csv(cardsDf,"D:/MTG/Meta analysis/r_mtgo_modern_analysis/MTGO_Data/Modern/2021-04-15_2021-05-17/Official Competitions/Results_as_csv/2021-04-15-2021-05-17_STX_cardData.csv",
          row.names = FALSE,col.names = TRUE)
write.xlsx(cardsDf, "D:/MTG/Meta analysis/r_mtgo_modern_analysis/MTGO_Data/Modern/2021-04-15_2021-05-17/Official Competitions/Results_as_csv/2021-04-15-2021-05-17_STX_cardData.xlsx",
           row.names = FALSE,col.names = TRUE)

##########################
#Get the URL for a specific archetype
archMaxDf=df[df$Archetype$Archetype=="Amulet Titan",]
archMaxDf$AnchorUri
archMaxDf[archMaxDf$Points+archMaxDf$T8Points==max(archMaxDf$Points+archMaxDf$T8Points),]$AnchorUri


names(df)

ArchetypeName="Elementals"
CardToLookFor="Teferi, Time Raveler"
ArchetypeDf=df[df$Archetype$Archetype==ArchetypeName,]
ArchetypeWithTheCard=ArchetypeDf[grep(CardToLookFor,ArchetypeDf$Mainboard),]
ArchetypeWoTheCard=ArchetypeDf[-grep(CardToLookFor,ArchetypeDf$Mainboard),]
WinrateWithTheCard=sum(ArchetypeWithTheCard$Points+ArchetypeWithTheCard$T8Points)/
  3/sum(ArchetypeWithTheCard$NRounds+ArchetypeWithTheCard$T8Matches)
WinrateWoTheCard=sum(ArchetypeWoTheCard$Points+ArchetypeWoTheCard$T8Points)/
  3/sum(ArchetypeWoTheCard$NRounds+ArchetypeWoTheCard$T8Matches)
print(paste("The winrate of ", ArchetypeName," decks with ",CardToLookFor," is ",
            format(round(WinrateWithTheCard*100, 2), nsmall = 2)," % for ",
            nrow(ArchetypeWithTheCard),
            " decklists between ", Beginning," and ", End,".",sep = ""))
print(paste("The winrate of ", ArchetypeName," decks without ",CardToLookFor," is ",
            format(round(WinrateWoTheCard*100, 2), nsmall = 2)," % for ",
            nrow(ArchetypeWoTheCard),
            " decklists between ", Beginning," and ", End,".",sep = ""))
