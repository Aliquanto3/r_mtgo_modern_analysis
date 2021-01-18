##############################################################################C
#STATS OF CARDS OVERALL
ModernCardsList=unique(unlist(df$DLCards))
length(ModernCardsList)

#LIST OF DIFFERENT CARDS
CardResults=setNames(data.frame(matrix(ncol = 4, nrow = length(ModernCardsList))), 
                     c("CardNames", "CardWins", "CardMatches","Presence"))

#A BIT LONG TO EXECUTE, LEAVE IT A FEW MINUTES
for (i in 1:length(ModernCardsList)){
  CardResults$CardNames[i]=ModernCardsList[i]
  
  card_id=grep(CardResults$CardNames[i],df$DLCards)
  
  CardResults$NbDecks[i]=length(grep(CardResults$CardNames[i],df$DLCards))
  
  CardResults$CardWins[i]=sum((df$POINTS[card_id] + 
                                 df$TOP8_PTS[card_id])/3)
  CardResults$CardMatches[i]=sum(df$NB_ROUNDS[card_id] + 
                                   df$TOP8_MATCHES[card_id])
  
  CardResults$Presence[i]=as.numeric(format(round(
    CardResults$CardMatches[i]/sum(df$NB_ROUNDS + df$TOP8_MATCHES)*100,1), 
    nsmall = 1))
    
  
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
}

CardResults[1,]
names(CardResults)
CardResults$NbDecks

#PRINT WINRATES OF THE MOST PLAYED CARDS (WITH THE MOST MATCHES)
CardResults = CardResults[order(-CardResults$NbDecks),]
mostPlayedCards=subset(CardResults[1:50,],select = c(CardNames,Presence,WINRATE_AVERAGE))
mostPlayedCards=arrange(mostPlayedCards,desc(Presence))
print(mostPlayedCards, row.names = TRUE)
write.csv(mostPlayedCards, paste('Results_as_CSV/',Beginning,'-',End,
                                 '_DF_Most_Played_Cards.csv',sep=''),
          row.names = TRUE)

#PRINT WINRATES OF THE TOP CARDS WITH THE BEST WINRATES
CardResults = CardResults[order(-CardResults$WINRATE_AVERAGE),]
highestWinrateCards=subset(CardResults[1:50,],select = c(CardNames,Presence,WINRATE_AVERAGE))
highestWinrateCards=arrange(highestWinrateCards,desc(WINRATE_AVERAGE))
print(highestWinrateCards, row.names = TRUE)
write.csv(highestWinrateCards, paste('Results_as_CSV/',Beginning,'-',End,
                                     '_DF_Highest_Winrate_Cards.csv',sep=''),
          row.names = TRUE)

#SAME BUT ONLY FOR CARDS WITH A SMALL CI ON THE WINRATE
df_small_CI = CardResults[CardResults$WINRATE_95_MAX - CardResults$WINRATE_95_MIN<7,]
length(df_small_CI$CardNames)
df_small_CI = df_small_CI[order(df_small_CI$WINRATE_AVERAGE),]
df_small_CI = df_small_CI[order(-df_small_CI$WINRATE_AVERAGE),]
print(subset(df_small_CI[1:50,],select = c(CardNames,Presence,WINRATE_AVERAGE)), 
      row.names = TRUE)

#INSTEAD WE TAKE THE LOWER BORN OF THE WINRATE CONFIDENCE INTERVAL NOW
CardResults = CardResults[order(-CardResults$WINRATE_95_MIN),]
highestLowerWinrateBorn=subset(CardResults[1:50,],select = c(CardNames,Presence,
                                                             WINRATE_95_MIN))
highestLowerWinrateBorn=arrange(highestLowerWinrateBorn,desc(WINRATE_95_MIN))
print(highestLowerWinrateBorn, row.names = TRUE)
write.csv(highestLowerWinrateBorn, paste('Results_as_CSV/',Beginning,'-',End,
                                         '_DF_Highest_Lower_Winrate_Born_Cards.csv',
                                         sep=''),row.names = TRUE)

max(CardResults$WINRATE_95_MIN)
min(CardResults$WINRATE_95_MIN)

max(CardResults$WINRATE_AVERAGE)
min(CardResults$WINRATE_AVERAGE)

################################################################################
#WINRATE OF THE "NEW" CARDS


################################################################################
#LET US COMPUTE THE WINRATE OF EACH ARTIST
CardResults[1:50,]
names(cardData)
length(unique(cardData$artist))

for (i in 1:length(CardResults$CardNames)){
  if (CardResults$CardNames[i] %in% cardData$name){
    CardResults$Artists[i]=cardData[cardData$name==CardResults$CardNames[i],]$artist[1]
  }else if (CardResults$CardNames[i] %in% cardData$faceName){
    CardResults$Artists[i]=cardData[cardData$faceName==CardResults$CardNames[i],]$artist[1]
  }
}

artistsList=unique(unlist(CardResults$Artists))
length(artistsList)
someZeros=rep(0,length(artistsList))
artistsResults = setNames(data.frame(artistsList,someZeros,someZeros),c("ArtistName","Winrate","Presence"))

for (i in 1:length(artistsList)){
  artistsResults$Winrate[i]=sum((CardResults[CardResults$Artists==artistsResults$ArtistName[i],]$WINRATE_AVERAGE*
                               CardResults[CardResults$Artists==artistsResults$ArtistName[i],]$Presence))/
    sum(CardResults[CardResults$Artists==artistsResults$ArtistName[i],]$Presence)
  
  artistsResults$Winrate[i]=as.numeric(format(round(artistsResults$Winrate[i],1), 
    nsmall = 1))
  
  artistsResults$Presence[i]=sum(CardResults[CardResults$Artists==artistsResults$ArtistName[i],]$Presence)
  artistsResults$Presence[i]=as.numeric(format(round(artistsResults$Presence[i],1), 
                                               nsmall = 1))
}

artistsResults=arrange(artistsResults,desc(Winrate))
head(artistsResults)
artistsResults

artistsResults[artistsResults$ArtistName=="Magali Villeneuve",]

write.csv(artistsResults, paste('Results_as_CSV/','-',Beginning,'-',End,
                                '_DF_Artists_by_Winrate.csv',sep=''),
          row.names = TRUE)

#GRAPHICS BELOW
###################################################################################
#DISPLAY THE CI OF WINRATES FOR EACH CARD WITH A SMALL CI
CI_length=7.5
df_small_CI = CardResults[CardResults$WINRATE_95_MAX -CardResults$WINRATE_95_MIN<CI_length,]
length(df_small_CI$CardNames)
df_small_CI$WINRATE_95_MAX =as.numeric(format(round(
  df_small_CI$WINRATE_95_MAX,3), nsmall = 3))
df_small_CI$WINRATE_AVERAGE=as.numeric(format(round(
  df_small_CI$WINRATE_AVERAGE,3), nsmall = 3))
df_small_CI$WINRATE_95_MIN=as.numeric(format(round(
  df_small_CI$WINRATE_95_MIN,3), nsmall = 3))

df_small_CI$CardNames = reorder(df_small_CI$CardNames, 
                                as.numeric(df_small_CI$WINRATE_AVERAGE))

y_label_small_CI="Winrates and confidence intervals"
graph_title_small_CI=paste("Winrate of each card between", Beginning, "and", 
                           End, "in MTGO",  EventType, "with a CI <", 
                           CI_length, "%",sep = " ")
graph_subtitle_small_CI="Red line for the average winrate and green lines for the 
average CI of those cards combined"

ggplot(df_small_CI, aes(x=CardNames, y=WINRATE_AVERAGE)) + theme_classic() +
  geom_point(size=1) + scale_x_discrete(guide = guide_axis(n.dodge=5))+
  labs(y=y_label_small_CI, title=graph_title_small_CI, 
       subtitle=graph_subtitle_small_CI) + #ylim(0,1) +
  geom_errorbar(aes(ymax = WINRATE_95_MAX , ymin = WINRATE_95_MIN)) + 
  geom_hline(yintercept = mean(df_small_CI$WINRATE_AVERAGE), color="green", 
             linetype="dashed", size=1.5)+
  geom_hline(yintercept = mean(df_small_CI$WINRATE_95_MIN), color="red", 
             linetype="dashed", size=1.5)+
  geom_hline(yintercept = mean(df_small_CI$WINRATE_95_MAX ), color="red", 
             linetype="dashed", size=1.5)+ 
  geom_text(aes(y = stat(df_small_CI$WINRATE_95_MAX ), label = WINRATE_95_MAX , 
                x = CardNames), vjust = -1)+ 
  geom_text(aes(y = stat(df_small_CI$WINRATE_95_MIN), label = WINRATE_95_MIN, 
                x = CardNames), vjust = 1)+ 
  geom_text(aes(y = stat(df_small_CI$WINRATE_AVERAGE), label = WINRATE_AVERAGE, 
                x = CardNames), vjust = -1) 

################################################################################
#CMC depending on land ratio
# ggplot(df[df$NB_LAND!=0,], aes(MD_LAND_RATIO*60, CMC))+ 
#   geom_point() + 
#   coord_cartesian() + theme_bw()+ 
#   labs(title="Average CMC (without lands) of each deck depending on land ratio
#        (Number of lands / Number of cards in the MD)")
