################################################################################
#CODE FOR THE REPORT

#IV.1- Analyse des indicateurs
#NUMBER OF DECKS IN THE DATA
length(df$ARCHETYPE)

#NUMBER OF DIFFERENT EXACT ARCHETYPES IN THE DATA
length(unique(df$ARCHETYPE))
#LIST OF DIFFERENT EXACT ARCHETYPES IN THE DATA
unique(df$ARCHETYPE)

#NUMBER OF DIFFERENT SUPER ARCHETYPES IN THE DATA
length(unique(df$SUPER_ARCH))
#LIST OF DIFFERENT SUPER ARCHETYPES IN THE DATA
unique(df$SUPER_ARCH)

#TOTAL NUMBER OF ROUNDS PLAYED IN THE DATA, INCLUDING TOP8 MATCHES
sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES)

#AVERAGE NUMBER OF ROUNDS
(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))/length(df$ARCHETYPE)

# IV.1.A - Indicateur 1 : présence de chaque archétype
#GENERATE THE METAGAME PIE CHART FOR THE SELECTED DATA
PieShare=2.5
metagame_pie_chart(df,"Players")
metagame_pie_chart(df,"Copies")
PieShare=2
metagame_pie_chart(df,"Matches")

#GENERATE THE METAGAME HISTOGRAM FOR THE SELECTED DATA
metagame_box_plot(df,"Matches")


# IV.1.B – Indicateur 2 : nombre de points par ronde (taux de victoire)
winrates_graph(df,arch_ranked,"Matches")

#IV.2 – Analyse de la combinaison des indicateurs
corIndicateurs=cor.test(metric_df$WINRATE_AVERAGE,metric_df$TOTAL_NB_MATCHES/
           max(metric_df$TOTAL_NB_MATCHES),method="pearson")
corIndicateurs

regIndicateurs=lm(WINRATE_AVERAGE~TOTAL_NB_MATCHES, data=metric_df)
summary(reg)
plot(regIndicateurs)

ggplot(metric_df, aes(x=WINRATE_AVERAGE, y=TOTAL_NB_MATCHES)) + 
  theme_classic() + geom_point(size=2,color="blue")

length(metric_df$WINRATE_AVERAGE)

# IV.2.A - Compilation 1 : analyse graphique
metric_graph(metric_df,"Matches","Players")

# IV.2.B - Compilation 2 : combinaison linéaire des indicateurs
PPR_Weight=1
arch_ranked=archetypes_ranking(metric_df)
linear_comb_graph(df,arch_ranked)

PPR_Weight=2
arch_ranked=archetypes_ranking(metric_df)
linear_comb_graph(df,arch_ranked)

PPR_Weight=4
arch_ranked=archetypes_ranking(metric_df)
linear_comb_graph(df,arch_ranked)

# IV.2.C - Compilation 3 : la borne inférieure de l’intervalle de confiance sur 
#les winrates
lower_born_ci_winrate_graph(df,arch_ranked)



















################################################################################



#DRAFT CODE, DO NOT TAKE THIS INTO ACCOUNT, ONLY USED FOR TESTING



################################################################################

#COUNT THE NUMBER OF POINTS PER ROUND
ppr_plot=metric_graph(metric_df,"Matches","Players")
ppr_plot

print(subset(arch_ranked,select = c(RANK,ARCHETYPES)), row.names = FALSE)
print(subset(arch_ranked,select = c(ARCHETYPES)), row.names = FALSE)
#View(subset(arch_ranked,select = c(ARCHETYPES,RANK,METRIC_COMB)))
arch_ranked$ARCHETYPES

#FIND THE URL OF AN ARCHETYPE - ex "Azorius Control"
df[grep("Stoneblade", df$ARCHETYPE),]$URL
#FIND THE NUMBER OF DIFFERENT PILOTES OF AN ARCHETYPE - ex "Azorius Control"
length(unique(df[grep("Hammer Time", df$ARCHETYPE),]$PLAYER))
#NUMBER OF COPIES OF AN ARCHETYPE IN THE DATA
length(df[grep("Hammer Time", df$ARCHETYPE),]$URL)

#FIND THE RANK OF A DECK - EX: "Sultai Control"
arch_ranked[grep("Tron", arch_ranked$ARCHETYPES), ]

#MEAN
ppr_m=mean(arch_ranked$METRIC_COMB)

#STANDARD DEVIATION
ppr_sd=sd(arch_ranked$METRIC_COMB)

#DISPLAYS EACH ARCHETYPE ABOVE THE AVERAGE + X * STANDARD DEVIATION
arch_ranked[arch_ranked$METRIC_COMB>ppr_m+0.5*ppr_sd,]$ARCHETYPES
arch_ranked[arch_ranked$METRIC_COMB>ppr_m+1*ppr_sd,]$ARCHETYPES
arch_ranked[arch_ranked$METRIC_COMB>ppr_m+1.5*ppr_sd,]$ARCHETYPES
arch_ranked[arch_ranked$METRIC_COMB>ppr_m+2*ppr_sd,]$ARCHETYPES
arch_ranked[arch_ranked$METRIC_COMB>ppr_m+2.5*ppr_sd,]$ARCHETYPES
arch_ranked[arch_ranked$METRIC_COMB>ppr_m+3*ppr_sd,]$ARCHETYPES
arch_ranked[arch_ranked$METRIC_COMB>ppr_m+3.5*ppr_sd,]$ARCHETYPES
arch_ranked[arch_ranked$METRIC_COMB>ppr_m+4*ppr_sd,]$ARCHETYPES

#PLOT THE AVERAGE PPR DEPENDING ON THE TOTAL PPR FOR EACH ARCHETYPE, RANKED BY 
#COMBINED PPR
x_label=paste("Archetype rank")
y_label="Combined number of points per round of each archetype (mix of number of 
copies and average number)"
graph_title=paste("Rank of each archetype ", "between", Beginning, "and", End, 
                  "in MTGO", EventType, "based on points per round",sep = " ")
graph_subtitle="Separated by mean + n standard deviations (n={0,1,2,3,4})"

ggplot(arch_ranked, aes(x=RANK, y=METRIC_COMB)) + theme_classic() + geom_point() + 
  geom_text_repel(aes(label=ARCHETYPES),hjust=0, vjust=0,point.padding = NA)+ 
  labs(x=x_label, y=y_label, title=graph_title, subtitle=graph_subtitle)+
  geom_abline(intercept = ppr_m, slope = 0, 
              color="red", linetype="dashed", size=1)+ 
  geom_abline(intercept = ppr_m+1*ppr_sd, slope = 0, 
              color="orange", linetype="dashed", size=1)+ 
  geom_abline(intercept = ppr_m+2*ppr_sd, slope = 0, 
              color="green", linetype="dashed", size=1)+ 
  geom_abline(intercept = ppr_m+3*ppr_sd, slope = 0, 
              color="blue", linetype="dashed", size=1)+ 
  geom_abline(intercept = ppr_m+4*ppr_sd, slope = 0, 
              color="purple", linetype="dashed", size=1)+ 
  scale_colour_manual(name = 'PC1 > 0', values = setNames(c('red','green'),c(T, F)))

################################################################################
#GET ONLY THE DECKS APPEARING THE MOST IN THE DATA
nb_copies_min=PieShare/100*length(df$ARCHETYPE)
ppr_most_played=arch_ranked[arch_ranked$NB_COPIES>=nb_copies_min,]
ppr_most_played = ppr_most_played[order(-ppr_most_played$WINRATE_AVERAGE),]
ppr_most_played
print(subset(ppr_most_played,select = c(NB_COPIES,ARCHETYPES,WINRATE)), 
      row.names = FALSE)
length(ppr_most_played$ARCHETYPES)
ppr_most_played$RANK=ppr_most_played$WINRATE
for (i in 1:length(ppr_most_played$RANK)){
  ppr_most_played$RANK[i]=i
}
################################################################################
#PLOTS OF THE ARCHETYPE METRICS

#PLOT THE AVERAGE PPR DEPENDING ON THE TOTAL PPR FOR EACH ARCHETYPE, RANKED BY 
#COMBINED PPR
arch_ranked$ARCHETYPES = reorder(arch_ranked$ARCHETYPES, 
                                as.numeric(arch_ranked$WINRATE_AVERAGE))
y_label_full_winrate="Winrates and confidence intervals"
graph_title_full_winrate=paste("Winrate of each archetype between", Beginning, 
                               "and", End, "in MTGO",  EventType,sep = " ")
graph_subtitle_full_winrate="Red line for the average winrate of those archetypes 
combined"

ggplot(arch_ranked, aes(x=ARCHETYPES, y=WINRATE_AVERAGE)) + theme_classic() +
  geom_point(size=4) + scale_x_discrete(guide = guide_axis(n.dodge=5))+
  labs(y=y_label_full_winrate, title=graph_title_full_winrate, 
       subtitle=graph_subtitle_full_winrate) + 
  geom_errorbar(aes(ymax = WINRATE_95_MAX , ymin = WINRATE_95_MIN)) + 
  geom_hline(yintercept = mean(arch_ranked$WINRATE_AVERAGE), color="red", 
             linetype="dashed", size=1.5)
 
#SAME FOR ONLY CI UNDER A DETERMINED LENGTH
#specify_decimal = function(x, k) trimws(format(round(x, k), nsmall=k))

CI_length=0.3
df_small_CI = arch_ranked[arch_ranked$WINRATE_95_MAX -arch_ranked$WINRATE_95_MIN<CI_length,]
df_small_CI$WINRATE_95_MAX =as.numeric(specify_decimal(df_small_CI$WINRATE_95_MAX ,3))
df_small_CI$WINRATE_AVERAGE=as.numeric(specify_decimal(df_small_CI$WINRATE_AVERAGE,3))
df_small_CI$WINRATE_95_MIN=as.numeric(specify_decimal(df_small_CI$WINRATE_95_MIN,3))

df_small_CI$ARCHETYPES = reorder(df_small_CI$ARCHETYPES, 
                                 as.numeric(df_small_CI$WINRATE_AVERAGE))

y_label_small_CI="Winrates and confidence intervals"
graph_title_small_CI=paste("Winrate of each archetype between", Beginning, "and", 
                               End, "in MTGO",  EventType, "with a CI <", 
                           CI_length*100, "%",sep = " ")
graph_subtitle_small_CI="Red line for the average winrate and green lines for the 
average CI of those archetypes combined"

ggplot(df_small_CI, aes(x=ARCHETYPES, y=WINRATE_AVERAGE)) + theme_classic() +
  geom_point(size=1) + scale_x_discrete(guide = guide_axis(n.dodge=5))+
  labs(y=y_label_small_CI, title=graph_title_small_CI, 
       subtitle=graph_subtitle_small_CI) + 
  geom_errorbar(aes(ymax = WINRATE_95_MAX , ymin = WINRATE_95_MIN)) + 
  geom_hline(yintercept = mean(df_small_CI$WINRATE_AVERAGE), color="green", 
             linetype="dashed", size=1.5)+
  geom_hline(yintercept = mean(df_small_CI$WINRATE_95_MIN), color="red", 
             linetype="dashed", size=1.5)+
  geom_hline(yintercept = mean(df_small_CI$WINRATE_95_MAX ), color="red", 
             linetype="dashed", size=1.5)+ 
  geom_text(aes(y = stat(df_small_CI$WINRATE_95_MAX ), label = WINRATE_95_MAX , 
                x = ARCHETYPES), vjust = -1)+ 
  geom_text(aes(y = stat(df_small_CI$WINRATE_95_MIN), label = WINRATE_95_MIN, 
                x = ARCHETYPES), vjust = 1)+ 
  geom_text(aes(y = stat(df_small_CI$WINRATE_AVERAGE), label = WINRATE_AVERAGE, 
                x = ARCHETYPES), vjust = -1)

################################################################################
#ANALYSIS OF THE PRESENCE AND NUMBERS OF EACH CARD
#RETURNS URI OF DECKS RUNNING A SPECIFIC CARD IN THE MD
# for (i in 1:length(ModernData$Raw$Mainboard)){
#   if(sum(ModernData$Raw$Mainboard[[i]]$CardName=="Aether Gust")>=1){
#     print(ModernData$Raw$AnchorUri[[i]])
#   }
# }

#NUMBER OF DIFFERENT CARDS OVERALL
length(ModernCardsStats$CardNames)
#NUMBER OF DIFFERENT CARDS IN SB
length(ModernSBStats$CardNames)
#NUMBER OF DIFFERENT CARDS IN MD
length(ModernMDStats$CardNames)
#NUMBER OF CARDS APPEARING BOTH IN MD AND SB
length(intersect(ModernMDStats$CardNames,ModernSBStats$CardNames))
#LIST OF CARDS APPEARING BOTH IN MD AND SB
#intersect(ModernMDStats$CardNames,ModernSBStats$CardNames)

#DATA FOR A SPECIFIC CARD OVERALL - FOR INSTANCE, "Aether Gust"
ModernCardsStats[ModernCardsStats$CardNames=="Uro, Titan of Nature's Wrath",]
#DATA FOR A SPECIFIC CARD IN MD - FOR INSTANCE, "Aether Gust"
ModernMDStats[ModernMDStats$CardNames=="Aether Gust",]
#DATA FOR A SPECIFIC CARD IN SB - FOR INSTANCE, "Aether Gust"
ModernSBStats[ModernSBStats$CardNames=="Aether Gust",]

#SORT THE CARDS ALPHABETICALLY
ModernCardsStats=ModernCardsStats[order(ModernCardsStats$CardNames,decreasing=FALSE),]
head(ModernCardsStats)
#SORT THE CARDS DEPENDING ON THEIR NUMBER OF COPIES
ModernCardsStats=ModernCardsStats[order(ModernCardsStats$CardCount,decreasing=TRUE),]
head(ModernCardsStats)
head(ModernCardsStats$CardNames)
#SORT THE CARDS DEPENDING ON THEIR NUMBER OF DECKS PLAYING THEM
ModernCardsStats=ModernCardsStats[order(ModernCardsStats$DeckCount,decreasing=TRUE),]
head(ModernCardsStats)
head(ModernCardsStats$CardNames)

#GET THE RANK OF A CARD IN THE DATA AFTER A CHOSEN SORTING ABOVE  
CardNameToTest="Wilderness Reclamation"
which(ModernCardsStats$CardNames==CardNameToTest)
ModernCardsStats[which(ModernCardsStats$CardNames==CardNameToTest),]

df[grep("Wilderness Reclamation",df$DLCards),]$URL
names(df)

#GET THE DECKLIST OF A DECK BASED ON ITS URI/L
RawResults=ModernData$Raw
URITest="https://magic.wizards.com/en/articles/archive/mtgo-standings/modern-preliminary-2020-11-25#fwdr_-" 
RawResults[RawResults$AnchorUri==URITest,]$Mainboard
names(RawResults)
RawResults$AnchorUri

#sub("modern-[^league].*?-(\\d{4}-\\d{1,2}-\\d{1,2})(-\\d)?\\.json", "\\1", "modern-preliminary-2020-12-03.json")
################################################################################
#STATS OF CARDS OVERALL
ModernCardsStats=unique(unlist(df$DLCards))

#LIST OF DIFFERENT CARDS
CardResults=setNames(data.frame(matrix(ncol = 3, nrow = length(ModernCardsStats))), 
                     c("CardNames", "CardWins", "CardDefs"))

for (i in 1:length(ModernCardsStats)){
  CardResults$CardNames[i]=ModernCardsStats[i]
  
  card_id=grep(CardResults$CardNames[i],df$DLCards)
  
  CardResults$NbDecks[i]=length(grep(CardResults$CardNames[i],df$DLCards))
  
  CardResults$CardWins[i]=sum((df$POINTS[card_id] + 
                                 df$TOP8_PTS[card_id])/3)
  CardResults$CardMatches[i]=sum(df$NB_ROUNDS[card_id] + 
                                df$TOP8_MATCHES[card_id])
  
  #CI WITH CLOPPER-PEARSON
  CardResults$WINRATE_AVERAGE[i]=binom.test(CardResults$CardWins[i], CardResults$CardMatches[i], p=0.5,
                                      alternative="two.sided", conf.level=0.95)$estimate
  
  CardResults$WINRATE_95_MIN[i]=binom.test(CardResults$CardWins[i], CardResults$CardMatches[i], p=0.5,
                                     alternative="two.sided", conf.level=0.95)$conf.int[1]
  
  CardResults$WINRATE_95_MAX [i]=binom.test(CardResults$CardWins[i], CardResults$CardMatches[i], p=0.5,
                                     alternative="two.sided", conf.level=0.95)$conf.int[2]
}

CardResults[1,]
names(CardResults)
CardResults$NbDecks
#PRINT WINRATES OF THE MOST PLAYED CARDS (WITH THE MOST MATCHES)
CardResults = CardResults[order(-CardResults$NbDecks),]
print(subset(CardResults[1:10,],select = c(CardNames,CardMatches,WINRATE_AVERAGE )), 
      row.names = FALSE)
#PRINT WINRATES OF THE TOP CARDS WITH THE BEST WINRATES
CardResults = CardResults[order(-CardResults$WINRATE_AVERAGE),]
print(subset(CardResults[1:50,],select = c(CardNames,CardMatches,WINRATE_AVERAGE )), 
      row.names = FALSE)

#SAME BUT ONLY FOR CARDS WITH A SMALL CI ON THE WINRATE
df_small_CI = CardResults[CardResults$WINRATE_95_MAX -CardResults$WINRATE_95_MIN<0.1,]
length(df_small_CI$CardNames)
df_small_CI = df_small_CI[order(df_small_CI$WINRATE_AVERAGE),]
df_small_CI = df_small_CI[order(-df_small_CI$WINRATE_AVERAGE),]
print(subset(df_small_CI[1:50,],select = c(CardNames,CardMatches,WINRATE_AVERAGE )), 
      row.names = FALSE)

#INSTEAD WE TAKE THE LOWER BORN OF THE WINRATE CONFIDENCE INTERVAL NOW
CardResults = CardResults[order(-CardResults$WINRATE_95_MIN),]
print(subset(CardResults[1:50,],select = c(CardNames,CardMatches,WINRATE_95_MIN )), 
      row.names = FALSE)

max(CardResults$WINRATE_95_MIN)
min(CardResults$WINRATE_95_MIN)

max(CardResults$WINRATE_AVERAGE)
min(CardResults$WINRATE_AVERAGE)



#DISPLAY THE CI OF WINRATES FOR EACH CARD WITH A SMALL CI
CI_length=0.05
df_small_CI = CardResults[CardResults$WINRATE_95_MAX -CardResults$WINRATE_95_MIN<CI_length,]
length(df_small_CI$CardNames)
df_small_CI$WINRATE_95_MAX =as.numeric(specify_decimal(df_small_CI$WINRATE_95_MAX ,3))
df_small_CI$WINRATE_AVERAGE=as.numeric(specify_decimal(df_small_CI$WINRATE_AVERAGE,3))
df_small_CI$WINRATE_95_MIN=as.numeric(specify_decimal(df_small_CI$WINRATE_95_MIN,3))

df_small_CI$CardNames = reorder(df_small_CI$CardNames, 
                                 as.numeric(df_small_CI$WINRATE_AVERAGE))

y_label_small_CI="Winrates and confidence intervals"
graph_title_small_CI=paste("Winrate of each card between", Beginning, "and", 
                           End, "in MTGO",  EventType, "with a CI <", 
                           CI_length*100, "%",sep = " ")
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


#CMC depending on land ratio
ggplot(df[df$NB_LAND!=0,], aes(MD_LAND_RATIO*60, CMC))+ 
  geom_point() + 
  coord_cartesian() + theme_bw()+ 
  labs(title="Average CMC (without lands) of each deck depending on land ratio
       (Number of lands / Number of cards in the MD)")
