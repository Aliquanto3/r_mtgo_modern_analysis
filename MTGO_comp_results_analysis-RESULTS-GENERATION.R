################################################################################
#USE THE METHODS BELOW TO GENERATE THE GRAPHS AND RESULTS YOU LOOK FOR

length(df$ARCHETYPE)

length(unique(df$ARCHETYPE))
unique(df$ARCHETYPE)

length(unique(df$SUPER_ARCH))
unique(df$SUPER_ARCH)

sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES)

#GENERATE THE METAGAME PIE CHART FOR THE SELECTED DATA
metagame_pie_chart(df)

#GENERATE THE METAGAME HISTOGRAM FOR THE SELECTED DATA
metagame_box_plot(df)

################################################################################
#COUNT THE NUMBER OF POINTS PER ROUND
ppr_df=points_per_round(df)
ppr_df
ppr_plot=metric_graph(ppr_df, "Points per round in Swiss & Top8")
ppr_plot
ppr_ranked=archetypes_ranking(ppr_df)
################################################################################

print(subset(ppr_ranked,select = c(RANK,ARCHETYPES)), row.names = FALSE)
print(subset(ppr_ranked,select = c(ARCHETYPES)), row.names = FALSE)
#View(subset(ppr_ranked,select = c(ARCHETYPES,RANK,COMB_PPR)))
ppr_ranked$ARCHETYPES

#FIND THE URL OF AN ARCHETYPE - ex "Azorius Control"
df[grep("WBR Shadow", df$ARCHETYPE),]$URL
#FIND THE NUMBER OF DIFFERENT PILOTES OF AN ARCHETYPE - ex "Azorius Control"
length(unique(df[grep("Hammer Time", df$ARCHETYPE),]$PLAYER))
#NUMBER OF COPIES OF AN ARCHETYPE IN THE DATA
length(df[grep("Hammer Time", df$ARCHETYPE),]$URL)

#FIND THE RANK OF A DECK - EX: "Sultai Control"
ppr_ranked[grep("Tron", ppr_ranked$ARCHETYPES), ]

#MEAN
ppr_m=mean(ppr_ranked$COMB_PPR)

#STANDARD DEVIATION
ppr_sd=sd(ppr_ranked$COMB_PPR)

#DISPLAYS EACH ARCHETYPE ABOVE THE AVERAGE + X * STANDARD DEVIATION
ppr_ranked[ppr_ranked$COMB_PPR>ppr_m+0.5*ppr_sd,]$ARCHETYPES
ppr_ranked[ppr_ranked$COMB_PPR>ppr_m+1*ppr_sd,]$ARCHETYPES
ppr_ranked[ppr_ranked$COMB_PPR>ppr_m+1.5*ppr_sd,]$ARCHETYPES
ppr_ranked[ppr_ranked$COMB_PPR>ppr_m+2*ppr_sd,]$ARCHETYPES
ppr_ranked[ppr_ranked$COMB_PPR>ppr_m+2.5*ppr_sd,]$ARCHETYPES
ppr_ranked[ppr_ranked$COMB_PPR>ppr_m+3*ppr_sd,]$ARCHETYPES
ppr_ranked[ppr_ranked$COMB_PPR>ppr_m+3.5*ppr_sd,]$ARCHETYPES
ppr_ranked[ppr_ranked$COMB_PPR>ppr_m+4*ppr_sd,]$ARCHETYPES

#PLOT THE AVERAGE PPR DEPENDING ON THE TOTAL PPR FOR EACH ARCHETYPE, RANKED BY 
#COMBINED PPR
x_label=paste("Archetype rank")
y_label="Combined number of points per round of each archetype (mix of number of 
copies and average number)"
graph_title=paste("Rank of each archetype ", "between", Beginning, "and", End, 
                  "in MTGO", EventType, "based on points per round",sep = " ")
graph_subtitle="Separated by mean + n standard deviations (n={0,1,2,3,4})"

ggplot(ppr_ranked, aes(x=RANK, y=COMB_PPR)) + theme_classic() + geom_point() + 
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

##################################################################################
#GET ONLY THE DECKS APPEARING THE MOST IN THE DATA
nb_copies_min=10
ppr_most_played=ppr_ranked[ppr_ranked$NB_COPIES>=nb_copies_min,]
ppr_most_played$WINRATE=ppr_most_played$PPR_AVERAGE/3
ppr_most_played = ppr_most_played[order(-ppr_most_played$PPR_AVERAGE),]
ppr_most_played
print(subset(ppr_most_played,select = c(NB_COPIES,ARCHETYPES,WINRATE)), 
      row.names = FALSE)
length(ppr_most_played$ARCHETYPES)
ppr_most_played$WIN_AVERAGE_RANK=ppr_most_played$WINRATE
for (i in 1:length(ppr_most_played$WIN_AVERAGE_RANK)){
  ppr_most_played$WIN_AVERAGE_RANK[i]=i
}
#################################################################################


#PLOT THE AVERAGE PPR DEPENDING ON THE TOTAL PPR FOR EACH ARCHETYPE, RANKED BY 
#COMBINED PPR
x_label_winrate="Archetype rank based on winrate"
y_label_winrate="Winrate of the most popular decks"
graph_title_winrate=paste("Rank of each popular archetype (at least",nb_copies_min,
                          "copies) between", Beginning, "and", End, "in MTGO", 
                          EventType, "based on winrate",sep = " ")

ggplot(ppr_most_played, aes(x=WIN_AVERAGE_RANK, y=PPR_AVERAGE/3)) + theme_classic() +
  geom_point(size=4) +  
  geom_text_repel(aes(label=ARCHETYPES),hjust=0, vjust=0,point.padding = NA)+ 
  labs(x=x_label_winrate, y=y_label_winrate, title=graph_title_winrate)+
  geom_errorbar(aes(ymax = PPR_95_MAX/3, ymin = PPR_95_MIN/3)) 
# + geom_hline(yintercept = 1.5, color="red", linetype="dashed", size=1.5)

#PLOT THE AVERAGE PPR DEPENDING ON THE TOTAL PPR FOR EACH ARCHETYPE, RANKED BY 
#COMBINED PPR
y_label_full_winrate="Winrates and confidence intervals"
graph_title_full_winrate=paste("Winrate of each archetype between", Beginning, "and", 
                               End, "in MTGO",  EventType,sep = " ")
graph_subtitle_full_winrate="Red line for the average winrate of those archetypes combined"

ggplot(ppr_ranked, aes(x=ARCHETYPES, y=PPR_AVERAGE)) + theme_classic() +
  geom_point(size=4) + scale_x_discrete(guide = guide_axis(n.dodge=5))+
  labs(y=y_label_full_winrate, title=graph_title_full_winrate, subtitle=graph_subtitle_full_winrate) + 
  geom_errorbar(aes(ymax = PPR_95_MAX, ymin = PPR_95_MIN)) + ylim(0,1)+
  geom_hline(yintercept = mean(ppr_ranked$PPR_AVERAGE), color="red", linetype="dashed", size=1.5)
 
#SAME FOR ONLY CI UNDER A DETERMINED LENGTH
specify_decimal = function(x, k) trimws(format(round(x, k), nsmall=k))

CI_length=0.3
df_small_CI = ppr_ranked[ppr_ranked$PPR_95_MAX-ppr_ranked$PPR_95_MIN<CI_length,]
df_small_CI$PPR_95_MAX=as.numeric(specify_decimal(df_small_CI$PPR_95_MAX,3))
df_small_CI$PPR_AVERAGE=as.numeric(specify_decimal(df_small_CI$PPR_AVERAGE,3))
df_small_CI$PPR_95_MIN=as.numeric(specify_decimal(df_small_CI$PPR_95_MIN,3))

df_small_CI$ARCHETYPES = reorder(df_small_CI$ARCHETYPES, as.numeric(df_small_CI$PPR_AVERAGE))

y_label_small_CI="Winrates and confidence intervals"
graph_title_small_CI=paste("Winrate of each archetype between", Beginning, "and", 
                               End, "in MTGO",  EventType, "with a CI <", CI_length*100, "%",sep = " ")
graph_subtitle_small_CI="Red line for the average winrate of those archetypes combined"

ggplot(df_small_CI, aes(x=ARCHETYPES, y=PPR_AVERAGE)) + theme_classic() +
  geom_point(size=1) + scale_x_discrete(guide = guide_axis(n.dodge=5))+
  labs(y=y_label_small_CI, title=graph_title_small_CI, subtitle=graph_subtitle_small_CI) + 
  geom_errorbar(aes(ymax = PPR_95_MAX, ymin = PPR_95_MIN)) + 
  geom_hline(yintercept = mean(df_small_CI$PPR_AVERAGE), color="red", linetype="dashed", size=1.5)+ 
  geom_text(aes(y = stat(df_small_CI$PPR_95_MAX), label = PPR_95_MAX, x = ARCHETYPES), vjust = -1)+ 
  geom_text(aes(y = stat(df_small_CI$PPR_95_MIN), label = PPR_95_MIN, x = ARCHETYPES), vjust = 1)+ 
  geom_text(aes(y = stat(df_small_CI$PPR_AVERAGE), label = PPR_AVERAGE, x = ARCHETYPES), vjust = -1)
