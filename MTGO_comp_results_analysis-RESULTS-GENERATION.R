################################################################################
#USE THE METHODS BELOW TO GENERATE THE GRAPHS AND RESULTS YOU LOOK FOR

length(df$ARCHETYPE)

length(unique(df$ARCHETYPE))
unique(df$ARCHETYPE)

length(unique(df$SUPER_ARCH))
unique(df$SUPER_ARCH)

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
df[grep("Selenya Taxes", df$ARCHETYPE),]$URL

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


