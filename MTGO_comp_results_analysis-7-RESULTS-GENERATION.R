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

#IV.3 – Choix d’un deck et optimisation de la liste

