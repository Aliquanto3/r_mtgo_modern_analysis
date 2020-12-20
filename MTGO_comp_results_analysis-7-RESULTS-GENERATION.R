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
metagame_pie_chart(df,"Matches")
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
summary(regIndicateurs)
#plot(regIndicateurs)

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

#régression linéaire sur les ratios de types de cartes
regBestArch1=lm(WINRATE~RATIO_SORCERY+RATIO_INSTANT+RATIO_PLANESWALKER+RATIO_CREATURE+
                 RATIO_LAND+RATIO_ARTIFACT+RATIO_ENCHANTMENT+TOTAL_MD+CMC,data=df_best_arch)
summary(regBestArch)

#ACP sur les ratios de types de cartes
activeBestArch1=select(df_best_arch, RATIO_SORCERY,RATIO_INSTANT,RATIO_PLANESWALKER,
                      RATIO_CREATURE,RATIO_LAND,RATIO_ARTIFACT,RATIO_ENCHANTMENT,
                      TOTAL_MD,CMC,WINRATE)
res.pca1 = PCA(activeBestArch1, graph = FALSE)
fviz_eig (res.pca1)
fviz_pca_var(res.pca1, col.var = "contrib", gradient.cols = c("blue", "green", "red"))

var1 = get_pca_var(res.pca1)
var1$coord


#régression linéaire sur la présence ou non de certaines cartes en sideboard
regBestArch2=lm(WINRATE~+PRESENCE_TIMELY_REINFORCEMENTS+PRESENCE_GAEA_S_BLESSING+
                PRESENCE_CONTAINMENT_PRIEST+PRESENCE_NATURE_S_CLAIM+
                PRESENCE_MADCAP_EXPERIMENT+PRESENCE_CONDEMN+PRESENCE_NEXUS_OF_FATE+
                PRESENCE_TALE_S_END+PRESENCE_DEICIDE+PRESENCE_STONY_SILENCE+
                PRESENCE_DISPEL+PRESENCE_KOZILEK_BUTCHER_OF_TRUTH+
                PRESENCE_DOVIN_S_VETO,data=df_best_arch)
summary(regBestArch2)


#ACP sur la présence ou non de certaines cartes en sideboard
activeBestArch2=select(df_best_arch, WINRATE, PRESENCE_TIMELY_REINFORCEMENTS,
                      PRESENCE_GAEA_S_BLESSING,PRESENCE_CONTAINMENT_PRIEST,
                      PRESENCE_NATURE_S_CLAIM,PRESENCE_MADCAP_EXPERIMENT,
                      PRESENCE_CONDEMN,PRESENCE_NEXUS_OF_FATE,
                      PRESENCE_TALE_S_END,PRESENCE_DEICIDE,
                      PRESENCE_STONY_SILENCE,PRESENCE_DISPEL,
                      PRESENCE_KOZILEK_BUTCHER_OF_TRUTH,PRESENCE_DOVIN_S_VETO)
res.pca2 = PCA(activeBestArch2, graph = FALSE)
fviz_eig (res.pca2)
fviz_pca_var(res.pca2, col.var = "contrib", gradient.cols = c("blue", "green", "red"))

var2 = get_pca_var(res.pca2)
var2$coord


