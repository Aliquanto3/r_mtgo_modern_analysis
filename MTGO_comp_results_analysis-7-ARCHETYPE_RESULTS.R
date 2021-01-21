###############################################################################
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

#TOTAL NUMBER OF EVENTS
length(unique(df$EVENT_NAME))

#FIND THE URL FOR AN ARCHETYPE
df[grep("Grinding Breach", df$ARCHETYPE), ]$URL

#RATIO MATCHES/PLAYERS - NOT IN THE REPORT
HistShare=1.2
metagame_box_plot(df,"Players",Beginning,End,EventType)

# IV.1.A - Indicateur 1 : présence de chaque archétype
#GENERATE THE METAGAME PIE CHART FOR THE SELECTED DATA
PieShare=2.5
metagame_pie_chart(df,"Players",Beginning,End,EventType)
metagame_pie_chart(df,"Copies",Beginning,End,EventType)
metagame_pie_chart(df,"Matches",Beginning,End,EventType)
PieShare=2
metagame_pie_chart(df,"Matches",Beginning,End,EventType)

#GENERATE THE METAGAME HISTOGRAM FOR THE SELECTED DATA
HistShare=2
metagame_box_plot(df,"Matches",Beginning,End,EventType)

####################################################################
#LET US EXPORT ALL THE ARCHETYPES RANKED BY PRESENCE
arch_list=generate_archetype_list(df,Beginning,End)
arch_list$PRESENCE=rep(0,length(arch_list$ARCHETYPES))
#BY DEFAULT, PRESENCE IS THE NUMBER OF MATCHES PLAYED
#HOWEVER, IN THE CASE OF A SINGLE EVENT, WITHOUT URL, WE CAN TAKE THE NUMBER OF 
#COPIES INSTEAD
for (i in 1:length(arch_list$PRESENCE)){
  arch_id=which(df[[archetype_acc]]==arch_list$ARCHETYPES[i])
  #NUMBER OF ROUNDS PLAYED
  arch_list$PRESENCE[i]=sum(df[arch_id,]$NB_ROUNDS,df[arch_id,]$TOP8_MATCHES)
  if (all(is.na(df$URL))){
    arch_list$PRESENCE[i]=arch_list$PRESENCE[i]/df$NB_ROUNDS[i]
  }
}
arch_list=arrange(arch_list,desc(PRESENCE))

print(subset(arch_list,select = c(ARCHETYPES,PRESENCE)), row.names = TRUE)
print(subset(arch_list,select = c(ARCHETYPES)), row.names = FALSE)
write.csv(arch_list,paste('Results_as_CSV/',Beginning,'-',
                          End,'_DF_Archetypes_Presence.csv',sep=''), row.names = TRUE)
###################################################################

# IV.1.B – Indicateur 2 : nombre de points par ronde (taux de victoire)
metric_df=metric_points_archetypes(df,Beginning,End)
arch_ranked=archetypes_ranking(metric_df,Beginning,End)
winrates_graph(df,arch_ranked,"Matches",Beginning,End,EventType)

####################################################################
#LET US EXPORT ALL THE ARCHETYPES RANKED BY WINRATE
names(arch_ranked)
arch_ranked=arrange(arch_ranked,desc(WINRATE_AVERAGE))
print(subset(arch_ranked,select = c(ARCHETYPES)), row.names = TRUE)
arch_ranked$WINRATE_AVERAGE = as.numeric(format(round(
  arch_ranked$WINRATE_AVERAGE*100,1), nsmall = 1))
print(subset(arch_ranked,select = c(WINRATE_AVERAGE)), row.names = FALSE)
write.csv(subset(arch_ranked,select = c(ARCHETYPES,WINRATE_AVERAGE)),
          paste('Results_as_CSV/',Beginning,'-',End,'_DF_Archetypes_Winrates.csv',sep=''), 
          row.names = TRUE)
###################################################################

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

#WHAT IF WE USE THE LOG OF THE PRESENCE INSTEAD?
metric_df=metric_points_archetypes(df,Beginning,End)
metric_df_log_matches=metric_df
metric_df_log_matches$TOTAL_NB_MATCHES=log(metric_df_log_matches$TOTAL_NB_MATCHES)


# IV.2.A - Compilation 1 : analyse graphique
metric_graph(metric_df,"Matches","Players",0.1,Beginning,End,"",TRUE,FALSE,EventType)

# IV.2.B - Compilation 2 : combinaison linéaire des indicateurs
PPR_Weight=1
arch_ranked=archetypes_ranking(metric_df,Beginning,End)
linear_comb_graph(df,arch_ranked,Beginning,End,EventType)

PPR_Weight=2
arch_ranked=archetypes_ranking(metric_df,Beginning,End)
linear_comb_graph(df,arch_ranked,Beginning,End,EventType)

PPR_Weight=4
arch_ranked=archetypes_ranking(metric_df,Beginning,End)
linear_comb_graph(df,arch_ranked,Beginning,End,EventType)

PPR_Weight=1
arch_ranked=archetypes_ranking(metric_df_log_matches,Beginning,End)
log_comb_graph(df,arch_ranked,Beginning,End,EventType)

####################################################################
#LET US EXPORT ALL THE ARCHETYPES RANKED BY NORMALIZED LOG(PRESENCE) + WINRATE
arch_ranked=archetypes_ranking(metric_df_log_matches,Beginning,End)
names(arch_ranked)
arch_ranked=arrange(arch_ranked,desc(METRIC_COMB))
print(subset(arch_ranked,select = c(ARCHETYPES,METRIC_COMB)), row.names = TRUE)
arch_ranked$METRIC_COMB = as.numeric(format(round(
  arch_ranked$METRIC_COMB,3), nsmall = 3))
print(subset(arch_ranked,select = c(METRIC_COMB)), row.names = FALSE)
write.csv(subset(arch_ranked,select = c(ARCHETYPES,METRIC_COMB)),
          paste('Results_as_CSV/',Beginning,'-',End,'_DF_Archetypes_Metric_Comb.csv',sep=''), 
          row.names = TRUE)
###################################################################

# IV.2.C - Compilation 3 : la borne inférieure de l’intervalle de confiance sur 
#les winrates
arch_ranked=archetypes_ranking(metric_df,Beginning,End)
lower_bound_ci_winrate_graph(df,arch_ranked,Beginning,End,EventType)

####################################################################
#LET US EXPORT ALL THE ARCHETYPES RANKED BY LOWER WINRATE ESTIMATION
arch_ranked=archetypes_ranking(metric_df,Beginning,End)
names(arch_ranked)
arch_ranked=arrange(arch_ranked,desc(WINRATE_95_MIN))
print(subset(arch_ranked,select = c(ARCHETYPES,WINRATE_95_MIN)), row.names = TRUE)
arch_ranked$WINRATE_95_MIN = as.numeric(format(round(
  arch_ranked$WINRATE_95_MIN*100,1), nsmall = 1))
print(subset(arch_ranked,select = c(WINRATE_95_MIN)), row.names = FALSE)
write.csv(subset(arch_ranked,select = c(ARCHETYPES,WINRATE_95_MIN)),
          paste('Results_as_CSV/',Beginning,'-',End,'_DF_Archetypes_Lower_Winrate.csv',sep=''), 
          row.names = TRUE)
###################################################################

#IV.3 – Choix d’un deck et optimisation de la liste

#régression linéaire sur les ratios de types de cartes
regBestArch1=lm(WINRATE~RATIO_SORCERY+RATIO_INSTANT+RATIO_PLANESWALKER+RATIO_CREATURE+
                 RATIO_LAND+RATIO_ARTIFACT+RATIO_ENCHANTMENT+TOTAL_MD+CMC,data=df_best_arch)
summary(regBestArch1)
#plot(regBestArch1)

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

################################################################################
#ARCHETYPE AVERAGE STRUCTURE
archName="WURG Control"
archNameFile=gsub('[[:punct:] ]+',' ',archName)
archNameFile=chartr(" ", "_", archName)
#A BIT LONG TO EXECUTE
df_best_arch=infos_best_arch(df,bestArchetype)

avgCardRatio=data.frame(as.numeric(format(round(sum(
  df_best_arch$RATIO_LAND)/length(df_best_arch$RATIO_LAND)*60,1), nsmall = 1)),
  as.numeric(format(round(sum(
    df_best_arch$RATIO_CREATURE)/length(df_best_arch$RATIO_CREATURE)*60,1), 
    nsmall = 1)),
  as.numeric(format(round(sum(
    df_best_arch$RATIO_PLANESWALKER)/length(df_best_arch$RATIO_PLANESWALKER)*60,1), 
    nsmall = 1)),
  as.numeric(format(round(sum(
    df_best_arch$RATIO_INSTANT)/length(df_best_arch$RATIO_INSTANT)*60,1), 
    nsmall = 1)),
  as.numeric(format(round(sum(
    df_best_arch$RATIO_SORCERY)/length(df_best_arch$RATIO_SORCERY)*60,1), 
    nsmall = 1)),
  as.numeric(format(round(sum(
    df_best_arch$RATIO_ARTIFACT)/length(df_best_arch$RATIO_ARTIFACT)*60,1), 
    nsmall = 1)),
  as.numeric(format(round(sum(
    df_best_arch$RATIO_ENCHANTMENT)/length(df_best_arch$RATIO_ENCHANTMENT)*60,1), 
    nsmall = 1))
)

names(avgCardRatio)=c("AvgLandRatio","AvgCreatureRatio","AvgPWRatio",
                          "AvgInstantRatio","AvgSorceryRatio","AvgArtifactRatio",
                          "AvgEnchantmentRatio")
avgCardRatioRound=round(avgCardRatio)
avgCardRatio$SumCards=sum(avgCardRatio)
avgCardRatioRound$SumCards=sum(avgCardRatioRound)
avgCardRatio
avgCardRatioRound
write.csv(avgCardRatioRound, paste('Results_as_CSV/',Beginning,'-',End,'-',
                                   archNameFile,'_DF_Average_Ratios.csv',sep=''), 
          row.names = FALSE)

#AVERAGE NUMBER OF EACH CARD IN THE ARCHETYPE
df_avg_cards=archAverageData(df,archName)
df_avg_cards
write.csv(df_avg_cards, paste('Results_as_CSV/',Beginning,'-',End,'-',archNameFile,
                              '_DF_Average_Cards.csv',sep=''), 
          row.names = FALSE)
#AVERAGE MD ROUNDING DOWN THE RATIOS
# avg_decklist_down=Average_decklist_round_down(df_avg_cards,avgCardRatio)
# print(avg_decklist_down, row.names = FALSE)
# sum(avg_decklist_down$CardCount)

avg_decklist_up=Average_decklist_round_up(df_avg_cards,avgCardRatio)
avg_decklist_up=rbind(avg_decklist_up,setNames(data.frame(
  "Total",sum(avg_decklist_up$CardCount)),c("CardName","CardCount")))
print(avg_decklist_up, row.names = FALSE)
write.csv(avg_decklist_up, paste('Results_as_CSV/',Beginning,'-',End,'-',
                                 archNameFile,'_DF_Average_Maindeck.csv',sep=''), 
          row.names = FALSE)

#AVERAGE MD ROUNDING UP THE RATIOS
avg_decklist_up=Average_decklist_round_up(df_avg_cards,avgCardRatio)
avg_decklist_up=rbind(avg_decklist_up,setNames(data.frame(
  "Total",sum(avg_decklist_up$CardCount)),c("CardName","CardCount")))
print(avg_decklist_up, row.names = FALSE)
write.csv(avg_decklist_up, paste('Results_as_CSV/',Beginning,'-',End,'-',
                                 archNameFile,'_DF_Average_Maindeck.csv',sep=''), 
          row.names = FALSE)

#AVERAGE SB ROUNDING UP THE RATIOS
avg_SB=Average_SB(df_avg_cards)
avg_SB=rbind(avg_SB,setNames(data.frame(
  "Total",sum(avg_SB$CardCount)),c("CardName","CardCount")))
print(avg_SB, row.names = FALSE)
write.csv(avg_SB, paste('Results_as_CSV/',Beginning,'-',End,'-',archNameFile,
                        '_DF_Average_Sideboard.csv',sep=''), 
          row.names = FALSE)

