#RUN FILES 1,2 AND 3 BEFORE THIS FILE

#CREATE THE DIRECTORY WHERE TO SAVE THE PICTURES
setwd(DirectoryFile)
eventList=c("Challenges","Competitions","Preliminaries")
dir.create(file.path(paste(DirectoryFile,"Results_as_pictures",sep="/"), 
                     paste(Beginning,End,sep="_")))
dir.create(file.path(paste(DirectoryFile,"Results_as_csv",sep="/"), 
                     paste(Beginning,End,sep="_")))

#LOOKS LIKE A "FOR" WON'T WORK TO EXPORT THE GRAPHS

#####################
EventType=eventList[1]

df=generate_df(rawData,EventType)
df=df[grep("Pauper",df$EVENT),]
df=add_super_archetypes(df)

#GRAPHIC ANALYSIS
metric_df=metric_points_archetypes(df,Beginning,End)
metric_df_log_matches=metric_df
metric_df_log_matches$TOTAL_NB_MATCHES=
  log(metric_df_log_matches$TOTAL_NB_MATCHES)

#WORKING DIRECTORY FOR THE CSV
setwd(file.path(paste(DirectoryFile,"/Results_as_csv/",sep="/"), 
                paste(Beginning,End,sep="_")))
dir.create(file.path(paste(DirectoryFile,"/Results_as_csv/",Beginning,
                           "_",End,sep=""), EventType))
setwd(file.path(paste(DirectoryFile,"/Results_as_csv/",Beginning,"_",
                      End,sep=""), EventType))
# write.csv(metric_df,paste(Beginning,'-',End,'_DF_Archetypes_Results.csv',sep=''), 
#           row.names = TRUE)

metric_df_play=metric_points_players(df,Beginning,End)
metric_df_play_sub=metric_df_play[c("PLAYERS","NO_APPEARANCES","TOTAL_NB_MATCHES",
                                     "WINRATE_AVERAGE","WINRATE_95_MIN","WINRATE_95_MAX",
                                    "TOTAL_POINTS")]
metric_df_play_sub[,"ARCHETYPE_NAMES"]=NA
metric_df_play_sub[,"ARCHETYPE_COUNT"]=NA
metric_df_play_sub[,"URL"]=NA
for (i in 1:length(metric_df_play_sub$URL)){
  metric_df_play_sub$ARCHETYPE_NAMES[i]=paste(unlist(metric_df_play$ARCHETYPE_NAMES[i]),collapse = "; ")
  metric_df_play_sub$ARCHETYPE_COUNT[i]=paste(unlist(metric_df_play$ARCHETYPE_COUNT[i]),collapse = "; ")
  metric_df_play_sub$URL[i]=paste(unlist(metric_df_play$URL[i]),collapse = "; ")
}
write.csv(metric_df_play_sub,paste(Beginning,'-',End,'_DF_Players_Overall_Results.csv',sep=''), 
          row.names = TRUE)

top8_df_play=players_top8(df,Beginning,End)
top8_df_play_cop=top8_df_play
top8_df_play_cop[,"ARCHETYPE_NAMES"]=NA
top8_df_play_cop[,"ARCHETYPE_COUNT"]=NA
top8_df_play_cop[,"URL"]=NA
for (i in 1:length(top8_df_play_cop$URL)){
  top8_df_play_cop$ARCHETYPE_NAMES[i]=paste(unlist(top8_df_play$ARCHETYPE_NAMES[i]),collapse = "; ")
  top8_df_play_cop$ARCHETYPE_COUNT[i]=paste(unlist(top8_df_play$ARCHETYPE_COUNT[i]),collapse = "; ")
  top8_df_play_cop$URL[i]=paste(unlist(top8_df_play$URL[i]),collapse = "; ")
}
write.csv(top8_df_play_cop,paste(Beginning,'-',End,'_DF_Top8_Players_Pauper_Results.csv',sep=''), 
          row.names = TRUE)

#WORKING DIRECTORY FOR THE GRAPHS
setwd(file.path(paste(DirectoryFile,"/Results_as_pictures/",sep="/"), 
                paste(Beginning,End,sep="_")))
dir.create(file.path(paste(DirectoryFile,"/Results_as_pictures/",Beginning,
                           "_",End,sep=""), EventType))
setwd(file.path(paste(DirectoryFile,"/Results_as_pictures/",Beginning,"_",
                      End,sep=""), EventType))

#PRESENCE
PieShare=2.4
# 1. Open jpeg file
jpeg(paste(generate_metagame_graph_title("Matches",Beginning,End,EventType),".jpg"), 
     width = 3500, height = 3500,res=300)
metagame_pie_chart(df,"Matches",Beginning,End,EventType)
dev.off()
jpeg(paste(generate_metagame_graph_title("Players",Beginning,End,EventType),".jpg"), 
     width = 3500, height = 3500,res=300)
metagame_pie_chart(df,"Players",Beginning,End,EventType)
dev.off()

#WINRATES
arch_ranked=archetypes_ranking(metric_df,Beginning,End)
jpeg(paste("0.95 CI on winrate between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
winrates_graph(df,arch_ranked,"Matches",Beginning,End,EventType)
dev.off()

jpeg(paste("Low winrate estimation in 0.95 CI between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
lower_bound_ci_winrate_graph(df,arch_ranked,Beginning,End,EventType)
dev.off()

jpeg(paste("Ratio of archetype winrates out of pilot winrates between", Beginning, 
           "and", End, "in MTGO", EventType,".jpg"), width = 8000, height = 3000, 
     res=400)
winrate_ratio_arch_out_player_graph(df,metric_df,Beginning,End,EventType)
dev.off()

#presence AND diameters CAN BE EITHER "Copies", "Players" or "Matches"
#tiers CAN BE EITHER "Win+Pres","Pres M+SD" or "Pres %"
#function(metric_df,presence,diameters,diam_ratio,beginning,end,tiers,isLog,
#only_best,EventType)
jpeg(paste("Winrates and presence based on Matches between", Beginning, "and", 
           End,"in MTGO", EventType,".jpg"), width = 6000, height = 3000,
     res=300)
metric_graph(metric_df,"Matches","Players",1,Beginning,End,"Pres M+SD",TRUE,
             FALSE,EventType)
dev.off()

#ARCHETYPE CLUSTERING
#function (metric_df,k,iter,init,algo,beginning,end,count_wr,
#only_best,EventType)
jpeg(paste("Best archetypes clustering between", Beginning, "and", End,
           "in MTGO", EventType,".jpg"), width = 5000, height = 3000,res=300)
kmeans_arch(metric_df,4,30,50,"Hartigan-Wong",Beginning,End,TRUE,TRUE,EventType)
dev.off()

#WITH LOGARITHM OF PRESENCE
arch_ranked=archetypes_ranking(metric_df_log_matches,Beginning,End)

metric_df_log_matches_sub=metric_df_log_matches[metric_df_log_matches$TOTAL_NB_MATCHES>
                                        mean(metric_df_log_matches$TOTAL_NB_MATCHES),]
arch_ranked_sub=archetypes_ranking(metric_df_log_matches_sub,Beginning,End)

#RANKING THE DECKS BASED ON COMBINATION OF WINRATE AND LOG(PRESENCE)
jpeg(paste("Deck score based on sum of winrate and log(presence) between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
log_comb_graph(df,arch_ranked_sub,Beginning,End,EventType)
dev.off()

df_tiers_list=generate_tiers_lists(arch_ranked_sub)

#View(df_tiers_list)

setwd(DirectoryFile)
setwd(file.path(paste(DirectoryFile,"/Results_as_csv/",Beginning,"_",
                      End,sep=""), EventType))
write.csv(df_tiers_list,paste(Beginning,'-',End,'_DF_Tiers_Lists.csv',sep=''), 
          row.names = TRUE)
