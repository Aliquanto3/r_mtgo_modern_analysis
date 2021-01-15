#RUN FILES 1,2 AND 3 BEFORE THIS FILE

#CREATE THE DIRECTORY WHERE TO SAVE THE PICTURES
setwd(DirectoryFile)
eventList=c("Competitions","Preliminaries","Challenges")
dir.create(file.path(paste(DirectoryFile,"Results_as_pictures",sep="/"), 
                     paste(Beginning,End,sep="_")))

#LOOKS LIKE A "FOR" WON'T WORK TO EXPORT THE GRAPHS

setwd(file.path(paste(DirectoryFile,"/Results_as_pictures/",sep="/"), 
                paste(Beginning,End,sep="_")))
EventType=eventList[1]
dir.create(file.path(paste(DirectoryFile,"/Results_as_pictures/",Beginning,
                           "_",End,sep=""), EventType))
setwd(file.path(paste(DirectoryFile,"/Results_as_pictures/",Beginning,"_",
                      End,sep=""), EventType))

df=generate_df(rawData,EventType)
df=add_super_archetypes(df)

#GRAPHIC ANALYSIS
metric_df=metric_points_archetypes(df,Beginning,End)
metric_df_log_matches=metric_df
metric_df_log_matches$TOTAL_NB_MATCHES=
  log(metric_df_log_matches$TOTAL_NB_MATCHES)

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

arch_ranked=archetypes_ranking(metric_df,Beginning,End)
jpeg(paste("Low winrate estimation in 0.95 CI between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
lower_born_ci_winrate_graph(df,arch_ranked,Beginning,End,EventType)
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
#function (metric_df,k,iter,init,algo,beginning,end,diam_ratio,count_wr,
#only_best,EventType)
jpeg(paste("Best archetypes clustering between", Beginning, "and", End,
           "in MTGO", EventType,".jpg"), width = 5000, height = 3000,res=300)
kmeans_arch(metric_df,4,30,50,"Hartigan-Wong",Beginning,End,3,TRUE,TRUE,EventType)
dev.off()

#########


setwd(file.path(paste(DirectoryFile,"/Results_as_pictures/",sep="/"), 
                paste(Beginning,End,sep="_")))
EventType=eventList[2]
dir.create(file.path(paste(DirectoryFile,"/Results_as_pictures/",Beginning,
                           "_",End,sep=""), EventType))
setwd(file.path(paste(DirectoryFile,"/Results_as_pictures/",Beginning,"_",
                      End,sep=""), EventType))

df=generate_df(rawData,EventType)
df=add_super_archetypes(df)

#GRAPHIC ANALYSIS
metric_df=metric_points_archetypes(df,Beginning,End)
metric_df_log_matches=metric_df
metric_df_log_matches$TOTAL_NB_MATCHES=
  log(metric_df_log_matches$TOTAL_NB_MATCHES)

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

arch_ranked=archetypes_ranking(metric_df,Beginning,End)
jpeg(paste("Low winrate estimation in 0.95 CI between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
lower_born_ci_winrate_graph(df,arch_ranked,Beginning,End,EventType)
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
#function (metric_df,k,iter,init,algo,beginning,end,diam_ratio,count_wr,
#only_best,EventType)
jpeg(paste("Best archetypes clustering between", Beginning, "and", End,
           "in MTGO", EventType,".jpg"), width = 5000, height = 3000,res=300)
kmeans_arch(metric_df,4,30,50,"Hartigan-Wong",Beginning,End,3,TRUE,TRUE,EventType)
dev.off()


##########

setwd(file.path(paste(DirectoryFile,"/Results_as_pictures/",sep="/"), 
                paste(Beginning,End,sep="_")))
EventType=eventList[3]
dir.create(file.path(paste(DirectoryFile,"/Results_as_pictures/",Beginning,
                           "_",End,sep=""), EventType))
setwd(file.path(paste(DirectoryFile,"/Results_as_pictures/",Beginning,"_",
                      End,sep=""), EventType))

df=generate_df(rawData,EventType)
df=add_super_archetypes(df)

#GRAPHIC ANALYSIS
metric_df=metric_points_archetypes(df,Beginning,End)
metric_df_log_matches=metric_df
metric_df_log_matches$TOTAL_NB_MATCHES=
  log(metric_df_log_matches$TOTAL_NB_MATCHES)

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

arch_ranked=archetypes_ranking(metric_df,Beginning,End)
jpeg(paste("Low winrate estimation in 0.95 CI between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
lower_born_ci_winrate_graph(df,arch_ranked,Beginning,End,EventType)
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
#function (metric_df,k,iter,init,algo,beginning,end,diam_ratio,count_wr,
#only_best,EventType)
jpeg(paste("Best archetypes clustering between", Beginning, "and", End,
           "in MTGO", EventType,".jpg"), width = 5000, height = 3000,res=300)
kmeans_arch(metric_df,4,30,50,"Hartigan-Wong",Beginning,End,3,TRUE,TRUE,EventType)
dev.off()


##########################################
# for (i in 1:length(eventList)){
#   setwd(file.path(paste(DirectoryFile,"/Results_as_pictures/",sep="/"), 
#                        paste(Beginning,End,sep="_")))
#   EventType=eventList[i]
#   dir.create(file.path(paste(DirectoryFile,"/Results_as_pictures/",Beginning,
#                              "_",End,sep=""), EventType))
#   setwd(file.path(paste(DirectoryFile,"/Results_as_pictures/",Beginning,"_",
#                         End,sep=""), EventType))
#   
#   df=generate_df(rawData,EventType)
#   df=add_super_archetypes(df)
# 
#   #GRAPHIC ANALYSIS
#   metric_df=metric_points_archetypes(df,Beginning,End)
#   metric_df_log_matches=metric_df
#   metric_df_log_matches$TOTAL_NB_MATCHES=
#     log(metric_df_log_matches$TOTAL_NB_MATCHES)
#   
#   #PRESENCE
#   PieShare=2.4
#   # 1. Open jpeg file
#   jpeg(paste(generate_metagame_graph_title("Matches",Beginning,End,EventType),".jpg"), 
#        width = 3500, height = 3500,res=300)
#   metagame_pie_chart(df,"Matches",Beginning,End,EventType)
#   dev.off()
#   jpeg(paste(generate_metagame_graph_title("Players",Beginning,End,EventType),".jpg"), 
#        width = 3500, height = 3500,res=300)
#   metagame_pie_chart(df,"Players",Beginning,End,EventType)
#   dev.off()
#   
#   #WINRATES
#   arch_ranked=archetypes_ranking(metric_df,Beginning,End)
#   jpeg(paste("0.95 CI on winrate between", Beginning, "and", End, 
#              "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
#   winrates_graph(df,arch_ranked,"Matches",Beginning,End,EventType)
#   dev.off()
#   
#   arch_ranked=archetypes_ranking(metric_df,Beginning,End)
#   jpeg(paste("Low winrate estimation in 0.95 CI between", Beginning, "and", End, 
#              "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
#   lower_born_ci_winrate_graph(df,arch_ranked,Beginning,End,EventType)
#   dev.off()
#   
#   #presence AND diameters CAN BE EITHER "Copies", "Players" or "Matches"
#   #tiers CAN BE EITHER "Win+Pres","Pres M+SD" or "Pres %"
#   #function(metric_df,presence,diameters,diam_ratio,beginning,end,tiers,isLog,
#   #only_best,EventType)
#   jpeg(paste("Winrates and presence based on Matches between", Beginning, "and", 
#              End,"in MTGO", EventType,".jpg"), width = 6000, height = 3000,
#        res=300)
#   metric_graph(metric_df,"Matches","Players",1,Beginning,End,"Pres M+SD",TRUE,
#                FALSE,EventType)
#   dev.off()
#   
#   #ARCHETYPE CLUSTERING
#   #function (metric_df,k,iter,init,algo,beginning,end,diam_ratio,count_wr,
#   #only_best,EventType)
#   jpeg(paste("Best archetypes clustering between", Beginning, "and", End,
#              "in MTGO", EventType,".jpg"), width = 5000, height = 3000,res=300)
#   kmeans_arch(metric_df,4,30,50,"Hartigan-Wong",Beginning,End,3,TRUE,TRUE,EventType)
#   dev.off()
# }