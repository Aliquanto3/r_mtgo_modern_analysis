#LAST FILE TO EXECUTE FOR ARTICLE DATA

##### SET THE WORKING DIRECTORY TO THE PROJECT's ###############################
library(rprojroot)
setwd(rprojroot::find_rstudio_root_file())

##### IMPORT FUNCTIONS #########################################################

source(file.path(paste(rprojroot::find_rstudio_root_file(),"1-PARAMETERS.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"2-ARCHETYPES-IMPORT.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"3-METAGAME_FUNCTIONS.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"4-CARD_DATA-IMPORT.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"5-CARD_STATS.R",sep="/")))
# source("D:/MTG/Meta analysis/r_mtgo_modern_analysis/6-DECKLISTS_ANALYSIS.R")
# source("D:/MTG/Meta analysis/r_mtgo_modern_analysis/7_ARCHETYPE_ANALYSIS.R")
source(file.path(paste(rprojroot::find_rstudio_root_file(),"A-OUTPUT_FUNCTIONS.R",sep="/")))
#source("D:/MTG/Meta analysis/r_mtgo_modern_analysis/EXPORT_GRAPHS_AND_TXT.R")

##### IMPORT DATA ##############################################################

df=generate_df(EventType,MTGFormat,RawFile,Date.autoupdate)
if(CardResults){
  #df=addCMC(df) #Unnecessary atm and long to run
  cardDataSub=getCardData(DirectoryFile)
  MDStats=CardsStatsGetter(df,"Mainboard")
  SBStats=CardsStatsGetter(df,"Sideboard")
}

# getConflictURL(df)
# getConflictArchetype(df)
# getUnknown(df)

#CREATE THE DIRECTORIES WHERE TO SAVE THE OUTPUTS
createDirectories(DirectoryFile,MTGFormat,Beginning,End,EventType)

#GRAPHIC ANALYSIS
metric_df=metric_points_archetypes(df)

if(PieShare.autoupdate){
  PieShare=updatedShare(metric_df)
}

if(HistShare.autoupdate){
  HistShare=updatedShare(metric_df)
}

#################################################################################

#WORKING DIRECTORY FOR THE CSV
setwd(file.path(paste(rprojroot::find_rstudio_root_file(),
                      DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                      EventType,sep="/"), "Results_as_csv"))

# write.csv(metric_df,paste(Beginning,'-',End,'_DF_Archetypes_Results.csv',sep=''), 
#           row.names = TRUE)

#OVERALL PLAYER RESULTS
if(PlayerResults){
  writePlayerResults(df)
}

#OVERALL CARD RESULTS
if(CardResults){
  writeCardResults(MDStats,SBStats)
}

################################################################################

#WORKING DIRECTORY FOR THE GRAPHS
setwd(file.path(paste(rprojroot::find_rstudio_root_file(),
                      DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                      EventType,sep="/"), "Results_as_pictures"))

#PRESENCE
# 1. Open jpeg and svg file
graph=metagame_pie_chart(df,PieShare,"Copies",Beginning,End,EventType)
if(DrawSVG){
  ggsave(file=paste("Pieplot_Copies_Presence_",Beginning,"_",End,"_",EventType,".svg",
                    sep=""), plot=graph, width=10, height=10)
}
jpeg(paste("Pieplot_Copies_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
graph
dev.off()

graph=metagame_pie_chart(df,PieShare,"Matches",Beginning,End,EventType)
if(DrawSVG){
  ggsave(file=paste("Pieplot_Matches_Presence_",Beginning,"_",End,"_",EventType,".svg",
                    sep=""), plot=graph, width=10, height=10)
}
jpeg(paste("Pieplot_Matches_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
graph
dev.off()

graph=metagame_pie_chart(df,PieShare,"Players",Beginning,End,EventType)
if(DrawSVG){
  ggsave(file=paste("Pieplot_Players_Presence_",Beginning,"_",End,"_",EventType,".svg",
                    sep=""), plot=graph, width=10, height=10)
}
jpeg(paste("Pieplot_Players_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
graph
dev.off()

graph=metagame_box_plot(df,"Matches",Beginning,End,EventType,0)
if(DrawSVG){
  ggsave(file=paste("Barplot_Matches_Presence_",Beginning,"_",End,"_",EventType,".svg",
                    sep=""), plot=graph, width=10, height=10)
}
jpeg(paste("Barplot_Matches_Presence_",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
graph
dev.off()

graph=metagame_box_plot(df,"Matches",Beginning,End,EventType,HistShare)
if(DrawSVG){
  ggsave(file=paste("Barplot_Matches_Presence_Most_Present",Beginning,"_",End,"_",EventType,".svg",
                    sep=""), plot=graph, width=10, height=10)
}
jpeg(paste("Barplot_Matches_Presence_Most_Present",Beginning,"_",End,"_",EventType,".jpg",
           sep=""), width = 3500, height = 3500,res=300)
graph
dev.off()

#WINRATES
arch_ranked=archetypes_ranking(metric_df,Beginning,End)

graph=winrates_graph(df,arch_ranked,"Matches",Beginning,End,EventType)
if(DrawSVG){
  ggsave(file=paste("0.95 CI on winrate between", Beginning, "and", End, 
                    "in MTGO", EventType,".svg"), plot=graph, width=24, height=9)
}
jpeg(paste("0.95 CI on winrate between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
graph
dev.off()

#TODO: actually do the low WR estimate
graph=winrates_graph(df,arch_ranked,"Matches",Beginning,End,EventType)
if(DrawSVG){
  ggsave(file=paste("Low winrate estimation in 0.95 CI between", Beginning, "and", End, 
                    "in MTGO", EventType,".svg"), plot=graph, width=24, height=9)
}
jpeg(paste("Low winrate estimation in 0.95 CI between", Beginning, "and", End, 
           "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
graph
dev.off()

graph=winrate_ratio_arch_out_player_graph(df,metric_df,Beginning,End,EventType)
if(DrawSVG){
  ggsave(file=paste("Ratio of archetype winrates out of pilot winrates between", Beginning, 
                    "and", End, "in MTGO", EventType,".svg"), plot=graph, width=24, height=9)
}
jpeg(paste("Ratio of archetype winrates out of pilot winrates between", Beginning, 
           "and", End, "in MTGO", EventType,".jpg"), width = 8000, height = 3000,res=400)
graph
dev.off()

#presence AND diameters CAN BE EITHER "Copies", "Players" or "Matches"
#tiers CAN BE EITHER "Win+Pres","Pres M+SD" or "Pres %"
#function(metric_df,presence,diameters,diam_ratio,beginning,end,tiers,isLog,
#only_best,EventType)
graph=metric_graph(metric_df,"Matches","Players",1,Beginning,End,"Pres M+SD",TRUE,
                   FALSE,EventType)
if(DrawSVG){
  ggsave(file=paste("Winrates and presence based on Matches between", Beginning, "and", 
                    End,"in MTGO", EventType,".svg"), plot=graph, width=18, height=9)
}
jpeg(paste("Winrates and presence based on Matches between", Beginning, "and", 
           End,"in MTGO", EventType,".jpg"), width = 6000, height = 3000, res=400)
graph
dev.off()

#ARCHETYPE CLUSTERING
#function (metric_df,k,iter,init,algo,beginning,end,count_wr,
#only_best,EventType)
graph=kmeans_arch(metric_df,4,30,50,"Hartigan-Wong",Beginning,End,TRUE,TRUE,EventType)
if(DrawSVG){
  ggsave(file=paste("Best archetypes clustering between", Beginning, "and", End,
                    "in MTGO", EventType,".svg"), plot=graph, width=15, height=9)
}
jpeg(paste("Best archetypes clustering between", Beginning, "and", End,
           "in MTGO", EventType,".jpg"), width = 5000, height = 3000, res=300)
graph
dev.off()

#WITH LOGARITHM OF PRESENCE
metric_df_log_matches=metric_df
metric_df_log_matches$TotalMatches=log(metric_df_log_matches$TotalMatches)
arch_ranked=archetypes_ranking(metric_df_log_matches,Beginning,End)

metric_df_log_matches_sub=metric_df_log_matches[
  metric_df_log_matches$TotalMatches>mean(
    metric_df_log_matches$TotalMatches),]
arch_ranked_sub=archetypes_ranking(metric_df_log_matches_sub,Beginning,End)

#RANKING THE DECKS BASED ON COMBINATION OF WINRATE AND LOG(PRESENCE)
graph=log_comb_graph(df,arch_ranked_sub,Beginning,End,EventType)
if(DrawSVG){
  ggsave(file=paste("Deck score based on sum of winrate and log(presence) between", 
                    Beginning, "and", End, "in MTGO", EventType,".svg"), plot=graph, width=24, height=9)
}
jpeg(paste("Deck score based on sum of winrate and log(presence) between", 
           Beginning, "and", End, "in MTGO", EventType,".jpg"), width = 8000, height = 3000, res=400)
graph
dev.off()

#RANKING THE DECKS BASED ON META SCORE OF WINRATE AND LOG(PRESENCE)
graph=meta_score_graph(df,arch_ranked_sub,Beginning,End,EventType)
if(DrawSVG){
  ggsave(file=paste("Deck score based on VS Meta Score between", 
                    Beginning, "and", End, "in MTGO", EventType,".svg"), plot=graph, width=24, height=9)
}
jpeg(paste("Deck score based on VS Meta Score between", 
           Beginning, "and", End, "in MTGO", EventType,".jpg"), width = 8000, height = 3000, res=400)
graph
dev.off()

matchup_matrix_data = generate_matchup_matrix(df,2,Beginning, End, MTGFormat, EventType)
jpeg(paste("Match Up matrix between", 
           Beginning, "and", End, "in", EventType,"events.jpg"), width = 8000, height = 4000, res=400)
matchup_matrix_data
dev.off()
df_tiers_list=generate_tiers_lists(arch_ranked_sub)

#View(df_tiers_list)

setwd(file.path(paste(rprojroot::find_rstudio_root_file(),
                      DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                      EventType,sep="/"), "Results_as_csv"))
write.csv(df_tiers_list,paste(Beginning,'-',End,'_DF_Tiers_Lists.csv',sep=''), 
          row.names = TRUE)

################################################################################
#CONVERT ALL CSV TO XLSX
convertCSV2XLSX(DirectoryFile,MTGFormat,Beginning,End,EventType)

################################################################################

#EXPORT A QUICK ANALYSIS OF THE DATA IN .TXT
setwd(file.path(paste(rprojroot::find_rstudio_root_file(),
                      DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                      EventType,sep="/"), "Results_as_txt"))

exportTextAnalysis()

################################################################################
if(CompanionResults){
  companionAnalysis()
}

#######################
#Compare specific cards

Cards=c("Urza's Saga", "Ragavan, Nimble Pilferer", "Dragon's Rage Channeler",
        "Solitude", "Fury", "Wrenn and Six")

# TODO : cardComparison(Cards)

get_matchup_data(df,"Rakdos Sacrifice","Simic Lotus Strings")
get_matchup_data(df,"Izzet Creativity","Abzan Grease Fang")
getURLofCard("Play with Fire",df)
getURLofDeck("Izzet Control",df)
getConflictURL(df)
getConflictArchetype(df)
getUnknown(df)

