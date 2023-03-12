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
drawPresenceGraphs(df,Beginning,End,EventType)

#WINRATES
drawWinRateGraphs(df,Beginning,End,EventType)

bidimensionGraphs(df,Beginning,End,EventType)

scoreGraphs(df,Beginning,End,EventType)

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

getURLofCard("Play with Fire",df)
getURLofDeck("Izzet Control",df)
getConflictURL(df)
getConflictArchetype(df)
getUnknown(df)

