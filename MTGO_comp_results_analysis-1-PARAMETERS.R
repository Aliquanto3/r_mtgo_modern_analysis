#1st file to execute
#Contains the parameters of the programs, such as the files to be used, the
#period to cover, the type of events, how to aggregate results in graphics...

###############################################################################
#PARAMETERS - CHANGE THEM TO ANALYSE DIFFERENT DATA SETS AND ARCHETYPES, OR
#TUNE THE METRICS. YOU CAN ALSO UPDATE THEM IN THE CONSOLE AFTER RUNNING THE
#PROGRAM, BEFORE USING THE FUNCTIONS AT THE END TO GENERATE GRAPHS AND RESULTS

#Directory of the file
#Requires restarting Data Treatment if updated
DirectoryFile="D:/MTG/Meta analysis/r_mtgo_modern_analysis/MTGO_Data"

#Name of the file
#Requires restarting Data Treatment if updated
RawFile="mtgo_data_2021_01_03.csv"
#RawFile="mtgo_data_2020_12_27.csv"

#Path of the repertory containing the multiple subdirectories of the results
#converted to JSON
#Repertory of the data imported from: https://github.com/Badaro/MTGODecklistCache 
#MTGODataPath="D:/MTG/Meta analysis/ManaTradersDecklistCache/Tournaments"
MTGODataPath="D:/MTG/Meta analysis/MTGODecklistCache/Tournaments"

#Earliest date - if NA, starts from the beginning of the data
#Requires restarting Data Treatment if updated
Beginning="2020-11-30"
#If you want to know the minimum date in the data, use:
#min(rawData$DATE)
#after you executed the 2nd file paragraph

#Latest date - if NA, goes up to the end of the data
#Requires restarting Data Treatment if updated
End="2021-01-04"

#If you want to know the maximum date in the data, use:
#max(rawData$DATE)
#after you executed the 2nd file paragraph

#Event type - "Competitions" (All but Leagues), "Preliminaries", 
#"Challenges" or "Events" (All)
#Requires restarting Data Treatment if updated
EventType="Competitions"

#Type of deck classification - "Super" or "Exact"
#Requires restarting Data Treatment if updated
Classification="Exact"

#Required metagame share to appear on pie chart (numeric, gets converted to %)
PieShare=2.5

#Required metagame share to appear on histogram (numeric, gets converted to %)
HistShare=2

#WEIGHT OF THE METRICS POINTS FOR THE COMPILATION REQUIRED FOR RANKING
Presence_Weight=1

#WEIGHT OF THE METRICS AVERAGE FOR THE COMPILATION REQUIRED FOR RANKING
PPR_Weight=1

###############################################################################