###############################################################################
#PARAMETERS - CHANGE THEM TO ANALYSE DIFFERENT DATA SETS AND ARCHETYPES, OR
#TUNE THE METRICS. YOU CAN ALSO UPDATE THEM IN THE CONSOLE AFTER RUNNING THE
#PROGRAM, BEFORE USING THE FUNCTIONS AT THE END TO GENERATE GRAPHS AND RESULTS

#Directory of the file
#Requires restarting Data Treatment if updated
DirectoryFile="D:\\MTG\\Meta analysis\\r_mtgo_modern_analysis\\MTGO_Data"

#Name of the file
#Requires restarting Data Treatment if updated
RawFile="mtgo_data_2020_11_29.csv"

#Path of the repertory containing the multiple subdirectories of the results
#converted to JSON
MTGODataPath="D:/MTG/Meta analysis/MTGODecklistCache/Tournaments/2020/11"

#Earliest date - if NA, starts from the beginning of the data
#Requires restarting Data Treatment if updated
Beginning="2020-11-01"
#If you want to know the minimum date in the data, use:
#min(rawData$DATE)
#after you executed the IMPORT DATA paragraph

#Latest date - if NA, goes up to the end of the data
#Requires restarting Data Treatment if updated
End=NA

#If you want to know the maximum date in the data, use:
#max(rawData$DATE)
#after you executed the IMPORT DATA paragraph

#Event type - "Competitions" (Preliminaries + Challenges), "Preliminaries" or 
#"Challenges"
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