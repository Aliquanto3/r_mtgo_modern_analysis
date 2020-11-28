###############################################################################
#PARAMETERS - CHANGE THEM TO ANALYSE DIFFERENT DATA SETS AND ARCHETYPES, OR
#TUNE THE METRICS. YOU CAN ALSO UPDATE THEM IN THE CONSOLE AFTER RUNNING THE
#PROGRAM, BEFORE USING THE FUNCTIONS AT THE END TO GENERATE GRAPHS AND RESULTS

#Directory of the file
DirectoryFile="D:\\MTG\\Meta analysis\\2020-26-11"

#Name of the file
RawFile="mtgo_data_2020_11_24.csv"

#Earliest date - if NA, starts from the beginning of the data
Beginning="2020-11-01"
#If you want to know the minimum date in the data, use:
#min(rawData$DATE)
#after you executed the IMPORT DATA paragraph

#Latest date - if NA, goes up to the end of the data
End=NA

#If you want to know the maximum date in the data, use:
#max(rawData$DATE)
#after you executed the IMPORT DATA paragraph

#Event type - "Competitions" (Preliminaries + Challenges), "Preliminaries" or 
#"Challenges"
EventType="Challenges"

#Type of deck classification - "Super" or "Exact"
Classification="Super"

#Required metagame share to appear on pie chart (numeric, gets converted to %)
PieShare=3

#Required metagame share to appear on histogramme (numeric, gets converted to %)
HistShare=2

#NUMBER OF POINTS FOR LISTS AT X-0
X_0_PTS=3

#NUMBER OF POINTS FOR LISTS AT X-1
X_1_PTS=2

#NUMBER OF POINTS FOR LISTS AT X-2
X_2_PTS=1

###############################################################################