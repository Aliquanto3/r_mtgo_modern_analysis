#1st file to execute
#Contains the parameters of the programs, such as the files to be used, the
#period to cover, the type of events, how to aggregate results in graphics...

###############################################################################
#PARAMETERS - CHANGE THEM TO ANALYSE DIFFERENT DATA SETS AND ARCHETYPES, OR
#TUNE THE METRICS. YOU CAN ALSO UPDATE THEM IN THE CONSOLE AFTER RUNNING THE
#PROGRAM, BEFORE USING THE FUNCTIONS AT THE END TO GENERATE GRAPHS AND RESULTS

#Directory of the file
#Requires restarting Data Treatment if updated
DirectoryFile="MTGO_Data"

#Format of the wanted events: NA if all
#"All_Formats,"Sealed","Standard","Pioneer","Modern","Legacy","Vintage","Pauper"
MTGFormat="Modern"

#Name of the file
#Requires restarting Data Treatment if updated
RawFile=paste(MTGFormat,"data.json",sep="_")

#Earliest date - if NA, starts from the beginning of the data
#Requires restarting Data Treatment if updated
Beginning="2021-02-17"

#If you want to know the minimum date in the data, use:
#min(rawData$DATE)
#after you executed the 2nd file paragraph

#Latest date - if NA, goes up to the end of the data
#Requires restarting Data Treatment if updated
End="2022-01-19"

#If you want to know the maximum date in the data, use:
#max(rawData$DATE)
#after you executed the 2nd file paragraph

#Event type:

#"All Events" = everything available including leagues
#"Everything but Leagues" = Major Events, official or not, and Preliminaries
#"Official Competitions" = Major Official Events + Preliminaries
#"Major Official Events" = Challenge, Champ, Showcase, Premier, Qualifier, MOCS
#"Major Events Top32" = Major Events, official or unofficial, only top32
#"Challenges" = Challenges
#"Preliminaries" = Preliminaries
#"ManaTraders Series" = ManaTraders Series
#"NRG Series" = NRG Series

#Requires restarting Data Treatment if updated
EventType="Challenges"

#Type of deck classification - "Super" or "Exact"
#Requires restarting Data Treatment if updated
Classification="Exact"

#Required metagame share to appear on pie chart (numeric, gets converted to %)
PieShare=2

#Required metagame share to appear on histogram (numeric, gets converted to %)
HistShare=2

#WEIGHT OF THE METRICS POINTS FOR THE COMPILATION REQUIRED FOR RANKING
Presence_Weight=1

#WEIGHT OF THE METRICS AVERAGE FOR THE COMPILATION REQUIRED FOR RANKING
PPR_Weight=1

#CODE OF THE LAST SET
lastSetCode="MH2"

#TRUE IF YOU WANT TO GET ONLY THE PLAYER DATA, SAVES A LOT OF TIME BY 
#REMOVING THE COMPUTATION OF THE CARD DATA
PlayerResultsOnly=T

###############################################################################