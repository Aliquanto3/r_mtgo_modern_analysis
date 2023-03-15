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
MTGFormat="Pioneer"

#Name of the file
#Requires restarting Data Treatment if updated
RawFile=paste(MTGFormat,"data.json",sep="_")

#Earliest date - if NA, starts from the beginning of the data
#Requires restarting Data Treatment if updated
Beginning="2022-06-07"

#If you want to know the minimum date in the data, use:
#min(rawData$DATE)
#after you executed the 2nd file paragraph

#Latest date - if NA, goes up to the end of the data
#Requires restarting Data Treatment if updated
End="2023-03-13"

#Automatically updates the period thresholds to cover the last 4 weeks in the data
Date.autoupdate = T

#If you want to know the maximum date in the data, use:
#max(rawData$DATE)
#after you executed the 2nd file paragraph

# Event type:
# All sources = Everything (except MTGO Leagues - for any filter)
# Top32 Only = Only events with a top32 (aka not MTGO Preliminaries)
# Full Meta = Only events with the full metagame available (not MTGO Official results)
# ManaTraders = ManaTraders Series results
# Paper = Results from MTG Melee
# MTGO Official Competitions = Results from the MTGO website
# MTGO Events with a Top32 = MTGO results with a top32 (so not Preliminaries)
# MTGO Preliminaries = As per name

#Requires restarting Data Treatment if updated
EventType="Paper"

#Type of deck classification - "Super" or "Exact"
#Requires restarting Data Treatment if updated
Classification="Exact"

#Required metagame share to appear on pie chart (numeric, gets converted to %)
PieShare=2
#Automatically update this value with the average presence based on matches
PieShare.autoupdate=T

#Required metagame share to appear on histogram (numeric, gets converted to %)
HistShare=2
#Automatically update this value with the average presence based on matches
HistShare.autoupdate=T

#WEIGHT OF THE METRICS POINTS FOR THE COMPILATION REQUIRED FOR RANKING
Presence_Weight=1

#WEIGHT OF THE METRICS AVERAGE FOR THE COMPILATION REQUIRED FOR RANKING
PPR_Weight=1

#CODE OF THE LAST SET
lastSetCode="ONE"

#TRUE IF YOU WANT TO GET THE PLAYER DATA, CAN SAVE A LOT OF TIME IF YOU DON'T
#NEED IT BY SETTING IT ON FALSE
PlayerResults = F

#TRUE IF YOU WANT TO GET THE CARD DATA, CAN SAVE A LOT OF TIME IF YOU DON'T
#NEED IT BY SETTING IT ON FALSE
CardResults = F

#TRUE IF YOU WANT TO GET THE COMPANION DATA, CAN SAVE A LITTLE OF TIME IF YOU DON'T
#NEED IT BY SETTING IT ON FALSE
CompanionResults = F

#GET FILE GRAPHS EASIER TO MANIPULATE WITH ILLUSTRATOR IF TRUE, BUT TAKES LONGER
DrawSVG = F

###############################################################################