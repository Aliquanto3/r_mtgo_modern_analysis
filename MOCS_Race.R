# MOCS Race algorithm
# 1.	Load data
# 2.	Keep only results after 2021-04-24
# 3.	Keep only tournaments including « Qual » in their name (Champ Qual/Super Qualifier)
# 4.	Create a table with 2 columns: Player and NoQualTop8
# 5.	Column “Player” is filled with the list of unique different player names
# 6.	Column “NoQualTop8” contains the number of times the player whose name is in the same row of the table got a placement for 1st place to 8th place in one of the Qual events
# 



#install.packages("jsonlite")
library("jsonlite")
#install.packages("readr")
library("readr")

# PARAMETERS
#Directory of the file
DirectoryFile="MTGO_Data"
#Name of the file
RawFile="mtgo_data_2021_05_20.json"
#Earliest date - if NA, starts from the beginning of the data
#Requires restarting Data Treatment if updated
Beginning="2021-04-24"
#Latest date - if NA, goes up to the end of the data
#Requires restarting Data Treatment if updated
End="2021-05-21"

#Import data
rawData=fromJSON(paste(DirectoryFile,RawFile,sep="/"))[[1]]

#Convert data to the right type
rawData$Date = as.Date(rawData$Date)
rawData$Points = as.numeric(rawData$Points)

if(is.na(Beginning)){
  Beginning=min(rawData$Date)
}
if(is.na(End)){
  End=max(rawData$Date)
}

#SELECT DATA FOR A SPECIFIC PERIOD
periodData=subset(rawData, Date >= as.Date(Beginning) & Date < as.Date(End))
unique(periodData$Tournament)

#Filter to keep only Champ Qual/Super Qualifier
QualData=periodData[grep("Qual", periodData$Tournament), ]
unique(QualData$Tournament)

#Create the dataframe to contain the results
dfPlayerData=setNames(data.frame(matrix(
  ncol = 3, nrow = length(unique(QualData$Player)))), 
  c("Player", "NoQualTop8", "Points"))

#Get the list of different players
dfPlayerData$Player=QualData$Player

#For each player, get the number of top8 it reached and the average 
#win rate
for (i in 1:nrow(dfPlayerData)){
  dfMDSeti=CardResultsMD[CardResultsMD$FirstSet==dfPlayerData$SetCode[i],]
  dfSBSeti=CardResultsSB[CardResultsSB$FirstSet==dfPlayerData$SetCode[i],]
  
  #Number of copies: sum of all the copies of each card of the set in the data
  dfPlayerData$NoCopies[i] = sum(dfMDSeti$CardCount) + sum(dfSBSeti$CardCount)
  
  #Win rate: get the number of wins of each card of the set divided by the
  # number of matches cards of the set played
  wri=(sum(dfMDSeti$WinCount) + sum(dfSBSeti$WinCount))/ 
    (sum(dfMDSeti$MatchCount) + sum(dfSBSeti$MatchCount))
  dfPlayerData$Winrate[i] = as.numeric(format(round(wri,3),  nsmall = 3))
}

#Sort by number of copies
dfPlayerData=arrange(dfPlayerData,desc(NoCopies))
dfPlayerData