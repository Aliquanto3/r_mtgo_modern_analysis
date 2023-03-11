### LIBRARIES ###
library("jsonlite")

### PARAMETERS ###
DirectoryFile="MTGO_Data"
MTGFormat="Modern"
RawFile=paste(MTGFormat,"data.json",sep="_")

### DATA IMPORT ###
rawData=fromJSON(paste(DirectoryFile,RawFile,sep="/"))[[1]]
rawData$Date = as.Date(rawData$Date)

# rawData$Points = as.numeric(rawData$Points)
# rawData$TournamentName=rep(NA,nrow(rawData))
# rawData$TournamentName=paste(rawData$Tournament,as.character(rawData$Date),sep=" ")
# min(rawData$Date)
# max(rawData$Date)
# unique(rawData$Tournament)

### Filter MTGO Data only ###
MTGOData=rawData[grep(pattern = "https://www.mtgo.com/en/mtgo/decklist",x=rawData$AnchorUri),]
MTGOData=MTGOData[!grepl("League", MTGOData$Tournament),]
MTGOData=MTGOData[!grepl("Daily Swiss", MTGOData$Tournament),]

unique(MTGOData$Tournament)

### Combine decklists in a single column ###
MTGOData$Allboards = rep(0,nrow(MTGOData))
pb = winProgressBar(title = "progress bar", min = 0,
                     max = nrow(MTGOData), width = 300)
for (i in 1:nrow(MTGOData)){
  mainboardi = MTGOData$Mainboard[[i]]
  sideboardi = MTGOData$Sideboard[[i]]
  MTGOData$Allboards[i] = list(aggregate(. ~ CardName, rbind(mainboardi, sideboardi), sum, na.rm = TRUE, na.action = NULL))
  setWinProgressBar(pb, i, title=paste( round(i/nrow(MTGOData)*100, 0),"% done"))
}
close(pb)

### Split the data by year
YearList = c(2016:2022)

YearData = lapply(YearList,function(x) 
  subset(MTGOData, Date >= as.Date(paste(x,"-01-01",sep="")) 
         & Date < as.Date(paste(x,"-12-31",sep=""))))

names(YearData)=YearList
# View(YearData)

### Get the list of different cards and their deck count per year ###
getDeckCount = function(x, df){
  sum(unlist(lapply(df[["Allboards"]], function(y) x %in% y$CardName)))
}

CardsPresence = function(df){
  CardsNames = unique(unlist(sapply(c(1:nrow(df)), function(i) list(df[["Allboards"]][[i]]$CardName))))
  DecksCounts = unlist(lapply(CardsNames, getDeckCount, df=df))
  CardsData=setNames(data.frame(CardsNames,DecksCounts), c("CardNames", "DeckCount"))
  return(list("Number of different cards" = length(CardsNames),
              "Number of decks" = nrow(df),
              "Different cards by deck" = signif(length(CardsNames)/nrow(df),3),
              "Average of card presence in decks" =  paste(100*signif(mean(DecksCounts)/nrow(df),3),"%"),
              "Standard deviation of card presence in decks" =  paste(100*signif(sd(DecksCounts)/nrow(df),3),"%"),
              "Maximum of card presence in decks" =  paste(100*signif(max(DecksCounts)/nrow(df),3),"%")))
  
}

YearResult = lapply(YearData, CardsPresence)

YearResultDf = do.call(rbind.data.frame, YearResult)
names(YearResultDf) = gsub(x = names(YearResultDf), pattern = "\\.", replacement = " ") 
View(YearResultDf)
write.csv(YearResultDf, "MTGO_Data\\Card diversity by year.csv", row.names=TRUE)
