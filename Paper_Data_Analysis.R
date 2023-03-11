source(file.path(paste(rprojroot::find_rstudio_root_file(),"1-PARAMETERS.R",sep="/")))
EventType = "Paper"
EventType = "Full metagame"

source(file.path(paste(rprojroot::find_rstudio_root_file(),"3-METAGAME_FUNCTIONS.R",sep="/")))

#install.packages("jsonlite")
library("jsonlite")
#install.packages("readr")
library("readr")

rawData=fromJSON(paste(DirectoryFile,RawFile,sep="/"))[[1]]
View(rawData)
rawData$Date = as.Date(rawData$Date)
rawData$Points = as.numeric(rawData$Points)
if(is.na(Beginning)){
  Beginning=min(rawData$Date)
}
if(is.na(End)){
  End=max(rawData$Date)
}
periodData=subset(rawData, Date >= as.Date(Beginning) & Date < as.Date(End))

MTGOData = periodData[grep("https://www.mtgo.com/en/mtgo/decklist/", periodData$AnchorUri), ]
MTData = periodData[grep("https://www.manatraders.com/webshop/personal/", periodData$AnchorUri), ]
PaperData = periodData[grep("https://mtgmelee.com/Decklist/View/", periodData$AnchorUri), ]
FullMetaData = rbind(MTData,PaperData)

dir.create(file.path(DirectoryFile,MTGFormat))
dir.create(file.path(paste(DirectoryFile,MTGFormat,sep="/"), 
                     paste(Beginning,End,sep="_")))
dir.create(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                           sep="/"), EventType))

dir.create(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                           EventType,sep="/"), "Results_as_pictures"))
dir.create(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                           EventType,sep="/"), "Results_as_csv"))
dir.create(file.path(paste(DirectoryFile,MTGFormat,paste(Beginning,End,sep="_"),
                           EventType,sep="/"), "Results_as_txt"))