#5th file to execute
#Imports all the data for each existing card: name, color, sets, mana costs,
#types...

#DATA FROM: https://mtgjson.com/downloads/all-files/
setwd(DirectoryFile)
cardData=read.csv("cards.csv",sep=",",header=T)
#View(cardData)
names(cardData)
cardDataSub=unique(subset(cardData,select=c(colors,convertedManaCost,faceName,
                                            layout,manaCost,name,subtypes,
                                            supertypes,type,types)))
names(cardDataSub)
# unique(cardDataSub$type)
# unique(cardDataSub$types)
# unique(cardDataSub$subtypes)
# unique(cardDataSub$supertypes)
# unique(cardDataSub$setCode)

#SELECT FIRST LINE CONTAINING THE NAME OF A CARD - EXAMPLE: "Lightning Bolt
boltData0=cardDataSub[cardDataSub$name=="Lightning Bolt",]
boltData=t(cardDataSub[grep("Lightning Bolt", cardDataSub$name),])[
  1:length(names(cardDataSub))]
mode(boltData0)

#TO BE USED FOR DUAL FACE CARDS
# if (card.layout == "transform" | card.layout == "flip" | 
#     card.layout == "adventure" | card.layout == "meld" | card.layout == "modal_dfc") {
#   cardName = card.faceName
# }else{
#   cardName = card.name;
# }

# for (i in 1:length(df$URL)){
#   list_cmc=rep(0,length(df$MDCards[1]))
#   for (i in 1:length(list_cmc)){
#     
#   }
#   df$CMC_LANDS[i]
#   df$CMC[i]
# }







