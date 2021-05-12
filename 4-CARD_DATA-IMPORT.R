#4th file to execute
#Imports all the data for each existing card: name, color, sets, mana costs,
#types...

getCardData = function(DirectoryFile){
  #DATA FROM: https://mtgjson.com/downloads/all-files/
  setwd(DirectoryFile)
  #IMPORT ALL THE DATA FOR ALL THE CARDS IN THE GAME
  cardData=read.csv("cards.csv",sep=",",header=T)
  #KEEP ONLY RELEVANT INFORMATION AND REMOVE DUPLICATES - CARDS PRINTED 
  #MULTIPLE TIMES
  cardDataSub=unique(subset(cardData,select=c(
    colors,convertedManaCost,faceName,layout,manaCost,name,subtypes,supertypes,
    type,types,isReprint,setCode,artist)))
  return(cardDataSub)
}

# #UNCOMMENT IF YOU EXECUTE THIS FILE ALONE
# cardDataSub=getCardData(DirectoryFile)
  
#filtrer pour ne garder que les cartes présentes dans les résultats au lieu de 
#toutes les cartes du jeu ? 

#ADD A COLUMN WITH THE AVERAGE CONVERTED MANA COST OF EACH DECK (WITH OR WITHOUT
#CONSIDERING LANDS, WHICH ALWAYS HAVE A CMC OF 0)

#STILL HAS WARNINGS, WHERE DO THEY COME FROM? - NOT REALLY AN EMERGENCY
addCMC = function(df){
  for (i in 1:nrow(df)){
    
    list_cmc_lands=rep(0,length(df$Mainboard[[i]]$CardName))
    
    for (j in 1:length(df$Mainboard[[i]]$CardName)){
      
      if (df$Mainboard[[i]]$CardName[j] %in% cardDataSub$name){
        
        list_cmc_lands[j]=cardDataSub[cardDataSub$name==df$Mainboard[[i]]$
                                        CardName[j],]$convertedManaCost[1]
        
      }else if (df$Mainboard[[i]]$CardName[j] %in% cardDataSub$faceName){
        
        list_cmc_lands[j]=cardDataSub[cardDataSub$faceName==df$Mainboard[[i]]$
                                        CardName[j],]$convertedManaCost[1]
      }
    }
    
    df$CMCLands[i]=sum(list_cmc_lands*df$Mainboard[[i]]$Count)/
      sum(df$Mainboard[[i]]$Count)
    
    list_cmc=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("CMC", "Count"))
    
    for (j in 1:length(df$Mainboard[[i]]$CardName)){
      
      if (df$Mainboard[[i]]$CardName[j] %in% cardDataSub$name){
        
        if(!cardDataSub[cardDataSub$name==
                        df$Mainboard[[i]]$CardName[j],]$types[1]=="Land"){
          
          list_cmc_temp=data.frame(cardDataSub[
            cardDataSub$name==df$Mainboard[[i]]$CardName[j],]$
                                     convertedManaCost,df$Mainboard[[i]]$Count[j])
          names(list_cmc_temp)=c("CMC", "Count")
          list_cmc=rbind(list_cmc, list_cmc_temp)
          
        }
        
      }else if (df$Mainboard[[i]]$CardName[j] %in% cardDataSub$faceName){
        
        if(!cardDataSub[cardDataSub$faceName==
                        df$Mainboard[[i]]$CardName[j],]$types[1]=="Land"){
          
          list_cmc_temp=data.frame(cardDataSub[
            cardDataSub$faceName==df$Mainboard[[i]]$CardName[j],]$
                                     convertedManaCost,df$Mainboard[[i]]$Count[j])
          names(list_cmc_temp)=c("CMC", "Count")
          list_cmc=rbind(list_cmc, list_cmc_temp)
        }
      }
    }
    
    df$CMC[i]=sum(list_cmc$CMC*list_cmc$Count)/sum(list_cmc$Count)
  }
  
  return(df)
}

#IT CAN BE A BIT LONG TO EXECUTE, LEAVE IT A MINUTE OR TWO
#TODO: REPAIR THE FUNCTION:
# Error in if (df$Mainboard[[i]]$CardName[j] %in% cardDataSub$name) { : 
#     l'argument est de longueur nulle
#UNCOMMENT IF YOU EXECUTE THIS FILE ALONE
# df=addCMC(df)

####################################################
#CHECK IF THERE IS ANY ABSURD VALUE IN THE RESULTS
# df[df$CMC==0,]$CMC
# df[df$CMC=="0",]$CMC
# df[is.na(df$CMC),]$CMC
# 
#AVERAGE CMC OF THE FORMAT
# mean(df$CMC)
# mean(df$CMCLands)
# 
#AVERAGE CMC OF A DECK
# mean(df[df$ARCHETYPE=="Oops All Spells",]$CMC)
# mean(df[df$ARCHETYPE=="Oops All Spells",]$CMCLands)
####################################################