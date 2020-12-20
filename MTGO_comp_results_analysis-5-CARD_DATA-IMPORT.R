#5th file to execute
#Imports all the data for each existing card: name, color, sets, mana costs,
#types...

#DATA FROM: https://mtgjson.com/downloads/all-files/
setwd(DirectoryFile)
#IMPORT ALL THE DATA FOR ALL THE CARDS IN THE GAME
cardData=read.csv("cards.csv",sep=",",header=T)
#KEEP ONLY RELEVANT INFORMATION AND REMOVE DUPLICATES - CARDS PRINTED 
#MULTIPLE TIMES
cardDataSub=unique(subset(cardData,select=c(colors,convertedManaCost,faceName,
                                            layout,manaCost,name,subtypes,
                                            supertypes,type,types)))

#filtrer pour ne garder que les cartes présentes dans les résultats au lieu de 
#toutes les cartes du jeu ? 

#ADD A COLUMN WITH THE AVERAGE CONVERTED MANA COST OF EACH DECK (WITH OR WITHOUT
#CONSIDERING LANDS, WHICH ALWAYS HAVE A CMC OF 0)
addCMC = function(df){
  for (i in 1:length(df$URL)){
    
    list_cmc_lands=rep(0,length(df$MDCards[[i]]))
    for (j in 1:length(df$MDCards[[i]])){
      if (df$MDCards[[i]][j] %in% cardDataSub$name){
        list_cmc_lands[j]=cardDataSub[cardDataSub$name==df$MDCards[[i]][j],]$
          convertedManaCost
      }else if (df$MDCards[[i]][j] %in% cardDataSub$faceName){
        list_cmc_lands[j]=cardDataSub[cardDataSub$faceName==df$MDCards[[i]][j],]$
          convertedManaCost
      }
    }
    
    df$CMC_LANDS[i]=sum(list_cmc_lands*df$MDCounts[[i]])/sum(df$MDCounts[[i]])
    
    list_cmc=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("CMC", "COUNT"))
    
    for (j in 1:length(df$MDCards[[i]])){
      if (df$MDCards[[i]][j] %in% cardDataSub$name){
        if(!cardDataSub[cardDataSub$name==df$MDCards[[i]][j],]$types=="Land"){
          list_cmc_temp=data.frame(cardDataSub[cardDataSub$name==df$MDCards[[i]][j],]$
                                     convertedManaCost,df$MDCounts[[i]][j])
          names(list_cmc_temp)=c("CMC", "COUNT")
          list_cmc=rbind(list_cmc, list_cmc_temp)
        }
      }else if (df$MDCards[[i]][j] %in% cardDataSub$faceName){
        if(!cardDataSub[cardDataSub$faceName==df$MDCards[[i]][j],]$types=="Land"){
          list_cmc_temp=data.frame(cardDataSub[cardDataSub$faceName==df$MDCards[[i]][j],]$
                                     convertedManaCost,df$MDCounts[[i]][j])
          names(list_cmc_temp)=c("CMC", "COUNT")
          list_cmc=rbind(list_cmc, list_cmc_temp)
        }
      }
    }
    
    df$CMC[i]=sum(list_cmc$CMC*list_cmc$COUNT)/sum(list_cmc$COUNT)
  }
  
  return(df)
}

#IT CAN BE A BIT LONG TO EXECUTE, LEAVE IT A MINUTE OR TWO
df=addCMC(df)

####################################################
#CHECK IF THERE IS ANY ABSURD VALUE IN THE RESULTS
# df[df$CMC==0,]$CMC
# df[df$CMC=="0",]$CMC
# df[is.na(df$CMC),]$CMC
# 
#AVERAGE CMC OF THE FORMAT
# mean(df$CMC)
# mean(df$CMC_LANDS)
# 
#AVERAGE CMC OF A DECK
# mean(df[df$ARCHETYPE=="Oops All Spells",]$CMC)
# mean(df[df$ARCHETYPE=="Oops All Spells",]$CMC_LANDS)
####################################################