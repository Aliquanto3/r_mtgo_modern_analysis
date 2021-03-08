#EXPORT AN ANALYSIS OF AN ARCHETYPE
archName="WURG Control"
archNameFile=gsub('[[:punct:] ]+',' ',archName)
archNameFile=chartr(" ", "_", archName)

#CREATE THE DIRECTORY WHERE TO SAVE THE CSV OF THE ARCHETYPE
setwd(DirectoryFile)
dir.create(file.path(paste(DirectoryFile,"Results_as_csv",sep="/"), 
                     paste(Beginning,End,sep="_")))
setwd(file.path(paste(DirectoryFile,"/Results_as_csv/",sep="/"), 
                paste(Beginning,End,sep="_")))
dir.create(file.path(paste(DirectoryFile,"/Results_as_csv/",Beginning,
                           "_",End,sep=""), EventType))
setwd(file.path(paste(DirectoryFile,"/Results_as_csv/",Beginning,"_",
                      End,sep=""), EventType))
dir.create(file.path(paste(DirectoryFile,"/Results_as_csv/",Beginning,"_",
                           End,"/",EventType,sep=""),archNameFile))
setwd(file.path(paste(DirectoryFile,"/Results_as_csv/",Beginning,"_",
                      End,"/",EventType,sep=""),archNameFile))

#AVERAGE NUMBER OF EACH CARD IN THE ARCHETYPE
df_avg_cards=archAverageData(df,archName)
write.csv(df_avg_cards, paste(Beginning,'-',End,'-',archNameFile,
                              '_DF_Average_Cards.csv',sep=''), 
          row.names = FALSE)
#AVERAGE MD ROUNDING DOWN THE RATIOS
avg_decklist_down=Average_decklist_round_down(df_avg_cards,avgCardRatio)
print(avg_decklist_down, row.names = FALSE)
write.csv(avg_decklist_up, paste(Beginning,'-',End,'-',archNameFile,
                                 '_DF_Average_Maindeck_Down.csv',sep=''), 
          row.names = FALSE)

#AVERAGE MD ROUNDING UP THE RATIOS
avg_decklist_up=Average_decklist_round_up(df_avg_cards,avgCardRatio)
avg_decklist_up=rbind(avg_decklist_up,setNames(data.frame(
  "Total",sum(avg_decklist_up$CardCount)),c("CardName","CardCount")))
write.csv(avg_decklist_up, paste(Beginning,'-',End,'-',archNameFile,
                                 '_DF_Average_Maindeck_Up.csv',sep=''), 
          row.names = FALSE)

#AVERAGE SB ROUNDING UP THE RATIOS
avg_SB=Average_SB(df_avg_cards)
avg_SB=rbind(avg_SB,setNames(data.frame(
  "Total",sum(avg_SB$CardCount)),c("CardName","CardCount")))
print(avg_SB, row.names = FALSE)
write.csv(avg_SB, paste(Beginning,'-',End,'-',archNameFile,
                        '_DF_Average_Sideboard.csv',sep=''), row.names = FALSE)

