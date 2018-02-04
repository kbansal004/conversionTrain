install.packages("RODBC")
library(RODBC)

install.packages("sqldf")
library(sqldf)

install.packages("reshape")
library(lubridate)
library(stringr)
library(plyr)
library(reshape)

install.packages("dplyr")
library(dplyr)

install.packages("readxl")
library(readxl)

# Getting the data
setwd("F:\\Multi_Touch_Attribution_Model")

getwd()

dcm_raw <- read_excel("DCM Data.xlsx", col_names = TRUE)
dcm_raw_clean <- dcm_raw[which(!is.na(dcm_raw$`Interaction Number`)),]

DCM_Data_Subset <- data.frame(dcm_raw_clean$`Customer ID`,dcm_raw_clean$`Interaction Channel`,dcm_raw_clean$`Interaction Number`
                              ,dcm_raw_clean$`Site (DCM)`,dcm_raw_clean$`Interaction Date/Time`)




#By Partner NOTE: "last touch Conversions" column must be added to table on the last right with 1 value on each row that has interaction number =1

DCM_Data_Grouping<-DCM_Data_Subset %>% group_by(dcm_raw_clean$`Customer ID`,
                                                dcm_raw_clean$`Site (DCM)`)

DCM_Data_Summary<-DCM_Data_Grouping %>% summarise(dcm_raw_clean..Interaction.Number.=n())

colnames(DCM_Data_Summary)[colnames(DCM_Data_Summary) == 'dcm_raw_clean$`Customer ID`']<-'CustomerID'
colnames(DCM_Data_Summary)[colnames(DCM_Data_Summary) == 'dcm_raw_clean$`Site (DCM)`']<-'SiteDCM'

DCM_Summary<- cast(DCM_Data_Summary,CustomerID ~ SiteDCM) # Get the summary by partners(How many times each partner was visited)

CustomerID_List <- data.frame(dcm_raw_clean$`Customer ID`)

indx <- sapply(CustomerID_List, is.factor)
CustomerID_List[indx] <- lapply(CustomerID_List[indx], function(x) as.character(x))

CustomerID_List<-unique(CustomerID_List,incomparables = FALSE) 

colnames(CustomerID_List)[colnames(CustomerID_List) == 'dcm_raw_clean..Customer.ID.']<-'CustomerID'

j<-1

ArrayRowNum<-nrow(CustomerID_List)

DCM_SequenceArray<-array("",dim=c(ArrayRowNum,4,1))

colnames(DCM_SequenceArray)<-c("CustomerID","Path","Last_Touch","First_Touch")

for(i in 1:nrow(CustomerID_List))
{
  Data_ByCustomerID <- subset(DCM_Data_Subset,
                                DCM_Data_Subset$dcm_raw_clean..Customer.ID.==CustomerID_List$CustomerID[1]
                                ,SELECT=DCM_Data_Subset(DCM_Data_Subset$dcm_raw_clean..Customer.ID.))
  
  indx <- sapply(Data_ByCustomerID, is.factor)
  Data_ByCustomerID[indx] <- lapply(Data_ByCustomerID[indx], function(x) as.character(x))
  
  Data_ByCustomerID <- Data_ByCustomerID[order(-Data_ByCustomerID$dcm_raw_clean..Interaction.Number.),]
  
  DCM_SequenceArray[j,1,1]<-CustomerID_List$CustomerID[i]
  DCM_SequenceArray[j,2,1]<-paste(gettext(Data_ByCustomerID$dcm_raw_clean..Site..DCM..),collapse = "-->")
  DCM_SequenceArray[j,3,1]<-Data_ByCustomerID$dcm_raw_clean..Site..DCM..[which(Data_ByCustomerID$dcm_raw_clean..Interaction.Number. == 1)]
  DCM_SequenceArray[j,4,1]<-Data_ByCustomerID$dcm_raw_clean..Site..DCM..[1]
  
  j<-j+1
  i<-i+1
}

DCM_SequenceArray<-data.frame(DCM_SequenceArray)

colnames(DCM_SequenceArray)[colnames(DCM_SequenceArray) == 'CustomerID.1']<-'CustomerID'

DCM_ConversionPath_Table<-merge(DCM_Summary,DCM_SequenceArray,by = "CustomerID") # Get the partner path by ConversionID




# Get the conversion rate of each path combination used

j<-1
i<-1

ArrayRowNum<-nrow(CustomerID_List)

DCM_SequenceArray<-array("",dim=c(ArrayRowNum,2,1))

colnames(DCM_SequenceArray)<-c("CustomerID","Path")

for(i in 1:nrow(CustomerID_List))
{
  
  Data_ByCustomerID <- subset(DCM_Data_Subset,
                              DCM_Data_Subset$dcm_raw_clean..Customer.ID.==CustomerID_List$CustomerID[1]
                              ,SELECT=DCM_Data_Subset(DCM_Data_Subset$dcm_raw_clean..Customer.ID.))
  
  indx <- sapply(Data_ByCustomerID, is.factor)
  Data_ByCustomerID[indx] <- lapply(Data_ByCustomerID[indx], function(x) as.character(x))
  
  Data_ByCustomerID <- Data_ByCustomerID[order(-Data_ByCustomerID$dcm_raw_clean..Interaction.Number.),]
  
  RowNumForCustomerID<-nrow(Data_ByCustomerID)
  
  k<-1
  
  count<-1
  
  PartialPathIndex<-1
  
  remove(PartialPath)
  
  PartialPath <- "Sequence"
  
  PartialPath[PartialPathIndex] <- Data_ByCustomerID$dcm_raw_clean..Site..DCM..[k]
  
  #View(PartialPath)
  
  while(count<=RowNumForCustomerID)
  {
    if(is.na(Data_ByCustomerID$dcm_raw_clean..Site..DCM..[k+1])==FALSE)
    {
      
      SecondElement<-Data_ByCustomerID$dcm_raw_clean..Site..DCM..[k+1]
      
    }else{
      
      SecondElement<-Data_ByCustomerID$dcm_raw_clean..Site..DCM..[k]
    }
    
    
    if(PartialPath[PartialPathIndex]!=SecondElement)
    {
      PartialPathIndex<-PartialPathIndex+1
      PartialPath[PartialPathIndex] <- SecondElement
      
    }
    
    count<-count+1
    k<-k+1
  }
  
  PartialPath <- data.frame(PartialPath)
  
  DCM_SequenceArray[j,1,1] <- CustomerID_List$CustomerID[i]
  DCM_SequenceArray[j,2,1] <- paste(gettext(PartialPath$PartialPath),collapse = "-->")
  
  j<-j+1
  i<-i+1
  
}

DCM_SequenceArray<-data.frame(DCM_SequenceArray)

colnames(DCM_SequenceArray)[colnames(DCM_SequenceArray) == 'CustomerID.1']<-'CustomerID'

Path_Grouping <- DCM_SequenceArray %>% group_by(Path.1)

Path_List <- Path_Grouping %>% summarise(CustomerID=n())

colnames(Path_List)[colnames(Path_List) == 'CustomerID']<-'PathCount'

Conversion_Rate <- data.frame(Path_List$Path.1,Path_List$PathCount,
                              format(round(Path_List$PathCount/nrow(CustomerID_List)*100,2),nsmall = 2))

colnames(Conversion_Rate)[colnames(Conversion_Rate) == 'format.round.Path_List.PathCount.nrow.CustomerID_List....100..']<-'ConversionRate'





# Get the hours between each touch and total time before the conversion

j<-1
i<-1

DCM_HoursArray<-array("",dim=c(ArrayRowNum,4,1))

colnames(DCM_HoursArray)<-c("CustomerID","Path","HoursByPath","TotalHours")

for(i in 1:nrow(CustomerID_List))
{
  Data_ByCustomerID <- subset(DCM_Data_Subset,
                              DCM_Data_Subset$dcm_raw_clean..Customer.ID.==CustomerID_List$CustomerID[1]
                              ,SELECT=DCM_Data_Subset(DCM_Data_Subset$dcm_raw_clean..Customer.ID.))
  
  indx <- sapply(Data_ByCustomerID, is.factor)
  Data_ByCustomerID[indx] <- lapply(Data_ByCustomerID[indx], function(x) as.character(x))
  
  Data_ByCustomerID <- Data_ByCustomerID[order(-Data_ByCustomerID$dcm_raw_clean..Interaction.Number.),]
  
  RowNumForCustomerID<-nrow(Data_ByCustomerID)
  
  k<-1
  
  HoursArray<-array("",dim=c(RowNumForCustomerID,1,1))
  
  colnames(HoursArray)<-c("HoursBySequence")
  
  for(k in 1:nrow(Data_ByCustomerID))
  {
    date1<-strptime(Data_ByCustomerID$dcm_raw_clean..Interaction.Date.Time.[k], format = "%m/%d/%Y %H:%M")
    
    #date1<-format(as.POSIXct(date1, format = "%y%m%d %H:%M"), "%m/%d/%Y %H:%M")
    
    date2<-strptime(Data_ByCustomerID$dcm_raw_clean..Interaction.Date.Time.[k-1], format = "%m/%d/%Y %H:%M")
    
    #date2<-format(as.POSIXct(date2, format = "%y%m%d %H:%M"), "%m/%d/%Y %H:%M")
    
    #if(length(date2)!=0)
    #{
      DiffOfHrs<-as.numeric(difftime(date2,date1,units = "hours"))
      
    #}else{
      
      #DiffOfHrs<-0
    #}
    
    if(length(DiffOfHrs)!=0)
    {
      HoursArray[k,1,1]<-format(round(DiffOfHrs,2),nsmall = 2)
    }else{
      
      HoursArray[k,1,1]<-0
    }
    
    k<-k+1
  }
  
  HoursArrayNumList<-as.numeric(unlist(HoursArray))
  HoursArray<-data.frame(HoursArray)
  
  DCM_HoursArray[j,1,1]<-CustomerID_List$CustomerID[i]
  DCM_HoursArray[j,2,1]<-paste(gettext(Data_ByCustomerID$dcm_raw_clean..Site..DCM..),collapse = "-->")
  DCM_HoursArray[j,3,1]<-paste(gettext(HoursArray$HoursBySequence.1),collapse = "-->")
  DCM_HoursArray[j,4,1]<-sum(HoursArrayNumList)
  
  j<-j+1
  i<-i+1
}

DCM_HoursArray<-data.frame(DCM_HoursArray)


#DCM_ConversionPath_Table$CustomerID <- paste("'",DCM_ConversionPath_Table$CustomerID,sep = "")
#DCM_HoursArray$CustomerID.1 <- paste("'",DCM_HoursArray$CustomerID.1,sep = "")


# Writing to the working directory

write.csv(DCM_ConversionPath_Table,"DCM_Partners_OutputPath.csv",row.names = FALSE)

#Note:Single partner conversion rate includes combinations where only one partner lead to conversion no matter how many times how many times it was touched
write.csv(Conversion_Rate,"DCM_Partners_ConversionRate.csv",row.names = FALSE)

write.csv(DCM_HoursArray,"DCM_Partners_OutputHours.csv",row.names = FALSE)
