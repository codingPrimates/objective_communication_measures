

#Load Packages

library(lubridate)
library(dplyr)
library(stringr)
library(data.table)
library(sqldf)
library(igraph)
library(reshape2)


#Read in Zoom transcript file (replace x with file path of Zoom transcript)

file <- read.delim(x, stringsAsFactors = F) 

#Create a dataframe with all relevant information

trans_info <- data.frame(time = file[seq(2, (nrow(file) - 1), by = 3),], transcript = file[seq(3, nrow(file), by = 3),]) %>%
  mutate(start = as.numeric(seconds(hms(substr(time, 1, 12)))), end = as.numeric(seconds(hms(substr(time, 18, 29))))) %>%
  mutate(name = sapply(strsplit(as.character(transcript), ":"), "[", 1)) %>%
  mutate(name = ifelse(transcript %in% .[grep(":", .$transcript), ]$transcript, name, "unknown")) %>% #if the ":" symbol (which separates speaker names from transcript text) is not present in a given transcript column, mark the speaker name as "unknown" 
  dplyr::select(start, end, name, transcript)


##Check Speaker Names

#Look at all speaker names in transcript. Use information to manually adjust name inconsistencies (e.g. if a given speaker logs in using a different user name than usual) or determine whether this meeting should be included in analysis (e.g. if there are unexpected speakers in the meeting)

table(trans_info$name)


##Combine sequential speech by the same speaker

#Combine speaking turns with less than criterion value between them

trans_info_gaps <- arrange(trans_info, start) %>%
  mutate(prev_speaker = lag(name)) %>%
  mutate(prev_turn_end = lag(end)) %>%
  mutate(silence_gap = start - prev_turn_end) %>%
  mutate(turn_id = 0)

#Add speaking turn IDs based on the criteria described below

r <- 1

while (r <= nrow(trans_info_gaps)) {   
  
  prevLine <- trans_info_gaps[r-1,]
  oneLine <- trans_info_gaps[r,]
  nexLine <- trans_info_gaps[r+1,]
  
  if(r == 1) {
    
    trans_info_gaps[r,"turn_id"] <- 1  
    
  }
  
  
  else{
    
    
    #If the current speaker is the same as the previous speaker and the amount of time between the end of the previous speech segment and beginning of the current speech segment is less than 5.66, input the same value in the "turn_id" column as the previous row. If not, enter the previous value + 1
    
    if(oneLine[,"name"] == prevLine[,"name"] && oneLine[,"silence_gap"] < 5.66) {
      
      
      trans_info_gaps[r,"turn_id"] <- prevLine[,"turn_id"]
      
      
    }
    
    
    else {
      
      trans_info_gaps[r,"turn_id"] <- prevLine[,"turn_id"] + 1
      
    }
    
  }
  
  
  r <- r + 1   #this causes function to loop sequentially through each row of the dataframe
  
}

head(trans_info_gaps)

#Compute new start and end times for all turns that have the same turn ID

trans_info_combined <- dplyr::group_by(trans_info_gaps, name, turn_id) %>%
  dplyr::summarize(new_start = min(start), new_end = max(end)) %>%  #new start time is earliest start time, new end time is latest end time
  ungroup() %>%
  dplyr::select(new_start, new_end, name) %>%
  arrange(new_start)

head(trans_info_combined)

##Find Meeting Duration

meeting_dur <- dplyr::summarize(trans_info_combined, min = min(new_start), max = max(new_end)) %>%
  dplyr::mutate(meeting_duration = max - min) 

meeting_dur


##Calculate Individual-level Measures

#Calculate Speaking Turn Duration and Silence Gap Duration

trans_info_combined2 <- mutate(trans_info_combined, duration = new_end - new_start) %>%
  dplyr::arrange(new_start) %>%
  dplyr::mutate(prev_speaker = lag(name)) %>%
  dplyr::mutate(prev_turn_end = lag(new_end)) %>%
  dplyr::mutate(silence_gap = new_start - prev_turn_end) %>%
  ungroup() %>%
  dplyr::select(new_start, new_end, name, duration, prev_speaker, silence_gap)

trans_info_combined2

#Calculate individual-level measures for a given meeting (except degree centrality)

indi_measures0 <- group_by(trans_info_combined2, name) %>%
  summarize(speaking_time = sum(duration) / meeting_dur$meeting_duration, turn_count = n() / meeting_dur$meeting_duration, mean_turn_duration = mean(duration), mean_silence_gap = mean(silence_gap, na.rm = TRUE))

indi_measures0   


#Calculate degree centrality


#Calculate silence gaps between combined turns

trans_info_combined_gaps <- dplyr::arrange(trans_info_combined, new_start) %>%
  dplyr::mutate(prev_speaker = lag(name)) %>%
  dplyr::mutate(prev_turn_end = lag(new_end)) %>%
  dplyr::mutate(silence_gap = new_start - prev_turn_end) %>%
  dplyr::select(name, new_start, new_end, prev_speaker, silence_gap)


#Find all turn sequences with less than 5.66 seconds between

turn_sequences <- mutate(trans_info_combined_gaps, seq_ID = 0)


r <- 1

while (r <= nrow(turn_sequences)) {
  
  prevLine <- turn_sequences[r-1,]
  oneLine <- turn_sequences[r,]
  nexLine <- turn_sequences[r+1,]
  
  if(r == 1) {
    
    turn_sequences[r,"seq_ID"] <- 1    #mark first k-turn in dataset as 1
    
  }
  
  
  else {
    
    
    #If the previous speaker on the previous turn is NA and the previous turn end is not NA, this means it is only the second turn of the meeting and so this turn should have the same seqID as the previous row (as long as less than 5.66 seconds has elapsed and the first and second speaker are not the same)
    
    if(is.na(prevLine[,"prev_speaker"]) == TRUE && is.na(prevLine[,"new_end"]) == FALSE && oneLine[,"silence_gap"] < 5.66) {
      
      
      turn_sequences[r,"seq_ID"] <- prevLine[,"seq_ID"]
      
      
    }
    
    
    else {
      
      
      #if the silence gap with previous turn is less than 5.66 seconds and both the current speaker and the previous speaker are not "unknown", add the same sequence ID as the previous row
      
      
      if((oneLine[,"silence_gap"] < 5.66 & oneLine[,"name"] != "unknown" & prevLine[,"name"] != "unknown"))
        
      {
        
        turn_sequences[r,"seq_ID"] <- prevLine[,"seq_ID"]
        
      }
      
      else {
        
        turn_sequences[r,"seq_ID"] <- prevLine[,"seq_ID"] + 1
        
      }
      
    }
    
    
  }
  
  r <- r + 1
  
}

turn_sequences


#Count how many time a given individual speaks after another given individual

turn_sequence_count <- group_by(turn_sequences, seq_ID) %>%
  mutate(prev_speaker = lag(name)) %>%
  ungroup() %>%
  group_by(name, prev_speaker) %>%
  summarize(In = n()) %>%
  filter(!is.na(prev_speaker)) %>%
  dplyr::rename(ID1 = name, ID2 = prev_speaker)


#Generate all possible dyads within the team

a <- data.frame(ID1 = unique(trans_info$name)) %>% filter(ID1 != "unknown")
b <- data.frame(ID2 = unique(trans_info$name)) %>% filter(ID2 != "unknown")

all_dyads <- merge(a, b) %>%
  filter(ID1 != ID2)

#Merge turn sequence count with all possible dyads in case some did not interact

turn_sequence_count2 <- merge(all_dyads, turn_sequence_count, by = c("ID1", "ID2"), all.x = TRUE) %>%
  mutate(In = ifelse(is.na(In), 0, In))

#Convert turn sequence counts to matrix

turn_sequence_matrix <- acast(turn_sequence_count2, ID1 ~ ID2, value.var = "In")

turn_sequence_matrix[is.na(turn_sequence_matrix)] <- 0

#Calculate directed social network from matrix

turn_sequence_graph <- graph_from_adjacency_matrix(turn_sequence_matrix, mode = "directed", weighted = TRUE)

#plot the network

plot(turn_sequence_graph, edge.width = E(turn_sequence_graph)$weight, vertex.label.dist = 5, vertex.label.cex = 1.5, layout = layout_with_gem, edge.curved=0.3)

#calculate degree centrality

degree_cent <- setDT(data.frame(degree_centrality = degree(
  turn_sequence_graph,
  v = V(turn_sequence_graph),
  mode = c("in"),
  loops = FALSE,
  normalized = FALSE
)), keep.rownames = TRUE)[] %>%
  dplyr::rename(name = rn)

degree_cent

#Count number of unique team members

head(trans_info)

team_size <- length(unique(filter(trans_info, name != "unknown")$name))

degree_cent_prop <- mutate(degree_cent, degree_centrality = degree_centrality / team_size)
degree_cent_prop

indi_measures <- merge(indi_measures0, degree_cent_prop, by = c("name"), all.x = TRUE)

indi_measures   #this represents final individual-level results


## Dyad-level measures

#Find k-turn sequences


#Give each speaking turn a k-turn id based on the criteria below

trans_info_combined3 <- mutate(trans_info_combined2, seq_id = 0)

colnames(trans_info_combined3)

r <- 1

while (r <= nrow(trans_info_combined3)) {   
  
  prevprevLine <- trans_info_combined3[r-2,]
  prevLine <- trans_info_combined3[r-1,]
  oneLine <- trans_info_combined3[r,]
  nexLine <- trans_info_combined3[r+1,]
  
  if(r == 1) {
    
    trans_info_combined3[r,"seq_id"] <- 1    #mark first k-turn in dataset as 1
    
  }
  
  
  else {
    
    
    #If it is the second turn of the meeting, this turn should have the same seqID as the previous row as long as the silence gap between the first and second turns is less than 5.66 seconds and the first and second speaker are not the same)
    
    if(r == 2 && oneLine[,"silence_gap"] < 5.66 && oneLine[,"name"] != prevLine[,"name"]) {
      
      
      trans_info_combined3[r,"seq_id"] <- prevLine[,"seq_id"]   
      
      
    }
    
    
    else {
      
      #if it is the second turn of the meeting and the above does not apply
      
      if(r == 2) {
        
        
        trans_info_combined3[r,"seq_id"] <- prevLine[,"seq_id"] + 1
        
        
      } 
      
      
      else{
        
        
        
        #if silence gap between current turn and previous turn is < 5.66 and the current speaker is the same as the speaker two turns prior and the current speaker is not the same as the previous speaker and the current speaker is not unknown and the previous speaker is not unknoqn, then input the seqID of the previous line 
        #OR 
        #if the silence gap is less than 5.66 and a new kturn sequence was initiated on the previous turn and the current speaker is not the same as the previous speaker and the current speaker and previous speaker aren't unknown be unknown . If not, add 1 to previous seqID
        
        
        if((oneLine[,"silence_gap"] < 5.66 && oneLine[,"name"] == prevprevLine[,"name"] && oneLine[,"name"] != prevLine[,"name"] && oneLine[,"name"] != "unknown" && prevLine[,"name"] != "unknown") || (oneLine[,"silence_gap"] < 5.66 && prevLine[,"seq_id"] - prevprevLine[,"seq_id"] == 1 && oneLine[,"name"] != prevLine[,"name"] && oneLine[,"name"] != "unknown" && prevLine[,"name"] != "unknown"))
          
        {
          
          trans_info_combined3[r,"seq_id"] <- prevLine[,"seq_id"]
          
        }
        
        #otherwise, add 1 to the previous sequence ID
        
        else {
          
          trans_info_combined3[r,"seq_id"] <- prevLine[,"seq_id"] + 1    
          
        }
        
      }
      
      
    }
    
    
    
    
  }
  
  r <- r + 1    #this causes function to loop sequentially through each row of the dataframe
  
}


trans_info_combined3


##Find each unique dyad that could have interacted within k-turn sequences


#find only one copy of each combination of IDs and provide a dyad ID

all_dyads_unique <- all_dyads[!duplicated(t(apply(all_dyads, 1, sort))),] %>%
  mutate(dyad_id = c(1:n()))

all_dyads_unique


##Find the two team members who interacted in each k-turn sequence of length 2 or greater and add in dyad IDs 


length_2 <- group_by(trans_info_combined3, name, seq_id) %>%
  summarize(n = n()) %>%
  group_by(seq_id) %>%
  summarize(n = n()) %>%
  filter(n == 2)

dyads <- group_by(trans_info_combined3, name, seq_id) %>%
  summarize(n = n()) %>%
  filter(seq_id %in% length_2$seq_id) %>%
  arrange(seq_id) %>%
  group_by(seq_id) %>%
  mutate(ord = c(1:2)) %>%
  dplyr::select(seq_id, name, ord)

dyads2 <- merge(filter(dyads, ord == 1), filter(dyads, ord == 2), by = c("seq_id")) %>%
  dplyr::select(seq_id, name.x, name.y) %>%
  dplyr::rename(name1 = name.x, name2 = name.y)

dyads2


#find only one copy of each combination of IDs and provide a dyad ID

all_dyads_unique <- all_dyads[!duplicated(t(apply(all_dyads, 1, sort))),] %>%
  mutate(dyad_id = c(1:n())) %>%
  dplyr::rename(name1 = ID1, name2 = ID2)

all_dyads_unique

seq_n_dyad_ids <- sqldf("SELECT dyads2.*, all_dyads_unique.dyad_id FROM dyads2 JOIN all_dyads_unique WHERE (all_dyads_unique.name1 == dyads2.name1 AND all_dyads_unique.name2 == dyads2.name2) OR (all_dyads_unique.name1 == dyads2.name2 AND all_dyads_unique.name2 == dyads2.name1)") %>%
  dplyr::select(seq_id, dyad_id)

trans_info_combined4 <- merge(trans_info_combined3, seq_n_dyad_ids, by = c("seq_id")) %>%
  arrange(new_start)


##Calculate Dyad-level Variables


kturn_info <- group_by(trans_info_combined4, dyad_id, seq_id) %>%
  summarize(kturn_start = min(new_start), kturn_end = max(new_end)) %>%
  ungroup() %>%
  mutate(kturndur = kturn_end - kturn_start) %>%
  arrange(kturn_start) %>%
  group_by(dyad_id) %>%
  summarize(kturn_duration = sum(kturndur) / meeting_dur$meeting_duration, kturn_count = n() / meeting_dur$meeting_duration)

kturn_info

kturn_length <- group_by(trans_info_combined4, dyad_id, seq_id) %>%
  summarize(length = n()) %>%
  ungroup() %>%
  group_by(dyad_id) %>%
  summarize(mean_length = mean(length)) 

kturn_info2 <- merge(kturn_info, kturn_length, by = c("dyad_id"))

kturn_info2

#Associate back to team member names and add zeros if some team members did not interact 

dyad_measures <- merge(all_dyads_unique, kturn_info2, by = c("dyad_id"), all.x = TRUE) %>%
  mutate(kturn_duration = ifelse(is.na(kturn_duration), 0, kturn_duration)) %>%
  mutate(kturn_count = ifelse(is.na(kturn_count), 0, kturn_count)) %>%
  mutate(mean_length = ifelse(is.na(mean_length), 0, mean_length))

dyad_measures   #this represents final dyad-level results



