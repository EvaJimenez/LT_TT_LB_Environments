########################################################################################
############ 1. TRANSCRIPTS PROCESSING
############ AUTHOR: EVA JIMENEZ ## 
########################################################################################
## 1 ## Load raw data ####

# Create a super-list with all the transcript files 

#setwd("--working directory--")

Transcriptions_info <-  read.csv(file = "Transcriptions_info.csv", header = T)
head(Transcriptions_info)

#setwd("--working directory with transcripts--")

Trans_super_df <- data.frame(Audio_file_name=NA,part_code=NA, topic=NA,recording_number=NA,day_of_recording=NA, Speaker=NA,Utterance=NA)

i=1
for (i in 1:nrow(Transcriptions_info) ) {
  
  trans_file_i <- read.csv(Transcriptions_info$file_name_csv[i])
  trans_file_i$part_code <- Transcriptions_info$part_code[i]
  trans_file_i$recording_number <- Transcriptions_info$recording_number[i]
  trans_file_i$day_of_recording <- Transcriptions_info$day_of_recording[i]
  trans_file_i$topic <- Transcriptions_info$topic[i]
  Trans_super_df <- rbind(Trans_super_df,trans_file_i)
  
}

head(Trans_super_df)
Trans_super_df<- Trans_super_df[-1,] ## This row is empty

## 2 ## Basic data cleaning ########

# Eliminate NA rows

Trans_super_df <- Trans_super_df[!is.na(Trans_super_df$Speaker),]
length(unique(Trans_super_df$Audio_file_name)) ### This number has to match the number of files processed


setwd("--project's working directory--")

# Check Speakers column
table(Trans_super_df$Speaker)

# Delete 'Break_in_conversation','Comment','Long Pause', noises

Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="Break_in_conversation"),]
Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="Break_in_Conversation"),]
Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="Break in Conversation"),]
Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="Break_in_Convesrsation"),]
Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="Break_In_Conversation"),]
Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="Comment"),]
Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="comment"),]
Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="Long Pause"),]
Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="Dog"),]
Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="[]"),]
Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="()"),]
Trans_super_df <- Trans_super_df[-which(Trans_super_df$Speaker=="___"),]


table(Trans_super_df$Speaker)

# some speaker labels belong to same category ## fix these 

for (i in 1:nrow(Trans_super_df)) {
  if(Trans_super_df$Speaker[i]=="Child"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="female"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Female"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Make"){
    Trans_super_df$Speaker[i] <- "Male_1"
  }
  if(Trans_super_df$Speaker[i]=="Male"){
    Trans_super_df$Speaker[i] <- "Male_1"
  }
  if(Trans_super_df$Speaker[i]=="Female _2"){
    Trans_super_df$Speaker[i] <- "Female_2"
  }
  if(Trans_super_df$Speaker[i]=="Male\r\n"){
    Trans_super_df$Speaker[i] <- "Male_1"
  }
  if(Trans_super_df$Speaker[i]=="Child_"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Child_1\r\n"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Child_2\r\n"){
    Trans_super_df$Speaker[i] <- "Child_2"
  }
  if(Trans_super_df$Speaker[i]=="Female\r\n"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  
  if(Trans_super_df$Speaker[i]=="Man"){
    Trans_super_df$Speaker[i] <- "Male_1"
  }
  if(Trans_super_df$Speaker[i]=="Mother:"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Father:"){
    Trans_super_df$Speaker[i] <- "Male_1"
  }
  if(Trans_super_df$Speaker[i]=="Female 1:"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Female"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Baby:"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Child 1:"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Child 2:"){
    Trans_super_df$Speaker[i] <- "Child_2"
  }
  if(Trans_super_df$Speaker[i]=="Child:"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  
  if(Trans_super_df$Speaker[i]=="Male:"){
    Trans_super_df$Speaker[i] <- "Male_1"
  }
  if(Trans_super_df$Speaker[i]=="Male 1:"){
    Trans_super_df$Speaker[i] <- "Male_1"
  }
  if(Trans_super_df$Speaker[i]=="Female:"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Chlid"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="child_2"){
    Trans_super_df$Speaker[i] <- "Child_2"
  }
  if(Trans_super_df$Speaker[i]=="child_2"){
    Trans_super_df$Speaker[i] <- "Child_2"
  }
  if(Trans_super_df$Speaker[i]=="Chiild"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Chlild_1"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Male & Child"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="child"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="child_1"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Parent:"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Adult:"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Baby"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Chid"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="CHILD"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Child _1"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Child _2"){
    Trans_super_df$Speaker[i] <- "Child_2"
  }
  if(Trans_super_df$Speaker[i]=="Child _3"){
    Trans_super_df$Speaker[i] <- "Child_3"
  }
  if(Trans_super_df$Speaker[i]=="Child 1"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Child 2"){
    Trans_super_df$Speaker[i] <- "Child_2"
  }
  if(Trans_super_df$Speaker[i]=="Child and Female"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Child."){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Child2"){
    Trans_super_df$Speaker[i] <- "Child_2"
  }
  if(Trans_super_df$Speaker[i]=="Chile"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Daddy"){
    Trans_super_df$Speaker[i] <- "Male_1"
  }
  if(Trans_super_df$Speaker[i]=="Child and Female"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="FEmale"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Female / Male"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Female 1"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Female 1 and Female 2"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Female 2"){
    Trans_super_df$Speaker[i] <- "Female_2"
  }
  if(Trans_super_df$Speaker[i]=="Female."){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Female/Male"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Female2"){
    Trans_super_df$Speaker[i] <- "Female_2"
  }
  if(Trans_super_df$Speaker[i]=="Femate"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="MALE"){
    Trans_super_df$Speaker[i] <- "Male_1"
  }
  if(Trans_super_df$Speaker[i]=="Male and Female"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Male/Child"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Male/Female"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Male2"){
    Trans_super_df$Speaker[i] <- "Male_2"
  }
  if(Trans_super_df$Speaker[i]=="Males"){
    Trans_super_df$Speaker[i] <- "Male_2"
  }
  if(Trans_super_df$Speaker[i]=="Child_1\n"){
    Trans_super_df$Speaker[i] <- "Child_1"
  }
  if(Trans_super_df$Speaker[i]=="Child_2\n"){
    Trans_super_df$Speaker[i] <- "Child_2"
  }
  if(Trans_super_df$Speaker[i]=="Female\n"){
    Trans_super_df$Speaker[i] <- "Female_1"
  }
  if(Trans_super_df$Speaker[i]=="Male\n"){
    Trans_super_df$Speaker[i] <- "Male_1"
  }
  
}

table(Trans_super_df$Speaker)
head(Trans_super_df)

# Save original
write.csv(Trans_super_df, file = "Trans_super_df_original.csv")

# Clean various things 
library(stringr)
library(textstem)
library(qdap)
library(qdapTools)


# Remove comments, interpretation of utterances, numbers and extra spaces
nrow(Trans_super_df)
for (i in 1:nrow(Trans_super_df)) {
  print(i)
  Trans_super_df$Utterance[i] <- gsub("\\s*\\([^\\)]+\\)","",as.character(Trans_super_df$Utterance[i]))
  Trans_super_df$Utterance[i] <- gsub("\\[","",as.character(Trans_super_df$Utterance[i]))
  Trans_super_df$Utterance[i] <- gsub("\\]","",as.character(Trans_super_df$Utterance[i]))
  Trans_super_df$Utterance[i] <- gsub('([0-9])',"",as.character(Trans_super_df$Utterance[i]))
  Trans_super_df$Utterance[i] <- gsub("\\___","",as.character(Trans_super_df$Utterance[i]))
  Trans_super_df$Utterance[i] <- gsub("\\__","",as.character(Trans_super_df$Utterance[i]))
  Trans_super_df$Utterance[i] <- noquote(as.character(Trans_super_df$Utterance[i]))
  Trans_super_df$Utterance[i] <- gsub("   "," ",as.character(Trans_super_df$Utterance[i]))
  Trans_super_df$Utterance[i] <- gsub("  "," ",as.character(Trans_super_df$Utterance[i]))
}

# Save original without comments
write.csv(Trans_super_df, file = "Trans_super_df_original_noComments.csv")

## 3 ## Create dfs with child-directed speech only ####
table(Trans_super_df$Speaker)
Trans_super_df_CDS <- Trans_super_df
Trans_super_df_CDS <- Trans_super_df_CDS[!Trans_super_df_CDS$Speaker=="Child_1",]
Trans_super_df_CDS <- Trans_super_df_CDS[!Trans_super_df_CDS$Speaker=="Child_2",]
Trans_super_df_CDS <- Trans_super_df_CDS[!Trans_super_df_CDS$Speaker=="Child_3",]

## 4 ## Delete punctuation, symbols and fix common contractions ####

nrow(Trans_super_df_CDS)  

for (i in 1:nrow(Trans_super_df_CDS)) {
  print(i)
  Trans_super_df_CDS$Utterance[i] <- tolower(Trans_super_df_CDS$Utterance[i])
  Trans_super_df_CDS$Utterance[i] <- gsub("\\.","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\,","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\?","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("-","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\;","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\’","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\”","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\''","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\:","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\...","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- str_trim(Trans_super_df_CDS$Utterance[i])
  Trans_super_df_CDS$Utterance[i] <- gsub("\\!","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\£","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("inaudible","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub('\"',"",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub('"',"",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub('\\"',"",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\…","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("'ve"," have",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("let's","let us",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("where's","where is",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("'s"," is",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("'re"," are",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("'d"," had",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\'","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\‘","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\“","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("_","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("—“","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\(","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("-","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("–","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("%","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("—","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("\\)","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("|excus","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("—","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub("—","",as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- gsub('list|[[:punct:]]', "", as.character(Trans_super_df_CDS$Utterance[i]))
  Trans_super_df_CDS$Utterance[i] <- str_trim(Trans_super_df_CDS$Utterance[i])
}

Trans_super_df_CDS <-   Trans_super_df_CDS[-which(Trans_super_df_CDS$Utterance==""),]
nrow(Trans_super_df_CDS) 

## 5 ## Eliminate misspellings in utterances #####

## Keep record of  words' roots before lemmatizing and stemming 
sentences_All <- str_c(Trans_super_df_CDS$Utterance, sep = " ")
sentences_All <- unlist(strsplit(as.character(sentences_All), " "))
sentences_All <- table(sentences_All)
sentences_All <- as.data.frame(sentences_All)
colnames(sentences_All)[1] <- "non_stem"
sentences_All$non_stem <- as.character(sentences_All$non_stem)
sentences_All$lemma <- lemmatize_words(as.character(sentences_All$non_stem))
sentences_All$lemma[is.na(sentences_All$lemma)] <- sentences_All$non_stem[is.na(sentences_All$lemma)]
sentences_All <-sentences_All[-which(sentences_All$lemma==""),]
numbers_list <- 0:10

#Keep ordinals
sentences_All$lemma[!is.na(match(sentences_All$lemma,numbers_list))] <-as.character(sentences_All$non_stem[!is.na(match(sentences_All$lemma,numbers_list))])

#Eliminate single letters
sentences_All <- sentences_All[nchar(as.character(sentences_All$non_stem))>1,]

#Dry changed to spin-dry, change this to dry
sentences_All$lemma[which(sentences_All$lemma=="spin-dry")] <- "dry"
sentences_All$lemma_stem <- unlist(strsplit(stem_strings( str_c(as.character(sentences_All$lemma),  collapse = " " )), " "))
head(sentences_All)

# Eliminate low frequency words and misspelled words
eliminate_words <- sentences_All$non_stem[sentences_All$Freq==1]
eliminate_words <- unique(eliminate_words)
length(eliminate_words)
a <- as.character(which_misspelled(str_c(as.character(sentences_All$non_stem),collapse = " " )))# check misspellings
eliminate_words <- c(eliminate_words,a)
eliminate_words <- unique(eliminate_words)

for (i in 1:length(Trans_super_df_CDS$Utterance)) {
  print(i)
  if(length(intersect(c(unlist(strsplit(Trans_super_df_CDS$Utterance[i], " "))),c(eliminate_words)))>0){
    words_eliminate <- intersect(c(unlist(strsplit(Trans_super_df_CDS$Utterance[i], " "))),c(eliminate_words))
    for (x in 1:length(words_eliminate)) {
      Trans_super_df_CDS$Utterance[i] <- gsub(words_eliminate[x],"",as.character(Trans_super_df_CDS$Utterance[i]))
    }
  }
}


## 7 ##  Each document is a file #####

Trans_super_df_CDS_FILE <- as.data.frame(matrix(nrow = length(unique(Trans_super_df_CDS$Audio_file_name)),ncol=ncol(Trans_super_df_CDS)))
colnames(Trans_super_df_CDS_FILE) <- colnames(Trans_super_df_CDS)
colnames(Trans_super_df_CDS_FILE)[2] <-"participant_id"
Trans_super_df_CDS_FILE$Speaker <-NULL
Trans_super_df_CDS_FILE$Audio_file_name <- unique(Trans_super_df_CDS$Audio_file_name)

## Create two new columns with participant id and topic

sum(is.na(match(Transcriptions_info$file_name,Trans_super_df_CDS_FILE$Audio_file_name))) # this should be 0

i=1
for (i in 1:nrow(Trans_super_df_CDS_FILE)) {
  Trans_super_df_CDS_FILE$participant_id[i] <- Transcriptions_info$part_code[match(Trans_super_df_CDS_FILE$Audio_file_name[i],Transcriptions_info$file_name)]
  Trans_super_df_CDS_FILE$topic[i] <- Transcriptions_info$topic[match(Trans_super_df_CDS_FILE$Audio_file_name[i],Transcriptions_info$file_name)]
  Trans_super_df_CDS_FILE$recording_number[i] <- Transcriptions_info$recording_number[match(Trans_super_df_CDS_FILE$Audio_file_name[i],Transcriptions_info$file_name)]
  Trans_super_df_CDS_FILE$day_of_recording[i] <- Transcriptions_info$day_of_recording[match(Trans_super_df_CDS_FILE$Audio_file_name[i],Transcriptions_info$file_name)]
  
}

head(Trans_super_df_CDS_FILE)


for (i in 1:nrow(Trans_super_df_CDS_FILE)) {
  Trans_super_df_CDS_FILE$Utterance[i] <-  str_c(unlist( Trans_super_df_CDS$Utterance[Trans_super_df_CDS$Audio_file_name==Trans_super_df_CDS_FILE$Audio_file_name[i]]),collapse =" ")
}


## 8 ## Delete stop words ####

Trans_super_df_CDS_FILE_with_stop <- Trans_super_df_CDS_FILE

for (i in 1:nrow(Trans_super_df_CDS_FILE)) {
  if(length(str_c(unlist(rm_stopwords(Trans_super_df_CDS_FILE$Utterance[i], tm::stopwords("english"))), collapse =" "))>0){
    Trans_super_df_CDS_FILE$Utterance[i] <-str_c(unlist(rm_stopwords(Trans_super_df_CDS_FILE$Utterance[i],  tm::stopwords("english"))), collapse = " ")
  }
  if(length(str_c(unlist(rm_stopwords(Trans_super_df_CDS_FILE$Utterance[i], tm::stopwords("english"))), collapse =" "))==0){
    Trans_super_df_CDS_FILE$Utterance[i] <- ""
  }
}

write.csv(Trans_super_df_CDS_FILE_with_stop, file = "Trans_super_df_CDS_FILE_with_stop.csv")

## 9 ## Lemmatization and Stemming ####


# lemmatize 
for (i in 1:nrow(Trans_super_df_CDS_FILE)) {
  Trans_super_df_CDS_FILE$Utterance[i] <-  lemmatize_words(as.character(Trans_super_df_CDS_FILE$Utterance[i]))
}

# Stem
for (i in 1:nrow(Trans_super_df_CDS_FILE)) {
  Trans_super_df_CDS_FILE$Utterance[i] <-  str_c(stem_strings(as.character(Trans_super_df_CDS_FILE$Utterance[i])),collapse = " ")
}

## copy with stop words
Trans_super_df_CDS_FILE_with_stop_stemLem <- Trans_super_df_CDS_FILE_with_stop

# lemmatize 
for (i in 1:nrow(Trans_super_df_CDS_FILE_with_stop_stemLem)) {
  Trans_super_df_CDS_FILE_with_stop_stemLem$Utterance[i] <-  lemmatize_words(as.character(Trans_super_df_CDS_FILE_with_stop_stemLem$Utterance[i]))
}

# Stem
for (i in 1:nrow(Trans_super_df_CDS_FILE_with_stop_stemLem)) {
  Trans_super_df_CDS_FILE_with_stop_stemLem$Utterance[i] <-  str_c(stem_strings(as.character(Trans_super_df_CDS_FILE_with_stop_stemLem$Utterance[i])),collapse = " ")
}

write.csv(Trans_super_df_CDS_FILE_with_stop_stemLem, file = "Trans_super_df_CDS_FILE_with_stop_stemLem.csv")

## 10 ##  Compute word frequency ####
library(tm)

# Unigrams
modi<-VCorpus(VectorSource(Trans_super_df_CDS_FILE$Utterance))
tdm = as.matrix(TermDocumentMatrix(modi, control = list(wordLengths = c(1, Inf))))
Freq_ALL_unigrams = rowSums(tdm) ## Unigrams
#Freq_ALL_unigrams <- names(Freq_ALL_unigrams)
write.csv(Freq_ALL_unigrams, file = "Freq_ALL_unigrams.csv")

# Unigrams with stop words
modi<-VCorpus(VectorSource(Trans_super_df_CDS_FILE_with_stop_stemLem$Utterance))
tdm = as.matrix(TermDocumentMatrix(modi, control = list(wordLengths = c(1, Inf))))
Freq_ALL_unigrams_STOP = rowSums(tdm) ## Unigrams
write.csv(Freq_ALL_unigrams_STOP, file = "Freq_ALL_unigrams_STOP.csv")

# Bigrams
modi<-VCorpus(VectorSource(Trans_super_df_CDS_FILE$Utterance))
BigramTokenizer <-  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm = as.matrix(TermDocumentMatrix(modi, control = list(tokenize = BigramTokenizer)))
Freq_ALL_bigrams = rowSums(tdm) ## Bigrams
Freq_ALL_bigrams_names <- names(Freq_ALL_bigrams)
Freq_ALL_bigrams_names <-  gsub(" ","_",Freq_ALL_bigrams_names)
names(Freq_ALL_bigrams) <- Freq_ALL_bigrams_names
write.csv(Freq_ALL_bigrams, file = "Freq_ALL_bigrams.csv")

# Bigrams with stop words
modi<-VCorpus(VectorSource(Trans_super_df_CDS_FILE_with_stop_stemLem$Utterance))

BigramTokenizer <-  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm = as.matrix(TermDocumentMatrix(modi, control = list(tokenize = BigramTokenizer)))
Freq_ALL_bigrams_STOP = rowSums(tdm) ## Bigrams
Freq_ALL_bigrams_STOP_names <- names(Freq_ALL_bigrams_STOP)
Freq_ALL_bigrams_STOP_names <-  gsub(" ","_",Freq_ALL_bigrams_STOP_names)
names(Freq_ALL_bigrams_STOP) <- Freq_ALL_bigrams_STOP_names
write.csv(Freq_ALL_bigrams_STOP, file = "Freq_ALL_bigrams_STOP.csv")


# Trigrams
modi<-VCorpus(VectorSource(Trans_super_df_CDS_FILE$Utterance))

TrigramTokenizer <-  function(x)unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdm = as.matrix(TermDocumentMatrix(modi, control = list(tokenize = TrigramTokenizer)))
Freq_ALL_trigrams = rowSums(tdm) ## Unigrams
Freq_ALL_trigrams_names <- names(Freq_ALL_trigrams)
Freq_ALL_trigrams_names <-  gsub(" ","_",Freq_ALL_trigrams_names)
names(Freq_ALL_trigrams) <- Freq_ALL_trigrams_names
write.csv(Freq_ALL_trigrams, file = "Freq_ALL_trigrams.csv")


## 11 ## Save datasets ####


saveRDS(Trans_super_df_CDS, file = "Trans_super_df_CDS.rds")
saveRDS(Trans_super_df_CDS_FILE, file = "Trans_super_df_CDS_FILE.rds")

# Words identified in the environment 
environment_words <- str_c(Trans_super_df_CDS_FILE$Utterance, sep = " ")
environment_words <- unlist(strsplit(as.character(environment_words), " "))
environment_words <- table(environment_words)
write.csv(names(environment_words),file = "environment_words.csv",row.names = F)
