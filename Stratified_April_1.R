#Read the fish dataset in R
fish <- read.table("final_alignment.phylip")
library(tidyverse)
library(ape)
library(phylotools)
#Rename columns
colnames(fish) <- c("species_name","seq")
#Extract CO1 nucleotides
fishCO1 <- substr(fish$seq,2292,2973)
#Check the class of fishCO1 dataset
class(fishCO1)
#Convert to dataframe
fishCO1 <- data.frame(fishCO1)
#Check the converted dataset
class(fishCO1)
#Bind the column "species name" to the fishCO1 dataframe
fishCO1_with_speciesname <- cbind(fish$species_name,fishCO1)
#Replace "-" with nothing 
df_0 <- gsub('-','',fishCO1_with_speciesname$fishCO1)
#Convert to dataframe
df_0 <- as.data.frame(df_0)
#Bind the colum "species name" of fish dataset with new dataframe
df_1 <- cbind(fish$species_name,df_0)
#Remove rows with empty sequences
df_2 <- subset(df_1, df_1$df_0 != "")
fishsamplefull_co1_with_speciesname <- df_2
#Convert to phylip format to be used in EPA
#dat2phylip(fishsamplefull_co1_with_speciesname,outfile= "fishsamplefull_co1_with_speciesname.phy")
#Rename column names
colnames(df_2) <- c("species_name","seq")
#Keep only rows with sequences
fish_multigene <- merge(fish,df_2, by= "species_name")
#New dataframe with first two columns
fish_multigene <- fish_multigene[, 1:2]
#Rename columns
colnames(fish_multigene) <- c("species_name","seq")

#Extract the range of COI gene to be used in EPA
fishsamplefull_co1 <- substr(fish_multigene$seq,2292,2973)
#Check the class
class(fishsamplefull_co1)
#Convert to dataframe
fishsamplefull_co1 <- data.frame(fishsamplefull_co1)
#Check the class
class(fishsamplefull_co1)
#Combine columns of COI dataframe and fishsample100 dataframe to get the sepecies name
fishsamplefull_co1_with_speciesname <- cbind(fish_multigene$species_name,fishsamplefull_co1)
#Assign column names
colnames(fishsamplefull_co1_with_speciesname) <- c("species_name","seq")
#rename species name without the "_"  
renamed_columns <- gsub('_',' ',fishsamplefull_co1_with_speciesname$species_name)
#Convert to dataframe
renamed_columns <- as.data.frame(rename_columns)
#Bind the colum "species name" of fish dataset with new dataframe
new_df <- cbind(renamed_columns,fishsamplefull_co1_with_speciesname$seq)
colnames(new_df) <- c("species_name","seq")
#Convert to phylip format
#dat2phylip(fishsamplefull_co1_with_speciesname,outfile= "fishsamplefull_co1_with_speciesname.phy")

family_details <- read.csv("PFC_taxonomy.csv")
names(family_details)
df_family <- family_details[,c("family","genus.species")]
colnames(df_family) <- c("family","species_name")

df_merged_family <- merge(new_df,df_family, by="species_name")
table(df_merged_family$family)
length(unique(df_merged_family$family))
max(table(df_merged_family$family))

tbl <- df_merged_family %>% 
  group_by(family) %>% 
  count(family) %>% 
  arrange(desc(n))

round((tbl$n)*(20/100))
round((tbl$n)*(40/100))
round((tbl$n)*(60/100))  
round((tbl$n)*(80/100))


library(splitstackshape)
outdf_80 <- stratified(df_merged_family, "family", .8, replace = FALSE)
outdf_60 <- stratified(df_merged_family, "family", .6,replace = FALSE)
outdf_40 <- stratified(df_merged_family, "family", .4,replace = FALSE)
outdf_20 <- stratified(df_merged_family, "family", .2,replace = FALSE) 
#sequential dropping
outdf_60_1 <- stratified(outdf_80, "family", 60/80,replace = FALSE)
outdf_40_1 <- stratified(outdf_60_1, "family", 40/60,replace = FALSE)
outdf_20_1 <- stratified(outdf_40_1, "family", 20/40,replace = FALSE)
class(outdf_20_1)
dat2phylip(outdf_20_1,outfile= "strat_20_1.phy")
data <- replicate(10,stratified(outdf_80, "family", 60/80))
