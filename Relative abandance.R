getwd()

#load needed libraries
library(tidyverse)
library(rlang)
library(ggplot2)
theme_set(theme_gray(base_size = 18))

#read metadata
meta <- read.delim("C:/Users/compu/Documents/Metadata.csv")
colnames(meta)[1] <- "sample_id"
meta$X.1=NULL

#read abundance data
abund <- read.delim("C:/Users/compu/Documents/otu_18s")
colnames(abund)[1]<-"OTU_ID"
#abund$X=NULL



#we have to data to have one
#column for the x-axis and another for the y-axis
#options `samples_to` and `names_to` indicate the
#names of the new columns in the new tibble.
abund %>%pivot_longer(-OTU_ID, names_to = "sample_id", values_to = "Abundance")


#smaller subset of the data to make some basic plots

dat <- abund %>%
  pivot_longer(-OTU_ID, names_to = "sample_id", values_to = "Abundance") %>%
  filter(OTU_ID %in% c("OTU10", "OTU100", "OTU1000", "OTU10000","OTU10001","OTU10002","OTU10003","OTU10004","OTU10005","OTU10006","OTU10007","OTU10008","OTU10009","OTU1001","OTU10010","OTU10011","OTU10012","OTU10013","OTU10014","OTU10015"))
print(dat)



#Now we create a basic barplot that shows relative abundances of the selected OTUs
dat %>%
  ggplot(aes(x = sample_id, y = Abundance)) +
  geom_bar(aes(fill = OTU_ID), stat = "identity", position = "fill")


dat %>%
  ggplot(aes(x = sample_id, y =  Abundance)) +
  geom_bar(aes(fill = OTU_ID), stat = "identity", position = "fill") +
  scale_y_continuous(name = "Relative Abundance", labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90))


#merge metadata with abundance data
dat <- dat %>%
  left_join(meta, by = "sample_id")
dat



p1 <- dat %>%
  ggplot(aes(x = sample_id, y = Abundance)) +
  geom_bar(aes(fill = OTU_ID), stat = "identity", position = "fill") +
  facet_grid(~ Habitat) +
  scale_y_continuous(name = "Relative Abundance", labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(color = "black"))
p1



p2 <- dat %>%
  ggplot(aes(x = sample_id, y = Abundance)) +
  geom_bar(aes(fill = OTU_ID), stat = "identity", position = "fill") +
  facet_grid(~ Site) +
  scale_y_continuous(name = "Relative Abundance", labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(color = "black"))
p2


# we an add another parameter from metadata in facet_grid()
# p3 <- dat %>%
#   ggplot(aes(x = sample_id, y = count)) +
#   geom_bar(aes(fill = OTU_ID), stat = "identity", position = "fill", width = 1) +
#   facet_grid(~ Habitat.x + Site.x, scales = "free_x") +
#   scale_y_continuous(name = "Relative abundance", labels = scales::percent) +
#   theme(axis.text.x = element_text(angle = 90),
#         axis.text.y = element_text(color = "black"),
#         strip.text = element_text(face = "bold"),
#         strip.background = element_blank())
# p3






############### with Rank5 ######

OTU_tax <- read.delim("C:/Users/compu/Documents/OTU_18s_tax_assignments.txt") 
tax <- OTU_tax[,c(1,6)]

#put character into dataframe 
taxa = data.frame(as.list(tax))
print(taxa)
colnames(taxa)[1] <- "OTU_ID"
#class(taxa)


#make subset of large data for faster visualyzing
dat <-abund %>%
  pivot_longer(-OTU_ID, names_to = "sample_id",
               values_to = "abundance")%>%
  filter(OTU_ID %in% c("OTU10", "OTU100", "OTU1000", "OTU10000","OTU10001","OTU10002","OTU10003","OTU10004","OTU10005","OTU10006","OTU10007","OTU10008","OTU10009","OTU1001","OTU10010","OTU10011","OTU10012","OTU10013","OTU10014","OTU10015"))
print(dat)

#class(dat)


#merge abundance data with taxa data(rank5)
dat <- dat %>%
  left_join(taxa, by = "OTU_ID")
print(dat)



#relative abundance Barplot for rank5 with its abundance
p3 <- dat %>%
  ggplot(aes(x = sample_id, y = abundance)) +
  geom_bar(aes(fill = Rank5), stat = "identity", position = "fill", width = 1) +
  scale_y_continuous(name = "Relative abundance", labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(face = "bold")) 
p3









