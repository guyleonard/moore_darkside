# load relevant libraries
library(ggvis) # replacement to ggplot2
library(reshape)
library(reshape2) # we need melt to convert tables to data frames
library(dplyr) # we'd be stuck without Hadley! do i use this?
library(gtools) # natural/human sorting of numbers

# Tell us where our files are
setwd("~/Dropbox/projects/deep_sea/otu_data")

# Read in all results-file locations
filenames <- list.files(".", pattern="*.csv_out", full.names=TRUE)
# Sort them in to human order e.g. 1, 2, 3, 4 not 10, 12, 1, 2
filenames <- mixedsort(filenames)

# Read them in to named data frames of Sample_i etc
# Important to have "as.is=T" to stop data being translated to 'factors'
# This will take several minutes....
for (i in seq_along(filenames)) {
    assign(paste("otu_df2_Sample", i, sep="_"), read.csv(filenames[i], header=FALSE, sep="\t", as.is=T))
}

#Create a list of our created dataframes
list_all_otu_df2_samples <- sapply(ls(pattern="otu_df2_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)

###########################
# Number of OTUS
###########################
# A little warning, I had to insert a dummy 0* entry in to Sample_9
# This is due to it having no "*" values in V7 (superkingdom) and so
# rbind was incorrectly binding the tables, shifting the positions
# as Sample_9 had only 3 values whereas the others had 4....
# Probably a way round this, but that was easy fix for now.
##########################
# For Super Kingdom
##########################
# there are other columns in the dataframe new to df=2, so we must move the column
# for taxonomy number up by a few...
for (k in names(list_all_otu_df2_samples)) {
	assign(paste("super_otus_df2", k, sep="_"), table(list_all_otu_df2_samples[[k]][7]))
}
# Get a list of all the skingdom tables
list_all_super_otus_df2 <- sapply(ls(pattern="super_otus_df2_otu_df2_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# rbind all the tables togetgher...
super_otus_df2 <- do.call("rbind", list_all_super_otus_df2)
super_otus_df2 <- melt(super_otus_df2)
# Rename column headings
super_otus_df2 = rename(super_otus_df2, c(X1="Sample",X2="Taxonomy"))
# Create the Data Plot :)
super_otus_df2 %>%
ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy) %>%
layer_bars() %>%
add_axis("y", title= "OTU Abundance", title_offset = 75) %>%
add_axis("x", title= "Samples", title_offset = 170, properties = axis_props(labels = list(angle = 45, align = "bottom")))

##########################
# For Kingdom
##########################
for (k in names(list_all_otu_df2_samples)) {
	assign(paste("kingdom_otus_df2", k, sep="_"), table(list_all_otu_df2_samples[[k]][8]))
}
# Get a list of all the skingdom tables
list_all_kingdom_otus_df2 <- sapply(ls(pattern="kingdom_otus_df2_otu_df2_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# rbind all the tables togetgher...
kingdom_otus_df2 <- do.call("rbind", list_all_kingdom_otus_df2)
kingdom_otus_df2 <- melt(kingdom_otus_df2)
# Rename column headings
kingdom_otus_df2 = rename(kingdom_otus_df2, c(X1="Sample",X2="Taxonomy"))
# Create the Data Plot :)
kingdom_otus_df2 %>%
ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy) %>%
layer_bars() %>%
add_axis("y", title= "OTU Abundance", title_offset = 75) %>%
add_axis("x", title= "Samples", title_offset = 170, properties = axis_props(labels = list(angle = 45, align = "bottom")))

##########################
# For phylum
##########################
for (k in names(list_all_otu_df2_samples)) {
	assign(paste("phylum_otus_df2", k, sep="_"), table(list_all_otu_df2_samples[[k]][9]))
}
# Get a list of all the sphylum tables
list_all_phylum_otus_df2 <- sapply(ls(pattern="phylum_otus_df2_otu_df2_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# rbind all the tables togetgher...
phylum_otus_df2 <- do.call("rbind", list_all_phylum_otus_df2)
phylum_otus_df2 <- melt(phylum_otus_df2)
# Rename column headings
phylum_otus_df2 = rename(phylum_otus_df2, c(X1="Sample",X2="Taxonomy"))
# Create the Data Plot :)
phylum_otus_df2 %>%
ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy) %>%
layer_bars() %>%
add_axis("y", title= "OTU Abundance", title_offset = 75) %>%
add_axis("x", title= "Samples", title_offset = 170, properties = axis_props(labels = list(angle = 45, align = "bottom")))

##########################
# For class
##########################
for (k in names(list_all_otu_df2_samples)) {
	assign(paste("class_otus_df2", k, sep="_"), table(list_all_otu_df2_samples[[k]][10]))
}
# Get a list of all the sclass tables
list_all_class_otus_df2 <- sapply(ls(pattern="class_otus_df2_otu_df2_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# rbind all the tables togetgher...
class_otus_df2 <- do.call("rbind", list_all_class_otus_df2)
class_otus_df2 <- melt(class_otus_df2)
# Rename column headings
class_otus_df2 = rename(class_otus_df2, c(X1="Sample",X2="Taxonomy"))
# Create the Data Plot :)
class_otus_df2 %>%
ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy) %>%
layer_bars() %>%
add_axis("y", title= "OTU Abundance", title_offset = 75) %>%
add_axis("x", title= "Samples", title_offset = 170, properties = axis_props(labels = list(angle = 45, align = "bottom")))

##########################
# For order
##########################
for (k in names(list_all_otu_df2_samples)) {
	assign(paste("order_otus_df2", k, sep="_"), table(list_all_otu_df2_samples[[k]][11]))
}
# Get a list of all the sorder tables
list_all_order_otus_df2 <- sapply(ls(pattern="order_otus_df2_otu_df2_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# rbind all the tables togetgher...
order_otus_df2 <- do.call("rbind", list_all_order_otus_df2)
order_otus_df2 <- melt(order_otus_df2)
# Rename column headings
order_otus_df2 = rename(order_otus_df2, c(X1="Sample",X2="Taxonomy"))
# Create the Data Plot :)
order_otus_df2 %>%
ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "OTU Abundance", title_offset = 75) %>%
add_axis("x", title= "Samples", title_offset = 170, properties = axis_props(labels = list(angle = 45, align = "bottom")))

##########################
# For family
##########################
for (k in names(list_all_otu_df2_samples)) {
	assign(paste("family_otus_df2", k, sep="_"), table(list_all_otu_df2_samples[[k]][12]))
}
# Get a list of all the sfamily tables
list_all_family_otus_df2 <- sapply(ls(pattern="family_otus_df2_otu_df2_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# rbind all the tables togetgher...
family_otus_df2 <- do.call("rbind", list_all_family_otus_df2)
family_otus_df2 <- melt(family_otus_df2)
# Rename column headings
family_otus_df2 = rename(family_otus_df2, c(X1="Sample",X2="Taxonomy"))
# Create the Data Plot :)
family_otus_df2 %>%
ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "OTU Abundance", title_offset = 75) %>%
add_axis("x", title= "Samples", title_offset = 170, properties = axis_props(labels = list(angle = 45, align = "bottom")))

# Output frequency count tables
write.table(cast(super_otus_df2, Taxonomy~Sample), file="super_kingdom.csv", sep=",")
write.table(cast(kingdom_otus_df2, Taxonomy~Sample), file="kingdom.csv", sep=",")
write.table(cast(phylum_otus_df2, Taxonomy~Sample), file="phylum.csv", sep=",")
write.table(cast(class_otus_df2, Taxonomy~Sample), file="class.csv", sep=",")
write.table(cast(order_otus_df2, Taxonomy~Sample), file="order.csv", sep=",")
write.table(cast(family_otus_df2, Taxonomy~Sample), file="family.csv", sep=",")
#write.table(cast(genus_otus_df2, Taxonomy~Sample), file="genus.csv", sep=",")


#

# Now I would like raw data tables and percentage tables of phylum only

# Raw tables
############
# the sum, margins="grand_col" adds up the (confusingly) ROWS...into a column "(all)"
phylum_otus_df2_cast <- cast(phylum_otus_df2, Sample ~ Taxonomy, sum, margins="grand_col")
# Counting does not recognise "NA" so lets make them all "0"
phylum_otus_df2_cast[is.na(phylum_otus_df2_cast)] <- 0
write.table(phylum_otus_df2_cast, file="phylum_df2_otus_raw_data.csv", sep=",")

# Percentage Tables
###################
phylum_otus_df2_cast_p <- cast(phylum_otus_df2, Sample ~ Taxonomy)
# Counting does not recognise "NA" so lets make them all "0"
phylum_otus_df2_cast_p[is.na(phylum_otus_df2_cast_p)] <- 0
phylum_otus_df2_cast_p <- cbind(id = phylum_otus_df2_cast_p[, 1], phylum_otus_df2_cast_p[, -1]/rowSums(phylum_otus_df2_cast_p[, -1]))
write.table(phylum_otus_df2_cast_p, file="phylum_df2_otus_percentages.csv", sep=",")


### Tom's >=2% Table
####################
#two_percent <- read.csv("2_percent_groups_for_R_plot.csv", header=TRUE, sep=",")
#two_percent_melt <- melt(two_percent)
#two_percent_melt %>% ggvis(x = ~id, y = ~value, fill = ~variable, strokeWidth := 0) %>%
#     layer_bars() %>%
#     add_axis("y", title= "Abundance", title_offset = 80) %>%
#     add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

#two_percent_no <- read.csv("2_percent_groups_for_R_plot_no_other.csv", header=TRUE, sep=",")
#two_percent_no_melt <- melt(two_percent_no)
#two_percent_no_melt %>% ggvis(x = ~id, y = ~value, fill = ~variable, strokeWidth := 0) %>%
#     layer_bars() %>%
#     add_axis("y", title= "Abundance", title_offset = 80) %>%
#     add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

# Tom's Alveolate Table
alveolate_table <- read.csv("alveolates_percentage_total_take2.csv", header=TRUE, sep=",")
alveolate_table_melt <- melt(alveolate_table)
alveolate_table_melt %>% ggvis(x = ~Sample, y = ~value, fill = ~variable, strokeWidth := 0) %>%
     layer_bars() %>%
     add_axis("y", title= "Abundance", title_offset = 80) %>%
     add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))



# Level 3 by Level 4 Plots
##########################

for (j in names(list_all_otu_df2_samples)) {
	assign(paste("df2_l3l4", j, sep="_"), list_all_otu_df2_samples[[j]] %>% regroup(list(quote(V9),quote(V10))) %>% summarise(total =sum(V5)))
}
df2_list_all_l3l4 <- sapply(ls(pattern="df2_l3l4_otu_df2_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)

df2_l3l4_counts <- do.call("rbind", df2_list_all_l3l4)
df2_l3l4_counts <- data.frame(rownames(df2_l3l4_counts), df2_l3l4_counts)

rownames(df2_l3l4_counts) <- NULL 
colnames(df2_l3l4_counts) <- c("Sample", "Taxonomy_L3", "Taxonomy_L4", "value")

df2_l3l4_counts = apply(df2_l3l4_counts,2,function(x) gsub("\\.\\d+",'',x))
df2_l3l4_counts_df <- as.data.frame(df2_l3l4_counts, stringsAsFactors = FALSE)
df2_l3l4_counts_df$value <- as.numeric(df2_l3l4_counts_df$value)

# GRAPHS
#Alveolata,Cercozoa,Chlorophyta,Cryptophyta,Discoba,Foraminifera,Fungi,Lobosa,Metazoa,Stramenopiles

df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Alveolata") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L4, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Cercozoa") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L4, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Chlorophyta") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L4, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Cryptophyta") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L4, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Discoba") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L4, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Foraminifera") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L4, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Fungi") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L4, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Lobosa") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L4, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Metazoa") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L4, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Stramenopiles") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L4, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

# Tables for L3xL4
##################
#Alveolata,,,,,,,,,
#
# Total table, although this becomes massive and unwieldly to view...
df2_l3l4_counts_cast <- cast(df2_l3l4_counts_df, Sample + Taxonomy_L3 ~ Taxonomy_L4, sum, margins="grand_col")
#
# Alveolata
df2_l3l4_alveolata_counts <- cast(df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Alveolata"), Sample + Taxonomy_L3 ~ Taxonomy_L4, sum, margins="gran_col")
write.table(df2_l3l4_alveolata_counts, file="df2_l3l4_alveolata_counts_otus_raw_data.csv", sep=",")
# Cercozoa
df2_l3l4_cercozoa_counts <- cast(df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Cercozoa"), Sample + Taxonomy_L3 ~ Taxonomy_L4, sum, margins="gran_col")
write.table(df2_l3l4_cercozoa_counts, file="df2_l3l4_cercozoa_counts_otus_raw_data.csv", sep=",")
# Chlorophyta
df2_l3l4_chlorophyta_counts <- cast(df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Chlorophyta"), Sample + Taxonomy_L3 ~ Taxonomy_L4, sum, margins="gran_col")
write.table(df2_l3l4_chlorophyta_counts, file="df2_l3l4_chlorophyta_counts_otus_raw_data.csv", sep=",")
# Cryptophyta
df2_l3l4_cryptophyta_counts <- cast(df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Cryptophyta"), Sample + Taxonomy_L3 ~ Taxonomy_L4, sum, margins="gran_col")
write.table(df2_l3l4_cryptophyta_counts, file="df2_l3l4_cryptophyta_counts_otus_raw_data.csv", sep=",")
# Discoba
df2_l3l4_discoba_counts <- cast(df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Discoba"), Sample + Taxonomy_L3 ~ Taxonomy_L4, sum, margins="gran_col")
write.table(df2_l3l4_discoba_counts, file="df2_l3l4_discoba_counts_otus_raw_data.csv", sep=",")
# Foraminifera
df2_l3l4_foraminifera_counts <- cast(df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Foraminifera"), Sample + Taxonomy_L3 ~ Taxonomy_L4, sum, margins="gran_col")
write.table(df2_l3l4_foraminifera_counts, file="df2_l3l4_foraminifera_counts_otus_raw_data.csv", sep=",")
# Fungi
df2_l3l4_fungi_counts <- cast(df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Fungi"), Sample + Taxonomy_L3 ~ Taxonomy_L4, sum, margins="gran_col")
write.table(df2_l3l4_fungi_counts, file="df2_l3l4_fungi_counts_otus_raw_data.csv", sep=",")
# Lobosa
df2_l3l4_lobosa_counts <- cast(df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Lobosa"), Sample + Taxonomy_L3 ~ Taxonomy_L4, sum, margins="gran_col")
write.table(df2_l3l4_lobosa_counts, file="df2_l3l4_lobosa_counts_otus_raw_data.csv", sep=",")
# Metazoa
df2_l3l4_metazoa_counts <- cast(df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Metazoa"), Sample + Taxonomy_L3 ~ Taxonomy_L4, sum, margins="gran_col")
write.table(df2_l3l4_metazoa_counts, file="df2_l3l4_metazoa_counts_otus_raw_data.csv", sep=",")
# Stramenopiles
df2_l3l4_stramenopiles_counts <- cast(df2_l3l4_counts_df %>% filter(Taxonomy_L3 == "Stramenopiles"), Sample + Taxonomy_L3 ~ Taxonomy_L4, sum, margins="gran_col")
write.table(df2_l3l4_stramenopiles_counts, file="df2_l3l4_stramenopiles_counts_otus_raw_data.csv", sep=",")

# L4L5 first
for (j in names(list_all_otu_df2_samples)) {
	assign(paste("df2_l4l5", j, sep="_"), list_all_otu_df2_samples[[j]] %>% regroup(list(quote(V10),quote(V11))) %>% summarise(total =sum(V5)))
}

df2_list_all_l4l5 <- sapply(ls(pattern="df2_l4l5_otu_df2_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)

df2_l4l5_counts <- do.call("rbind", df2_list_all_l4l5)
df2_l4l5_counts <- data.frame(rownames(df2_l4l5_counts), df2_l4l5_counts)

rownames(df2_l4l5_counts) <- NULL 
colnames(df2_l4l5_counts) <- c("Sample", "Taxonomy_L4", "Taxonomy_L5", "value")

df2_l4l5_counts = apply(df2_l4l5_counts,2,function(x) gsub("\\.\\d+",'',x))
df2_l4l5_counts_df <- as.data.frame(df2_l4l5_counts, stringsAsFactors = FALSE)
df2_l4l5_counts_df$value <- as.numeric(df2_l4l5_counts_df$value)

# GRAPHS

#l4l5_counts_df %>% filter(Taxonomy_L4 == "Ciliophora") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L5, strokeWidth := 0) %>%
#layer_bars() %>%
#add_axis("y", title= "Abundance", title_offset = 80) %>%
#add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l4l5_counts_df %>% filter(Taxonomy_L4 == "Euglenozoa") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L5, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

#l4l5_counts_df %>% filter(Taxonomy_L4 == "MAST") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L5, strokeWidth := 0) %>%
#layer_bars() %>%
#add_axis("y", title= "Abundance", title_offset = 80) %>%
#add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

# L4 vs L6
##########
for (j in names(list_all_otu_df2_samples)) {
	assign(paste("df2_l4l6", j, sep="_"), list_all_otu_df2_samples[[j]] %>% regroup(list(quote(V10),quote(V12))) %>% summarise(total =sum(V5)))
}

df2_list_all_l4l6 <- sapply(ls(pattern="df2_l4l6_otu_df2_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)

df2_l4l6_counts <- do.call("rbind", df2_list_all_l4l6)
df2_l4l6_counts <- data.frame(rownames(df2_l4l6_counts), df2_l4l6_counts)

rownames(df2_l4l6_counts) <- NULL 
colnames(df2_l4l6_counts) <- c("Sample", "Taxonomy_L4", "Taxonomy_L6", "value")

df2_l4l6_counts = apply(df2_l4l6_counts,2,function(x) gsub("\\.\\d+",'',x))
df2_l4l6_counts_df <- as.data.frame(df2_l4l6_counts, stringsAsFactors = FALSE)
df2_l4l6_counts_df$value <- as.numeric(df2_l4l6_counts_df$value)

# GRAPHS

df2_l4l6_counts_df %>% filter(Taxonomy_L4 == "Ciliophora") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L6, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l4l6_counts_df %>% filter(Taxonomy_L4 == "Cryptophyceae") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L6, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l4l6_counts_df %>% filter(Taxonomy_L4 == "Foraminifera") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L6, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

df2_l4l6_counts_df %>% filter(Taxonomy_L4 == "MAST") %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L6, strokeWidth := 0) %>%
layer_bars() %>%
add_axis("y", title= "Abundance", title_offset = 80) %>%
add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))



# #################################
# # Number of reads in OTU counts...
# ##################################

# ##########################
# # For super counts
# ##########################
# for (k in names(list_all_samples)) {
# 	assign(paste("super_counts", k, sep="_"), list_all_samples[[k]] %>% group_by(V4) %>% summarise(total =sum(V2)))
# }
# list_all_count_super <- sapply(ls(pattern="super_counts_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# super_counts <- do.call("rbind", list_all_count_super)
# super_counts <- data.frame(rownames(super_counts), super_counts)
# rownames(super_counts) <- NULL 
# colnames(super_counts) <- c("Sample", "Taxonomy", "value")
# super_counts = apply(super_counts,2,function(x) gsub("\\.\\d+",'',x))
# super_counts_df <- as.data.frame(super_counts, stringsAsFactors = FALSE)
# super_counts_df$value <- as.numeric(super_counts_df$value)
# super_counts_df %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy, strokeWidth := 0) %>%
# layer_bars() %>%
# add_axis("y", title= "Abundance", title_offset = 80) %>%
# add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

# ##########################
# # For kingdom counts
# ##########################
# for (k in names(list_all_samples)) {
# 	assign(paste("kingdom_counts", k, sep="_"), list_all_samples[[k]] %>% group_by(V5) %>% summarise(total =sum(V2)))
# }
# list_all_count_super <- sapply(ls(pattern="kingdom_counts_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# kingdom_counts <- do.call("rbind", list_all_count_super)
# kingdom_counts <- data.frame(rownames(kingdom_counts), kingdom_counts)
# rownames(kingdom_counts) <- NULL 
# colnames(kingdom_counts) <- c("Sample", "Taxonomy", "value")
# kingdom_counts = apply(kingdom_counts,2,function(x) gsub("\\.\\d+",'',x))
# kingdom_counts_df <- as.data.frame(kingdom_counts, stringsAsFactors = FALSE)
# kingdom_counts_df$value <- as.numeric(kingdom_counts_df$value)
# View(kingdom_counts_df)
# kingdom_counts_df %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy, strokeWidth := 0) %>%
# layer_bars() %>%
# add_axis("y", title= "Abundance", title_offset = 80) %>%
# add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

# ##########################
# # For phylum_counts
# ##########################
# for (k in names(list_all_samples)) {
# 	assign(paste("phylum_counts", k, sep="_"), list_all_samples[[k]] %>% group_by(V6) %>% summarise(total =sum(V2)))
# }
# list_all_count_super <- sapply(ls(pattern="phylum_counts_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# phylum_counts <- do.call("rbind", list_all_count_super)
# phylum_counts <- data.frame(rownames(phylum_counts), phylum_counts)
# rownames(phylum_counts) <- NULL 
# colnames(phylum_counts) <- c("Sample", "Taxonomy", "value")
# phylum_counts = apply(phylum_counts,2,function(x) gsub("\\.\\d+",'',x))
# phylum_counts_df <- as.data.frame(phylum_counts, stringsAsFactors = FALSE)
# phylum_counts_df$value <- as.numeric(phylum_counts_df$value)
# phylum_counts_df %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy, strokeWidth := 0) %>%
# layer_bars() %>%
# add_axis("y", title= "Abundance", title_offset = 80) %>%
# add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

# ##########################
# # For class_counts
# ##########################
# for (k in names(list_all_samples)) {
# 	assign(paste("class_counts", k, sep="_"), list_all_samples[[k]] %>% group_by(V7) %>% summarise(total =sum(V2)))
# }
# list_all_count_super <- sapply(ls(pattern="class_counts_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# class_counts <- do.call("rbind", list_all_count_super)
# class_counts <- data.frame(rownames(class_counts), class_counts)
# rownames(class_counts) <- NULL 
# colnames(class_counts) <- c("Sample", "Taxonomy", "value")
# class_counts = apply(class_counts,2,function(x) gsub("\\.\\d+",'',x))
# class_counts_df <- as.data.frame(class_counts, stringsAsFactors = FALSE)
# class_counts_df$value <- as.numeric(class_counts_df$value)
# class_counts_df %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy, strokeWidth := 0) %>%
# layer_bars() %>%
# add_axis("y", title= "Abundance", title_offset = 80) %>%
# add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

# ##########################
# # For order_counts
# ##########################
# for (k in names(list_all_samples)) {
# 	assign(paste("order_counts", k, sep="_"), list_all_samples[[k]] %>% group_by(V8) %>% summarise(total =sum(V2)))
# }
# list_all_count_super <- sapply(ls(pattern="order_counts_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# order_counts <- do.call("rbind", list_all_count_super)
# order_counts <- data.frame(rownames(order_counts), order_counts)
# rownames(order_counts) <- NULL 
# colnames(order_counts) <- c("Sample", "Taxonomy", "value")
# order_counts = apply(order_counts,2,function(x) gsub("\\.\\d+",'',x))
# order_counts_df <- as.data.frame(order_counts, stringsAsFactors = FALSE)
# order_counts_df$value <- as.numeric(order_counts_df$value)
# order_counts_df %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy, strokeWidth := 0) %>%
# layer_bars() %>%
# add_axis("y", title= "Abundance", title_offset = 80) %>%
# add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

# ##########################
# # For family_counts
# ##########################
# for (k in names(list_all_samples)) {
# 	assign(paste("family_counts", k, sep="_"), list_all_samples[[k]] %>% group_by(V9) %>% summarise(total =sum(V2)))
# }
# list_all_count_super <- sapply(ls(pattern="family_counts_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# family_counts <- do.call("rbind", list_all_count_super)
# family_counts <- data.frame(rownames(family_counts), family_counts)
# rownames(family_counts) <- NULL 
# colnames(family_counts) <- c("Sample", "Taxonomy", "value")
# family_counts = apply(family_counts,2,function(x) gsub("\\.\\d+",'',x))
# family_counts_df <- as.data.frame(family_counts, stringsAsFactors = FALSE)
# family_counts_df$value <- as.numeric(family_counts_df$value)
# family_counts_df %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy, strokeWidth := 0) %>%
# layer_bars() %>%
# add_axis("y", title= "Abundance", title_offset = 80) %>%
# add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))

# ##########################
# # For genus_counts
# ##########################
# for (k in names(list_all_samples)) {
# 	assign(paste("genus_counts", k, sep="_"), list_all_samples[[k]] %>% group_by(V10) %>% summarise(total =sum(V2)))
# }
# list_all_count_super <- sapply(ls(pattern="genus_counts_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)
# genus_counts <- do.call("rbind", list_all_count_super)
# genus_counts <- data.frame(rownames(genus_counts), genus_counts)
# rownames(genus_counts) <- NULL 
# colnames(genus_counts) <- c("Sample", "Taxonomy", "value")
# genus_counts = apply(genus_counts,2,function(x) gsub("\\.\\d+",'',x))
# genus_counts_df <- as.data.frame(genus_counts, stringsAsFactors = FALSE)
# genus_counts_df$value <- as.numeric(genus_counts_df$value)
# genus_counts_df %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy, strokeWidth := 0) %>%
# layer_bars() %>%
# add_axis("y", title= "Abundance", title_offset = 80) %>%
# add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))





# Ha can't do this as the last images overwrites all the previous due to
# it not saving files directly but to the viewer instead...hmmm
#
#l2l4_taxon_list <- unique(l2l4_counts_df$Taxonomy_L2)
#for (j in names(l2l4_taxon_list)) {
#	l2l4_counts_df %>% filter(Taxonomy_L2 == j) %>% ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy_L4, strokeWidth := 0) %>%
#	layer_bars() %>%
#	add_axis("y", title= "Abundance", title_offset = 80) %>%
#	add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))
#}

#
#level_2_level_4_counts <- Sample_1 %>% regroup(list(quote(V5),quote(V7))) %>% summarise(total =sum(V2))
#
#level_2_level_4_counts %>% filter(V5 == "Alveolata") %>% ggvis(x = ~V5, y = ~total, fill = ~V7, strokeWidth := 0) %>%
#     layer_bars() %>%
#     add_axis("y", title= "Abundance", title_offset = 80) %>%
#     add_axis("x", title= "Samples", title_offset = 120, properties = axis_props(labels = list(angle = 45, align = "bottom")))


#######

#phylum_counts_cast_p <- sweep(phylum_counts_cast_p[,-1], 1, rowSums(phylum_counts_cast_p[,-1]), FUN="/")

# for (k in names(list_all_samples)) {
# 	assign(paste("super_counts", k, sep="_"), list_all_samples[[k]] %>% group_by(V4) %>% summarise(total =sum(V2)))
# }
# list_all_count_super <- sapply(ls(pattern="super_counts_Sample\\_[0-9]+"), function(x) get(x), USE.NAMES=TRUE, simplify=FALSE)

# super_counts <- do.call("rbind", list_all_count_super)

# super_counts <- data.frame(rownames(super_counts), super_counts)

# colnames(super_counts) <- c("Sample", "Taxonomy", "value")

# super_counts <- apply(super_counts,2,function(x) gsub("\\.\\d+",'',x))

# super_counts_df <- as.data.frame(super_counts)

# # get rid of the annoying row.names ID which isn't a column
# # have to do it last as that's where we get the names from first
# rownames(super_counts_df) <- NULL 


####
# table13 <- Sample_13 %>% group_by(V4) %>% summarise(total =sum(V2))
# table13 <- setNames(data.frame(t(table13[,-1])), table13[,1])
# table13 <- cbind(a = "Sample13", table13)

### Workings to get to the above!!

#colnames(varName) <- c("ID", "Abundance", "PID", "superkingdom", "kingdom", "phylum", "class", "order", "family", "genus", "species", "Accession")

#library(tidyr) # easy separating of data in columns 
# for (i in seq_along(filenames)) {
# 	varName = paste("Sample", i, sep="_")
# 	assign(varName, cbind(varName, colsplit(V4, pattern = "\\|", names = c("superkingdom", "kingdom", "phylum", "class", "order", "family", "genus", "species"))))
# }
# Sample_1_sep <- separate(Sample_1, V4, c("superkingdom", "kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = "\\|")
#library(reshape2) # colsplit etc
# split column V4 - the taxonomy - in to 8 new columns (V6-13) and name them accordingly

# I could have also created a new file in bash and imported that too...but I don't like to edit source files
#results_2 <- cbind(results_2, colsplit(results_2$V4, pattern = "\\|", names = c("superkingdom", "kingdom", "phylum", "class", "order", "family", "genus", "species")))
# results_2 <- read.table("2_GATCTG_L001.results", header=FALSE, sep="\t")
# names(results_2)[4] <- "taxonomy"
# results_2_sk <- str_split_fixed(results_2$taxonomy, "\\|", 8)

# results_2_sk_table <- table(results_2_sk[,1])

# results_all_sk <-rbind(results_1_fixed_table,results_2_sk_table,results_3_sk_table,results_4_sk_table,results_5_sk_table,results_6_sk_table,results_7_sk_table,results_8_sk_table,results_9_sk_table,results_10_sk_table,results_11_sk_table,results_12_sk_table,results_13_sk_table,results_14_sk_table,results_15_sk_table,results_16_sk_table,results_17_sk_table)

# prop_results <- prop.table(results_all_sk,1)

# barplot(as.matrix(t(results_all_sk)), legend=rownames(t(results_all_sk)), main="Sample 2 - Superkingdoms", ylab="# Swarms", ylim=c(0,500000), col=c("green","darkred","darkorange","yellow"))
# library(reshape)
# prop_results_df <- melt(prop_results)
# prop_results_df = rename(prop_results_df, c(X1="Sample",X2="Taxonomy"))

# In GGPLOT2
# library(ggplot2)
# a = ggplot(prop_results_df, aes(x=Sample, y=value, fill=Taxonomy))
# a + geom_bar(stat = "identity", position = "stack") + ylab("Abundance") + theme_bw() + theme(axis.text.x=element_text(angle=90,))

# OR in GGVIS
# prop_results_df %>%
# ggvis(x = ~Sample, y = ~value, fill = ~Taxonomy) %>%
# layer_bars() %>%
# add_axis("y", title= "Abundance") %>%
# add_axis("x", title= "Samples", title_offset = 100, properties = axis_props(labels = list(angle = 45, align = "bottom")))