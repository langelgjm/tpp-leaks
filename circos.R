attribution_string <- "@gabrieljmichael, www.gabrieljmichael.com"

setwd("/Users/gjm/Documents/_Works in Progress/TPP/leak3/tpp-leaks")
# Have to specify number of columns because read.table only looks at first 5 rows
groups <- read.table("groups.csv", sep=",", header=FALSE,fill=TRUE,col.names=c(1:12),
stringsAsFactors=FALSE)
countries <- read.table("countries.csv", sep=",", header=FALSE,fill=TRUE,col.names=c(1:12),
stringsAsFactors=FALSE)
countries_old <- read.table("countries_old.csv", sep=",", header=FALSE,fill=TRUE,col.names=c(1:12),
		stringsAsFactors=FALSE)

# create list of country codes
codes <- c("AU","BN","CA","CL","JP","MX","MY","NZ","PE","SG","US","VN")
# create transposed matrix of unique, unordered combinations of country codes
code_combos <- t(combn(codes,2))
# Do some tests
#code_combos[1,]
#apply(head(groups), 1, function(x) all(code_combos[44,] %in% x))
#apply(code_combos, 1, function(y) all(y %in% groups[2,]))
single_codes <- t(combn(codes,1))
single_matrix <- apply(countries, 1, function(x) apply(single_codes, 1, function(y) all(y %in% x)))
single_matrix <- single_matrix * 1
single_freq <- rowSums(single_matrix)
single_df <- data.frame(country = single_codes)
single_df$freq <- single_freq
#single_df$label <- single_df$country
#single_df <- single_df[order(single_df$freq),]
row.names(single_df) <- rev(seq(1:nrow(single_df)))
#single_df$label <- paste(row.names(single_df), ". ", single_df$label, " (", single_df$freq, ")", sep="")
# all that produces total number of proposals per country


# This should run through each row of code_combos, and tell us if 
# it is present in each line of groups, producing a matrix
groups_matrix <- apply(groups, 1, function(x) apply(code_combos, 1, function(y) all(y %in% x)))
# Trick to convert logical matrix to numeric matrix
groups_matrix <- groups_matrix * 1
# Now row sums will provide a frequency count for each pair
groups_freq <- rowSums(groups_matrix)
# Start making a data frame
groups_df <- data.frame(country1 = code_combos[,1], country2 = code_combos[,2])
groups_df$freq <- groups_freq
# Make labels
groups_df$label <- apply(groups_df, 1, function(x) paste(x[1], x[2]))
# Reorder
groups_df <- groups_df[order(groups_df$freq),]
row.names(groups_df) <- rev(seq(1:nrow(groups_df)))
groups_df$label <- paste(row.names(groups_df), ". ", groups_df$label, " (", groups_df$freq, ")", sep="")
#####################
# Including self proposals
#####################

# create list of country codes
codes <- c("AU","BN","CA","CL","JP","MX","MY","NZ","PE","SG","US","VN")
# create transposed matrix of unique, unordered combinations of country codes
# later we add identities to the combinations
##code_combos <- rbind(t(combn(codes,2)), matrix(rep(t(combn(codes,1)),2),12,2))
code_combos <- t(combn(codes,2))
# Create matrix of identical country codes
single_code_combos <- matrix(rep(t(combn(codes,1)),2),12,2)

# This should run through each row of code_combos, and tell us if 
# it is present in each line of groups, producing a matrix
countries_matrix <- apply(countries, 1, 
function(x) apply(code_combos, 1, 
function(y) all(y %in% x)))
# Now handle the sole proposers
# This runs through for just the single code combos
countries_matrix_sole <- apply(countries, 1, 
function(x) apply(single_code_combos, 1, 
function(y) all(y %in% x[1] & nchar(x[2])==0 ) ) )

# now bind matrices together
countries_matrix <- rbind(countries_matrix,countries_matrix_sole)
code_combos <- rbind(code_combos, single_code_combos)

# Trick to convert logical matrix to numeric matrix
countries_matrix <- countries_matrix * 1
# Now row sums will provide a frequency count for each pair
countries_freq <- rowSums(countries_matrix)

# Start making a data frame
countries_df <- data.frame(country1 = code_combos[,1], country2 = code_combos[,2])
countries_df$freq <- countries_freq
# Make labels
countries_df$label <- apply(countries_df, 1, function(x) paste(x[1], x[2]))
# Reorder
countries_df <- countries_df[order(countries_df$freq),]

# Create a barplot of just self proposals
# This gets us rows where there is only one country
sole_proposer <- countries[nchar(countries[,2])==0,1]


single_df <- merge(as.data.frame(table(sole_proposer)), single_df,  all=TRUE, by.x="sole_proposer", by.y="country")
single_df$sp_ratio <- single_df$Freq / single_df$freq
single_df$percent <- single_df$sp_ratio * 100
single_df <- single_df[rev(order(single_df$sp_ratio)),]

###########################################################################
# Clustering approach

library(MASS)
library(ggplot2)
library(Matrix)
library(xtable)
library(RColorBrewer)

# create matrix of dyadic frequencies
m <- matrix(ncol=12, nrow=12)
# convert countries_df to matrix
m[cbind(countries_df$country1, countries_df$country2)] <- countries_df$freq
colnames(m) <- codes
row.names(m) <- codes

# now bind matrices together
countries_matrix <- rbind(countries_matrix,countries_matrix_sole)
code_combos <- rbind(code_combos, single_code_combos)

# Trick to convert logical matrix to numeric matrix
countries_matrix <- countries_matrix * 1
# Now row sums will provide a frequency count for each pair
countries_freq <- rowSums(countries_matrix)


#####################################################################
# Circos data output
#####################################################################

# Karyotype data file output
# chr - au 1 0 66 set3-12-qual-1

# Produce count of number of times a country appears in any group
# This will specify the width 
w <- rowSums(apply(groups, 1, function(x) codes %in% x))
#w <- rowSums(apply(countries, 1, function(x) codes %in% x))

# Really should scale widths by total number of frequency of country's dyads

# Produce output file data
#d <- paste("chr - ", codes, " 1 0 ", w, " set3-12-qual-", rep(1:12), sep="")
#write(d, "karyotype.tpp.txt")

#d <- paste("chr - ", codes, " 1 0 ", nrow(groups), " set3-12-qual-", rep(1:12), sep="")
#write(d, "karyotype.tpp.txt")

# Link data file output
# recall that links can (and should?) overlap up to 12 times
# deal with this by dividing up width of country by width of all links?
# us 1 25 au 10 30 color=blue
links <- groups_df[1:3]
#links <- merge(links, data.frame(country1=codes, w=w), by="country1")
# starting points must be incremented so that links don't overlap
# strategy: go through df by country, add freq (width) of prior link to a start variable
library(plyr)
# this will produce the total available width for start1 by country
# ddply(links, .(country1), summarise, sum(freq))
# define start1 as a running total of freq (widths) by country
# so that each link starts after the previous link
# define minstart2 as total of all freqs for country1
# so that minimum start2 for country1 accounts for all links emanating from country1 
links <- ddply(links, .(country1), transform, start1=cumsum(freq), minstart2=sum(freq))

# copy start1 to end1
links$end1 <- links$start1
# modify start1 so that it begins at 0 (substract freq)
links$start1 <- links$start1 - links$freq
# Now start1 and end1 will have the correct width and position

# create start2/end2
# first create temp df to pull minstart2 from as necessary
df <- unique(data.frame(country2=links$country1, minstart2c2=links$minstart2))
# there is no entry for VN in this, though, because there is no VN in country1
# this means that VN's minstart2 should be 0 anyway

# merge this data frame into links based on country2
# need all.x, but NOT all.y (creates reverse entry for AU-VN while missing VN level)
links <- merge(links, df, by="country2", all.x=TRUE)
# add in minstart data for VN (or any missing minstart2c2)
links[is.na(links$minstart2c2),"minstart2c2"] <- 0
# same as above but starting at the minstart2c2
links <- ddply(links, .(country2), transform, start2=minstart2c2 + cumsum(freq))
# copy start2 to end2
links$end2 <- links$start2
# modify start2 so that it begins at minstart2c2 (substract freq)
links$start2 <- links$start2 - links$freq
# Now start2 and end2 will have the correct width and position


# for mapping to width domain
#links <- ddply(links, .(country1), transform, runtot1=cumsum(freq))
# Now repeat this process for country2 (other side of the link)
# start2 must begin at the sum of all start1s for that country
#links <- ddply(links, .(country1), transform, start1tot=sum(freq))
#links <- ddply(links, .(country2), transform, start2=start1tot + cumsum(freq))
# copy start2 to end2
#links$end2 <- links$start2
# for mapping to width domain
#links <- ddply(links, .(country2), transform, runtot2=cumsum(freq))
# modify start2 so that it begins at 0 (substract freq)
#links$start2 <- links$start2 - links$freq

d <- paste(links$country1, links$start1, links$end1, 
		links$country2, links$start2, links$end2)
write(d, "links.tpp.txt")

########## Ideogram file

# ideogram widths based on number of dyads
a <- ddply(links, .(country1), summarize, tot=sum(freq))
names(a)[1] <- "country"
b <- ddply(links, .(country2), summarize, tot=sum(freq))
names(b)[1] <- "country"
df <- merge(a, b, by="country", all=TRUE)
df[is.na(df)] <- 0
df$tot <- df$tot.x + df$tot.y

library(countrycode)
names <- countrycode(df$country, "iso2c", "country.name")
capwords <- function(s, strict = FALSE) {
	cap <- function(s) paste(toupper(substring(s, 1, 1)),
				{s <- substring(s, 2); if(strict) tolower(s) else s},
				sep = "", collapse = " " )
	sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
names <- capwords(names, strict=TRUE)
d <- paste("chr - ", df$country, " ", df$country, " 0 ", df$tot, " set3-12-qual-", rep(1:12), sep="")
write(d, "karyotype.tpp.txt")

######################
# Probably would make sense to do it by line (group)

# This gets me a list of matrices
# each list element is a line (group)
# each matrix row is a dyad from that line
#m <- apply(groups, 1, function(x) t(combn(x[x != ""],2)))
#
## Let's graph the links of each line emanating from a single point?
## the starting and ending point will be defined by the line (group)
#
#tmp <- lapply(m, function(x) apply(x, 1, function(y) paste(y[1], y[2])))
#tmp <- lapply(m, function(x) apply(x, 1, function(y) y))
#
#
#links <- do.call(rbind, m)
## this gets me the number of rows of each matrix in m
#lengths <- unlist(lapply(m, function(x) nrow(x)))
#
## vectorized repeat, where each row has the value and number of times
#rep(a, lengths)
##rle(rep(a, lengths))
#
#links_df <- data.frame(country1=links[,1], country2=links[,2], start1=rep(a, lengths))
#
#links_df$end1 <- links_df$start1
#links_df$start2 <- links_df$start1
#links_df$end2 <- links_df$start1
#
#d <- paste(links_df$country1, links_df$start1, links_df$end1, 
#		links_df$country2, links_df$start2, links_df$end2)
#write(d, "links.tpp.txt")
