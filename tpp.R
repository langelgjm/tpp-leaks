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
#write.csv(groups_df, "groups_df.csv", row.names=FALSE)
# Create a barplot
#pdf("tpp_barplot.pdf", height=12,width=6)
opar <- par()
png("tpp_barplot.png", height=1024,width=480)
par(mar=c(5.1,8.1,4.1,2.1))
colors <- heat.colors(nrow(groups_df))
colors <- rev(colors)
barplot(groups_df$freq, horiz=TRUE, names.arg=groups_df$label, las=1, col=colors)
title(main="Frequency of Country Dyads in\nTPP IP Negotiating Positions", sub=attribution_string)
dev.off()

#library(ggplot2)
#ggplot(groups_df, aes(label)) + geom_bar()

# Created weighted network graph
library(igraph)
gr <- graph.data.frame(groups_df, directed=FALSE)
# Play with different ways of transforming the frequencies
#E(gr)$width <- groups_df$freq / 15
E(gr)$width <- rank(groups_df$freq)/7
E(gr)$curved <- 0.2
#E(gr)$color <- ifelse(grepl("US", E(gr)$label), "blue", "grey")
# Produce color vector; add 2 for rounding down and up
colors <- heat.colors(max(rank(groups_df$freq)/7)+2)
# reverse color order
colors <- rev(colors)
E(gr)$color <- colors[round(E(gr)$width)+1]
# Delete labels for now
E(gr)$label <- ""
V(gr)$label.cex <- 2
V(gr)$label.family <- "sans"
V(gr)$label.color <- "white"
V(gr)$color <- rgb(8,104,172,maxColorValue=255)
V(gr)$frame.color <- rgb(8,104,172,maxColorValue=255)
#write.csv(groups_df, "groups_df.csv", row.names=FALSE)
#pdf("tpp_network.pdf", width=10,height=10)
png("tpp_network.png", width=1024,height=1024)
plot(gr, layout=layout.circle)
#plot(gr, layout=layout.spring(gr, repulse=TRUE, equil=2))
title(main="Weighted Network Graph of Country Dyad Negotiating Positions\n in TPP IP Chapter", , sub=attribution_string)
dev.off()

# 3rd quantile
strong_groups <- groups_df
strong_groups <- strong_groups[strong_groups$freq>=quantile(groups_df$freq)[4],]
gr <- graph.data.frame(strong_groups, directed=FALSE)
E(gr)$width <- rank(strong_groups$freq)
E(gr)$label <- strong_groups$freq
E(gr)$curved <- 0.2
# Produce color vector; add 2 for rounding down and up
colors <- heat.colors(max(rank(strong_groups$freq))+2)
# reverse color order
colors <- rev(colors)
E(gr)$color <- colors[round(E(gr)$width)+1]
V(gr)$label.cex <- 2
V(gr)$label.family <- "sans"
V(gr)$label.color <- "white"
V(gr)$color <- rgb(8,104,172,maxColorValue=255)
V(gr)$frame.color <- rgb(8,104,172,maxColorValue=255)
#pdf("tpp_network_3rd.pdf", width=10,height=10)
png("tpp_network_3rd.png", width=800,height=800)
plot(gr, layout=layout.fruchterman.reingold(gr))
title(main=paste("Weighted Network Graph of Country Dyad Negotiating Positions\n", 
"in TPP IP Chapter\n",
"(Cutoff = 3rd Quartile,", quantile(groups_df$freq)[4], "Connections)"), , sub=attribution_string)
dev.off()

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
# Do some tests
#code_combos[75,]
#apply(head(countries), 1, function(x) all(code_combos[75,] %in% x))
#apply(code_combos, 1, function(y) all(y %in% countries[11,]))

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
#pdf("tpp_barplot_self.pdf", height=8,width=8)
png("tpp_barplot_sole.png", height=640,width=640)
par(mar=c(5.1,8.1,4.1,2.1))
tbl <- table(sole_proposer)
tbl <- tbl[order(tbl)]
cl <- row.names(tbl)
nl <- rev(seq(1:nrow(tbl)))
row.names(tbl) <- paste(nl, ". ", cl, " (", tbl, ")", sep="")
colors <- heat.colors(nrow(tbl))
colors <- rev(colors)
barplot(tbl, horiz=TRUE, las=1, col=colors)
title(main="Frequency of Sole-Country Proposals in\nTPP IP Negotiating Positions", , sub=attribution_string)
dev.off()

# Created weighted network graph
library(igraph)
gr <- graph.data.frame(countries_df, directed=FALSE)
# Play with different ways of transforming the frequencies
#E(gr)$width <- countries_df$freq / 15
E(gr)$width <- rank(countries_df$freq)/10
E(gr)$curved <- 0.2
#E(gr)$color <- ifelse(grepl("US", E(gr)$label), "blue", "grey")
# Produce color vector; add 2 for rounding down and up
colors <- heat.colors(max(rank(countries_df$freq)/10)+2)
# reverse color order
colors <- rev(colors)
E(gr)$color <- colors[round(E(gr)$width)+1]
# Delete labels for now
E(gr)$label <- ""
V(gr)$label.cex <- 2
V(gr)$label.family <- "sans"
V(gr)$label.color <- "white"
V(gr)$color <- rgb(8,104,172,maxColorValue=255)
V(gr)$frame.color <- rgb(8,104,172,maxColorValue=255)
angles <- c(0,0,0,0,-pi,-pi,-pi,0,-pi,-pi,-pi,0)
E(gr)$loop.angle[is.loop(gr)] <- angles
#pdf("tpp_network_all.pdf", width=12,height=10)
png("tpp_network_all.png", width=1280,height=1024)
plot(gr, layout=layout.circle)
title(main="Weighted Network Graph of Country Dyad Negotiating Positions\n in TPP IP Chapter, including Sole-Country Proposals", , sub=attribution_string)
dev.off()

single_df <- merge(as.data.frame(table(sole_proposer)), single_df,  all=TRUE, by.x="sole_proposer", by.y="country")
single_df$sp_ratio <- single_df$Freq / single_df$freq
single_df$percent <- single_df$sp_ratio * 100
single_df <- single_df[rev(order(single_df$sp_ratio)),]

names(single_df) <- c("Country", "Sole Proposals", "Total Proposals", "Ratio", "Percent")

library(xtable)
print(xtable(single_df), type="html", include.rownames=FALSE, file="table1.html")

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
#plot(hclust(dist(m)))
positions <- m

# For comparison, create another from the earlier leak
# This should run through each row of code_combos, and tell us if 
# it is present in each line of groups, producing a matrix
countries_matrix <- apply(countries_old, 1, 
		function(x) apply(code_combos, 1, 
					function(y) all(y %in% x)))
# Now handle the sole proposers
# This runs through for just the single code combos
countries_matrix_sole <- apply(countries_old, 1, 
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
countries_old_df <- data.frame(country1 = code_combos[,1], country2 = code_combos[,2])
countries_old_df$freq <- countries_freq
# Make labels
countries_old_df$label <- apply(countries_old_df, 1, function(x) paste(x[1], x[2]))
# Reorder
countries_old_df <- countries_old_df[order(countries_old_df$freq),]

# create matrix of dyadic frequencies
m_old <- matrix(ncol=12, nrow=12)
# convert countries_df to matrix
m_old[cbind(countries_old_df$country1, countries_old_df$country2)] <- countries_old_df$freq
colnames(m_old) <- codes
row.names(m_old) <- codes
#plot(hclust(dist(m)))
positions_old <- m_old



# Euclidean distance
tmp <- as.matrix(dist(t(m)))
tmp[upper.tri(tmp, diag=TRUE)] <- NA
#xtable(tmp, digits=c(0,rep(1,12)))
# Manhattan distance
#dist(t(positions), method="manhattan")

#library(gplots)
png("tpp_heatmap_clust.png", height=640,width=640)
par(mar=c(0,0,0,0))
#par(mar=c(5.1,8.1,4.1,2.1))
# Euclidean distance
tmp <- as.matrix(dist(t(positions)))
#tmp[lower.tri(tmp, diag=TRUE)] <- NA
heatmap(tmp, symm=TRUE, col=rev(brewer.pal(9,"Blues")), cexRow=1.5, cexCol=1.5)
#heatmap.2(tmp, symm=TRUE, col=rev(brewer.pal(9,"Blues")))
#heatmap(tmp, symm=TRUE, main="Heat Map of Euclidean Distances between TPP Negotiating Positions")
#heatmap(tmp, Rowv=NA, Colv=NA, col=rev(heat.colors(24)), symm = TRUE,
#		main=expression(atop("Heat Map of TPP Negotiating Position Overlap", atop("CC BY-SA 3.0, Gabriel J. Michael, gmichael@gwu.edu"))))
dev.off()

############ MDS ################

library(lsa)

create_ggplot <- function(input_data, legend_title, 
		chapter_title, 
		file_name, 
		x_min, x_max, x_by,
		y_min, y_max, y_by,
		new_jitter,
		x_min_jitter, x_max_jitter, 
		y_min_jitter, y_max_jitter,
		width, height, dpi) {
	
	# Only create new jitter if explicitly told to do so
	if (new_jitter == TRUE) {
		input_data$xj <- input_data$x + runif(nrow(input_data),x_min_jitter,x_max_jitter)
		input_data$yj <- input_data$y + runif(nrow(input_data),y_min_jitter,y_max_jitter)
	}
	
	ggp <- ggplot(data=input_data, aes(x=xj, y=yj, color=issue)) + 
			geom_point(x=0, y=0, size=6, shape=8, color="black") + 
			geom_point(size=15) +
			geom_point(size=12, 
					color="white", 
					show_guide=FALSE) +
			geom_text(size=6, 
					aes(label=nums), 
					color="black") +
			theme(axis.text = element_text(size=20), 
					axis.ticks=element_blank(), 
#					axis.title = element_text(size=20), 
					axis.title = element_blank(), 
					plot.title=element_text(size=20), 
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					panel.grid.minor = element_blank(), 
#					legend.title = element_text(size=16), 
#					legend.text=element_text(size=16),
					legend.position="none") +
			scale_x_continuous(limits=c(x_min,x_max), 
					breaks=seq(x_min,x_max,by=x_by), 
					name="x") + 
			scale_y_continuous(limits=c(y_min,y_max), 
					breaks=seq(y_min,y_max,by=y_by), 
					name="y") + 
			scale_color_discrete(name=legend_title) + 
			guides(colour = guide_legend(ncol=2, override.aes = list(size=7), 
							title.position="bottom", 
							title.hjust=0.5)) +
			coord_fixed() + 
#			geom_vline(xintercept=0, 
#					colour="#999999", 
#					linetype="longdash") + 
#			geom_hline(yintercept=0, 
#					colour="#999999", 
#					linetype="longdash") + 
			ggtitle(
					bquote(
							atop(paste("TPP IP Chapter Negotiating Positions, ", .(chapter_title), sep=""), 
									atop("CC BY-SA Gabriel J. Michael, gabriel.michael@yale.edu", ""))))
	ggsave(file_name, ggp, width=width, height=height, dpi=dpi)
	return(ggp)
}


labels <- codes
label_nums <- 1:length(codes)

positions_dist <- dist(positions)
# Alternatively
#tpp[is.na(tpp)] <- 0
#positions_dist <- acos(cosine(as.matrix(t(tpp))))
positions_mds <- isoMDS(positions_dist)
positions_mds_gg <- data.frame(x=positions_mds$points[,1], 
		y=positions_mds$points[,2], 
		issue=labels, 
		nums=labels)


create_ggplot(positions_mds_gg, "Intellectual Property Chapter", "May 16, 2014 Leak", 
		"tpp_may_16_2014_dist.png", 
		-70, 60, 10, -45, 30, 10, TRUE, -.5, .5, -.5, .5, 9, 7, 72)


positions_old_dist <- dist(positions_old)
# Alternatively
#tpp[is.na(tpp)] <- 0
#positions_dist <- acos(cosine(as.matrix(t(tpp))))
positions_old_mds <- isoMDS(positions_old_dist)
positions_old_mds_gg <- data.frame(x=positions_old_mds$points[,1], 
		y=positions_old_mds$points[,2], 
		issue=labels, 
		nums=labels)

create_ggplot(positions_old_mds_gg, "Intellectual Property Chapter", "August 30, 2013", 
		"tpp_august_30_2013_dist.png", 
		-300, 450, 50, -250, 200, 50, TRUE, -.5, .5, -.5, .5, 9, 7, 72)


p <- positions_old_mds_gg
p$xend <- positions_mds_gg$x
p$yend <- positions_mds_gg$y

input_data <- p
input_data$x <- input_data$x + runif(nrow(input_data),-15,15)
input_data$y <- input_data$y + runif(nrow(input_data),-10,10)


require(grid)
ggplot(data=input_data, aes(x=x, y=y, color=issue)) +
		geom_rect(xmin=-70, xmax=60, ymin=-45, ymax=30, linetype=2, 
				color="black", fill="grey") + 
		geom_point(x=0, y=0, size=6, shape=8, color="black") + 
		geom_segment(data=input_data, aes(x=x, y=y, xend=xend, yend=yend), size=2, 
				arrow=arrow(angle=22.5, length=unit(.125, "inches") ,type="closed")) +
		geom_point(size=15) +
		geom_point(size=12, 
				color="white", 
				show_guide=FALSE) +
		geom_text(size=6, 
				aes(label=codes), 
				color="black") +
		theme(axis.text = element_text(size=20), 
				axis.ticks=element_blank(), 
#				axis.title = element_text(size=20), 
				axis.title = element_blank(), 
				plot.title=element_text(size=20), 
				axis.text.x = element_blank(),
				axis.text.y = element_blank(),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(), 
#					legend.title = element_text(size=16), 
#					legend.text=element_text(size=16),
				legend.position="none") +
#		scale_x_continuous(limits=c(x_min,x_max), 
#				breaks=seq(x_min,x_max,by=x_by), 
#				name="x") + 
#		scale_y_continuous(limits=c(y_min,y_max), 
#				breaks=seq(y_min,y_max,by=y_by), 
#				name="y") + 
#		scale_color_discrete(name=legend_title) + 
#		guides(colour = guide_legend(ncol=2, override.aes = list(size=7), 
#						title.position="bottom", 
#						title.hjust=0.5)) +
		coord_fixed() + 
#		geom_vline(xintercept=0, 
#				colour="#999999", 
#				linetype="longdash") + 
#		geom_hline(yintercept=0, 
#				colour="#999999", 
#				linetype="longdash") + 
		ggtitle(
				bquote(
						atop(paste("Changes in TPP IP Chapter Negotiating Positions, ", "8/30/13 to 5/16/14", sep=""), 
								atop("CC BY-SA Gabriel J. Michael, gabriel.michael@yale.edu", ""))))
ggsave("tmp.png", width=9, height=7, dpi=72)






positions_mds <- cmdscale(positions_dist)
cmdscale(positions_dist, eig=TRUE)
positions_mds_gg <- data.frame(x=positions_mds[,1], 
		y=positions_mds[,2], 
		issue=labels, 
		nums=labels)


