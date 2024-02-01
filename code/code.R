library(tidyverse)
library(ggplot2)
library(ggrepel)
library(plyr)
library(stringr)
library(scales)
library(RColorBrewer)
library(data.table)
library(reshape2)
library(ggeasy)


# Load and tidy data
mydata = read.csv("../data/cancer_patents.csv")
longdata = reshape2::melt(mydata, id.vars=colnames(mydata)[-c(13:21)],variable.name='category')
tidydata = longdata %>% 
	filter(value==1, Grant_or_Publication_Date > '1975-12-31') %>%
	mutate(publish_date = strptime(as.integer(Grant_or_Publication_Date), format = "%Y%m%d"),
		     filing_date = strptime(as.integer(Filing_Date), format = "%Y%m%d")) %>%
		subset(select=-c(4,5))

# Labelled Pie chart with fractions
catdata <- tidydata %>%
	ddply(.(category), summarize, tot=sum(value)) %>%
	mutate(fraction = round(tot/sum(tot)*100,digits=1),
	 	    category = str_replace_all(category, "_", " "),	 
	 		arrange(desc(fraction)))

# Pie plot data for percentages
posdata_frac <- catdata %>% 
	mutate(csum = rev(cumsum(rev(fraction))), 
           pos = fraction/2 + lead(csum, 1))
posdata_frac$pos = if_else(is.na(posdata_frac$pos), posdata_frac$fraction/2, posdata_frac$pos)
mycolors = c(brewer.pal(8, "Set2"),"#ffffff")

# Pie plot data for counts
posdata <- catdata %>% 
	mutate(csum = rev(cumsum(rev(tot))), 
           pos = tot/2 + lead(csum, 1))
posdata$pos = if_else(is.na(posdata$pos), posdata$tot/2, posdata$pos)
mycolors = c(brewer.pal(8, "Set2"),"#ffffff")

png("../figures/patent_pie_fractions4.png",width = 900, height = 900)
ggplot(catdata, aes(x = "" , y = fraction, fill = fct_inorder(category))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values=mycolors) +
  geom_label_repel(data = posdata_frac,
                   aes(y = pos, label = paste0(fraction, "%")),
                   size = 8, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "")) +
  ggtitle("Patent category proportions") +
  theme(panel.background = element_blank(),text = element_text(size = 25)) + 
  easy_remove_x_axis(what = c("ticks", "title", "text", "line"), teach = FALSE) + 
  easy_remove_y_axis(what = c("ticks", "title", "text", "line"), teach = FALSE)
dev.off()

png("../figures/patent_pie_counts.png",width = 600, height = 600)
ggplot(catdata, aes(x = "" , y = tot, fill = fct_inorder(category))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values=mycolors) +
  geom_label_repel(data = posdata,
                   aes(y = pos, label = as.character(tot)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "")) +
  ggtitle("Patent category counts") + 
  theme_void()
 	ggsave("../figures/patent_pie_counts.png",width = 6, height = 6)
dev.off()

# Plots of cumulative sum over time

sumdata = mydata %>% 
	dplyr::rename(date = Grant_or_Publication_Date) %>%
	plyr::arrange(date)
sumdata = sumdata[,c(5,13:21)]
sumdata[,-1] = apply(sumdata[,-1],2,cumsum)

longdata =  melt(sumdata,id.vars=colnames(sumdata)[1],variable.name='category')
datedata = longdata %>%
	mutate(category = str_replace_all(longdata$category, "_", " "))

# Raw Scale

ggplot(datedata, aes(x = as.Date(date), y = value)) + 
	geom_line(aes(color = category),size=.8) + 
	guides(color = guide_legend(title = "Category")) + 
  	scale_color_manual(values=mycolors) +
	xlab("Date") + 
	ylab("Count") + 
	ggtitle("Cumulative Patents Published") + 
	theme_dark()
 	ggsave("../figures/patent_timeseries_raw.png",width = 7, height = 5)

# Log10 Scale

ggplot(datedata, aes(x = as.Date(date), y = value+.1)) + 
	geom_line(aes(color = category),size=.8) + 
	coord_trans(y = "log") +
	guides(color = guide_legend(title = "Category")) + 
  	scale_color_manual(values=mycolors) +
	xlab("Date") + 
	ylab("Count") + 
	ggtitle("Cumulative Patents Published") + 
	theme_dark()
 	ggsave("../figures/patent_timeseries_log.png",width = 7, height = 5)

# Time to patent grant across categories

time_data = tidydata %>%
	mutate(uspto_interval = round((publish_date-filing_date)/(60*60*24)),
		   category = str_replace_all(category, "_", " "))

mean_table = ddply(time_data[,c("category","uspto_interval")], .(category), summarize, 
		 			mean_interval = mean(uspto_interval[which(!is.na(uspto_interval))]),
				   	sd_interval = sd(uspto_interval[which(!is.na(uspto_interval))]))

ggplot(time_data, aes(x=uspto_interval,fill=category)) + 
  scale_fill_manual(values=mycolors) +
  geom_histogram(show.legend = FALSE) + 
  facet_wrap(~category) +
  guides(color = guide_legend(title = "Category")) + 
  xlab("Days to Patent Grant") + 
  theme_dark()
  ggsave("../figures/histogram_time_to_patent_approval.png",width=6.5,height=6.5)


ggplot(time_data, aes(x=uspto_interval,y=category,fill=category)) + 
  scale_fill_manual(values=mycolors) +
  geom_boxplot(show.legend = FALSE) + 
  guides(color = guide_legend(title = "Category")) + 
  xlab("Days to Patent Grant") + 
  ylab("") + 
  theme_dark()
  ggsave("../figures/boxplot_time_to_patent_approval.png",width=7,height=5)

# split FDA applications into rows
fda_filt_data = filter(tidydata, FDA_Approval_Date!="")
split_list= apply(fda_filt_data, 1, function(X) data.frame(strsplit(X[c(13:19)],"\\|")))
for (i in 1:length(split_list)) {
	split_list[[i]] = merge(split_list[[i]],fda_filt_data[i,-c(13:19)])
}
split_data = as.data.frame(bind_rows(split_list)) 

# Do it again for semicolons
split_list= apply(split_data, 1, function(X) data.frame(strsplit(X[c(2:5)],";")))
for (i in 1:length(split_list)) {
	split_list[[i]] = merge(split_list[[i]],split_data[i,-c(13:19)])
}
split_data = as.data.frame(bind_rows(split_list)) 
fda_data = mutate(split_data,fda_approval_date = mdy(FDA_Approval_Date))

na_data = fda_data[which(is.na(fda_data$fda_approval_date)),]

# Number of Patents in each category

summary(as.factor(tidydata$category))

# Number of FDA approvals

summary(as.factor(fda_long_data$category))

# Number of patents with 1 or more FDA approvals

summary(fda_data$category)















