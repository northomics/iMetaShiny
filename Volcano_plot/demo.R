source("Sub_functions_volcano.R")

# Load sample data. This is the same file as Z:\Leyuan\5-Data Processing\7 - Analysis\20190704_RS\Analysis\2-differentialProteins\4_proteins_LFQ_volunteer_q25_4v_noFOS_combat_for_metabo_rs3.txt
data = read.table("Sample_data.txt", header = TRUE, sep = "\t", row.names = 1)
meta = read.table("Sample_data_meta.txt", header = TRUE, sep = "\t", row.names = 1)

# Extract grouping information (group ids, group length)
group_id = as.character(unique(meta[,1]))
group_length = as.integer(count(unique(meta)))

a <- as.list(group_id)

# return all group ids
for (i in 1:group_length) {
  print(group_id[i])
}



# return value to a selection window 
# selection as input 
# ----------------------------

# Ask user: Please select inputs for the comparison (Group1 vs Group2)
# Please define your Group1
data1 = as.data.frame(subset(t(data), meta == group_id[1]))

data1 = as.data.frame(subset(t(data), meta[,1] == "Treatment"))

# data1 = as.data.frame(subset(t(data), meta == "Treatment"))
# Please define your Group2
data2 = as.data.frame(subset(t(data), meta == group_id[2]))

data1[,1:6]
data2[,1:6]


# Separate data
data1 = t(data1)
data2 = t(data2)

# Normalize by sum of each sample, other normailze methods, see stats_univariates.R by MetaboAnalyst
# User select other means of normalization
data1<-apply(data1, 2, SumNorm)
data2<-apply(data2, 2, SumNorm)

head(data2, 10L)

colnames(data1)[]<- group_id[1]
colnames(data2)[]<- group_id[2]
ncol(data1)
ncol(data2)

data <- cbind(data1, data2)
ncol(data)


# Calculate t text
stats_t <- lapply(1:nrow(data), function(x) t.test(data[x, 1:ncol(data1)], data[x, (ncol(data1)+1):ncol(data)]))
stats_wc <- lapply(1:nrow(data), function(x) wilcox.test(data[x, 1:ncol(data1)], data[x, (ncol(data1)+1):ncol(data)]))

names(stats_t) <- row.names(data)
names(stats_wc) <- row.names(data)

as.data.frame(stats_t[[1]]$p.value)

# p.value and p.adjust
p.value.t <- lapply(1:nrow(data), function(x) stats_t[[x]]$p.value)
p.value.wc <- lapply(1:nrow(data), function(x) stats_wc[[x]]$p.value)

names(p.value.t) <- row.names(data)
names(p.value.wc) <- row.names(data)

p.adj.t <- as.data.frame(p.adjust(p.value.t, method = "fdr"))
p.adj.wc <- as.data.frame(p.adjust(p.value.wc, method = "fdr"))

# Calculate fold change
log2FC <- lapply(1:nrow(data), function(x) log2(mean(data[x, 1:ncol(data1)])/mean(data[x, (ncol(data1)+1):ncol(data)])))
names(log2FC) <- row.names(data)

# prepare data for plotting
log2FC <- as.matrix(t(as.data.frame(log2FC)))
p.adj.t <- as.matrix(p.adj.t)
p.adj.wc <- as.matrix(p.adj.wc)
head(log2FC)
head(p.adj.t)
head(p.adj.wc)

# mean value of intensity for ggplot, for now in all data, can be customized to control / treatment dataset
intensity <- lapply(1:nrow(data), function(x) mean(data[x,]))
names(intensity) <- row.names(data)
intensity <- as.matrix(t(as.data.frame(intensity)))



# ###############################
# # plot volcano -- according to y > c/(X-log2FC.cut.off)+p.val.cut.off ------------------------------------
# # Part 1, t-test ----------------------------------------
# mycolor <- lapply(1:nrow(data), function(x) ifelse((-log10(p.adj.t[x]) < (0.3/(abs(log2FC[x])-1))+1.3), "gray50", ifelse(abs(log2FC[x])<1, "gray50", "red")))
# 
# names(mycolor) <- row.names(data)
# mycolor <- as.data.frame(mycolor)
# 
# fun1 <- function(x) ifelse(0.3/(abs(x)-1)>0, 0.3/(abs(x)-1)+1.3, NA)
# plot(log2FC, -log10(p.adj.t), col = ifelse(mycolor == 'gray50', 'gray50', 'red'))
# 
# plot (fun1, -4, 4, type = "l", add = TRUE)
# 
# ## trying out curve function
# fun1 <- function(x) ifelse(0.3/(abs(x)-1)>0, 0.3/(abs(x)-1)+1.3, NA)
# plot (fun1, xlim=range(-4:4), ylim=range(-1,10))
# 
# 
# # Part 2, nonparametric test --------------------------------
# 
# mycolor <- lapply(1:nrow(data), function(x) ifelse((-log10(p.adj.wc[x]) < (0.3/(abs(log2FC[x])-1))+1.3), "gray50", ifelse(abs(log2FC[x])<1, "gray50", "red")))
# 
# names(mycolor) <- row.names(data)
# mycolor <- as.data.frame(mycolor)
# 
# fun1 <- function(x) ifelse(abs(x)>1, 0.3/(abs(x)-1)+1.3, NA)
# plot(log2FC, -log10(p.adj.wc), col = ifelse(mycolor == 'gray50', 'gray50', 'red'))
# 
# curve(fun1, -10, 10, n=1000, type = "l", add = TRUE)
# 
# ## trying out curve function
# fun1 <- function(x) ifelse(0.3/(abs(x)-1)>0, 0.3/(abs(x)-1)+1.3, NA)
# plot (fun1, xlim=range(-4:4), ylim=range(-1,10))



############## Prettier plot, with size information
library(ggplot2)
log_p_adj_wc <- -log10(p.adj.wc)
# mycolor <- lapply(1:nrow(data), function(x) ifelse((-log10(p.adj.wc[x]) < (0.3/(abs(log2FC[x])-1))+1.3), "N.S.", ifelse(abs(log2FC[x])<1, "N.S.", "Significant")))
mycolor <- lapply(1:nrow(data), function(x,
                                         FC_CutOff = 1,
                                         P_cutOff = -log10(0.01),
                                         curvature = 0.5)  
                                {
                                ifelse((-log10(p.adj.wc[x]) < (curvature/(abs(log2FC[x])-FC_CutOff))+P_cutOff), "N.S.", ifelse(abs(log2FC[x])<FC_CutOff, "N.S.", ifelse(log2FC[x]>FC_CutOff, "S.Increased", "S.Decreased")))
                                }
                  )
names(mycolor) <- row.names(data)
mycolor <- as.data.frame(mycolor)
mycolorg <- as.data.frame(t(mycolor))
head(log_p_adj_wc)
head(mycolorg)
head(log2FC)

# For plot
dataplot <- as.data.frame(cbind(log2FC, log_p_adj_wc, mycolorg, intensity))
head(dataplot)

names(dataplot) <- c("log2FC", "log_p_adj_wc", "mycolorg", "intensity")

# For table output
data_output <- as.data.frame(cbind(log2FC, p.adj.wc, mycolorg))
names(data_output) <- c("Log2 fold-change", "p value, FDR-adjusted", "Change")
head(data_output)

fun1 <- function(x,
                 FC_CutOff = 1,
                 P_cutOff = -log10(0.01),
                 curvature = 0.5) 
{
  ifelse(curvature/(abs(x)-FC_CutOff)>0, curvature/(abs(x)-FC_CutOff)+P_cutOff , NA)
}


# ggplot(dataplot, aes(x=log2FC, y=log_p_adj_wc)) +
#   geom_point(aes(fill = factor(mycolorg), size = intensity,color = factor(mycolorg)),  alpha = 0.9, shape = 21) +
#   scale_alpha(intensity, range = c(0.3, 1))+
#   xlab(expression(paste(Log[2]," fold-change", sep = " "))) + ylab(expression(paste(-Log[10],"(", italic(p), ")",", FDR-adjusted", sep = " "))) +
#   stat_function(fun = fun1, n=3000,geom = "line",color="#999999", size=1, alpha = 0.8, linetype = "dashed") + ylim(-0.2,max(log_p_adj_wc)) + 
#   scale_fill_manual(values=c("#f5f5f5", "#33ff33","#ff0066")) +
#   scale_color_manual(values=c("#999999", "#009900","#990000")) +
#   theme_bw(base_size = 12) + labs(fill = 'Changes', color = 'Changes', size = "Intensity") + guides(size = FALSE)
# 
# 
# blues = c("#f5f5f5","#3e64ff","#ff6600")
# reds = c("#f5f5f5","#009900","#ff0000")
roses = c("#f5f5f5","#666666","#ff33ff")

ggplot(dataplot, aes(x=log2FC, y=log_p_adj_wc)) +
  geom_point(aes(fill = factor(mycolorg), size = NULL),  color = "gray60", alpha = 0.9, shape = 21) +
  scale_alpha(intensity, range = c(0.3, 1))+
  xlab(expression(paste(Log[2]," fold-change", sep = " "))) + ylab(expression(paste(-Log[10],"(", italic(p), ")",", FDR-adjusted", sep = " "))) +
  stat_function(fun = fun1, n=3000,geom = "line",color="#999999", size=1, alpha = 0.8, linetype = "dashed") + ylim(-0.2,max(log_p_adj_wc)) + 
  scale_fill_manual(values=roses) +
  theme_bw(base_size = 12) + labs(fill = 'Changes', color = 'Changes', size = "Intensity") + guides(size = FALSE, fill = guide_legend(reverse = TRUE))

# ggplot(dataplot, aes(x=log2FC, y=log_p_adj_wc)) +
#   geom_point(aes(fill = factor(mycolorg), size = intensity),  color = "gray60", alpha = 0.9, shape = 21) +
#   scale_alpha(intensity, range = c(0.3, 1))+
#   xlab(expression(paste(Log[2]," fold-change", sep = " "))) + ylab(expression(paste(-Log[10],"(", italic(p), ")",", FDR-adjusted", sep = " "))) +
#   stat_function(fun = fun1, n=3000,geom = "line",color="#999999", size=1, alpha = 0.8, linetype = "dashed") + ylim(-0.2,max(log_p_adj_wc)) + 
#   scale_fill_manual(values=reds) +
#   theme_bw(base_size = 12) + labs(fill = 'Changes', color = 'Changes', size = "Intensity") + guides(size = FALSE, fill = guide_legend(reverse = TRUE))
# 
# # show size or not
# # customized color combinations
# # FDR adjusted or not
# 
# 
# ggplot(dataplot, aes(x=log2FC, y=log_p_adj_wc)) +
#   geom_point(aes(color = factor(mycolorg), size = intensity), alpha = 0.8, shape = 16) +
#   scale_alpha(intensity, range = c(0.3, 1))+
#   xlab(expression(paste(Log[2]," fold-change", sep = " "))) + ylab(expression(paste(-Log[10],"(", italic(p), ")",", FDR-adjusted", sep = " "))) +
#   stat_function(fun = fun1, n=3000,geom = "line",color="#ff0066", size=1, alpha = 0.5)+ ylim(-0.2,max(log_p_adj_wc)) + 
#   scale_color_manual(values=c("#cccccc", "#ff0066")) +
#   theme_bw(base_size = 12) + labs(color = 'Changes', size = "Intensity")  + guides(size = FALSE)


# plot volcano -- if user selected basic
mycolor <- lapply(1:nrow(data), function(x,
                                         FC_CutOff = 1,
                                         P_cutOff = -log10(0.01),
                                         curvature = 0.5)  
{
  ifelse(-log10(p.adj.wc[x]) < P_cutOff, "N.S.", ifelse(abs(log2FC[x])<FC_CutOff, "N.S.", ifelse(log2FC[x]>FC_CutOff, "S.Increased", "S.Decreased")))
}
)
names(mycolor) <- row.names(data)
mycolor <- as.data.frame(mycolor)
mycolorg <- as.data.frame(t(mycolor))
FC_CutOff = 1
P_cutOff = -log10(0.01)
  
dataplot <- as.data.frame(cbind(log2FC, log_p_adj_wc, mycolorg, intensity))
head(dataplot)

names(dataplot) <- c("log2FC", "log_p_adj_wc", "mycolorg", "intensity")


ggplot(dataplot, aes(x=log2FC, y=log_p_adj_wc)) +
  geom_point(aes(fill = factor(mycolorg), size = NULL),  color = "gray60", alpha = 0.9, shape = 21) +
  scale_alpha(intensity, range = c(0.3, 1))+
  xlab(expression(paste(Log[2]," fold-change", sep = " "))) + ylab(expression(paste(-Log[10],"(", italic(p), ")",", FDR-adjusted", sep = " "))) +
  geom_hline(yintercept = P_cutOff, color="#999999", size=1, alpha = 0.8, linetype = "dashed") + 
  geom_vline(xintercept = FC_CutOff, color="#999999", size=1, alpha = 0.8, linetype = "dashed") + 
  geom_vline(xintercept = -FC_CutOff,color="#999999", size=1, alpha = 0.8, linetype = "dashed") +
  ylim(-0.2,max(log_p_adj_wc)) + 
  scale_fill_manual(values=roses) +
  theme_bw(base_size = 12) + labs(fill = 'Changes', color = 'Changes', size = "Intensity") + guides(size = FALSE, fill = guide_legend(reverse = TRUE))

 

plot(log2FC, -log10(p.adj.t), col = ifelse(p.adj.t > 0.05,'gray50',ifelse(abs(log2FC) > 1, 'red','gray50')))
abline(h = -log10(0.05), v = -1)
abline(v = 1)
