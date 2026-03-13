library(mclust)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(ICSNP)
library(reshape2)
library(mclust)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(patchwork)
library(ICSNP)
library(ggh4x)
library(MASS)
library(ggnewscale)
library(stringr)
library(ggrepel)
library(magrittr)
library(tidyverse)
library(reshape2)
library(ggbreak)
library(export)

#### Read in Data ####
excel.files.2.read = Sys.glob("Data/*.xlsx")

all.positive.df = c()
for(i in excel.files.2.read){
  i.data = read_xlsx(path = i)
  i.data = i.data %>% 
    mutate(across(pdm_cam_sw63_21:N1_hpdm_Ca_04_09, ~ifelse(.<0, 1, .))) %>%
    mutate(across(pdm_cam_sw63_21:N1_hpdm_Ca_04_09, ~ifelse(.==0, 1, .))) %>% 
    gather(Gp, MFI, `pdm_cam_sw63_21` : `N1_hpdm_Ca_04_09`)
  i.data$Location =  strsplit(strsplit(i, "/")[[1]][3], "-")[[1]][2]
  all.positive.df = rbind(all.positive.df, i.data)
}

#### Set Groups ####
group1 = c("pdm_cam_sw63_21",	"hpdm_ca_07_09",	"DH1_Brisb_59_007",	"N1_hpdm_Vic_4897_22",
           "CS1_cam_sw54_21",	"CS1_SK_SD56_14",	"EA1_Cam_SW60_21",
           "EA1_PV_65_16",	"H3_Cam_SW64_22",	"H3_Cam_e0826_20",	
           "N1_CS1_Alb_154_16",	"H3_Cam_SW16_20",	"H3_Thai_CU_P53_12",	"N1_EA_SD_1207_16",
           "N2_SZN_CHN_JG20_19", "H5N1_Cam_NPH_23",	"H5N8_Ast_3212_20",	"hpdm_Wis_67_22",
           "H7N9_AH_1_13",	"H9N2_HK_3239_08",	"N2_CS1_Alb_217_17",	"Vic_Au_1359417_21",
           "N1_hpdm_Ca_04_09")



fig.1.list = vector("list", length(group1))
positive.2.save.group.1 = c()
group.1.summary = c()



mod.col = c()
for(a in 1:3){
  for(b in (a+1):4){
    mod.col = rbind(mod.col, c(a, b))
  }
}

for(g in group1){
  positive.df.1 = all.positive.df[all.positive.df$Gp == g, ]
  g.loglik = matrix(1:4, ncol = 1, byrow = FALSE)
  for(i in 1:4){
    fit = Mclust(positive.df.1$MFI, G=i)
    sum.fit = summary(fit)
    g.loglik[i] = sum.fit$loglik
  }
  g.bf = pair.diff(g.loglik)
  best.idx = which.max(g.bf)
  best.i = mod.col[best.idx]
  #best.i = 3

  
  ### Option 2
  #best.i = 8
  ###

  list.of.em.class.nr = vector("list", 1)

 list.of.em.class.nr = c(3, 3, 2, 3, 
                         3, 3, 3, 3, 
                         3, 2, 3, 2, 
                         4, 2, 2, 3, 
                         2, 3, 3, 2,
                         3, 3, 3, 3, 
                         3)
  
  #best.i = list.of.em.class.nr

  #best.fit = Mclust(positive.df.1$MFI, G = best.i[which(g == group1)])
  best.fit = Mclust(positive.df.1$MFI, G=best.i)
  positive.df.1$emClass <- best.fit$classification
  fig = ggplot(data=positive.df.1, aes(x=MFI, color=as.factor(emClass))) +
    geom_histogram(aes(y=after_stat(density)), bins=75)+
    geom_density() +
    ggtitle(paste0(g)) +
    guides(color = guide_legend(title = "emClass")) +
    theme_bw() +
    theme(
          plot.title = element_text(face="bold.italic", size=12)
          )
  fig.1.list[[which(g == group1)]] = fig
  
  g.res = paste0(paste0("Group", names(table(positive.df.1$emClass)), "=", as.numeric(table(positive.df.1$emClass))), collapse = ", ")
  g.sum = paste0(paste0("Group", names(table(positive.df.1$emClass)), "=", round(100 * as.numeric(table(positive.df.1$emClass))  / length(positive.df.1$emClass), 2)), collapse = ", ")
  group.1.summary = rbind(group.1.summary, c(g, g.res, g.sum))
  positive.2.save.group.1 = rbind(positive.2.save.group.1, positive.df.1)
}
write.xlsx(as.data.frame(positive.2.save.group.1), file = "PFC_EM_500X_FINAL_V1_cutoffs.xlsx", rowNames=FALSE)

#### Plot Histograms of EMclass ####
library(tikzDevice)
library(patchwork)

fig.group.1 = fig.1.list[[1]] + fig.1.list[[2]] + fig.1.list[[3]] + fig.1.list[[4]] + fig.1.list[[5]] + 
  fig.1.list[[6]] + fig.1.list[[7]] + fig.1.list[[8]] + fig.1.list[[9]] + fig.1.list[[10]] + 
  fig.1.list[[11]] + fig.1.list[[12]] + fig.1.list[[13]] + fig.1.list[[14]] + fig.1.list[[15]] + 
  fig.1.list[[16]] + fig.1.list[[17]] + fig.1.list[[18]] + fig.1.list[[19]] + fig.1.list[[20]] + 
  fig.1.list[[21]] + fig.1.list[[22]] + fig.1.list[[23]] + plot_layout(ncol = 4, nrow = 7) 

pdf(file = "XXXX.pdf", width=16, height=24)
fig.group.1
dev.off()

#### Annotate Data with Location Info ####
all.positive.df = read.xlsx(xlsxFile = "PFC_EM_500X_FINAL_V1_cutoffs.xlsx")

sample.names.col = all.positive.df$Sample
sample.location.col = sapply(1:length(sample.names.col), function(x) strsplit(sample.names.col[x], "")[[1]][2])
sample.location.col[which(sample.location.col == "D")] = "Kandal"
sample.location.col[which(sample.location.col == "S")] = "Kampong_Speu"
sample.location.col[which(sample.location.col == "K")] = "Takeo"
sample.location.col[which(sample.location.col == "P")] = "Phnom_Penh"
all.positive.df$Location = sample.location.col
write.xlsx(as.data.frame(all.positive.df), file = "PFC_EM_500_Data.xlsx", rowNames=FALSE)

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################

#### Plot the Boxplot/Jitterplot With Controls Labelled ####
group = c("pdm_cam_sw63_21",	"hpdm_ca_07_09",	"DH1_Brisb_59_007",	"N1_hpdm_Vic_4897_22",
          "CS1_cam_sw54_21",	"CS1_SK_SD56_14",	"EA1_Cam_SW60_21",	"EA1_SD_1207_16",
          "EA1_PV_65_16",	"EA1_LN_SY514_20",	"H3_Cam_SW64_22",	"H3_Cam_e0826_20",	
          "N1_CS1_Alb_154_16",	"H3_Cam_SW16_20",	"H3_Thai_CU_P53_12",	"N1_EA_SD_1207_16",
          "N2_SZN_CHN_JG20_19", "H5N1_Cam_NPH_23",	"H5N8_Ast_3212_20",	"hpdm_Wis_67_22",
          "H7N9_AH_1_13",	"H9N2_HK_3239_08",	"N2_CS1_Alb_217_17",	"Vic_Au_1359417_21",
          "N1_hpdm_Ca_04_09")

host.figs = vector("list", 12)
fig.idx = 1
for(this.host in c("Pig")) {
  raw.tab.1 = read.xlsx(xlsxFile = "PFC_EM_500_Data.xlsx", sheet =  this.host)
  df_500X_1 = c()
  em.cutoffs.1 = c()
  for(g in group){
    g.df = raw.tab.1[raw.tab.1$Gp == g, ]
    df_500X_1 = rbind(df_500X_1, g.df)
    em.cutoff = max(as.numeric(g.df$emClass))
    g.df.r = g.df[as.numeric(g.df$emClass) == em.cutoff, ]
    if(em.cutoff == 1) {
      g.df.r$cutoff = max(g.df$MFI)
    } else {
      
      g.df.r1 = g.df[as.numeric(g.df$emClass) == (em.cutoff - 1), ]
      if(max(g.df.r1$MFI) > min(as.numeric(g.df.r$MFI))){ 
        g.df.r = g.df.r[-which(as.numeric(g.df.r$MFI) == min(as.numeric(g.df.r$MFI))),]
      }
      g.df.r$cutoff = min(as.numeric(g.df.r$MFI))
    }
    em.cutoffs.1 = rbind(em.cutoffs.1, g.df.r)
  }
  
######################## 0 positive controls
i.data = read.xlsx(xlsxFile = "Controls.xlsx")
control.df <- melt(i.data)
colnames(control.df)[3] = "MFI"
colnames(control.df)[2] = "Gp"
control.df$sampleLabel = control.df$Sample
control.df.2 <- control.df
  control.mean.df = c()
  for(i in group){
    for(j in unique(control.df$sampleLabel)){
      this.df = control.df[grepl(i, control.df$Gp) & control.df$sampleLabel == j, ]
      this.row =  c(this.df$Sample[1], as.character(this.df$Gp[1]), mean(this.df$MFI), this.df$sampleLabel[1])
      control.mean.df = rbind(control.mean.df, this.row)
    }
  }
  colnames(control.mean.df) = colnames(control.df)
  control.mean.df = as.data.frame(control.mean.df)
  control.mean.df$Gp = factor(control.mean.df$Gp, levels = group)
  df_500X_1$Cutoff <- em.cutoffs.1$cutoff[match(df_500X_1$Gp, em.cutoffs.1$Gp)]
  df_500X_1$Calculation <- df_500X_1$MFI-df_500X_1$Cutoff
  df_500X_1$Shape[df_500X_1$Calculation>0] <- "16"
  df_500X_1$Shape[df_500X_1$Calculation<0] <- "1"
  df_500X_1$Shape <- as.factor(df_500X_1$Shape)
  control.mean.df$Shape <- "17"
  control.mean.df$Shape <- as.factor(control.mean.df$Shape)
  control.df.2$Shape <- "17"
  control.df.2$Shape <- as.factor(control.df.2$Shape)
  write.xlsx(as.data.frame(df_500X_1), file = "DF_500X.xlsx", rowNames=FALSE)

#### Plot Here ####
  fig = ggplot(df_500X_1) + 
    geom_jitter(aes(x=Gp, y=MFI, colour=Gp, shape = Shape), position= position_jitter(width=0.2),show.legend = FALSE, alpha = 1) + 
    geom_boxplot(aes(Gp, MFI), outlier.size=NA, alpha=0, width = 0.2, colour = "gray") +    
    new_scale_color() + 
    scale_shape_manual(values=c(1,16,17)) +
    geom_jitter(data = control.mean.df,aes(x=Gp, y=as.numeric(MFI), shape = Shape), position= position_jitter(width=0.2)) +   
    geom_jitter(data = control.df.2, aes(x=Gp, y=MFI, shape = Shape), position= position_jitter(width=0.2)) +
    geom_errorbar(data=em.cutoffs.1, mapping=aes(x=Gp, ymin= cutoff, ymax= cutoff), linetype = "solid", color="red", linewidth= 1.2, width = 0.6) + 
    scale_x_discrete(guide = guide_axis_nested(delim = "!"), name = "") +
    scale_y_continuous(breaks = seq(0, 30000, by = 2500)) +
    theme_bw() +
    geom_text_repel(data = control.mean.df, aes(Gp, as.numeric(MFI), label=sampleLabel), size=2.3, max.overlaps = 10) +  
    geom_text_repel(data = control.df.2, aes(Gp, as.numeric(MFI), label=sampleLabel), size=2.3,max.overlaps = 10) +
    theme(legend.position="right",
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face="bold.italic", size=12),
          axis.text.x = element_text(angle = 45, hjust=1, size=10)) +
    ggtitle(paste0("PFC_Multiplex Serology, Host = ", this.host, ", EM cutoff"))
  host.figs[[fig.idx]] = fig
  fig.idx = fig.idx + 1
  
}
  
host.fig.2.save = host.figs[[2]] 
ggsave(filename="EMClass_init_V2.pdf", plot = host.fig.2.save, width=400, height=400, units="mm")

#### Export PDF ####
graph2pdf(file="EMClass_init_V2.pdf", width=dev.size(units="px")[[1]]/90, height=dev.size(units="px")[[2]]/90)
dev.off()

#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################

#### Repeat Plotting With Splitting HA and NA ####

#### HA ####
#### Set Function ####
mod.col = c()
for(a in 1:9){
  for(b in (a+1):10){
    mod.col = rbind(mod.col, c(a, b))
  }
}

#### Read in Data ####
excel.files.2.read = Sys.glob("Data/*.xlsx")

all.positive.df = c()
for(i in excel.files.2.read){
  i.data = read_xlsx(path = i)
  i.data = i.data %>% 
    mutate(across(pdm_cam_sw63_21:N1_hpdm_Ca_04_09, ~ifelse(.<0, 1, .))) %>%
    mutate(across(pdm_cam_sw63_21:N1_hpdm_Ca_04_09, ~ifelse(.==0, 1, .))) %>% 
    gather(Gp, MFI, `pdm_cam_sw63_21` : `N1_hpdm_Ca_04_09`)
  i.data$Location =  strsplit(strsplit(i, "/")[[1]][3], "-")[[1]][2]
  all.positive.df = rbind(all.positive.df, i.data)
}

#### Set Groups ####
groupHA = c("pdm_cam_sw63_21",	"hpdm_ca_07_09","hpdm_Wis_67_22",	"DH1_Brisb_59_007",
            "CS1_SK_SD56_14", "CS1_cam_sw54_21","EA1_PV_65_16",	
            "EA1_Cam_SW60_21")

groupHA = c("H3_Cam_SW64_22",	"H3_Cam_e0826_20",	
          "H3_Cam_SW16_20",	"H3_Thai_CU_P53_12",
          "H5N1_Cam_NPH_23",	"H5N8_Ast_3212_20",
          "H7N9_AH_1_13",	"H9N2_HK_3239_08",	"Vic_Au_1359417_21")

fig.1.list = vector("list", length(groupHA))
positive.2.save.group.1 = c()
group.1.summary = c()

for(g in groupHA){
  positive.df.1 = all.positive.df[all.positive.df$Gp == g, ]
  
  ### Option1
  g.loglik = matrix(1:10, ncol = 1, byrow = FALSE)
  for(i in 1:10){
    fit = Mclust(positive.df.1$MFI, G=i)
    sum.fit = summary(fit)
    g.loglik[i] = sum.fit$loglik
  }
  g.bf = pair.diff(g.loglik)
  best.idx = which.max(g.bf)
  #best.i = mod.col[best.idx]
  #best.i = 3
  ######
  
  ### Option 2
  #best.i = 8
  ###
  
  list.of.em.class.nr = vector("list", 1)
  
  
  list.of.em.class.nr = c(3, 3, 2, 2, 
                          3, 3, 3, 3, 
                          2, 3, 2, 2,
                          2, 3, 3, 3,
                          3, 3, 3)
  
  best.i = list.of.em.class.nr
  
  best.fit = Mclust(positive.df.1$MFI, G = best.i[which(g == groupHA)])
  #best.fit = Mclust(positive.df.1$MFI, G=best.i)
  positive.df.1$emClass <- best.fit$classification
  fig = ggplot(data=positive.df.1, aes(x=MFI, color=as.factor(emClass))) +
    geom_histogram(aes(y=after_stat(density)), bins=75)+
    geom_density() +
    ggtitle(paste0(g)) +
    guides(color = guide_legend(title = "emClass")) +
    theme_bw() +
    theme(
      plot.title = element_text(face="bold.italic", size=12)
    )
  fig.1.list[[which(g == groupHA)]] = fig
  
  g.res = paste0(paste0("Group", names(table(positive.df.1$emClass)), "=", as.numeric(table(positive.df.1$emClass))), collapse = ", ")
  g.sum = paste0(paste0("Group", names(table(positive.df.1$emClass)), "=", round(100 * as.numeric(table(positive.df.1$emClass))  / length(positive.df.1$emClass), 2)), collapse = ", ")
  group.1.summary = rbind(group.1.summary, c(g, g.res, g.sum))
  positive.2.save.group.1 = rbind(positive.2.save.group.1, positive.df.1)
}
write.xlsx(as.data.frame(positive.2.save.group.1), file = "PFC_HA_cutoffs.xlsx", rowNames=FALSE)

#### Plot Histograms of EMclass ####
library(tikzDevice)
library(patchwork)

fig.group.1 = fig.1.list[[1]] + fig.1.list[[2]] + fig.1.list[[3]] + fig.1.list[[4]] + fig.1.list[[5]] + 
  fig.1.list[[6]] + fig.1.list[[7]] + fig.1.list[[8]] + fig.1.list[[9]] + plot_layout(ncol = 4, nrow = 7) 

pdf(file = "New_Figures/PFC_HA2_Histograms_FINAL.pdf", width=16, height=24)
fig.group.1
dev.off()

#### Annotate Data with Location Info ####
all.positive.df = read.xlsx(xlsxFile = "PFC_HA_cutoffs.xlsx")

sample.names.col = all.positive.df$Sample
sample.location.col = sapply(1:length(sample.names.col), function(x) strsplit(sample.names.col[x], "")[[1]][2])
sample.location.col[which(sample.location.col == "D")] = "Kandal"
sample.location.col[which(sample.location.col == "S")] = "Kampong_Speu"
sample.location.col[which(sample.location.col == "K")] = "Takeo"
sample.location.col[which(sample.location.col == "P")] = "Phnom_Penh"
all.positive.df$Location = sample.location.col
write.xlsx(as.data.frame(all.positive.df), file = "PFC_HA_Data.xlsx", rowNames=FALSE)

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################

#### Plot the Boxplot/Jitterplot With Controls Labelled ####
group = c("pdm_cam_sw63_21",	"hpdm_ca_07_09","hpdm_Wis_67_22",	"DH1_Brisb_59_007",
            "CS1_SK_SD56_14", "CS1_cam_sw54_21","EA1_PV_65_16",	
            "EA1_Cam_SW60_21")

host.figs = vector("list", 12)
fig.idx = 1
for(this.host in c("Pig")) {
  raw.tab.1 = read.xlsx(xlsxFile = "New_Figures/PFC_HA1_Data.xlsx", sheet =  this.host)
  df_500X_1 = c()
  em.cutoffs.1 = c()
  for(g in group){
    g.df = raw.tab.1[raw.tab.1$Gp == g, ]
    df_500X_1 = rbind(df_500X_1, g.df)
    em.cutoff = max(as.numeric(g.df$emClass))
    g.df.r = g.df[as.numeric(g.df$emClass) == em.cutoff, ]
    if(em.cutoff == 1) {
      g.df.r$cutoff = max(g.df$MFI)
    } else {
      
      g.df.r1 = g.df[as.numeric(g.df$emClass) == (em.cutoff - 1), ]
      if(max(g.df.r1$MFI) > min(as.numeric(g.df.r$MFI))){ 
        g.df.r = g.df.r[-which(as.numeric(g.df.r$MFI) == min(as.numeric(g.df.r$MFI))),]
      }
      g.df.r$cutoff = min(as.numeric(g.df.r$MFI))
    }
    em.cutoffs.1 = rbind(em.cutoffs.1, g.df.r)
  }
  
  ######################## 0 positive controls
  i.data = read.xlsx(xlsxFile = "New_Figures/Controls_HA1X.xlsx")
  control.df <- melt(i.data)
  colnames(control.df)[3] = "MFI"
  colnames(control.df)[2] = "Gp"
  control.df$sampleLabel = control.df$Sample
  control.df.2 <- control.df
  control.mean.df = c()
  for(i in group){
    for(j in unique(control.df$sampleLabel)){
      this.df = control.df[grepl(i, control.df$Gp) & control.df$sampleLabel == j, ]
      this.row =  c(this.df$Sample[1], as.character(this.df$Gp[1]), mean(this.df$MFI), this.df$sampleLabel[1])
      control.mean.df = rbind(control.mean.df, this.row)
    }
  }
  colnames(control.mean.df) = colnames(control.df)
  control.mean.df = as.data.frame(control.mean.df)
  control.mean.df$Gp = factor(control.mean.df$Gp, levels = group)
  df_500X_1$Cutoff <- em.cutoffs.1$cutoff[match(df_500X_1$Gp, em.cutoffs.1$Gp)]
  df_500X_1$Calculation <- df_500X_1$MFI-df_500X_1$Cutoff
  df_500X_1$Shape[df_500X_1$Calculation>0] <- "16"
  df_500X_1$Shape[df_500X_1$Calculation<0] <- "1"
  control.mean.df$Cutoff <- em.cutoffs.1$cutoff[match(control.mean.df$Gp, em.cutoffs.1$Gp)]
  control.mean.df$MFI <- as.numeric(control.mean.df$MFI)
  control.mean.df$Calculation <- control.mean.df$MFI-control.mean.df$Cutoff
  control.mean.df$Positive[control.mean.df$Calculation>0] <- "16"
  control.mean.df$Positive[control.mean.df$Calculation<0] <- "1"
  negative.controls = c("Pig_3196")
  control.mean.df <- control.mean.df %>%
    mutate(Positive = if_else(Sample %in% negative.controls, "16", as.character(Positive)))
  control.mean.df <- subset(control.mean.df,Positive=="16")
  
  
  control.mean.df <- control.mean.df[-c(8)]
  control.mean.df$Shape[control.mean.df$Calculation>0] <- "17"
  control.mean.df$Shape[control.mean.df$Calculation<0] <- "15"
  
  
  control.mean.df <- control.mean.df %>%
    mutate(Shape = if_else(Sample == "CA09", "17", Shape))
  
  df_500X_1$Shape <- as.factor(df_500X_1$Shape)
  control.mean.df$Shape <- as.factor(control.mean.df$Shape)
  control.df.2$Shape <- "17"
  control.df.2$Shape <- as.factor(control.df.2$Shape)
  strain.names = read.xlsx(xlsxFile = "New_Figures/Strain_Names.xlsx")
  control.df.2 <- control.df.2 %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)
  control.mean.df <- control.mean.df %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)
  control.mean.df$MFI <- as.numeric(control.mean.df$MFI)
  control.mean.df$Gp <- as.factor(control.mean.df$Gp)
  df_500X_1 <- df_500X_1 %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)
  em.cutoffs.1 <- em.cutoffs.1 %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)
  df_500X_1$Gp <- factor(df_500X_1$Gp,
                           levels = c("A/swine/Cambodia/PFC63/2021",	"A/California/07/2009","A/Wisconsin/67/2022",
                                      "A/swine/Cambodia/PFC54/2021", "A/swine/Saskatchewan/SD0056/2014",
                                      "A/swine/Cambodia/PFC60/2021", "A/Pavia/65/2016",
                                      "A/Brisbane/59/2007"), ordered = TRUE)

  #### Plot Here ####
  fig = ggplot(df_500X_1) + 
    geom_jitter(data = df_500X_1, aes(x=Gp, y=MFI, colour=Gp, shape = Shape), position= position_jitter(width=0.2),show.legend = FALSE, alpha = 1) + 
    geom_boxplot(aes(Gp, MFI), outlier.size=NA, alpha=0, width = 0.2, colour = "gray") + 
    scale_color_manual(values = c("#F8766D", "#E9842C", "#D69100", "#BC9D00", "#9CA700", "#6FB000", "#00B813", "#00BD61", "#00C08E", "#00C0B4", "#00BDD4",
                                  "#00B5EE", "#00A7FF", "#7F96FF", "#BC81FF", "#E26EF7", "#F763DF", "#FF62BF", "#FF6A9A")) +
    scale_fill_manual(values = c("#F8766D", "#E9842C", "#D69100", "#BC9D00", "#9CA700", "#6FB000", "#00B813", "#00BD61", "#00C08E", "#00C0B4", "#00BDD4",
                                 "#00B5EE", "#00A7FF", "#7F96FF", "#BC81FF", "#E26EF7", "#F763DF", "#FF62BF", "#FF6A9A")) +
    new_scale_color() + 
    geom_jitter(data = control.mean.df, aes(x=Gp, y=MFI, shape = Shape), position = position_jitter(width=0.2)) +   
    scale_shape_manual(values=c(1,16,15,17)) +
    geom_jitter(data = control.mean.df, aes(x=Gp, y=MFI, shape = Shape), position= position_jitter(width=0.2)) +
    geom_errorbar(data=em.cutoffs.1, mapping=aes(x=Gp, ymin= cutoff, ymax= cutoff), linetype = "solid", color="red", linewidth= 1.2, width = 0.6) + 
    scale_x_discrete(guide = guide_axis_nested(delim = "!"), name = "") +
    scale_y_continuous(breaks = seq(0, 15000, by = 2500),limits = c(0, 15000)) +
    theme_bw() +
    labs(x="",y="Median Fluorescence Intensity (MFI)")+
    geom_text_repel(data = control.mean.df, aes(x=Gp, y=as.numeric(MFI), label=sampleLabel), size=2.3, max.overlaps = 30) +  
    #geom_text_repel(data = control.mean.df, aes(Gp, as.numeric(MFI), label=sampleLabel), size=2.3,max.overlaps = 10) +
    theme(legend.position="none",
          panel.grid.major.x = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          plot.title = element_text(face="bold", size=15),
          axis.title.y = element_text(face="bold",size=14),
          axis.text.y = element_text(face="bold", hjust=1, size=10, color="black"),
          axis.text.x = element_text(face="bold", angle =30, hjust=1, size=8, color="black")) +
    ggtitle(paste0(""))
  fig
  host.figs[[fig.idx]] = fig
  fig.idx = fig.idx + 1
  figHA1 <- fig
}

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################

#### Plot the Boxplot/Jitterplot With Controls Labelled ####
group = c("H3_Cam_SW64_22",	"H3_Cam_e0826_20",	
          "H3_Cam_SW16_20",	"H3_Thai_CU_P53_12",
          "H5N1_Cam_NPH_23",	"H5N8_Ast_3212_20",
          "H7N9_AH_1_13",	"H9N2_HK_3239_08",	"Vic_Au_1359417_21")

host.figs = vector("list", 12)
fig.idx = 1
for(this.host in c("Pig")) {
  raw.tab.1 = read.xlsx(xlsxFile = "New_Figures/PFC_HA2_Data.xlsx", sheet =  this.host)
  df_500X_1 = c()
  em.cutoffs.1 = c()
  for(g in group){
    g.df = raw.tab.1[raw.tab.1$Gp == g, ]
    df_500X_1 = rbind(df_500X_1, g.df)
    em.cutoff = max(as.numeric(g.df$emClass))
    g.df.r = g.df[as.numeric(g.df$emClass) == em.cutoff, ]
    if(em.cutoff == 1) {
      g.df.r$cutoff = max(g.df$MFI)
    } else {
      
      g.df.r1 = g.df[as.numeric(g.df$emClass) == (em.cutoff - 1), ]
      if(max(g.df.r1$MFI) > min(as.numeric(g.df.r$MFI))){ 
        g.df.r = g.df.r[-which(as.numeric(g.df.r$MFI) == min(as.numeric(g.df.r$MFI))),]
      }
      g.df.r$cutoff = min(as.numeric(g.df.r$MFI))
    }
    em.cutoffs.1 = rbind(em.cutoffs.1, g.df.r)
  }
  
  ######################## 0 positive controls
  i.data = read.xlsx(xlsxFile = "New_Figures/Controls_HA2X.xlsx")
  control.df <- melt(i.data)
  colnames(control.df)[3] = "MFI"
  colnames(control.df)[2] = "Gp"
  control.df$sampleLabel = control.df$Sample
  control.df.2 <- control.df
  control.mean.df = c()
  for(i in group){
    for(j in unique(control.df$sampleLabel)){
      this.df = control.df[grepl(i, control.df$Gp) & control.df$sampleLabel == j, ]
      this.row =  c(this.df$Sample[1], as.character(this.df$Gp[1]), mean(this.df$MFI), this.df$sampleLabel[1])
      control.mean.df = rbind(control.mean.df, this.row)
    }
  }
  colnames(control.mean.df) = colnames(control.df)
  control.mean.df = as.data.frame(control.mean.df)
  control.mean.df$Gp = factor(control.mean.df$Gp, levels = group)
  df_500X_1$Cutoff <- em.cutoffs.1$cutoff[match(df_500X_1$Gp, em.cutoffs.1$Gp)]
  df_500X_1$Calculation <- df_500X_1$MFI-df_500X_1$Cutoff
  df_500X_1$Shape[df_500X_1$Calculation>0] <- "16"
  df_500X_1$Shape[df_500X_1$Calculation<0] <- "1"
  control.mean.df$Cutoff <- em.cutoffs.1$cutoff[match(control.mean.df$Gp, em.cutoffs.1$Gp)]
  control.mean.df$MFI <- as.numeric(control.mean.df$MFI)
  control.mean.df$Calculation <- control.mean.df$MFI-control.mean.df$Cutoff
  control.mean.df$Positive[control.mean.df$Calculation>0] <- "16"
  control.mean.df$Positive[control.mean.df$Calculation<0] <- "1"
  negative.controls = c("Pig_3196")
  control.mean.df <- control.mean.df %>%
    mutate(Positive = if_else(Sample %in% negative.controls, "16", as.character(Positive)))
  control.mean.df <- subset(control.mean.df,Positive=="16")
  df_500X_1$Shape <- as.factor(df_500X_1$Shape)
  
  control.mean.df <- control.mean.df[-c(8)]
  control.mean.df$Shape[control.mean.df$Calculation>0] <- "17"
  control.mean.df$Shape[control.mean.df$Calculation<0] <- "15"
  
  control.mean.df <- control.mean.df %>%
    mutate(Shape = if_else(Sample == "CA09", "17", Shape))
  
  control.mean.df$Shape <- as.factor(control.mean.df$Shape)
  control.df.2$Shape <- "17"
  control.df.2$Shape <- as.factor(control.df.2$Shape)
  strain.names = read.xlsx(xlsxFile = "New_Figures/Strain_Names.xlsx")
  control.df.2 <- control.df.2 %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)
  control.mean.df <- control.mean.df %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)
  control.mean.df$MFI <- as.numeric(control.mean.df$MFI)
  control.mean.df$Gp <- as.factor(control.mean.df$Gp)
  df_500X_1 <- df_500X_1 %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)
  em.cutoffs.1 <- em.cutoffs.1 %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)

  control.mean.df$MFI <- abs(control.mean.df$MFI)
  df_500X_1$Gp <- factor(df_500X_1$Gp,
                         levels = c("A/swine/Cambodia/PFC64/2022",	"A/Cambodia/e0826360/2020","A/swine/Cambodia/PFC16/2020",
                                    "A/swine/Thailand/CU-P53/2012", "A/Cambodia/NPH230032/2023",
                                    "A/Astrakhan/3212/2020","A/Anhui/1/2013", "A/Hong Kong/3239/2008", "B/Austria/1359417/2021"), ordered = TRUE)
  
  #### Plot Here ####
  fig = ggplot(df_500X_1) + 
    geom_jitter(data = df_500X_1, aes(x=Gp, y=MFI, colour=Gp, shape = Shape), position= position_jitter(width=0.2),show.legend = FALSE, alpha = 1) + 
    geom_boxplot(aes(Gp, MFI), outlier.size=NA, alpha=0, width = 0.2, colour = "gray") + 
    scale_color_manual(values = c("#00BDD4","#00B5EE", "#00A7FF", "#7F96FF", "#BC81FF", "#E26EF7", "#F763DF", "#FF62BF", "#FF6A9A")) +
    scale_fill_manual(values = c("#00BDD4","#00B5EE", "#00A7FF", "#7F96FF", "#BC81FF", "#E26EF7", "#F763DF", "#FF62BF", "#FF6A9A")) +
    new_scale_color() + 
    geom_jitter(data = control.mean.df, aes(x=Gp, y=MFI, shape = Shape), position = position_jitter(width=0.2)) +   
    scale_shape_manual(values=c(1,16,15,17)) +
    geom_jitter(data = control.mean.df, aes(x=Gp, y=MFI, shape = Shape), position= position_jitter(width=0.2)) +
    geom_errorbar(data=em.cutoffs.1, mapping=aes(x=Gp, ymin= cutoff, ymax= cutoff), linetype = "solid", color="red", linewidth= 1.2, width = 0.6) + 
    scale_x_discrete(guide = guide_axis_nested(delim = "!"), name = "") +
    scale_y_continuous(breaks = seq(0, 15000, by = 2500),limits = c(0, 15000)) +
    theme_bw() +
    labs(x="",y="Median Fluorescence Intensity (MFI)")+
    geom_text_repel(data = control.mean.df, aes(x=Gp, y=as.numeric(MFI), label=sampleLabel), size=2.3, max.overlaps = 30) +  
    #geom_text_repel(data = control.mean.df, aes(Gp, as.numeric(MFI), label=sampleLabel), size=2.3,max.overlaps = 10) +
    theme(legend.position="none",
          panel.grid.major.x = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          plot.title = element_text(face="bold", size=15),
          axis.title.y = element_text(face="bold",size=14),
          axis.text.y = element_text(face="bold", hjust=1, size=10, color="black"),
          axis.text.x = element_text(face="bold", angle =30, hjust=1, size=8, color="black")) +
    ggtitle(paste0(""))
  fig
  host.figs[[fig.idx]] = fig
  fig.idx = fig.idx + 1
  figHA2 <- fig
}

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################


#### NA ####
#### Set Function ####
mod.col = c()
for(a in 1:9){
  for(b in (a+1):10){
    mod.col = rbind(mod.col, c(a, b))
  }
}

#### Read in Data ####
excel.files.2.read = Sys.glob("Data/*.xlsx")

all.positive.df = c()
for(i in excel.files.2.read){
  i.data = read_xlsx(path = i)
  i.data = i.data %>% 
    mutate(across(pdm_cam_sw63_21:N1_hpdm_Ca_04_09, ~ifelse(.<0, 1, .))) %>%
    mutate(across(pdm_cam_sw63_21:N1_hpdm_Ca_04_09, ~ifelse(.==0, 1, .))) %>% 
    gather(Gp, MFI, `pdm_cam_sw63_21` : `N1_hpdm_Ca_04_09`)
  i.data$Location =  strsplit(strsplit(i, "/")[[1]][3], "-")[[1]][2]
  all.positive.df = rbind(all.positive.df, i.data)
}

#### Set Groups ####
groupNA = c("N1_hpdm_Ca_04_09", "N1_hpdm_Vic_4897_22", "N1_CS1_Alb_154_16", "N1_EA_SD_1207_16", 
            "N2_CS1_Alb_217_17", "N2_SZN_CHN_JG20_19")

fig.1.list = vector("list", length(groupNA))
positive.2.save.group.1 = c()
group.1.summary = c()

for(g in groupNA){
  positive.df.1 = all.positive.df[all.positive.df$Gp == g, ]
  
  ### Option1
  g.loglik = matrix(1:10, ncol = 1, byrow = FALSE)
  for(i in 1:10){
    fit = Mclust(positive.df.1$MFI, G=i)
    sum.fit = summary(fit)
    g.loglik[i] = sum.fit$loglik
  }
  g.bf = pair.diff(g.loglik)
  best.idx = which.max(g.bf)
  #best.i = mod.col[best.idx]
  #best.i = 3
  ######
  
  ### Option 2
  #best.i = 8
  ###
  
  list.of.em.class.nr = vector("list", 1)
  
  
  list.of.em.class.nr = c(3, 3, 4, 3, 
                          2, 3)
  
  best.i = list.of.em.class.nr
  
  best.fit = Mclust(positive.df.1$MFI, G = best.i[which(g == groupNA)])
  #best.fit = Mclust(positive.df.1$MFI, G=best.i)
  positive.df.1$emClass <- best.fit$classification
  fig = ggplot(data=positive.df.1, aes(x=MFI, color=as.factor(emClass))) +
    geom_histogram(aes(y=after_stat(density)), bins=75)+
    geom_density() +
    ggtitle(paste0(g)) +
    guides(color = guide_legend(title = "emClass")) +
    theme_bw() +
    theme(
      plot.title = element_text(face="bold.italic", size=12)
    )
  fig.1.list[[which(g == groupNA)]] = fig
  
  g.res = paste0(paste0("Group", names(table(positive.df.1$emClass)), "=", as.numeric(table(positive.df.1$emClass))), collapse = ", ")
  g.sum = paste0(paste0("Group", names(table(positive.df.1$emClass)), "=", round(100 * as.numeric(table(positive.df.1$emClass))  / length(positive.df.1$emClass), 2)), collapse = ", ")
  group.1.summary = rbind(group.1.summary, c(g, g.res, g.sum))
  positive.2.save.group.1 = rbind(positive.2.save.group.1, positive.df.1)
}
write.xlsx(as.data.frame(positive.2.save.group.1), file = "PFC_NA_cutoffs.xlsx", rowNames=FALSE)

#### Plot Histograms of EMclass ####
library(tikzDevice)
library(patchwork)

fig.group.1 = fig.1.list[[1]] + fig.1.list[[2]] + fig.1.list[[3]] + fig.1.list[[4]] + fig.1.list[[5]] + 
  fig.1.list[[6]] + plot_layout(ncol = 4, nrow = 7) 

pdf(file = "New_Figures/PFC_NA_Histograms_FINAL.pdf", width=16, height=24)
fig.group.1
dev.off()

#### Annotate Data with Location Info ####
all.positive.df = read.xlsx(xlsxFile = "PFC_NA_cutoffs.xlsx")

sample.names.col = all.positive.df$Sample
sample.location.col = sapply(1:length(sample.names.col), function(x) strsplit(sample.names.col[x], "")[[1]][2])
sample.location.col[which(sample.location.col == "D")] = "Kandal"
sample.location.col[which(sample.location.col == "S")] = "Kampong_Speu"
sample.location.col[which(sample.location.col == "K")] = "Takeo"
sample.location.col[which(sample.location.col == "P")] = "Phnom_Penh"
all.positive.df$Location = sample.location.col
write.xlsx(as.data.frame(all.positive.df), file = "New_Figures/PFC_NA_Data.xlsx", rowNames=FALSE)

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################

#### Plot the Boxplot/Jitterplot With Controls Labelled ####
group = c("N1_hpdm_Ca_04_09", "N1_hpdm_Vic_4897_22", "N1_CS1_Alb_154_16", "N1_EA_SD_1207_16", 
            "N2_CS1_Alb_217_17", "N2_SZN_CHN_JG20_19")

host.figs = vector("list", 12)
fig.idx = 1
for(this.host in c("Pig")) {
  raw.tab.1 = read.xlsx(xlsxFile = "New_Figures/PFC_NA_Data.xlsx", sheet =  this.host)
  df_500X_1 = c()
  em.cutoffs.1 = c()
  for(g in group){
    g.df = raw.tab.1[raw.tab.1$Gp == g, ]
    df_500X_1 = rbind(df_500X_1, g.df)
    em.cutoff = max(as.numeric(g.df$emClass))
    g.df.r = g.df[as.numeric(g.df$emClass) == em.cutoff, ]
    if(em.cutoff == 1) {
      g.df.r$cutoff = max(g.df$MFI)
    } else {
      
      g.df.r1 = g.df[as.numeric(g.df$emClass) == (em.cutoff - 1), ]
      if(max(g.df.r1$MFI) > min(as.numeric(g.df.r$MFI))){ 
        g.df.r = g.df.r[-which(as.numeric(g.df.r$MFI) == min(as.numeric(g.df.r$MFI))),]
      }
      g.df.r$cutoff = min(as.numeric(g.df.r$MFI))
    }
    em.cutoffs.1 = rbind(em.cutoffs.1, g.df.r)
  }
  
  ######################## 0 positive controls
  i.data = read.xlsx(xlsxFile = "New_Figures/Controls_NA.xlsx")
  control.df <- melt(i.data)
  colnames(control.df)[3] = "MFI"
  colnames(control.df)[2] = "Gp"
  control.df$sampleLabel = control.df$Sample
  control.df.2 <- control.df
  control.mean.df = c()
  for(i in group){
    for(j in unique(control.df$sampleLabel)){
      this.df = control.df[grepl(i, control.df$Gp) & control.df$sampleLabel == j, ]
      this.row =  c(this.df$Sample[1], as.character(this.df$Gp[1]), mean(this.df$MFI), this.df$sampleLabel[1])
      control.mean.df = rbind(control.mean.df, this.row)
    }
  }
  colnames(control.mean.df) = colnames(control.df)
  control.mean.df = as.data.frame(control.mean.df)
  control.mean.df$Gp = factor(control.mean.df$Gp, levels = group)
  df_500X_1$Cutoff <- em.cutoffs.1$cutoff[match(df_500X_1$Gp, em.cutoffs.1$Gp)]
  df_500X_1$Calculation <- df_500X_1$MFI-df_500X_1$Cutoff
  df_500X_1$Shape[df_500X_1$Calculation>0] <- "16"
  df_500X_1$Shape[df_500X_1$Calculation<0] <- "1"
  control.mean.df$Cutoff <- em.cutoffs.1$cutoff[match(control.mean.df$Gp, em.cutoffs.1$Gp)]
  control.mean.df$MFI <- as.numeric(control.mean.df$MFI)
  control.mean.df$Calculation <- control.mean.df$MFI-control.mean.df$Cutoff
  control.mean.df$Positive[control.mean.df$Calculation>0] <- "16"
  control.mean.df$Positive[control.mean.df$Calculation<0] <- "1"
  negative.controls = c("Pig_3196")
  control.mean.df <- control.mean.df %>%
    mutate(Positive = if_else(Sample %in% negative.controls, "16", as.character(Positive)))
  control.mean.df <- subset(control.mean.df,Positive=="16")
  df_500X_1$Shape <- as.factor(df_500X_1$Shape)
  
  control.mean.df <- control.mean.df[-c(8)]
  control.mean.df$Shape[control.mean.df$Calculation>0] <- "17"
  control.mean.df$Shape[control.mean.df$Calculation<0] <- "15"
  
  control.mean.df <- control.mean.df %>%
    mutate(Shape = if_else(Sample == "CA09", "17", Shape))
  
  control.mean.df$Shape <- as.factor(control.mean.df$Shape)
  control.df.2$Shape <- "17"
  control.df.2$Shape <- as.factor(control.df.2$Shape)
  strain.names = read.xlsx(xlsxFile = "Strain_Names_NA.xlsx")
  control.df.2 <- control.df.2 %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)
  control.mean.df <- control.mean.df %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)
  control.mean.df$MFI <- as.numeric(control.mean.df$MFI)
  control.mean.df$Gp <- as.factor(control.mean.df$Gp)
  df_500X_1 <- df_500X_1 %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)
  em.cutoffs.1 <- em.cutoffs.1 %>%
    left_join(strain.names, by = "Gp") %>%
    mutate(Gp = Isolates)

  df_500X_1$Gp <- factor(df_500X_1$Gp,
                         levels = c("A/California/07/2009",	"A/Victoria/4897/2022"," A/swine/Alberta/SD0154/2016",
                                    "A/swine/Shandong/1207/2016", "A/swine/Alberta/SD0217/2017",
                                    "A/swine/China/JG20/2019"), ordered = TRUE)
  
  brewer.pal(6, "Set1")
  
  #### Plot Here ####
  fig = ggplot(df_500X_1) + 
    geom_jitter(data = df_500X_1, aes(x=Gp, y=MFI, colour=Gp, shape = Shape), position= position_jitter(width=0.2),show.legend = FALSE, alpha = 1) + 
    geom_boxplot(aes(Gp, MFI), outlier.size=NA, alpha=0, width = 0.2, colour = "gray") + 
    #scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "", "", "",)) +
    #scale_fill_manual(values = c()) +
    new_scale_color() + 
    geom_jitter(data = control.mean.df, aes(x=Gp, y=MFI, shape = Shape), position = position_jitter(width=0.2)) +   
    scale_shape_manual(values=c(1,16,15,17)) +
    geom_jitter(data = control.mean.df, aes(x=Gp, y=MFI, shape = Shape), position= position_jitter(width=0.2)) +
    geom_errorbar(data=em.cutoffs.1, mapping=aes(x=Gp, ymin= cutoff, ymax= cutoff), linetype = "solid", color="red", linewidth= 1.2, width = 0.6) + 
    scale_x_discrete(guide = guide_axis_nested(delim = "!"), name = "") +
    scale_y_continuous(breaks = seq(0, 30000, by = 2500)) +
    theme_bw() +
    labs(x="",y="Median Fluorescence Intensity (MFI)")+
    geom_text_repel(data = control.mean.df, aes(x=Gp, y=as.numeric(MFI), label=sampleLabel), size=2.3, max.overlaps = 30) +  
    #geom_text_repel(data = control.mean.df, aes(Gp, as.numeric(MFI), label=sampleLabel), size=2.3,max.overlaps = 10) +
    theme(legend.position="none",
          panel.grid.major.x = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          plot.title = element_text(face="bold", size=15),
          axis.title.y = element_text(face="bold",size=14),
          axis.text.y = element_text(face="bold", hjust=1, size=10, color="black"),
          axis.text.x = element_text(face="bold", angle =30, hjust=1, size=8, color="black")) +
    ggtitle(paste0(""))
  fig
  host.figs[[fig.idx]] = fig
  fig.idx = fig.idx + 1
  figNA <- fig
}

host.fig.2.save = figHA1 / figHA2 / figNA 
ggsave(filename="XXX.pdf", plot = host.fig.2.save, width=210, height=297, units="mm")

host.fig.2.save = figHA1 / figHA2 
ggsave(filename="YYY.pdf", plot = host.fig.2.save, width=210, height=297, units="mm")

host.fig.2.save = figNA
ggsave(filename="NAAAAAAAA.pdf", plot = host.fig.2.save, width=210, height=150, units="mm")


###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################

#### Only Positives Data - All Proteins ####
df.1 = read.xlsx(xlsxFile = "Overall_Data_500X.xlsx", rowNames = TRUE)
cutoffs = read.xlsx(xlsxFile = "Cutoffs.xlsx", rowNames = TRUE)

group = c("pdm_cam_sw63_21",	"hpdm_ca_07_09",	"DH1_Brisb_59_007",	"N1_hpdm_Vic_4897_22",
           "CS1_cam_sw54_21",	"CS1_SK_SD56_14",	"EA1_Cam_SW60_21",	"EA1_SD_1207_16",
           "EA1_PV_65_16",	"EA1_LN_SY514_20",	"H3_Cam_SW64_22",	"H3_Cam_e0826_20",	
           "N1_CS1_Alb_154_16",	"H3_Cam_SW16_20",	"H3_Thai_CU_P53_12",	"N1_EA_SD_1207_16",
           "N2_SZN_CHN_JG20_19", "H5N1_Cam_NPH_23",	"H5N8_Ast_3212_20",	"hpdm_Wis_67_22",
           "H7N9_AH_1_13",	"H9N2_HK_3239_08",	"N2_CS1_Alb_217_17",	"Vic_Au_1359417_21",
           "N1_hpdm_Ca_04_09")

for(i in group){
  df.1[[i]][df.1[[i]] < cutoffs[[i]]] <- 0
}
for(i in group){
  df.1[[i]][df.1[[i]] > cutoffs[[i]]] <- 1
}
for(i in group){
  df.1[[i]][df.1[[i]] == cutoffs[[i]]] <- 1
}

#df.2 <- df.1[apply(df.1[,-1], 1, function(x) !all(x==0)),]
df.2 <- df.1
#write.xlsx(as.data.frame(df.2), file = "Sero_Positives_Negatives.xlsx", rowNames=TRUE)
metadata <- df.2[c(1)]
sample.names.col = rownames(metadata)
sample.location.col = sapply(1:length(sample.names.col), function(x) strsplit(sample.names.col[x], "")[[1]][2])
sample.location.col[which(sample.location.col == "D")] = "Kandal"
sample.location.col[which(sample.location.col == "S")] = "Kampong_Speu"
sample.location.col[which(sample.location.col == "K")] = "Takeo"
sample.location.col[which(sample.location.col == "P")] = "Phnom_Penh"
metadata$Location = sample.location.col
metadata <- metadata[-c(1)]
total <- df.2
total <- tibble::rownames_to_column(total, "Sample")
library(janitor)
total.1 <- total %>%
  adorn_totals("row")
total.2 <- total.1[c(957) ,]
total.2 <- total.2[-c(1)]
rownames(total.2)[rownames(total.2) == "957"] <- "Positives"
new_row <- rep(1236, ncol(total.2))
total.3 <- rbind(total.2, new_row)
rownames(total.3)[rownames(total.3) == "2"] <- "Total"
total.3 <- as.data.frame(t(total.3))
total.3$Percentage <- total.3$Positives/total.3$Total*100
write.xlsx(as.data.frame(total.3), file = "Percentage_Prevalence_All_Proteins.xlsx", rowNames=TRUE)
location <- cbind(metadata, df.2)
location.1 <- subset(location, Location=="Phnom_Penh")
location.1 <- location.1 %>%
  adorn_totals("row")
PP <- location.1[c(292), ]
new_row <- rep(291, ncol(location.1))
PP <- rbind(PP, new_row)
PP <- PP[-c(1)]
rownames(PP)[rownames(PP) == "1"] <- "Positives_PP"
rownames(PP)[rownames(PP) == "2"] <- "Total_PP"
PP <- as.data.frame(t(PP))
PP$Percentage <- PP$Positives_PP/PP$Total_PP*100
location.2 <- subset(location, Location=="Kandal")
location.2 <- location.2 %>%
  adorn_totals("row")
KL <- location.2[c(236), ]
new_row <- rep(235, ncol(location.2))
KL <- rbind(KL, new_row)
KL <- KL[-c(1)]
rownames(KL)[rownames(KL) == "1"] <- "Positives_KL"
rownames(KL)[rownames(KL) == "2"] <- "Total_KL"
KL <- as.data.frame(t(KL))
KL$Percentage <- KL$Positives_KL/KL$Total_KL*100
location.3 <- subset(location, Location=="Takeo")
location.3 <- location.3 %>%
  adorn_totals("row")
TK <- location.3[c(320), ]
new_row <- rep(319, ncol(location.3))
TK <- rbind(TK, new_row)
TK <- TK[-c(1)]
rownames(TK)[rownames(TK) == "1"] <- "Positives_TK"
rownames(TK)[rownames(TK) == "2"] <- "Total_TK"
TK <- as.data.frame(t(TK))
TK$Percentage <- TK$Positives_TK/TK$Total_TK*100
location.4 <- subset(location, Location=="Kampong_Speu")
location.4 <- location.4 %>%
  adorn_totals("row")
KS <- location.4[c(392), ]
new_row <- rep(391, ncol(location.4))
KS <- rbind(KS, new_row)
KS <- KS[-c(1)]
rownames(KS)[rownames(KS) == "1"] <- "Positives_KS"
rownames(KS)[rownames(KS) == "2"] <- "Total_KS"
KS <- as.data.frame(t(KS))
KS$Percentage <- KS$Positives_KS/KS$Total_KS*100
location.prev <- cbind(PP,KL,TK,KS)
write.xlsx(as.data.frame(location.prev), file = "XXXXXXXX.xlsx", rowNames=TRUE)

#### Logistic Regression ####
model.metadata <- metadata
regression.data <- cbind(model.metadata, df.2)
model.metadata <- setdiff(colnames(regression.data)[1], c("Location"))
proteins <- setdiff(colnames(regression.data)[2:26], c("Location"))
regression.data$Location <- as.factor(regression.data$Location)

# Get all pairwise combinations of Location
location_pairs <- combn(levels(regression.data$Location), 2, simplify = FALSE)

# Create data frames to store estimates, p-values, and q-values for each pairwise comparison
estimates <- as.data.frame(matrix(NA, nrow = length(proteins), ncol = length(location_pairs)))
p_values <- as.data.frame(matrix(NA, nrow = length(proteins), ncol = length(location_pairs)))
q_values <- as.data.frame(matrix(NA, nrow = length(proteins), ncol = length(location_pairs)))
rownames(estimates) <- proteins
rownames(p_values) <- proteins
rownames(q_values) <- proteins
colnames(estimates) <- sapply(location_pairs, function(pair) paste(pair, collapse = "_vs_"))
colnames(p_values) <- sapply(location_pairs, function(pair) paste(pair, collapse = "_vs_"))
colnames(q_values) <- sapply(location_pairs, function(pair) paste(pair, collapse = "_vs_"))

# Loop over each pair of locations
for (pair_idx in seq_along(location_pairs)) {
  pair <- location_pairs[[pair_idx]]
  
  # Subset the data for the current pair of locations
  pair_data <- regression.data[regression.data$Location %in% pair, ]
  
  for (i in 1:length(proteins)) {
    regression.proteins <- proteins[i]
    t_fit <- glm(as.formula(paste0("as.factor(Location) ~ ", regression.proteins)), data = pair_data, family = "binomial")
    summary_t_fit <- summary(t_fit)
    estimates[i, pair_idx] <- summary_t_fit$coefficients[nrow(summary_t_fit$coefficients), 1]
    p_values[i, pair_idx] <- summary_t_fit$coefficients[nrow(summary_t_fit$coefficients), 4]
  }
}

# Adjust p-values to get q-values
for (i in 1:nrow(p_values)) {
  q_values[i, ] <- p.adjust(p_values[i, ], method = "fdr")
}
write.xlsx(as.data.frame(q_values), file = "Logistic_Regression_Location_Q_Values.xlsx", rowNames=TRUE)
write.xlsx(as.data.frame(estimates), file = "Logistic_Regression_Location_Estimates.xlsx", rowNames=TRUE)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

#### Spearman Correlations ---- H1 versus H3 Profile ####
df.2 = read.xlsx(xlsxFile = "Sero_Positives_Negatives.xlsx", rowNames = TRUE)
df.2 <- as.data.frame(t(df.2))
#correlations <- corr.test(df.2, adjust="fdr",method="spearman")
cor.r <- as.data.frame(correlations$r)
cor.r[is.na(cor.r)] <- 0  
cor.heatmap <- heatmap.2(as.matrix(cor.r),
          key = FALSE,
          margins=c(25,25),density="none",trace="none",
          col=brewer.pal(8,"RdBu"),rowCol="white",
          ColSideColors=c("#F8766D","#00BFC4")[as.factor(sero.profiles$Group)],
          #RowSideColors=c("red","blue","purple","green")[as.factor(sero.group.meta$Location)],
          labRow = FALSE,
          labCol = FALSE)

sero.profiles <- cutree(cor.heatmap$colDendrogram,2)                           
sero.profiles <- as.data.frame(sero.profiles)
colnames(sero.profiles)[colnames(sero.profiles) == "sero.profiles"] <- "Group"
df.2 <- as.data.frame(t(df.2))
sero.group <- cbind(sero.profiles, df.2)
sero.group.1 <- subset(sero.group, Group=="1")
library(janitor)
sero.group.1 <- sero.group.1 %>%
  adorn_totals("row")
G1 <- sero.group.1[c(605), ]
new_row <- rep(604, ncol(sero.group.1))
G1 <- rbind(G1, new_row)
G1 <- G1[-c(1)]
rownames(G1)[rownames(G1) == "1"] <- "Positives_G1"
rownames(G1)[rownames(G1) == "2"] <- "Total_G1"
G1 <- as.data.frame(t(G1))
G1$Percentage <- G1$Positives_G1/G1$Total_G1*100

sero.group.2 <- subset(sero.group, Group=="2")
library(janitor)
sero.group.2 <- sero.group.2 %>%
  adorn_totals("row")
G2 <- sero.group.2[c(353), ]
new_row <- rep(352, ncol(sero.group.2))
G2 <- rbind(G2, new_row)
G2 <- G2[-c(1)]
rownames(G2)[rownames(G2) == "1"] <- "Positives_G2"
rownames(G2)[rownames(G2) == "2"] <- "Total_G2"
G2 <- as.data.frame(t(G2))
G2$Percentage <- G2$Positives_G2/G2$Total_G2*100
sero.porfile.prev <- cbind(G1,G2)
#write.xlsx(as.data.frame(sero.porfile.prev), file = "Sero_Profile_Prevalence_Percentages.xlsx", rowNames=TRUE)
sero.group.meta <- cbind(metadata, sero.profiles)
#write.xlsx(as.data.frame(sero.group.meta), file = "Sero_Profile_Metadata.xlsx", rowNames=TRUE)

#### Wilcox Test Between Groups ####
library(effsize)
wilcox_batch = function(x,y)
{
  P_Value <- NULL;
  Directionality <- NULL;
  Mean1 <- NULL;
  Mean2 <- NULL;
  Effect_Size <- NULL
  Magnitude <- NULL
  #x <- x[abs(rowSums(x,na.rm=TRUE)) > 0,];
  #y <- y[abs(rowSums(y,na.rm=TRUE)) > 0,];
  z <- intersect(rownames(x),rownames(y));
  for(i in 1:length(z))
  {
    P_Value[i] <- wilcox.test(as.numeric(x[z[i],]),as.numeric(y[z[i],]))$p.value;
    Directionality[i] <- ifelse(mean(as.numeric(x[z[i],]),na.rm=TRUE) > mean(as.numeric(y[z[i],]),na.rm=TRUE), 1, ifelse(mean(as.numeric(x[z[i],]),na.rm=TRUE) < mean(as.numeric(y[z[i],]),na.rm=TRUE),-1,0));
    Mean1[i] <- mean(as.numeric(x[z[i],]),na.rm=TRUE);
    Mean2[i] <- mean(as.numeric(y[z[i],]),na.rm=TRUE);
    temp_effsize <- effsize::cohen.d(as.numeric(x[z[i],]),as.numeric(y[z[i],]))
    Effect_Size[i] <- temp_effsize$estimate
    Magnitude[i] <- temp_effsize$magnitude
    i <- i + 1;
  }
  out <- as.data.frame(cbind(P_Value,Directionality,p.adjust(P_Value),Mean1,Mean2,Effect_Size, Magnitude));
  rownames(out) <- z;
  out <- apply(out,1,function(x)(ifelse(is.nan(x),1,x)));
  return(t(out));
}

df.2 = read.xlsx(xlsxFile = "New_Figures/Sero_Positives_Negatives.xlsx", rowNames = TRUE)
sero.meta = read.xlsx(xlsxFile = "New_Figures/Sero_Profile_Metadata.xlsx", rowNames = TRUE)
#df.2 <- as.data.frame(t(df.2))
G1 <- subset(sero.meta, Group=="1")
G2 <- subset(sero.meta, Group=="2")
G1.names <- row.names(G1)
G2.names <- row.names(G2)
Wilcox <- wilcox_batch(t(df.2[c(G1.names),c(colnames(df.2))]),t(df.2[c(G2.names),c(colnames(df.2))]))
Wilcox <- as.data.frame(Wilcox)
write.xlsx(as.data.frame(Wilcox), file = "Sero_Profile_Wilcox_Test.xlsx", rowNames=TRUE)

#### Plot Different HA Profiles ####
df.ha = read.xlsx(xlsxFile = "New_Figures/DF_HA_500X.xlsx", rowNames = FALSE)
sero.meta = read.xlsx(xlsxFile = "New_Figures/Sero_Profile_Metadata.xlsx", rowNames = TRUE)

group1_HA = c("pdm_cam_sw63_21",	"hpdm_ca_07_09",	"DH1_Brisb_59_007",
          "CS1_cam_sw54_21",	"CS1_SK_SD56_14",	"EA1_Cam_SW60_21",
          "EA1_PV_65_16",
          "hpdm_Wis_67_22")

group2_HA = c("H3_Cam_SW64_22",	"H3_Cam_e0826_20",	
          "H3_Cam_SW16_20",	"H3_Thai_CU_P53_12")

df.2 = read.xlsx(xlsxFile = "New_Figures/Sero_Positives_Negatives.xlsx", rowNames = TRUE)
sero.meta = read.xlsx(xlsxFile = "New_Figures/Sero_Profile_Metadata.xlsx", rowNames = TRUE)
df.2 <- tibble::rownames_to_column(df.2, "Sample")
df.melt <- melt(df.2, id.vars = "Sample")
colnames(df.melt)[colnames(df.melt) == "variable"] <- "Gp"
sero.meta <- tibble::rownames_to_column(sero.meta, "Sample")
df.melt$Group <- sero.meta$Group[match(df.melt$Sample, sero.meta$Sample)]
df.melt.1 <- subset(df.melt, Gp %in% group1_HA)
df.melt.2 <- subset(df.melt, Gp %in% group2_HA)
df.melt.1$Group[df.melt.1$Group == "1"] <- "H1N1"
df.melt.1$Group[df.melt.1$Group == "2"] <- "H3N2"
df.melt.1$Group <- as.factor(df.melt.1$Group)
df.melt.2$Group[df.melt.2$Group == "1"] <- "H1N1"
df.melt.2$Group[df.melt.2$Group == "2"] <- "H3N2"
df.melt.2$Group <- as.factor(df.melt.2$Group)
df.combined <- rbind(df.melt.1, df.melt.2)
strain.names = read.xlsx(xlsxFile = "New_Figures/Strain_Names.xlsx")
df.combined <- df.combined %>%
  left_join(strain.names, by = "Gp") %>%
  mutate(Gp = Isolates)
df.combined <- df.combined %>%
  filter(!(Gp %in% c("A/swine/Liaoning/SY514/2020", "A/swine/Shandong/1207/2016")))

df.combined$Gp <- factor(df.combined$Gp,
                                      levels = c("A/swine/Cambodia/PFC63/2021",	"A/California/07/2009",	"A/Wisconsin/67/2022", "A/swine/Cambodia/PFC54/2021",
                                                 "A/swine/Saskatchewan/SD0056/2014",	"A/swine/Cambodia/PFC60/2021",
                                                 "A/Pavia/65/2016",	"A/Brisbane/59/2007",
                                                 "A/swine/Cambodia/PFC64/2022",	"A/Cambodia/e0826360/2020",	
                                                 "A/swine/Cambodia/PFC16/2020",	"A/swine/Thailand/CU-P53/2012"), ordered = TRUE)

HAs_Barplot <- ggplot(data = df.combined) +
  geom_bar(aes(Group, y = as.numeric(value), fill=Group), stat="identity", size = 0.5,  position = "stack") +
  scale_fill_manual(values = c("H1N1" = "#F8766D", "H3N2" = "#00BFC4")) +
  labs(title="", subtitle="", x="", y="Seropositives") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(title="", reverse=TRUE)) +
  facet_grid(~ Gp, scales = "free_x", space = "free_x", switch = "x") +
  theme(panel.background = element_rect(fill='white'),
        axis.line.y = element_line(linetype = 1, size = 1, colour = 'black'),
        axis.line.x = element_line(linetype = 1, size = 1, colour = 'black'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", size=12, colour = "black"),
        axis.title.y = element_text(face="bold", size=12, angle=90),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(face="bold", size=10, colour = "black", angle = 30, vjust = 1, hjust = 1),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position="none",
        legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(colour="black", size = 10, face = "bold.italic"),
        plot.title = element_text(size=20, hjust = 0.5, face = "bold"),
        plot.margin=unit(c(20,20,20,20),"mm"))

HAs_Barplot

host.fig.2.save = HAs_Barplot
ggsave(filename="New_Figures/Fig2_Barplot.pdf", plot = host.fig.2.save, width=210, height=200, units="mm")

#### Total Summary of H1 and H3 Seropositives ####
df.combined <- df.combined %>%
  mutate(HA_Type = if_else(Gp %in% group1_HA, "H1", NA_character_)) %>%
  replace_na(list(HA_Type = "H3"))

Summary_HAs_Barplot <- ggplot(data = df.combined) +
  geom_bar(aes(Group, y = as.numeric(value), fill=Group), stat="identity", size = 0.5,  position = "stack") +
  scale_fill_manual(values = c("H1N1" = "#F8766D", "H3N2" = "#00BFC4")) +
  labs(title="", subtitle="", x="", y="Seropositives") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(title="", reverse=TRUE)) +
  facet_grid(~ HA_Type, scales = "free_x", space = "free_x", switch = "x") +
  theme(panel.background = element_rect(fill='white'),
        axis.line.y = element_line(linetype = 1, size = 1.5, colour = 'black'),
        axis.line.x = element_line(linetype = 1, size = 1.5, colour = 'black'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", size=15, colour = "black"),
        axis.title.y = element_text(face="bold", size=15, angle=90),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(face="bold", size=10, colour = "black", angle = 90, vjust = 1, hjust = 1),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position="none",
        legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(colour="black", size = 13, face = "bold.italic"),
        plot.title = element_text(size=20, hjust = 0.5, face = "bold"),
        plot.margin=unit(c(10,10,10,10),"mm"))

Summary_HAs_Barplot

#### Percentages Area Plot ####
sero.percentage = read.xlsx(xlsxFile = "New_Figures/Sero_Group_Percentage_Data.xlsx", rowNames = TRUE)
sero.percentage <- tibble::rownames_to_column(sero.percentage, "Gp")
sero.melt <- melt(sero.percentage, id.vars = "Gp")
sero.melt.1 <- subset(sero.melt, Gp %in% group1_HA)
sero.melt.1$Group <- sero.melt.1$variable
sero.melt.1$Group <- as.character(sero.melt.1$Group)
sero.melt.1$Group[sero.melt.1$Group == "H1N1_Positives"] <- "H1N1"
sero.melt.1$Group[sero.melt.1$Group == "H1N1_Negatives"] <- "H1N1"
sero.melt.1$Group[sero.melt.1$Group == "H3N2_Positives"] <- "H3N2"
sero.melt.1$Group[sero.melt.1$Group == "H3N2_Negatives"] <- "H3N2"
sero.melt.1.h1n1 <- subset(sero.melt.1, Group=="H1N1")
sero.melt.1.h3n2 <- subset(sero.melt.1, Group=="H3N2")
sero.melt.2 <- subset(sero.melt, Gp %in% group2_HA)
sero.melt.2$Group <- sero.melt.2$variable
sero.melt.2$Group <- as.character(sero.melt.2$Group)
sero.melt.2$Group[sero.melt.2$Group == "H1N1_Positives"] <- "H1N1"
sero.melt.2$Group[sero.melt.2$Group == "H1N1_Negatives"] <- "H1N1"
sero.melt.2$Group[sero.melt.2$Group == "H3N2_Positives"] <- "H3N2"
sero.melt.2$Group[sero.melt.2$Group == "H3N2_Negatives"] <- "H3N2"
sero.melt.2.h1n1 <- subset(sero.melt.2, Group=="H1N1")
sero.melt.2.h3n2 <- subset(sero.melt.2, Group=="H3N2")
sero.combined.h1n1 <- rbind(sero.melt.1.h1n1, sero.melt.2.h1n1)
sero.combined.h3n2 <- rbind(sero.melt.1.h3n2, sero.melt.2.h3n2)
strain.names = read.xlsx(xlsxFile = "New_Figures/Strain_Names.xlsx")
sero.combined.h1n1 <- sero.combined.h1n1 %>%
  left_join(strain.names, by = "Gp") %>%
  mutate(Gp = Isolates)
sero.combined.h1n1 <- sero.combined.h1n1 %>%
  filter(!(Gp %in% c("A/swine/Liaoning/SY514/2020", "A/swine/Shandong/1207/2016")))
sero.combined.h1n1$Gp <- factor(sero.combined.h1n1$Gp,
                         levels = c("A/swine/Cambodia/PFC63/2021",	"A/California/07/2009",	"A/Wisconsin/67/2022", "A/swine/Cambodia/PFC54/2021",
                                    "A/swine/Saskatchewan/SD0056/2014",	"A/swine/Cambodia/PFC60/2021",
                                    "A/Pavia/65/2016",	"A/Brisbane/59/2007",
                                    "A/swine/Cambodia/PFC64/2022",	"A/Cambodia/e0826360/2020",	
                                    "A/swine/Cambodia/PFC16/2020",	"A/swine/Thailand/CU-P53/2012"), ordered = TRUE)
sero.combined.h3n2 <- sero.combined.h3n2 %>%
  left_join(strain.names, by = "Gp") %>%
  mutate(Gp = Isolates)
sero.combined.h3n2 <- sero.combined.h3n2 %>%
  filter(!(Gp %in% c("A/swine/Liaoning/SY514/2020", "A/swine/Shandong/1207/2016")))
sero.combined.h3n2$Gp <- factor(sero.combined.h3n2$Gp,
                                levels = c("A/swine/Cambodia/PFC63/2021",	"A/California/07/2009",	"A/Wisconsin/67/2022", "A/swine/Cambodia/PFC54/2021",
                                           "A/swine/Saskatchewan/SD0056/2014",	"A/swine/Cambodia/PFC60/2021",	"A/swine/Liaoning/SY514/2020",	"A/swine/Shandong/1207/2016",
                                           "A/Pavia/65/2016",	"A/Brisbane/59/2007",
                                           "A/swine/Cambodia/PFC64/2022",	"A/Cambodia/e0826360/2020",	
                                           "A/swine/Cambodia/PFC16/2020",	"A/swine/Thailand/CU-P53/2012"), ordered = TRUE)
sero.combined.h1n1$variable <- factor(sero.combined.h1n1$variable,
                                levels = c("H1N1_Negatives", "H1N1_Positives"), ordered = TRUE)
sero.combined.h3n2$variable <- factor(sero.combined.h3n2$variable,
                                      levels = c("H3N2_Negatives", "H3N2_Positives"), ordered = TRUE)

plot.h1n1 <- sero.combined.h1n1 %>% ggplot(aes(y = value, x = Gp))+
  geom_area(aes(color = variable, group = variable, fill = variable), position = "stack", alpha = 0.8, na.rm = T, lineend = "round", size = 2)+
  scale_fill_manual(values = c("H1N1_Positives" = "#7AB6D8", "H1N1_Negatives" = "#F69FAD"))+
  scale_colour_manual(values = c("H1N1_Positives" = "#7AB6D8", "H1N1_Negatives" = "#F69FAD"))+
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))+
  scale_x_discrete(expand = c(0, 0))+
  labs(title="",subtitle="",x="",y="Percentage (%)")+
  guides(fill = guide_legend(title="",reverse=TRUE))+
  theme(panel.background = element_rect(fill='white'),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position ="none",
        legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold", color = "black"),
        axis.line.y = element_line(linetype = 1,size = 1,colour = 'black'),
        axis.line.x = element_line(linetype = 1,size = 1,colour = 'black'),
        axis.text.x = element_text(size=10,face="bold",color="black", angle = 45, hjust=1, vjust=1),
        axis.text.y = element_text(size=10,face="bold",color="black"),
        axis.title.y = element_text(face="bold",size=12),
        axis.title.x = element_text(face="bold",size=12),
        plot.margin = unit(c(0,4,0,4), "cm"))

plot.h3n2 <- sero.combined.h3n2 %>% ggplot(aes(y = value, x = Gp))+
  geom_area(aes(color = variable, group = variable, fill = variable), position = "stack", alpha = 0.8, na.rm = T, lineend = "round", size = 2)+
  scale_fill_manual(values = c("H3N2_Positives" = "#7AB6D8", "H3N2_Negatives" = "#F69FAD"))+
  scale_colour_manual(values = c("H3N2_Positives" = "#7AB6D8", "H3N2_Negatives" = "#F69FAD"))+
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))+
  scale_x_discrete(expand = c(0, 0))+
  labs(title="",subtitle="",x="",y="Percentage (%)")+
  guides(fill = guide_legend(title="",reverse=TRUE))+
  theme(panel.background = element_rect(fill='white'),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position ="none",
        legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold", color = "black"),
        axis.line.y = element_line(linetype = 1,size = 1,colour = 'black'),
        axis.line.x = element_line(linetype = 1,size = 1,colour = 'black'),
        axis.text.x = element_text(size=10,face="bold",color="black", angle = 45, hjust=1, vjust=1),
        axis.text.y = element_text(size=10,face="bold",color="black"),
        axis.title.y = element_text(face="bold",size=12),
        axis.title.x = element_text(face="bold",size=15),
        plot.margin = unit(c(0,4,0,4), "cm"))

host.fig.2.save = plot.h1n1 / plot.h3n2
ggsave(filename="Supp_Fig2_Area_Plot.pdf", plot = host.fig.2.save, width=210, height=297, units="mm")

#### Summary Percentages - Stacked - Barplot ####
sero.combined.all <- rbind(sero.combined.h1n1, sero.combined.h3n2)
sero.combined.all <- sero.combined.all %>%
  mutate(HA_Type = if_else(Gp %in% group1_HA, "H1", NA_character_)) %>%
  replace_na(list(HA_Type = "H3"))

sero.summary = read.xlsx(xlsxFile = "Summary_Percenatge_SeroProfiles.xlsx", rowNames = FALSE)
sero.positive <- subset(sero.summary, Type=="Positive")

Summary_HAs_Barplot <- ggplot(data = sero.positive) +
  geom_bar(aes(Group, y = as.numeric(Percenatge), fill=Group), stat="identity", size = 0.5,  position = "stack") +
  #scale_fill_manual(values = c("H1N1" = "#F8766D", "H3N2" = "#00BFC4")) +
  labs(title="", subtitle="", x="", y="Percentage (%)") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(title="", reverse=TRUE)) +
  facet_grid(~ X1, scales = "free_x", space = "free_x", switch = "x") +
  theme(panel.background = element_rect(fill='white'),
        axis.line.y = element_line(linetype = 1, size = 1, colour = 'black'),
        axis.line.x = element_line(linetype = 1, size = 1, colour = 'black'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", size=15, colour = "black"),
        axis.title.y = element_text(face="bold", size=15, angle=90),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(face="bold", size=10, colour = "black", angle = 90, vjust = 1, hjust = 1),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position="none",
        legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(colour="black", size = 13, face = "bold.italic"),
        plot.title = element_text(size=20, hjust = 0.5, face = "bold"),
        plot.margin=unit(c(10,10,10,10),"mm"))

Summary_HAs_Barplot

#### Calculate Sero Profiles Distance ####
df.2 = read.xlsx(xlsxFile = "New_Figures/Sero_Positives_Negatives.xlsx", rowNames = TRUE)
sero.group.meta = read.xlsx(xlsxFile = "New_Figures/Sero_Profile_Metadata.xlsx", rowNames = TRUE)

df.2 <- as.data.frame(t(df.2))
proteins = otu_table(df.2 ,taxa_are_rows = TRUE)
physeq = phyloseq(proteins)
physeq = t(physeq)
jaccard = vegdist(physeq, method = "jaccard")
jaccard <- as.dist(jaccard)
Jaccard_PCoA <- dudi.pco(jaccard,scannf=FALSE, nf=3)
Jaccard_Li <- Jaccard_PCoA$li
Jaccard_Li$Group <- sero.group.meta[rownames(Jaccard_Li),"Group"]
Jaccard_Adonis <- adonis2(jaccard ~ sero.group.meta$Group, permutations = 1000)
pairwise.adonis <- function(x,factors, sim.method = 'bray', p.adjust.m ='fdr')
{
  library(vegan)
  co = combn(unique(factors),2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  for(elem in 1:ncol(co)){
    ad = adonis(x[factors %in% c(co[1,elem],co[2,elem]),] ~ factors[factors %in% c(co[1,elem],co[2,elem])] , method =sim.method);
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted)
  return(pairw.res)
}
library(pairwiseAdonis)
Jaccard_Pairwise_PERMANOVA <- pairwiseAdonis::pairwise.adonis(jaccard, sero.group.meta[,"Group"], p.adjust.m = "fdr")

#### Get eigen values for each axis #########
Jaccard_PCoA$eig[1]/sum(Jaccard_PCoA$eig)*100 #Axis1
Jaccard_PCoA$eig[2]/sum(Jaccard_PCoA$eig)*100 #Axis2

Jaccard_Li$Group[Jaccard_Li$Group == "1"] <- "H1N1"
Jaccard_Li$Group[Jaccard_Li$Group == "2"] <- "H3N2"
Jaccard_Li$Group <- as.factor(Jaccard_Li$Group)

Jaccard_Centroid <- Jaccard_Li[-c(3)]
G1 <- subset(Jaccard_Centroid, Group=="H1N1")
G2 <- subset(Jaccard_Centroid, Group=="H3N2")
G1_X_Median <- mean(G1$A1)
G1_Y_Median <- mean(G1$A2)
G2_X_Median <- mean(G2$A1)
G2_Y_Median <- mean(G2$A2)

Jaccard_Sero <- ggplot(Jaccard_Li, aes(x = A1, y = A2, color = Group)) +
  geom_point(mapping = aes(x = A1, y = A2),size = 3) +
  coord_fixed()+
  geom_segment(data = G1, x = -0.2028144, y =  0.006577972, mapping = aes(xend = A1, yend = A2), linewidth = 1)+
  geom_segment(data = G2, x = 0.3480111, y =  -0.0112872, mapping = aes(xend = A1, yend = A2), linewidth = 1)+
  #stat_ellipse(type = "t", level = 0.2, aes(fill = Group, alpha=0.5), geom = "polygon") +
  #scale_color_manual(values = c("H1N1" = "#F8766D", "H3N2" = "#00BFC4"))+
  #scale_fill_manual(values = c("1" = "#F8766D","2" = "#00BFC4"))+
  theme_classic() + 
  labs(title="",subtitle="P<0.0001***, R2=0.213",x = "PCo1 (26.16%)", y = "PCo2 (13.11%)")+
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.line.y = element_line(linetype = 1,size = 1,colour = 'black'),
        axis.line.x = element_line(linetype = 1,size = 1,colour = 'black'),
        axis.title.y = element_text(face="bold",size=12),
        axis.title.x = element_text(face="bold",size=12),
        axis.text.x = element_text(face="bold",size=10,color="black"),
        axis.text.y = element_text(face="bold",size=10,color="black"),
        legend.position="none")

Jaccard_Sero

#### Calculating Cross_Reactivity ####
sero.df = read.xlsx(xlsxFile = "New_Figures/Sero_Positives_Negatives.xlsx", rowNames = TRUE)

group_HA = c("pdm_cam_sw63_21",	"hpdm_ca_07_09",	"DH1_Brisb_59_007",
           "CS1_cam_sw54_21",	"CS1_SK_SD56_14",	"EA1_Cam_SW60_21",
           "EA1_PV_65_16",	"H3_Cam_SW64_22",	"H3_Cam_e0826_20",	
           "H3_Cam_SW16_20",	"H3_Thai_CU_P53_12",
           "H5N1_Cam_NPH_23",	"H5N8_Ast_3212_20",	"hpdm_Wis_67_22",
           "H7N9_AH_1_13",	"H9N2_HK_3239_08", "Vic_Au_1359417_21")

#### Set Function ####
shared_positives <- function(df) {
  n <- ncol(sero.df)
  result <- matrix(0, n, n)
  colnames(result) <- colnames(df)
  rownames(result) <- colnames(df)
  
  for (i in 1:n) {
    for (j in i:n) {
      shared <- sum(df[, i] & df[, j])
      result[i, j] <- shared
      result[j, i] <- shared
    }
  }
  return(as.data.frame(result))
}
shared_positives_matrix <- shared_positives(sero.df)
shared_positives_long <- shared_positives_matrix %>%
  rownames_to_column(var = "Protein1") %>%
  pivot_longer(-Protein1, names_to = "Protein2", values_to = "SharedPositives") %>%
  filter(Protein1 != Protein2)
network.edges <- shared_positives_long
total_positives <- colSums(sero.df)
total_positives_df <- data.frame(
  Protein = names(total_positives),
  TotalSum = total_positives
)
network.nodes <- total_positives_df
unique <- !duplicated(t(apply(network.edges,  1, sort)))
network.edges <- network.edges[unique, ]
network.edges.ha <- subset(network.edges, Protein1 %in% group_HA)
network.edges.ha <- subset(network.edges.ha, Protein2 %in% group_HA)
network.nodes.ha <- subset(network.nodes, Protein %in% group_HA)
network.edges.ha <- network.edges.ha %>%
  left_join(total_positives_df, by = c("Protein1" = "Protein")) %>%
  mutate(PercentageShared = (SharedPositives / TotalSum) * 100)
#### Get Percentages as well ####
#network.edges.ha <- network.edges.ha %>%
  #left_join(total_positives_df, by = c("Protein1" = "Protein")) %>%
  #mutate(PercentageShared = (SharedPositives / TotalSum) * 100)
#write.xlsx(as.data.frame(network.edges.ha), file = "New_Figures/Cross_Reactivity_Network_Edges_HA.xlsx", rowNames=TRUE)
#write.xlsx(as.data.frame(network.nodes.ha), file = "New_Figures/Cross_Reactivity_Network_Nodes_HA.xlsx", rowNames=TRUE)

#### Filters NAs on their own --- Chord Diagram ####
group_NA = c("N1_hpdm_Ca_04_09", "N1_hpdm_Vic_4897_22", "N1_CS1_Alb_154_16", "N1_EA_SD_1207_16", "N2_CS1_Alb_217_17", "N2_SZN_CHN_JG20_19")
network.edges.na <- subset(network.edges, Protein1 %in% group_NA)
network.edges.na <- subset(network.edges.na, Protein2 %in% group_NA)
network.nodes.na <- subset(network.nodes, Protein %in% group_NA)
#write.xlsx(as.data.frame(network.edges), file = "Cross_Reactivity_Network_Edges.xlsx", rowNames=TRUE)
#write.xlsx(as.data.frame(network.nodes), file = "Cross_Reactivity_Network_Nodes.xlsx", rowNames=TRUE)
par(cex = 2, font = 2)
circos.par(gap.after = c( "N1_hpdm_Vic_4897_22" = 5, "N1_CS1_Alb_154_16" = 5,
                          "N1_EA_SD_1207_16" = 5, "N2_SZN_CHN_JG20_19" = 5, 
                          "N2_CS1_Alb_217_17" = 5, "N1_hpdm_Ca_04_09" = 5))
grid.col = c("N1_hpdm_Vic_4897_22" = "mediumpurple1", "N1_EA_SD_1207_16" = "#D55E00",
             "N1_CS1_Alb_154_16" = "#CC79A7", "N2_SZN_CHN_JG20_19" = "#0072B2", 
             "N2_CS1_Alb_217_17" = "#F0E442", "N1_hpdm_Ca_04_09" = "#009E73")

chordDiagram(network.edges.na, 
             grid.col = grid.col,
             transparency = 0.5, 
             link.lwd = 1, 
             #link.border = link.border,
             annotationTrack = c("name","grid"),
             annotationTrackHeight = c(0.01, 0.1))
circos.clear()

#### Heatmap ####
protein.group = read.xlsx(xlsxFile = "New_Figures/Protein_Groups.xlsx", rowNames = TRUE)
shared_positives_matrix <- shared_positives(sero.df)
Colv <- as.dendrogram(hclust(dist(shared_positives_matrix)))
shared_positives_matrix <- shared_positives_matrix[, col_clust$order]
shared_positives_matrix <- shared_positives_matrix[col_clust$order, ]

# Read the Excel file
protein.group <- read.xlsx(xlsxFile = "New_Figures/Protein_Groups.xlsx", rowNames = TRUE)

# Generate shared positives matrix
shared_positives_matrix <- shared_positives(sero.df)

# Perform hierarchical clustering
col_clust <- hclust(dist(shared_positives_matrix))  
Colv <- as.dendrogram(col_clust)

# Apply clustering order
shared_positives_matrix <- shared_positives_matrix[, col_clust$order]
shared_positives_matrix <- shared_positives_matrix[col_clust$order, ]

strain.names = read.xlsx(xlsxFile = "New_Figures/Strain_Names2.xlsx", rowNames = TRUE)
common_elements <- intersect(rownames(shared_positives_matrix), group_HA)
filtered_matrix <- shared_positives_matrix[common_elements, common_elements]
shared_positives_matrix <- filtered_matrix
strain.names$Isolates <- make.unique(as.character(strain.names$Isolates))
isolate_names <- setNames(strain.names$Isolates, strain.names$Gp)
rownames(shared_positives_matrix) <- isolate_names[rownames(shared_positives_matrix)]
colnames(shared_positives_matrix) <- isolate_names[colnames(shared_positives_matrix)]
new_order <- c("A/swine/Cambodia/PFC63/2021",	"A/California/07/2009","A/Wisconsin/67/2022",
               "A/swine/Cambodia/PFC54/2021", "A/swine/Saskatchewan/SD0056/2014",
               "A/swine/Cambodia/PFC60/2021","A/Pavia/65/2016",
               "A/Brisbane/59/2007", "A/swine/Cambodia/PFC64/2022",	"A/Cambodia/e0826360/2020","A/swine/Cambodia/PFC16/2020",
               "A/swine/Thailand/CU-P53/2012", "A/Cambodia/NPH230032/2023",
               "A/Astrakhan/3212/2020","A/Anhui/1/2013", "A/Hong Kong/3239/2008", "B/Austria/1359417/2021")
shared_positives_matrix_x <- shared_positives_matrix[new_order, new_order]
shared_positives_matrix_x[upper.tri(shared_positives_matrix_x)] <- NA

cross.reactive.heatmap <- heatmap.2(as.matrix(shared_positives_matrix_x),
                                    key = FALSE,
                                    Colv = FALSE,
                                    Rowv = FALSE,
                                    margins = c(25, 25),
                                    cellnote = shared_positives_matrix_x,
                                    notecol = "black",
                                    notecex = 1,
                                    density.info = "none",
                                    trace = "none",
                                    sepwidth = c(2, 2),
                                    sepcolor = "black",
                                    #ColSideColors=c("#0072B2","#009E73","#D55E00","#CC79A7", "#E69F00", "#F0E442", "yellow", "purple", "grey", "black")[as.factor(protein.group$Group)],
                                    col = brewer.pal(5, "Blues"),
                                    rowCol = "white",
                                    labCol = as.expression(lapply(rownames(shared_positives_matrix_x), function(a) bquote(bold(.(a))))),
                                    labRow=as.expression(lapply(rownames(shared_positives_matrix_x), function(a) bquote(bold(.(a))))),
                                    dendrogram = "col",
                                    na.color = "white")  


shared_positives_matrix <- shared_positives(sero.df)
common_elements <- intersect(rownames(shared_positives_matrix), group_NA)
filtered_matrix <- shared_positives_matrix[common_elements, common_elements]
shared_positives_matrix <- filtered_matrix
strain.names$Isolates <- make.unique(as.character(strain.names$Isolates))
isolate_names <- setNames(strain.names$Isolates, strain.names$Gp)
rownames(shared_positives_matrix) <- isolate_names[rownames(shared_positives_matrix)]
colnames(shared_positives_matrix) <- isolate_names[colnames(shared_positives_matrix)]
new_order <- c("A/California/07/2009.1", "A/Victoria/4897/2022", "A/swine/Alberta/SD0154/2016",
                                    "A/swine/Shandong/1207/2016.1","A/swine/Alberta/SD0217/2017",
                                    "A/swine/China/JG20/2019")
shared_positives_matrix_x <- shared_positives_matrix[new_order, new_order]
shared_positives_matrix_x[upper.tri(shared_positives_matrix_x)] <- NA

missing_names <- setdiff(new_order, rownames(shared_positives_matrix))
if (length(missing_names) > 0) {
  stop("The following names are missing in the matrix: ", paste(missing_names, collapse = ", "))
}

# Example new_order
new_order <- c("A/California/07/2009.1", "A/Victoria/4897/2022", "A/swine/Alberta/SD0154/2016",
               "A/swine/Shandong/1207/2016.1", "A/swine/Alberta/SD0217/2017", "A/swine/China/JG20/2019")

# Ensure row and column names are trimmed of any leading or trailing whitespace
rownames(shared_positives_matrix) <- trimws(rownames(shared_positives_matrix))
colnames(shared_positives_matrix) <- trimws(colnames(shared_positives_matrix))

# Check if all names in new_order are present in the row and column names of shared_positives_matrix
missing_names <- setdiff(new_order, rownames(shared_positives_matrix))
if (length(missing_names) > 0) {
  stop("The following names are missing in the matrix: ", paste(missing_names, collapse = ", "))
}

# Reorder the matrix
shared_positives_matrix_x <- shared_positives_matrix[new_order, new_order]

# View the resulting matrix
print(shared_positives_matrix_x)
cross.reactive.heatmap <- heatmap.2(as.matrix(shared_positives_matrix_x),
                                    key = FALSE,
                                    Colv = FALSE,
                                    Rowv = FALSE,
                                    margins = c(25, 25),
                                    cellnote = shared_positives_matrix_x,
                                    notecol = "black",
                                    notecex = 1,
                                    density.info = "none",
                                    trace = "none",
                                    sepwidth = c(2, 2),
                                    sepcolor = "black",
                                    #ColSideColors=c("#0072B2","#009E73","#D55E00","#CC79A7", "#E69F00", "#F0E442", "yellow", "purple", "grey", "black")[as.factor(protein.group$Group)],
                                    col = brewer.pal(5, "Blues"),
                                    rowCol = "white",
                                    labCol = as.expression(lapply(rownames(shared_positives_matrix_x), function(a) bquote(bold(.(a))))),
                                    labRow=as.expression(lapply(rownames(shared_positives_matrix_x), function(a) bquote(bold(.(a))))),
                                    dendrogram = "col",
                                    na.color = "white")  

#### Now Calculate By Group ####
sero.df = read.xlsx(xlsxFile = "Sero_Positives_Negatives.xlsx", rowNames = TRUE)
protein.group = read.xlsx(xlsxFile = "Protein_Groups.xlsx", rowNames = TRUE)

sero.df <- as.data.frame(t(sero.df))
sero.new = aggregate(sero.df, protein.group["Group"], FUN = sum)
sero.new <- sero.new %>% remove_rownames %>% column_to_rownames(var="Group")
sero.new <- as.data.frame(t(sero.new))

#### Set Function ####
shared_positives <- function(df) {
  n <- ncol(sero.new)
  result <- matrix(0, n, n)
  colnames(result) <- colnames(df)
  rownames(result) <- colnames(df)
  
  for (i in 1:n) {
    for (j in i:n) {
      shared <- sum(df[, i] & df[, j])
      result[i, j] <- shared
      result[j, i] <- shared
    }
  }
  return(as.data.frame(result))
}
shared_positives_matrix <- shared_positives(sero.new)
shared_positives_long <- shared_positives_matrix %>%
  rownames_to_column(var = "Protein1") %>%
  pivot_longer(-Protein1, names_to = "Protein2", values_to = "SharedPositives") %>%
  filter(Protein1 != Protein2)
network.edges <- shared_positives_long
total_positives <- colSums(sero.df)
total_positives_df <- data.frame(
  Protein = names(total_positives),
  TotalSum = total_positives
)
network.nodes <- total_positives_df
unique <- !duplicated(t(apply(network.edges,  1, sort)))
network.edges <- network.edges[unique, ]
#write.xlsx(as.data.frame(network.edges), file = "Group_Cross_Reactivity_Network_Edges.xlsx", rowNames=TRUE)
#write.xlsx(as.data.frame(network.nodes), file = "Group_Cross_Reactivity_Network_Nodes.xlsx", rowNames=TRUE)
meta <- shared_positives_matrix
chord <- network.edges[c(1,2,3)]

#### Plot  ####
chordDiagram(chord, 
             #grid.col = grid.col,
             transparency = 0, 
             link.lwd = 1, 
             #link.border = link.border,
             annotationTrack = c("name","grid"),
             annotationTrackHeight = c(0.01, 0.1))

circos.clear()
dev.off()

#### Plot Different HA Profiles ####
df.ha = read.xlsx(xlsxFile = "DF_HA_500X.xlsx", rowNames = FALSE)
sero.meta = read.xlsx(xlsxFile = "Sero_Profile_Metadata.xlsx", rowNames = TRUE)

group1_HA = c("N1_hpdm_Ca_04_09", "N1_hpdm_Vic_4897_22", "N1_CS1_Alb_154_16", "N1_EA_SD_1207_16")

group2_HA = c("N2_CS1_Alb_217_17", "N2_SZN_CHN_JG20_19")

df.2 = read.xlsx(xlsxFile = "Sero_Positives_Negatives.xlsx", rowNames = TRUE)
sero.meta = read.xlsx(xlsxFile = "Sero_Profile_Metadata.xlsx", rowNames = TRUE)
df.2 <- tibble::rownames_to_column(df.2, "Sample")
df.melt <- melt(df.2, id.vars = "Sample")
colnames(df.melt)[colnames(df.melt) == "variable"] <- "Gp"
sero.meta <- tibble::rownames_to_column(sero.meta, "Sample")
df.melt$Group <- sero.meta$Group[match(df.melt$Sample, sero.meta$Sample)]
df.melt.1 <- subset(df.melt, Gp %in% group1_HA)
df.melt.2 <- subset(df.melt, Gp %in% group2_HA)
df.melt.1$Group[df.melt.1$Group == "1"] <- "H1N1"
df.melt.1$Group[df.melt.1$Group == "2"] <- "H3N2"
df.melt.1$Group <- as.factor(df.melt.1$Group)
df.melt.2$Group[df.melt.2$Group == "1"] <- "H1N1"
df.melt.2$Group[df.melt.2$Group == "2"] <- "H3N2"
df.melt.2$Group <- as.factor(df.melt.2$Group)
df.combined <- rbind(df.melt.1, df.melt.2)
write.xlsx(df.combined, file = "H1_H3_Barplot_Data.xlsx")
df.combined$Gp <- factor(df.combined$Gp,
                         levels = c("N1_hpdm_Ca_04_09", "N1_hpdm_Vic_4897_22", "N1_CS1_Alb_154_16", "N1_EA_SD_1207_16",
                                    "N2_CS1_Alb_217_17", "N2_SZN_CHN_JG20_19"), ordered = TRUE)

HAs_Barplot <- ggplot(data = df.combined) +
  geom_bar(aes(Group, y = as.numeric(value), fill=Group), stat="identity", size = 0.5,  position = "stack") +
  scale_fill_manual(values = c("H1N1" = "#F8766D", "H3N2" = "#00BFC4")) +
  labs(title="", subtitle="", x="", y="Seropositives") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(title="", reverse=TRUE)) +
  facet_grid(~ Gp, scales = "free_x", space = "free_x", switch = "x") +
  theme(panel.background = element_rect(fill='white'),
        axis.line.y = element_line(linetype = 1, size = 1.5, colour = 'black'),
        axis.line.x = element_line(linetype = 1, size = 1.5, colour = 'black'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", size=15, colour = "black"),
        axis.title.y = element_text(face="bold", size=15, angle=90),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(face="bold", size=10, colour = "black", angle = 90, vjust = 1, hjust = 1),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position="none",
        legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(colour="black", size = 13, face = "bold.italic"),
        plot.title = element_text(size=20, hjust = 0.5, face = "bold"),
        plot.margin=unit(c(10,10,10,10),"mm"))

HAs_Barplot

#================================================
#### Heatmap of MFI Values
#================================================
setwd("/Users/peterc/NUS Dropbox/Peter Brian Cronin/Research_Fellow/Pig_Serology/01.Rebuttal_January_2026")

HA1 <- read.xlsx("PFC_HA1_Data.xlsx")
HA2 <- read.xlsx("PFC_HA2_Data.xlsx")
NAX <- read.xlsx("PFC_NA_Data.xlsx")
Strain_Names <- read.xlsx("Strain_Names.xlsx")
Protein_Groups <- read.xlsx("Protein_Groups.xlsx")

HA1$Gp <- Strain_Names$Isolates[
  match(HA1$Gp, Strain_Names$Gp)
]
HA2$Gp <- Strain_Names$Isolates[
  match(HA2$Gp, Strain_Names$Gp)
]
NAX$Gp <- Strain_Names$Isolates[
  match(NAX$Gp, Strain_Names$Gp)
]
Protein_Groups$ID <- Strain_Names$Isolates[
  match(Protein_Groups$ID, Strain_Names$Gp)
]
HA1$Cutoff <- Strain_Names$Cutoff[
  match(HA1$Gp, Strain_Names$Isolates)
]
HA2$Cutoff <- Strain_Names$Cutoff[
  match(HA2$Gp, Strain_Names$Isolates)
]
NAX$Cutoff <- Strain_Names$Cutoff[
  match(NAX$Gp, Strain_Names$Isolates)
]

HA1 <- HA1[HA1$MFI >= HA1$Cutoff, ]
HA2 <- HA2[HA2$MFI >= HA2$Cutoff, ]
NAX <- NAX[NAX$MFI >= NAX$Cutoff, ]

HA <- rbind(HA1, HA2)

mat_df <- reshape2::acast(
  HA,
  Sample ~ Gp,
  value.var = "MFI",
  fun.aggregate = mean,
  fill = 0
)
mat_df <- as.data.frame(mat_df)



rank_scale <- function(x)
{
  x <- rank(x);
  y <- (rank(x)-min(rank(x)))/(max(rank(x))-min(rank(x)));
  return(y);
}

mat_rank <- apply(mat_df,2,rank_scale)
mat_df <- mat_rank

mat <- as.matrix(mat_df)
magma_no_black <- c(
  "white",
  magma(5)[c(2, 3, 4, 5)]   
)

col_fun <- colorRamp2(
  c(0, 0.25, 0.5, 0.75, 1),
  magma_no_black
)

magma_no_black <- c(
  "#4A4A4A",                
  magma(5)[c(2, 3, 4, 5)]
)

col_fun <- colorRamp2(
  c(0, 0.25, 0.5, 0.75, 1),
  magma_no_black
)

ht <- Heatmap(
  mat,
  name = "MFI",
  col = col_fun,
  cluster_rows = TRUE,
  cluster_columns = TRUE,
  show_row_names = FALSE,
  show_column_names = TRUE,
  column_names_rot = 45,
  column_names_gp = gpar(fontsize = 10),
  
  heatmap_legend_param = list(
    title = "Rank(MFI)",
    at = c(0, 0.25, 0.5, 0.75, 1),          
    labels = c("0", "0.25", "0.5", "0.75", "1"),
    legend_height = unit(40, "mm"),
    title_position = "topcenter"
  )
)

draw(ht)
width_mm  <- 200   
height_mm <- 400

pdf(
  file   = "MFI_heatmap.pdf",
  width  = width_mm / 25.4,  
  height = height_mm / 25.4,
  useDingbats = FALSE         
)

draw(ht)
dev.off()















