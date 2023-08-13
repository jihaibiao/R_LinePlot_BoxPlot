library(lsr)
library(ggplot2)
library(dbplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(tidyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggplot2)
library(simr)
library(plyr)

# 加载数据
data_fnirs <- readxl::read_excel("/Users/mac/Desktop/data_fnirs.xlsx")
data_fnirs$condition <- factor(data_fnirs$condition, levels = c("D2W1.2", "D6W1.2", "D2W0.4", "D6W0.4"))

# 将name列转换为因子
data_fnirs$name <- as.factor(data_fnirs$name)

# 对BA4和BA6分别进行分析
variables <- c("BA4", "BA6")
titles <- c("BA4 Activation", "BA6 Activation")
plots <- list()

for (i in 1:2) {
  var <- variables[i]
  # 使用lmer进行混合效应模型分析
  fit <- lmer(paste(var, "~ condition + (1|name)"), data = data_fnirs)
  print(summary(fit))
  
  # 使用aov进行方差分析
  formula_aov <- as.formula(paste(var, "~ condition"))
  fit_aov <- aov(formula_aov, data = data_fnirs)
  print(summary(fit_aov))
  
  # 计算均值和标准差
  aov_data <- data_fnirs %>%
    group_by(condition) %>%
    get_summary_stats(var, type = "mean_sd")
  
  # 绘图
  p <- ggplot(aov_data, aes(x=condition, y=mean)) + 
    geom_point(aes(group=condition), position=position_dodge(width=0.3)) + 
    geom_line(aes(group=condition), color="black", position=position_dodge(width=0.3)) + 
    geom_errorbar(aes(x=condition, ymin=mean-sd, ymax=mean+sd), color="black", width=0.1, position=position_dodge(width=0.3)) + 
    geom_point(data=data_fnirs, aes_string(x="condition", y=var, color="name"), position=position_nudge(x=0.1), size=2, alpha=0.6) +
    geom_line(data=data_fnirs, aes_string(x="condition", y=var, group="name", color="name"), position=position_nudge(x=0.1)) +
    scale_y_continuous(limits = c(0, 0.2), name="BETA") + 
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_blank(),
          axis.line=element_line(colour="black"),
          plot.title=element_text(hjust=0.5)) +  # 调整标题位置
    ggtitle(titles[i])
  
  plots[[var]] <- p
}

# 打印图形
print(plots[["BA4"]])
print(plots[["BA6"]])

# 为BA4和BA6绘制箱形图
data_long <- gather(data_fnirs, variable, Beta, BA4:BA6)
p_box <- ggplot(data_long, aes(x=condition, y=Beta, fill=variable)) + 
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(aes(x=condition, y=Beta), color="black", position=position_dodge(width=0.75), size=2, alpha=0.7) + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"))

print(p_box)

