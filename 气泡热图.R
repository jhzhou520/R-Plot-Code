#模拟数据
library(ggplot2)
library(dplyr)
library(patchwork)

# 模拟数据-------------------------------------------------------------
set.seed(1122)
patient <- factor(seq(1,12,1), levels = seq(1,12,1))
tumor_type = factor(c(rep("PDAC", 5), rep("NSCLC", 2),"CRC", "NSCLC","CRC", rep("NSCLC", 2)))
treatment = factor(c(rep("ERKi", 5), rep("KRASi", 7)))
pathway <- factor(c(
  "med_rank_KRAS_ERK_UP", "E2F_TARGETS", "G2M_CHECKPOINT",
  "PDAC_KRAS_ERK_UP", "MYC_TARGETS_V1", "MTORC1_SIGNALING",
  "SPERMATOGENESIS", "TNFA_SIGNALING_VIA_NFKB",
  "MYC_TARGETS_V2", "IL2_STAT5_SIGNALING",
  "ESTROGEN_RESPONSE_EARLY", "ALLOGRAFT_REJECTION",
  "ESTROGEN_RESPONSE_LATE", "P53_PATHWAY",
  "CHOLESTEROL_HOMEOSTASIS", "PI3K_AKT_MTOR_SIGNALING",
  "UV_RESPONSE_UP", "TGF_BETA_SIGNALING",
  "FATTY_ACID_METABOLISM", "UNFOLDED_PROTEIN_RESPONSE",
  "MITOTIC_SPINDLE", "DNA_REPAIR", "ROS_PATHWAY",
  "ANDROGEN_RESPONSE", "OXIDATIVE_PHOSPHORYLATION",
  "PROTEIN_SECRETION", "HEME_METABOLISM",
  "WNT_BETA_CATENIN_SIGNALING", "NOTCH_SIGNALING",
  "PEROXISOME", "APOPTOSIS", "PANCREAS_BETA_CELLS",
  "PDAC_KRAS_ERK_DN", "GLYCOLYSIS",
  "INTERFERON_ALPHA_RESPONSE", "HEDGEHOG_SIGNALING",
  "HYPOXIA", "INTERFERON_GAMMA_RESPONSE",
  "ADIPOGENESIS", "APICAL_JUNCTION",
  "med_rank_KRAS_ERK_DN", "APICAL_SURFACE",
  "UV_RESPONSE_DN", "KRAS_SIGNALING_UP",
  "KRAS_SIGNALING_DN", "XENOBIOTIC_METABOLISM",
  "IL6_JAK_STAT3_SIGNALING", "COMPLEMENT",
  "INFLAMMATORY_RESPONSE", "BILE_ACID_METABOLISM",
  "ANGIOGENESIS", "COAGULATION", "EMT",
  "MYOGENESIS"))
levels(pathway) <- rev(pathway)
NES <- rnorm(length(patient) * length(pathway), mean = 0, sd = 1)
adj_p_val  <- sample(c(0.05, 0.001, 0.0001, 0.00001, 1e-10), size = length(patient) * length(pathway), prob = c(0.6, 0.1, 0.15, 0.1, 0.05), replace = T)

data <- data.frame(
  patient = rep(patient, each = length(pathway)),
  tumor_type = rep(tumor_type, each = length(pathway)),
  treatment = rep(treatment, each = length(pathway)),
  pathway = rep(pathway, length(patient)),
  NES = NES,
  adj_p_val = adj_p_val
)

data <- data |> 
  mutate(adj_p_val = ifelse(abs(NES) > 2, 1e-10, adj_p_val)) |> 
  mutate(NES = ifelse(NES > 1.5, 3.5, NES)) |> 
  mutate(NES = ifelse(NES < -1.5, -3.5, NES)) |> 
  mutate(adj_p_val = factor(data$adj_p_val)) |> 
  mutate(tumor_type = factor(data$tumor_type, levels = c("PDAC","CRC","NSCLC")))  

#定义纵轴文本标签的颜色
# 纵轴标签文本颜色-------------------------------------------------------------
group <- levels(data$pathway)
group <- case_when(group == "med_rank_KRAS_ERK_UP" | group == "PDAC_KRAS_ERK_UP" ~ "darkblue",
                   group == "med_rank_KRAS_ERK_DN" | group == "PDAC_KRAS_ERK_DN" ~ "darkred",
                   group == "KRAS_SIGNALING_UP" ~ "orange",
                   group == "KRAS_SIGNALING_DN" ~ "grey",
                   .default = "black")
#绘制气泡热图
# 绘制气泡热图-------------------------------------------------------------
p <-  ggplot(data) +
  geom_hline(yintercept = pathway, 
             linetype = 1, 
             linewidth = 0.2, 
             color = "grey80") +
  geom_point(aes(x = patient, 
                 y = rev(pathway), 
                 size = adj_p_val, 
                 fill = NES),
             alpha = 1, 
             shape = 21) +
  scale_y_discrete(expand = c(0,1)) +
  scale_fill_gradient2(low = "blue",mid = "white",high = "red") +
  scale_size_manual(breaks = rev(c("1e-10","0.001", "0.05")),
                    values = c("1e-10" = 5, "1e-05" = 3,"1e-04" =2.5, "0.001" = 2,  "0.05" = 1.5)) +
  labs(
    title = "",
    x = "",
    y = "",
    size = "adj. p-val."
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 7, color = group),
    axis.ticks.length = unit(0.5, "mm"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 2, linewidth = 0.8),
    legend.frame = element_rect(color = "black"),
    legend.ticks = element_line(color = "black"),
    legend.position = "right"
  ) +
  facet_grid(.~treatment, scales = "free", space = "free")
p
添加注释条块

# 绘制注释条块-------------------------------------------------------------
anno <- ggplot(data) +
  geom_col(aes(x = patient, y = 0.1, fill = tumor_type),width = 1) +
  scale_fill_manual(name = "Tumor type",
                    values = c("PDAC" = "#0EA079","CRC" = "#069BDA","NSCLC" ="#CC79A8")) +
  theme_void() +
  theme(
    axis.text.x = element_blank(),
    strip.text  = element_blank(),
    strip.background = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.length = unit(0.5, "mm"),
    legend.margin = margin(1, 1, 1, 0.2, "cm"),
    legend.position = "right"
  ) +
  facet_grid(.~treatment, scales = "free", space = "free")
anno

bubble <- p / anno   + plot_layout(heights = c(10,0.25), guides = "collect")
bubble

ggsave(bubble,file = "Bubble heatmap with sample annotations.pdf",width = 6, height = 8)
