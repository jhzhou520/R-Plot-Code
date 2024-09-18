#加载R包和数据
#需要先安装linkET包
#devtools::install_github("Hy4m/linkET", force = TRUE)
#packageVersion("linkET")

library(linkET)
library(vegan)
library(RColorBrewer)
library(tidyverse)
library(ggnewscale)
library(RColorBrewer)
#display.brewer.all()

#作者没有提供绘图数据，这里使用vegan包提供的植被与环境数据集
data("varespec")
data("varechem")

#修改列名以和Science文章中的名称保持相似
colnames(varechem) <- c("Temperature", "Salinity", "Oxygen", "PO4", "NO2NO3","Si", "PAR",
                        "Lyapunou","Retention","MLD","Fluorescence", "MaxO2", "MinO2", "Nitracline")
                        
#Mantel test
mantel <- mantel_test(varespec, varechem,
                      spec_select = list("Taxonomic\ncomposition\n(16S OTUs)" = 1:8,
                                         "Gene\nfunctional\ncomposition" = 9:27,
                                         "Taxonomic\ncomposition\n(mOTUs)" = 28:44)) %>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.3, Inf),
                  labels = c("< 0.2", "0.2 - 0.3", ">= 0.3")),
         pd = cut(p, breaks = c(-Inf, 0.005, 0.01, 0.05, Inf),
                  labels = c("< 0.005", "0.005 - 0.01", "0.01 - 0.05", ">= 0.05")))

#网络相关性热图(版本1)
p1 <- qcorrplot(correlate(varechem), 
          grid_col = "grey50",
          grid_size = 0.5,
          type = "upper", 
          diag = FALSE) +
  geom_square() +
  geom_mark(size = 4,
            only_mark = T,
            sig_level = c(0.05, 0.01, 0.001),
            sig_thres = 0.05,
            colour = 'white') +
  geom_couple(data = mantel,
              aes(color = pd, size = rd),  
              label.size = 3.88,
              label.family = "",
              label.fontface = 1,
              nudge_x = 0.2,
              curvature = nice_curvature(by = "from")) +    
  scale_fill_gradientn(limits = c(-0.8,0.8),
                       breaks = seq(-0.8,0.8,0.4),
                       colors = rev(brewer.pal(11, "RdBu"))) +
  scale_size_manual(values = c(0.5, 1.5, 3)) +
  scale_color_manual(values = color_pal(4, alpha = 0.6)) +  
  guides(size = guide_legend(title = "Mantel's r",                               
                             order = 2,                               
                             keyheight = unit(0.5, "cm")),           
         colour = guide_legend(title = "Mantel's p",                                  
                               order = 1,                                 
                               keyheight = unit(0.5, "cm")),           
         fill = guide_colorbar(title = "Pearson's r", 
                               keyheight = unit(2.2, "cm"),
                               keywidth = unit(0.5, "cm"),
                               order = 3)) + 
  theme(legend.box.spacing = unit(0, "pt"))
ggsave(p1,file = "Corr heatmap1 with Mantel's test.pdf",width = 8.8, height = 6)

#网络相关性热图(版本2)
p2 <- qcorrplot(correlate(varechem), type = "upper", diag=FALSE, grid_col = NA) +    
  geom_point(shape=21, size=8, fill = NA, stroke = 0.35, color = "black") +    
  geom_point(aes(size=abs(r), fill=r),               
             shape=21, 
             stroke = 0.35, 
             color = "black") +    
  scale_size(range = c(1, 8), guide = "none") +        
  new_scale("size") +    
  geom_couple(data = mantel,
              aes(color = pd, size = rd),  
              label.size = 3.88,
              label.family = "",
              label.fontface = 1,
              nudge_x = 0.2,
              curvature = nice_curvature(by = "from")) +    
  scale_fill_gradientn(limits = c(-0.8,0.8),
                       breaks = seq(-0.8,0.8,0.4),
                       colors = rev(brewer.pal(11, "Spectral"))) +
  scale_size_manual(values = c(0.5, 1.5, 3)) +
  scale_color_manual(values = color_pal(4, alpha = 0.6)) +  
  guides(size = guide_legend(title = "Mantel's r",                               
                             order = 2,                               
                             keyheight = unit(0.5, "cm")),           
         colour = guide_legend(title = "Mantel's p",                                  
                               order = 1,                                 
                               keyheight = unit(0.5, "cm")),           
         fill = guide_colorbar(title = "Pearson's r", 
                               keyheight = unit(2.2, "cm"),
                               keywidth = unit(0.5, "cm"),
                               order = 3)) + 
  theme(legend.box.spacing = unit(0, "pt"))
ggsave(p2,file = "Corr heatmap2 with Mantel's test.pdf",width = 8.8, height = 6)