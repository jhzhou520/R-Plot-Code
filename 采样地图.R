#准备采样点数据
library(tibble)
library(ggplot2)
library(dplyr)
library(stars)
library(ggspatial)
library(showtext)

# 添加字体支持
showtext_auto()

# Sampling points in the world---------------------------------------------------------------
cities <- tibble(
  city = c("Beijing", "Shanghai", "Guangzhou", "Chengdu", "Shenzhen",
           "New York", "Los Angeles", "Chicago", "Houston", "Phoenix",
           "London", "Paris", "Berlin", "Madrid", "Rome",
           "Moscow", "Tokyo", "Seoul", "Sydney", "Toronto",
           "Mexico City", "São Paulo", "Buenos Aires", "Cairo", "Istanbul",
           "Mumbai", "Delhi", "Jakarta", "Bangkok", "Singapore",
           "Johannesburg", "Nairobi", "Cape Town", "Melbourne", "Perth",
           "San Francisco", "Miami", "Boston", "Vancouver", "Montreal",
           "Rio de Janeiro", "Lima", "Bogotá", "Santiago", "Caracas",
           "Lisbon", "Amsterdam", "Brussels", "Vienna", "Zurich",
           "Warsaw", "Prague", "Budapest", "Dublin", "Stockholm",
           "Oslo", "Helsinki", "Copenhagen", "Athens", "Bratislava",
           "Luxembourg", "Monaco", "Andorra la Vella", "Ljubljana", "Vilnius"),
  country = c("China", "China", "China", "China", "China",
              "USA", "USA", "USA", "USA", "USA",
              "UK", "France", "Germany", "Spain", "Italy",
              "Russia", "Japan", "South Korea", "Australia", "Canada",
              "Mexico", "Brazil", "Argentina", "Egypt", "Turkey",
              "India", "India", "Indonesia", "Thailand", "Singapore",
              "South Africa", "Kenya", "South Africa", "Australia", "Australia",
              "USA", "USA", "USA", "Canada", "Canada",
              "Brazil", "Peru", "Colombia", "Chile", "Venezuela",
              "Portugal", "Netherlands", "Belgium", "Austria", "Switzerland",
              "Poland", "Czech Republic", "Hungary", "Ireland", "Sweden",
              "Norway", "Finland", "Denmark", "Greece", "Slovakia",
              "Luxembourg", "Monaco", "Andorra", "Slovenia", "Lithuania"),
  latitude = c(39, 31, 23, 30, 22,
               40, 34, 41, 29, 33,
               51, 48, 52, 40, 41,
               55, 35, 37, -33, 43,
               19, -23, -34, 30, 41,
               19, 28, -6, 13, 1,
               -26, -1, -34, -37, -31,
               37, 25, 42, 49, 45,
               -22, -12, 4, -33, 10,
               38, 52, 50, 48, 47,
               52, 50, 47, 53, 59,
               59, 60, 55, 37, 48,
               49, 43, 42, 46, 54),
  longitude = c(116, 121, 113, 104, 114,
                -74, -118, -87, -95, -112,
                -0, 2, 13, -3, 12,
                37, 139, 127, 151, -79,
                -99, -46, -58, 31, 29,
                72, 77, 106, 100, 103,
                28, 36, 18, 144, 115,
                -122, -80, -71, -123, -73,
                -43, -77, -74, -70, -66,
                -9, 4, 4, 16, 8,
                21, 14, 19, -6, 18,
                10, 25, 12, 23, 17,
                6, 7, 1, 14, 25)
)  |> 
  mutate(group = sample(c("A", "B", "C", "D"), n(), replace = TRUE))
cities$group <- factor(cities$group, 
                       levels = c("A", "B", "C", "D"),
                       labels = c("No HCN cline", "Positive HCN cline", "Negative HCN cline", "Genomic data sampled"))

df_st_as_sf <- st_as_sf(cities, 
                        coords = c("longitude", "latitude"),
                        crs = 4326
                        )

#附带地形信息的采样地图
# Word Map with Shaded Relief --------------------------------------------
nat.earth <- raster::brick("./NE2_50M_SR/NE2_50M_SR/NE2_50M_SR.tif")
ggplot(df_st_as_sf)+
  layer_spatial(nat.earth)+
  theme_bw()+
  geom_sf(mapping = aes(fill = group), shape = 21, size = 2) +
  scale_fill_manual(values = c("grey60","#E50F04","#2B799F","#753f93")) +
  guides(fill = guide_legend(override.aes = list(size = 3))) +
  # annotation_north_arrow(location = "tl", which_north = F,
  #                        pad_x = unit(0.03, "in"),
  #                        pad_y = unit(0.03, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.line = element_blank(),
        legend.position = c(0.7,0.2),
        legend.title = element_blank(),
        panel.background = element_rect("#f5f6f1"),
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent")
        )+
  #annotation_scale(location="bl",width_hint=0.3) +
  labs(x='', y='',color=NULL)
ggsave("Word Map with Shaded Relief and Water.pdf", width = 9, height = 4.5)

#附带地形和水文信息的采样地图

# Word Map with Shaded Relief and Water-----------------------------------
nat.earth <- raster::brick("./NE1_LR_LC_SR_W_DR/NE1_LR_LC_SR_W_DR.tif")
ggplot(df_st_as_sf)+
  layer_spatial(nat.earth)+
  theme_bw()+
  geom_sf(mapping = aes(fill = group), shape = 21, size = 2) +
  scale_fill_manual(values = c("grey60","#E50F04","#2B799F","#753f93")) +
  guides(fill = guide_legend(override.aes = list(size = 3))) +
  # annotation_north_arrow(location = "tl", which_north = F,
  #                        pad_x = unit(0.03, "in"),
  #                        pad_y = unit(0.03, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.line = element_blank(),
        legend.position = c(0.7,0.2),
        legend.title = element_blank(),
        panel.background = element_rect("#f5f6f1"),
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent")
  )+
  #annotation_scale(location="bl",width_hint=0.3) +
  labs(x='', y='',color=NULL)

ggsave("Word Map with Shaded Relief and Water.pdf", width = 9, height = 4.5)

#接下来，我们可以把目光聚焦在亲爱的祖国身上，先试试将各省2023年的GDP数据映射到地图上。画完发现广东和江苏真的太突出了！
# GDP of Each Province of China----------------------------------------------------------------
#GDP数据可以在国家统计局官网拿到
gdp2023 <- tibble(
  province = c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区",
               "辽宁省", "吉林省", "黑龙江省", "上海市", "江苏省",
               "浙江省", "安徽省", "福建省", "江西省", "山东省",
               "河南省", "湖北省", "湖南省", "广东省", "广西壮族自治区",
               "海南省", "重庆市", "四川省", "贵州省", "云南省",
               "西藏自治区", "陕西省", "甘肃省", "青海省", "宁夏回族自治区",
               "新疆维吾尔自治区","香港特别行政区","澳门特别行政区","台湾省"),
  gdp = c(43760.7, 16737.3, 43944.1, 25698.2, 24627,
          30209.4, 13531.2, 15883.9, 47218.7, 128222.2,
          82553.2, 47050.6, 54355.1, 32200.1, 92068.7,
          59132.4, 55803.6, 50012.9, 135673.2, 27202.4,
          7551.2, 30145.8, 60132.9, 20913.3, 30021.1,
          2392.7, 33786.1, 11863.8, 3799.1, 5315,
          19125.9,50697,3337.8,53274.3)
)
china_map <- sf::st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json")[c("adcode", "name", "geometry")]
china_map <- merge(china_map, gdp2023, by.x = "name", by.y = "province", all.x = TRUE)
china_map <- mutate_all(china_map, ~replace(., is.na(.), 0))

# China Map with Shaded Relief and GDP of Each Province----------------------------------------------------------------
nat.earth <- raster::brick("./NE2_50M_SR/NE2_50M_SR/NE2_50M_SR.tif")
ggplot(china_map)+
  layer_spatial(nat.earth)+
  theme_bw()+
  geom_sf(aes(fill = gdp), color='black',size=1.2)+
  scale_fill_gradientn(limits = c(0,150000),
                       breaks = seq(0, 150000, 50000),
                       labels = c("0","5万亿", "10万亿", "15万亿"),
                       colors = c("#f7fbfe", "#9fcbe2", "#6badd7", "#4292c7", "#07519c", "#08306c")) +
  annotation_north_arrow(location = "tl", which_north = F,
                         pad_x = unit(0.05, "in"),
                         pad_y = unit(0.05, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.line = element_blank(),
        panel.background = element_rect("white"))+#地图底层颜色设置
  coord_sf(ylim = c(-3687082,1654989),
           xlim = c(-3000000,2700000),
           crs = "+proj=laea +lat_0=40 +lon_0=104")+
  annotation_scale(location="bl",width_hint=0.3)+
  labs(x='', y='',color=NULL)
ggsave("China map with Shaded Relief and GDP of each province.pdf", device = cairo_pdf, width = 7, height = 4.5)

#然后，把我自己去过的城市标注在地图上，可视化一下我的旅行足迹，看起来我去过的地方也不少哇！
# 旅行过的地方-------------------------------------------------
#各个城市的经纬度信息可以让ChatGPT等人工智能帮你查找
travel <- tibble(
  city = c("北京", "西安", "泾川", "天津", "宜昌", 
           "呼和浩特", "赤峰", "酒泉", "兰州", "嘉峪关", 
           "青岛", "成都", "福州", "贵阳", "秦皇岛", 
           "郑州", "洛阳", "上海", "长沙", "凤凰古镇"),
  latitude = c(39.90, 34.27, 35.33, 39.13, 30.70, 
               40.82, 42.27, 39.73, 36.06, 39.77, 
               36.07, 30.67, 26.08, 26.65, 39.94, 
               34.75, 34.62, 31.23, 28.23, 27.95),
  longitude = c(116.40, 108.93, 107.37, 117.20, 111.29, 
                111.66, 118.96, 98.50, 103.82, 98.29, 
                120.38, 104.07, 119.30, 106.63, 119.60, 
                113.62, 112.45, 121.47, 112.93, 109.60)
)
travel_st_as_sf <- st_as_sf(travel, 
                        coords = c("longitude", "latitude"),
                        crs = 4326
)
# China Map with Shaded Relief and Water----------------------------------------------------------------
china_map <- sf::st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json")[c("adcode", "name", "geometry")]
nat.earth <- raster::brick("./NE1_LR_LC_SR_W_DR/NE1_LR_LC_SR_W_DR.tif")
p <- ggplot(china_map) +
  layer_spatial(nat.earth)+
  theme_bw() +
  geom_sf(fill = "white",color='black',size=1.2) +
  geom_sf(data = travel_st_as_sf, shape = 21, fill =  "#07519c", size = 2) +
  annotation_north_arrow(location = "tl", which_north = F,
                         pad_x = unit(0.05, "in"),
                         pad_y = unit(0.05, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.line = element_blank(),
        panel.background = element_rect("white"))+#地图底层颜色设置
  coord_sf(ylim = c(-3687082,1654989),
           xlim = c(-3000000,2700000),
           crs = "+proj=laea +lat_0=40 +lon_0=104")+
  annotation_scale(location="bl",width_hint=0.3)+
  labs(x='', y='',color=NULL)

ggsave("China map with Shaded Relief and Water and Traveled Sites.pdf", plot = p, device = cairo_pdf, width = 7, height = 4.5)
