Siljan2
================
Femke van Dam
2025-03-11

# Siljan Rmarkdown

### Load packges

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     stamp

``` r
library(ggpubr)
```

    ## 
    ## Attaching package: 'ggpubr'
    ## 
    ## The following object is masked from 'package:cowplot':
    ## 
    ##     get_legend

``` r
library(readxl)
library(ggthemes) 
```

    ## 
    ## Attaching package: 'ggthemes'
    ## 
    ## The following object is masked from 'package:cowplot':
    ## 
    ##     theme_map

``` r
library(patchwork)
```

    ## 
    ## Attaching package: 'patchwork'
    ## 
    ## The following object is masked from 'package:cowplot':
    ## 
    ##     align_plots

``` r
library(rlang)
```

    ## 
    ## Attaching package: 'rlang'
    ## 
    ## The following objects are masked from 'package:purrr':
    ## 
    ##     %@%, flatten, flatten_chr, flatten_dbl, flatten_int, flatten_lgl,
    ##     flatten_raw, invoke, splice

``` r
library(scales) 
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(ggplot2)
library(ggpubr)
library(tidyr)
library(forcats)
library(dplyr)
```

### Load gas data

``` r
Timeseries1 <- read_excel("C:/Users/fevaaa/OneDrive - Linnéuniversitetet/Dokument/Siljan/Manuscript2/Data Manuscript 2.xlsx")
```

    ## New names:
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`

``` r
Timeseries2 <- read_excel("C:/Users/fevaaa/OneDrive - Linnéuniversitetet/Dokument/Siljan/Manuscript2/Data Manuscript 2.xlsx", sheet= 'Timeseries2')
CO2_1 <- read_excel("C:/Users/fevaaa/OneDrive - Linnéuniversitetet/Dokument/Siljan/Manuscript2/Data Manuscript 2.xlsx", sheet= 'CO2_series1')
CO2_2 <- read_excel("C:/Users/fevaaa/OneDrive - Linnéuniversitetet/Dokument/Siljan/Manuscript2/Data Manuscript 2.xlsx", sheet= 'CO2_series2')
```

### Plot gas data

``` r
dfsumm_ppm <- Timeseries2 %>%
  filter(Treatment %in% c('oil', 'medium', 'methanol', 'acetate', 'yeast', 'casein')) %>%
  group_by(Treatment, days) %>%
  summarise(
    N = dplyr::n(),  
    mean = mean(total_ppm, na.rm = TRUE),  
    sd = sd(total_ppm, na.rm = TRUE),
    se = sd / sqrt(N),
    .groups = "drop"  
  )

(p2 <- ggplot(data = dfsumm_ppm, 
              aes(x=days, y = mean, colour = Treatment)) +
    geom_line(linetype=1, size=1) +
    geom_point(aes(shape = Treatment, fill= Treatment), color='black', size = 5) +
    scale_shape_manual(values = c(21,23,24, 21, 22,25), labels = c("acetate", "casein", "medium","methanol","oil","yeast")) +
    scale_fill_manual(values = c('yellow', 'black', '#56B4E9', "#009E73", "#A91E90", "#FF9039"), labels = c("acetate", "casein", "medium","methanol","oil","yeast")) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1.5) +
    scale_color_manual(values = c('yellow', 'black', '#56B4E9', "#009E73", "#A91E90", "#FF9039"), labels = c("acetate", "casein", "medium","methanol","oil","yeast")) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(10^0,10^6))
          + theme_clean(base_size=10) + 
    scale_x_continuous(expand = c(0, 0), limits = c(0,170)) +
    xlab("Time  (days) ") +
    ylab("Methane concentration (ppm)") + 
    theme_pubr()  +
    theme(legend.key.size = unit(0.3, "cm"), 
      axis.title = element_text(size = 14),  
      legend.position = c(0.9, 0.02),        
      legend.justification = c(1, 0),        
      legend.text = element_text(size = 11),
      legend.direction = "horizontal"        
    )
)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    ## 3.5.0.
    ## ℹ Please use the `legend.position.inside` argument of `theme()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Siljancode_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# Filter and clean data for correlation and linear models
df1 <- Timeseries2 %>%
  filter(Treatment %in% c('oil', 'medium', 'methanol')) %>%
  slice(-145) %>%
  drop_na(d13C_CH4, d13C_CO2)

# pearson correlations
# Calculate correlations
correlations <- df1 %>%
  group_by(Treatment) %>%
  summarise(
    d13C_CH4_cor = cor(d13C_CH4, total_ppm, method = 'pearson'),
    d13C_CO2_cor = cor(d13C_CO2, total_ppm, method = 'pearson'),
    d13C_CH4_d13C_CO2_cor = cor(d13C_CH4, d13C_CO2, method = 'pearson')
  )



# Linear models for d13C_CH4
lm1_CH4 <- lm(d13C_CH4 ~ total_ppm, data = df1 %>% filter(Treatment == 'oil'))
lm2_CH4 <- lm(d13C_CH4 ~ total_ppm, data = df1 %>% filter(Treatment == 'medium'))
lm3_CH4 <- lm(d13C_CH4 ~ total_ppm, data = df1)

# Linear models for d13C_CO2
lm1_CO2 <- lm(d13C_CO2 ~ total_ppm, data = df1 %>% filter(Treatment == 'oil'))
lm2_CO2 <- lm(d13C_CO2 ~ total_ppm, data = df1 %>% filter(Treatment == 'medium'))
lm3_CO2 <- lm(d13C_CO2 ~ total_ppm, data = df1)

# Linear models for d13C_CO2 vs d13C_CH4
lm_medium <- lm(d13C_CO2 ~ d13C_CH4, data = df1 %>% filter(Treatment == 'medium'))
lm_oil <- lm(d13C_CO2 ~ d13C_CH4, data = df1 %>% filter(Treatment == 'oil'))
lm_methanol <- lm(d13C_CO2 ~ d13C_CH4, data = df1 %>% filter(Treatment == 'methanol'))


(p4 <- df1 %>%
    group_by(Treatment, days) %>%
    summarise(
      mean_d13C_CH4 = mean(d13C_CH4, na.rm = TRUE),
      mean_d13C_CO2 = mean(d13C_CO2, na.rm = TRUE),
      se_d13C_CH4 = sd(d13C_CH4, na.rm = TRUE) / sqrt(n()),  # Standard Error for d13C_CH4
      se_d13C_CO2 = sd(d13C_CO2, na.rm = TRUE) / sqrt(n()),  # Standard Error for d13C_CO2
      .groups = "drop"   # Ensures grouping is dropped after summarization
    ) %>% 
    ggplot(aes(x= mean_d13C_CH4, mean_d13C_CO2, colour=Treatment)) +
  scale_colour_manual(values = c( '#56B4E9', "#009E73", "#A91E90"),    # Legend label, use darker colors
                      breaks=c( "medium","methanol","oil"),
                      labels=c( "Medium, r = 0.99, m = 0.58","Methanol, r = 0.82, m = 0.47","Oil, r = 0.92, m = 0.61"))+
  geom_point(aes(shape = Treatment, fill= Treatment), color= 'black', size = 3.5) +
  geom_smooth(aes(colour = Treatment), method = "lm", formula = y ~ x, se = F, 
              size = 1, linetype = "dashed") +
    geom_errorbar(aes(
      xmin = mean_d13C_CH4 - se_d13C_CH4, xmax = mean_d13C_CH4 + se_d13C_CH4,
      ymin = mean_d13C_CO2 - se_d13C_CO2, ymax = mean_d13C_CO2 + se_d13C_CO2
    ), width = 1, size = 0.5) +  # Adding error bars
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#A91E90"), labels = c("Medium, r = 0.99, m = 0.58","Methanol, r = 0.82, m = 0.47","Oil, r = 0.92, m = 0.61")) +
  scale_shape_manual(values = c(24, 21, 22), labels = c("Medium, r = 0.99, m = 0.58","Methanol, r = 0.82, m = 0.47","Oil, r = 0.92, m = 0.61")) + 
  #stat_smooth(method = "lm",formula = y~x,  se=TRUE) +
  ylab(expression(delta^{"13"} * C[CO2] ~ '(‰)')) +
  xlab(expression(delta^{"13"} * C[CH4] ~ '(‰)'))  +
    scale_x_continuous(expand = c(0, 0), limits = c(-122,-40)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-22,32)) +
  theme_clean(base_size=10) +  # Remove ugly grey background
  guides(size= 'none', colour=guide_legend(title="Treatment"),) +
    theme_pubr() +
  theme( legend.title=element_blank(), 
         #legend.spacing = unit(0.1, "cm"),        # Minimize space between legend items
         legend.key.size = unit(0.3, "cm"), 
         axis.title = element_text(size = 14),  # Increase axis title size
         legend.position = c(1, 0.01),         # Position legend in bottom-right corner
         legend.justification = c(1, 0),        # Anchor legend to its bottom-right corner
         legend.text = element_text(size = 11), # Adjust legend text size
         legend.direction = "vertical"        # Make the legend horizontal
  )
)
```

![](Siljancode_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
df_summary_p5 <- df1 %>%
  group_by(Treatment, days) %>%
  summarise(
    mean_total_ppm = mean(total_ppm, na.rm = TRUE),
    mean_d13C_CH4 = mean(d13C_CH4, na.rm = TRUE),
    se_total_ppm = sd(total_ppm, na.rm = TRUE) / sqrt(n()),  # Standard Error for total_ppm
    se_d13C_CH4 = sd(d13C_CH4, na.rm = TRUE) / sqrt(n()),  # Standard Error for d13C_CH4
    .groups = "drop"
  )

(p5 <- ggplot(df_summary_p5, aes(x = mean_total_ppm, y = mean_d13C_CH4, colour = Treatment)) +
    scale_colour_manual(values = c( '#56B4E9', "#009E73", "#A91E90"),    
                        breaks=c( "medium","methanol","oil"),
                        labels=c( "Medium, r = 0.85","Methanol, r = 0.97","Oil, r = 0.94"))+
    geom_point(aes(shape = Treatment, fill= Treatment), color= 'black', size = 3.5) +
    geom_smooth(aes(colour = Treatment), method = "lm", formula = y ~ x, se = F, 
                size = 1, linetype = "dashed") +
      geom_errorbar(aes(
    xmin = mean_total_ppm - se_total_ppm, xmax = mean_total_ppm + se_total_ppm,  # Error bars for total_ppm
    ymin = mean_d13C_CH4 - se_d13C_CH4, ymax = mean_d13C_CH4 + se_d13C_CH4  # Error bars for d13C_CH4
  ), width = 2000, size = 0.5) +  
    scale_fill_manual(values=c("#56B4E9", "#009E73", "#A91E90"), labels = c("Medium, r = 0.85","Methanol, r = 0.97","Oil, r = 0.94")) +
    scale_shape_manual(values = c(24, 21, 22), labels = c("Medium, r = 0.85","Methanol, r = 0.97","Oil, r = 0.94")) + 
    ylab(expression(delta^{"13"} * C[CH4] ~ '(‰)')) +  
    xlab('Methane concentration (ppm)') +  
    scale_x_continuous(expand = c(0, 0), limits = c(0,340000)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-122,-40)) +
    theme_clean(base_size=10) +  # Remove ugly grey background
    theme(plot.title=element_text(size=10))+
    guides(size= 'none', colour=guide_legend(title="Treatment"),) +
    theme_pubr()  + 
    theme(legend.title=element_blank(),  
          legend.key.size = unit(0.3, "cm"), 
          axis.title = element_text(size = 14),  
           legend.position = c(1, 0.01),         
           legend.justification = c(1, 0),       
           legend.text = element_text(size = 11), 
           legend.direction = "vertical"        
    )

)
```

![](Siljancode_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
df_summary_p6 <- df1 %>%
  group_by(Treatment, days) %>%
  summarise(
    mean_total_ppm = mean(total_ppm, na.rm = TRUE),
    mean_d13C_CO2 = mean(d13C_CO2, na.rm = TRUE),
    se_total_ppm = sd(total_ppm, na.rm = TRUE) / sqrt(n()),  # Standard Error for total_ppm
    se_d13C_CO2 = sd(d13C_CO2, na.rm = TRUE) / sqrt(n()),  # Standard Error for d13C_CO2
    .groups = "drop"
  )


(p6 <- ggplot(df_summary_p6, aes(x = mean_total_ppm, y = mean_d13C_CO2, colour = Treatment)) +
    scale_colour_manual(values = c( '#56B4E9', "#009E73", "#A91E90"),    
                        breaks=c( "medium","methanol","oil"),
                        labels=c("Medium, r = 0.93","Methanol, r = 0.79","Oil, r = 0.83"))+
    geom_point(aes(shape = Treatment, fill= Treatment), color= 'black', size = 3.5) +
    geom_smooth(aes(colour = Treatment), method = "lm", formula = y ~ x, se = F, 
                size = 1, linetype = "dashed") +
    geom_errorbar(aes(
      xmin = mean_total_ppm - se_total_ppm, xmax = mean_total_ppm + se_total_ppm,  # Error bars for total_ppm
      ymin = mean_d13C_CO2 - se_d13C_CO2, ymax = mean_d13C_CO2 + se_d13C_CO2  # Error bars for d13C_CO2
    ), width = 2000, size = 0.5) +
    scale_fill_manual(values=c("#56B4E9", "#009E73", "#A91E90"), labels = c("Medium, r = 0.93","Methanol, r = 0.79","Oil, r = 0.83")) +
    scale_shape_manual(values = c(24, 21, 22), labels = c("Medium, r = 0.93","Methanol, r = 0.79","Oil, r = 0.83")) + 
    ylab(expression(delta^{"13"} * C[CO2] ~ '(‰)')) +  
    xlab('Methane concentration (ppm)') +  
    scale_x_continuous(expand = c(0, 0), limits = c(0,340000)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-22,32)) +
    theme_clean(base_size=10) +  # Remove ugly grey background
    theme(plot.title=element_text(size=10))+
    guides(size= 'none', colour=guide_legend(title='Treatment'),) +
    theme_pubr()  + 
  theme( legend.title=element_blank(),
         #legend.spacing = unit(0.1, "cm"),       
         legend.key.size = unit(0.3, "cm"), 
    axis.title = element_text(size = 14),  
    legend.position = c(1, 0.01),         
    legend.justification = c(1, 0),        
    legend.text = element_text(size = 11), 
    legend.direction = "vertical"       
  )
)
```

![](Siljancode_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
# Arrange the plots 
multi_plot2 <- ggarrange(
  p2, p5,
  p4, p6,
  labels = c("a", "b", "c","d"),  
  ncol = 2, nrow = 2,
  common.legend = FALSE  
)


#ggsave("Figure2.png", dpi = "retina")
```

#### Supplemental figures

``` r
df2 <- Timeseries2 %>%
  filter(Treatment %in% c('oil', 'medium', 'methanol')) %>%
  slice(-145) %>%
  drop_na(d13C_CH4, dD_CH4)

(p3 <- ggplot(df2, aes(y= dD_CH4, x= d13C_CH4, colour=Treatment)) +
  scale_colour_manual(values = c( '#56B4E9', "#009E73", "#A91E90"),    # Legend label, use darker colors
                      breaks=c( "medium","methanol","oil"),
                      labels=c( "medium","methanol","oil")) +
  geom_point(aes(shape = Treatment, fill= Treatment), color= 'black', size = 3.5) +
  #geom_smooth(aes(colour = Treatment), method = "lm", formula = y ~ x, se = F, 
             #   size = 1, linetype = "dashed") +
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#A91E90"), labels = c("medium","methanol","oil")) +
  scale_shape_manual(values = c(24, 21, 22), labels = c("medium","methanol","oil")) + 
  #stat_smooth(method = "lm",formula = y~x,  se=TRUE) +
  xlab(expression(delta^{"13"} * C[CH4] ~ '(‰)')) +  
  ylab(expression(delta * D[CH4] ~ '(‰)')) +  
  #annotate("text", x = -60, y = -295, label = paste0(
   #   "R =",
    #  round(medium, 2)), colour="#56B4E9") +
    #annotate("text", x = -85, y = -290, label = paste0(
     # "R =",
      #round(oil, 2)
    #), colour= "#A91E90") +
    #annotate("text", x = -80, y = -320, label = paste0(
     # "R =", round(methanol, 2)), colour= "#009E73") +
  ylim(-320,-270) +  # Set x axis limits, 
  xlim(-120, -40) +
  theme_clean(base_size=10) +  # Remove ugly grey background
  theme(plot.title=element_text(size=14),
        legend.title=element_blank(), 
        #legend.spacing = unit(0.1, "cm"),        # Minimize space between legend items
        legend.key.size = unit(0.3, "cm"), 
        axis.title = element_text(size = 14),  # Increase axis title size
        legend.justification = c(1, 0),        # Anchor legend to its bottom-right corner
        legend.text = element_text(size = 14), # Adjust legend text size
        legend.direction = "vertical"   
                                ) +
  guides(size= 'none', colour=guide_legend(title="Treatment"),) +
    theme_pubr()
)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](Siljancode_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
df <- Timeseries2
df3 <- df[which(df$Treatment == 'methanol'),]
df3a <- df3[which(df3$Replicate == 'a'),]
df3b <- df3[which(df3$Replicate == 'b'),]
df3c <- df3[which(df3$Replicate == 'c'),]
df3d <- df3[which(df3$Replicate == 'd'),]

(lm3a <- lm(df3a$d13C_CH4~df3a$total_ppm))
```

    ## 
    ## Call:
    ## lm(formula = df3a$d13C_CH4 ~ df3a$total_ppm)
    ## 
    ## Coefficients:
    ##    (Intercept)  df3a$total_ppm  
    ##     -1.178e+02       2.444e-04

``` r
(lm3b <- lm(df3b$d13C_CH4~df3b$total_ppm))
```

    ## 
    ## Call:
    ## lm(formula = df3b$d13C_CH4 ~ df3b$total_ppm)
    ## 
    ## Coefficients:
    ##    (Intercept)  df3b$total_ppm  
    ##     -1.190e+02       2.777e-04

``` r
(lm3c <- lm(df3c$d13C_CH4~df3c$total_ppm))
```

    ## 
    ## Call:
    ## lm(formula = df3c$d13C_CH4 ~ df3c$total_ppm)
    ## 
    ## Coefficients:
    ##    (Intercept)  df3c$total_ppm  
    ##     -1.189e+02       2.646e-04

``` r
(lm3d <- lm(df3d$d13C_CH4~df3d$total_ppm))
```

    ## 
    ## Call:
    ## lm(formula = df3d$d13C_CH4 ~ df3d$total_ppm)
    ## 
    ## Coefficients:
    ##    (Intercept)  df3d$total_ppm  
    ##     -1.188e+02       2.436e-04

``` r
(methanol <- ggplot(df3, aes(x= total_ppm, y= d13C_CH4, colour=Replicate)) +
    scale_colour_manual(values = c( '#56B4E9', "#009E73", "#A91E90", "#FF9039"),    # Legend label, use darker colors
                        breaks=c( "a","b","c","d"),
                        labels=c( "a","b","c","d"))+
    geom_point(aes(shape = Replicate, fill= Replicate), color= 'black', size = 3.5) +
    geom_smooth(aes(colour = Replicate), method = "lm", formula = y ~ x, se = F, 
                size = 1, linetype = "dashed") +
    scale_fill_manual(values=c("#56B4E9", "#009E73", "#A91E90", "#FF9039"), labels = c("a","b","c","d")) +
    scale_shape_manual(values = c(24, 21, 22, 23), labels = c("a","b","c","d")) + 
    ylab(expression(delta^{"13"} * C[CH4] ~ '(‰)')) +  
    xlab('Methane concentration (ppm)') +  
    theme_clean(base_size=10) +  # Remove ugly grey background
    theme(plot.title=element_text(size=10))+
    guides(size= 'none', colour=guide_legend(title="Replicate"),) +
    theme_pubr() # + scale_y_reverse()
  
)
```

    ## Warning: Removed 26 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 26 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](Siljancode_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

### Relative abundance

``` r
# Read data
TPM_metaT <- read_excel("C:/Users/fevaaa/OneDrive - Linnéuniversitetet/Dokument/Siljan/Manuscript2/coverM_metaT.xlsx", sheet = 'Sheet1')
Abundance <- read_excel("C:/Users/fevaaa/OneDrive - Linnéuniversitetet/Dokument/Siljan/Manuscript2/Data Manuscript 2.xlsx", sheet = 'Samples') %>%
  select(2:13) %>%
  rename_with(~ c('Genome', 'Media_a', 'Media_b', 'Media_d', 'Oil_b', 'Oil_c', 'Oil2_b', 'Methanol_a', 'Methanol_c', 'Methanol_d', 'Total', 'Classification')) %>%
  filter(Total > 0.1) %>%
  select(1:10, 12)
```

    ## New names:
    ## • `` -> `...1`
    ## • `` -> `...12`
    ## • `` -> `...14`
    ## • `` -> `...17`

``` r
# Clean and transform data
cols <- 2:11
TPM_metaT <- TPM_metaT %>%
  rename_with(~ c('Genome', 'Media_a2', 'Media_a', 'Media_b', 'Media_d', 'Oil_b', 'Oil_c', 'Oil2_b', 'Methanol_a', 'Methanol_c', 'Methanol_d'))

library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
bintax <- Abundance %>%
  mutate(Classification = gsub("[a-z]__", "", Classification)) %>%
  separate(Classification, into = c("domain", "phylum", "class", "order", "family", "genus", "species"), sep = ";", fill = "right") %>%
  mutate(across(where(is.character), ~ na_if(.x, "")),
         taxon = coalesce(species, genus, family, order, class, phylum)) %>%
  mutate(across(cols, as.numeric))
```

    ## Warning: There were 2 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(cols, as.numeric)`.
    ## Caused by warning:
    ## ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ## ℹ Please use `all_of()` or `any_of()` instead.
    ##   # Was:
    ##   data %>% select(cols)
    ## 
    ##   # Now:
    ##   data %>% select(all_of(cols))
    ## 
    ## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
# Perform the join
bintax_T <- TPM_metaT %>%
  left_join(select(Abundance, Genome, Classification), by = "Genome") %>%
  mutate(Classification = gsub("[a-z]__", "", Classification)) %>%
  separate(Classification, into = c("domain", "phylum", "class", "order", "family", "genus", "species"), sep = ";", fill = "right") %>%
  mutate(across(where(is.character), ~ na_if(.x, "")),
         taxon = coalesce(species, genus, family, order, class, phylum)) %>%
  mutate(across(cols, as.numeric))

# Melt data
bintax_T_melt <- melt(bintax_T)
```

    ## Using Genome, domain, phylum, class, order, family, genus, species, taxon as id variables

``` r
bintax_melt <- melt(bintax)
```

    ## Using Genome, phylum, class, order, family, genus, species, taxon as id variables

``` r
# Reorder and update taxon
desired_order <- c("Methanol_a", "Methanol_c", "Methanol_d", "Media_a", "Media_b", "Media_d", "Oil_b", "Oil_c", "Oil2_b")
bintax_melt <- bintax_melt %>%
  mutate(variable = factor(variable, levels = desired_order),
         taxon = case_when(phylum == "Patescibacteria" ~ "CPR", TRUE ~ taxon))

cols.taxa <- c( "Methanogranum sp019262145"= '#56B4E9',"Acetobacterium sp003260995" ="#009E73", "Acetobacterium"= "#FF9039", "CPR"= "#A91E90", "Other"=  "grey")

taxon_order <- c("Other","CPR", "Acetobacterium", "Acetobacterium sp003260995", 
                   "Methanogranum sp019262145")

metaG <- bintax_melt %>%
  mutate(taxon = case_when(
    phylum == "Patescibacteria" ~ "CPR",
    TRUE ~ taxon  # Keep existing value if condition is not met
  )) %>%
  mutate(taxon = ifelse(taxon %in% names(cols.taxa), taxon, "Other")) %>%
  mutate(taxon = factor(taxon, levels = taxon_order))  %>% 
  mutate(variable = factor(variable, levels = desired_order)) %>%
  ggplot(aes(x = fct_reorder(variable, value), y = value /1000000, fill = taxon)) +
  geom_col() +
  labs(x = NULL, y = ' Relative abundance', fill = 'Taxa') +
  scale_fill_manual(values = (cols.taxa), name = "") +
  theme_bw(base_size=10) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),  # Increase axis title size
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        strip.text = element_text(size = 14, face = "bold")) +  # Bold facet labels
  geom_segment(aes(x = 0.55, xend = 3.45, y = -0.01, yend = -0.01), color = "black", size = 1) +  
  geom_segment(aes(x = 3.55, xend = 6.45, y = -0.01, yend = -0.01), color = "black", size = 1) +  
  geom_segment(aes(x = 6.55, xend = 9.45, y = -0.01, yend = -0.01), color = "black", size = 1) +  
  annotate("text", x = 3.2, y = min(bintax_melt$value / 1000000) -0.03, label = "Methanol", size = 5, hjust = 1.2) + 
  annotate("text", x = 5.75, y = min(bintax_melt$value / 1000000) -0.03, label = "Media", size = 5, hjust = 1.2) + 
  annotate("text", x = 9.4, y = min(bintax_melt$value / 1000000) -0.03, label = "Media + oil", size = 5, hjust = 1.2)

(metaT <- bintax_T_melt %>%
    mutate(taxon = case_when(
      phylum == "Patescibacteria" ~ "CPR",
      TRUE ~ taxon  # Keep existing value if condition is not met
    )) %>%
  mutate(taxon = ifelse(taxon %in% names(cols.taxa), taxon, "Other")) %>%
    mutate(taxon = factor(taxon, levels = taxon_order))  %>% 
    mutate(variable = factor(variable, levels = desired_order)) %>%
 ggplot(aes(x = fct_reorder(variable, value), y = value /1000000, fill = taxon)) +
  geom_col() +
    labs(x = NULL, y = ' Relative abundance', fill = 'Taxa') +
    scale_fill_manual(values = (cols.taxa), name = "") +
    theme_bw(base_size=10) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 14, face = "bold"),
          axis.ticks.x = element_blank(),
          axis.title = element_text(size = 14),  # Increase axis title size
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),
          strip.text = element_text(size = 14, face = "bold")) +  # Bold facet labels
    geom_segment(aes(x = 0.55, xend = 3.45, y = -0.01, yend = -0.01), color = "black", size = 1) +  
    geom_segment(aes(x = 3.55, xend = 7.45, y = -0.01, yend = -0.01), color = "black", size = 1) +  
    geom_segment(aes(x = 7.55, xend = 10.45, y = -0.01, yend = -0.01), color = "black", size = 1) +  
    annotate("text", x = 3.2, y = min(bintax_T_melt$value / 1000000) -0.03, label = "Methanol", size = 5, hjust = 1.2) + 
    annotate("text", x = 6.5, y = min(bintax_T_melt$value / 1000000) -0.03, label = "Media", size = 5, hjust = 1.2) + 
    annotate("text", x = 10.5, y = min(bintax_T_melt$value / 1000000) -0.03, label = "Media + oil", size = 5, hjust = 1.2)
)
```

    ## Warning: `fct_reorder()` removing 10 missing values.
    ## ℹ Use `.na_rm = TRUE` to silence this message.
    ## ℹ Use `.na_rm = FALSE` to preserve NAs.

    ## Warning: Removed 10 rows containing missing values or values outside the scale range
    ## (`geom_col()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).

![](Siljancode_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
barplot2 <- ggarrange(metaG,metaT, #plots that are going to be included in this multipanel figure
                         labels = c("a", "b" ), #labels given each panel 
                         ncol = 2, nrow = 1,
                         legend =  'bottom',
                         common.legend = T,
                         legend.grob = NULL)
```

    ## Warning: `fct_reorder()` removing 33 missing values.
    ## ℹ Use `.na_rm = TRUE` to silence this message.
    ## ℹ Use `.na_rm = FALSE` to preserve NAs.

    ## Warning: Removed 33 rows containing missing values or values outside the scale range
    ## (`geom_col()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).

    ## Warning: `fct_reorder()` removing 33 missing values.
    ## ℹ Use `.na_rm = TRUE` to silence this message.
    ## ℹ Use `.na_rm = FALSE` to preserve NAs.

    ## Warning: Removed 33 rows containing missing values or values outside the scale range
    ## (`geom_col()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).

    ## Warning: `fct_reorder()` removing 10 missing values.
    ## ℹ Use `.na_rm = TRUE` to silence this message.
    ## ℹ Use `.na_rm = FALSE` to preserve NAs.

    ## Warning: Removed 10 rows containing missing values or values outside the scale range
    ## (`geom_col()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).

``` r
barplot2
```

![](Siljancode_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
#Repeated for highest taxa, phylum, family, genus

ggsave("taxon.png", dpi = "retina")
```

    ## Saving 7 x 5 in image

## METABOLIC

### MW-score heatmap

``` r
MW <- read_excel("C:/Users/fevaaa/OneDrive - Linnéuniversitetet/Dokument/Siljan/Manuscript2/METABOLIC_result.xlsx", sheet= 'MW_scores')
```

    ## New names:
    ## • `` -> `...11`

``` r
MW_heatmap <- MW[, -c(11) ] #remove last column

colnames(MW_heatmap) <- c("Function",
"MM_a",
"MM_c",
"MM_d",
"M_a",
"M_b",
"M_d",
"MO_b",
"MO_c",
"MO_b2"

)

Functions <- c(
  'C-S-01:Organic carbon oxidation - amino acid utilization',
  'C-S-01:Organic carbon oxidation - aromatics degradation', 
  'C-S-01:Organic carbon oxidation - complex carbon degradation',
  'C-S-01:Organic carbon oxidation - formate oxidation',
  'C-S-02:Carbon fixation - Wood-Ljungdahl pathway', 
  'C-S-03:Ethanol oxidation', 
  'C-S-05:Hydrogen generation', 
  'C-S-06:Fermentation', 
  'C-S-07:Methanogenesis',
  'N-S-01:N2 fixation - nifDK||vnfDKG||nifH', 
  'O-S-01:Iron reduction', 
  'S-S-03:Sulfur oxidation - sdo',
  'S-S-06:Sulfite reduction - asrABC'
)


MW_heatmap <- MW_heatmap %>% dplyr::filter(Function %in% Functions)

melted_data <- melt(MW_heatmap) 
```

    ## Using Function as id variables

``` r
replacements <- c(".*[:]" = "", "Organic" = "Org.", "carbon" = "C", "oxidation" = "ox.", "degradation" = "degr.", "fixation" = "fix.")

for (key in names(replacements)) {
  melted_data$Function <- gsub(key, replacements[[key]], melted_data$Function)
}

melted_data$Function <- factor(melted_data$Function, levels=unique(melted_data$Function))


# MetaT
MW_T <- read_excel("C:/Users/fevaaa/OneDrive - Linnéuniversitetet/Dokument/Siljan/Manuscript2/METABOLIC_result_MetaT.xlsx", sheet= 'MW_score')
MW_heatmap_T <- MW_T[, -c(5)] # remove  additional sample
MW_heatmap_T <-MW_heatmap_T %>% dplyr::filter(Function %in% Functions)
colnames(MW_heatmap_T) <- c("Function",
                          "MM_a",
                          "MM_c",
                          "MM_d",
                          "M_a",
                          "M_b",
                          "M_d",
                          "MO_b",
                          "MO_c",
                          "MO_b2"
)

melted_data_T <- melt(MW_heatmap_T) 
```

    ## Using Function as id variables

``` r
for (key in names(replacements)) {
  melted_data_T$Function <- gsub(key, replacements[[key]], melted_data_T$Function)
}

melted_data_T$Function <- factor(melted_data_T$Function, levels=unique(melted_data_T$Function))


multiplotmw <- bind_rows(list(Metatranscriptomics = melted_data_T, Metagenomics= melted_data), .id="id")

(multiplot_mw <- ggplot(multiplotmw, aes(x = variable, forcats::fct_rev(Function))) +
    geom_tile(aes(fill= value), col="black" ) +
    #geom_point(aes(size= value, fill= value), shape=21, stroke=0)  +
    #geom_hline(yintercept = seq(.5, 16.5, 1),  size = .1) +
    #geom_vline(xintercept = seq(.5, 9.5, 1), size= .1) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(labels = c("N2 fix. - nifDK||vnfDKG||nifH" = expression(N[2]~"fixation"~-italic("nifDK|vnfDKG|nifH")),
                                "Sulfur ox. - sdo" = expression("Sulfur oxidation"~-italic("sdo")),
                                "Sulfite reduction - asrABC"  = expression("Sulfur reduction"~-italic("asrABC"))
    )) +
    # scale_radius(range = c(1, 15)) +
    scale_fill_gradient(low = "white", high = "navy", breaks = c(0, 4 , 8, 14) , limits=c(0,14), guide = guide_legend()) +
    labs(x = NULL, y = NULL, title = "Molecular Weight per sample", fill= 'MW score', size = 'MW score') +
    theme_bw(base_size = 15) +
    scale_size_continuous(limits=c(0,14), breaks = c(0,4,8,14), range=c(1,11)) +
    theme(legend.position = "right", 
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.text.x=element_text(angle=60, hjust=0),
          strip.text = element_text(size = 16, face = "bold")) +
    guides(size = guide_legend(),
           fill = guide_legend() 
    ) +
    facet_wrap(~id, strip.position = 'bottom')+
    theme(strip.background =element_rect(fill="white"))
)
```

![](Siljancode_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
multiplot_mw
```

![](Siljancode_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
#ggsave("MW-score.png", dpi = "retina")
```

### Contribution Metabolic

``` r
sample_names <- c(
  "VK-3515-media-a_S10" = "M_a2",
  "VK-3515-media-gw-b_S2" = "M_b",
  "VK-3515-media-gw-oil-b_S4" = "MO_b",
  "VK-3515-media-gw-oil-c_S5" = "MO_c",
  "VK-3515-media-gw-a_S1" = "M_a",
  "VK-3515-methanol-c_S7" = "MM_c",
  "VK-3515-methanol-a_S6" = "MM_a",
  "VK-3515-media-oil-b_S9" = "MO_b2",
  "VK-3515-methanol-d_S8" = "MM_d",
  "VK-3515-media-gw-d_S3" = "M_d"
)

selected_columns <- c(
  'Function',
  'MEGAHIT-MaxBin2Refined-VK-3516-media-gw-oil-b_S38_L002.007',
  'MEGAHIT-MetaBAT2Refined-VK-3516-media-oil-b_S28_L002.19',
  'MEGAHIT-MetaBAT2Refined-VK-3516-media-gw-a_S11_L002.11',
  'MEGAHIT-MetaBAT2Refined-VK-3516-media-oil-b_S28_L002.17',
  'MEGAHIT-MetaBAT2Refined-VK-3516-media-gw-oil-b_S38_L002.4',
  'MEGAHIT-MaxBin2Refined-VK-3516-media-gw-oil-b_S38_L002.011_sub',
  'MEGAHIT-MaxBin2Refined-VK-3516-media-gw-oil-b_S38_L002.006_sub',
  'MEGAHIT-MaxBin2Refined-VK-3516-media-gw-a_S11_L002.001',
  'MEGAHIT-MetaBAT2Refined-VK-3516-methanol-a_S14_L002.1',
  'MEGAHIT-MetaBAT2Refined-VK-3516-methanol-d_S36_L002.5',
  'MEGAHIT-MaxBin2Refined-VK-3516-media-gw-oil-b_S38_L002.009',
  'MEGAHIT-MetaBAT2Refined-VK-3516-media-gw-oil-b_S38_L002.3'
)


process_sheet <- function(sheet_name) {
  df <- read_excel("C:/Users/fevaaa/OneDrive - Linnéuniversitetet/Dokument/Siljan/Manuscript2/METABOLIC_result.xlsx", sheet = sheet_name) %>%
    filter(Function %in% Functions) %>%
    select(all_of(selected_columns)) %>%
    melt(id.vars = "Function")
  colnames(df)[2] <- "Genome"
  df %>% left_join(bintax, by = "Genome") %>% select(taxon, Function, value)
}

# Process Data
Media_d <- process_sheet('Media_d')
Oil_c <- process_sheet('Oil_c')
Methanol_d <- process_sheet('Methanol_d')

mw <- bind_rows(list(Methanol = Methanol_d, Media = Media_d, Oil = Oil_c), .id = "id")

# Clean Function Names
mw$Function <- mw$Function %>%
  sub(".*:", "", .) %>%
  sub("Organic carbon oxidation", "Org. C ox.", .) %>%
  sub("fixation", "fix.", .) %>%
  factor(levels = unique(.))

# Factorize ID
mw$id <- factor(as.character(mw$id), levels = unique(mw$id))

# Process MetaT Sheets
process_metaT_sheet <- function(sheet_name) {
  df <- read_excel("C:/Users/fevaaa/OneDrive - Linnéuniversitetet/Dokument/Siljan/Manuscript2/METABOLIC_result_MetaT.xlsx", sheet = sheet_name) %>%
    filter(Function %in% Functions) %>%
    select(all_of(selected_columns)) %>%
    melt(id.vars = "Function")
  colnames(df)[2] <- "Genome"
  df %>% left_join(bintax, by = "Genome") %>% mutate(value = as.numeric(value))
}

# Process MetaT Data
Media_d_T <- process_metaT_sheet('media-gw-d')
```

    ## New names:
    ## • `` -> `...2`

``` r
Oil_c_T <- process_metaT_sheet('oil-gw-c')
```

    ## New names:
    ## • `` -> `...2`

``` r
Methanol_d_T <- process_metaT_sheet('methanol-d')
```

    ## New names:
    ## • `` -> `...2`

``` r
# Bind MetaT Data
contribution_T <- bind_rows(list(Methanol = Methanol_d_T, Media = Media_d_T, Oil = Oil_c_T), .id = "id")

# Clean Function Names in MetaT Data
contribution_T$Function <- contribution_T$Function %>%
  sub(".*:", "", .) %>%
  sub("Organic carbon oxidation", "Org. C ox.", .) %>%
  sub("fixation", "fix.", .) %>%
  factor(levels = unique(.))

# Factorize ID
contribution_T$id <- factor(as.character(contribution_T$id), levels = unique(contribution_T$id))

####### combined heatmaps ######


library(patchwork)

library(RColorBrewer)
mw$taxon <- as.factor(mw$taxon)
my_colors <-  c("Proteiniclasticum" = '#19381F', 
                                           "Sphaerochaeta" = "#6457A6", 
                                           "Proteiniphilum" = "#8dd3c7", 
                                           'Paludibacter sp018054805'= "#A4243B", 
                                           'Sedimentibacter'= "#583E23" , 
                                           'Acetobacterium'= "#FF9039", 
                                           'Methanogranum sp019262145'= '#56B4E9', 
                                           'Acetobacterium sp003260995' ="#009E73", 
                                           'Humidesulfovibrio'= "grey",
                                           'Trichococcus'= "#DA342F",
                                           'UBA1547'= "#A91E90", 
                                           'Proteiniclasticum'= "#FFE347",
                                           'Macellibacteroides sp018054455'= "#125E8A")
# Ensure both columns are characters first
mw$Function <- as.character(mw$Function)
contribution_T$Function <- as.character(contribution_T$Function)

# Get the shared functions between both datasets
shared_functions <- intersect(mw$Function, contribution_T$Function)

# Apply the same factor levels in the same order for both datasets
mw$Function <- factor(mw$Function, levels = shared_functions)
contribution_T$Function <- factor(contribution_T$Function, levels = shared_functions)


# Plot 1 (Panel a)
hm5_plot1 <- ggplot(mw, aes(x = taxon, forcats::fct_rev(Function))) +
  geom_point(aes(size = value, fill = taxon), shape = 21, stroke = 0.6) +
  scale_x_discrete(position='top')+
  scale_y_discrete(labels = c("N2 fix. - nifDK||vnfDKG||nifH" = expression(N[2]~"fix."~-italic("nifDK|vnfDKG|nifH")),
                                                "Sulfur oxidation - sdo" = expression("Sulfur ox."~-italic("sdo")),
                                                "Sulfite reduction - asrABC"  = expression("Sulfur reduction"~-italic("asrABC"))
                                                )) +
  scale_fill_manual(values = my_colors, guide = 'none') + 
# scale_color_brewer(palette = "Set2")  +
  labs(x = NULL, y = NULL, title = NULL, fill = NULL, size = 'Contribution') +
  theme_void(base_size = 15) +
  scale_size_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), range = c(1, 7), guide = guide_legend()) +
  theme(
    legend.position = "right", 
    panel.grid.major = element_blank(),
    axis.text.y = element_text(size = 15, face = "bold", hjust = 1),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 14),
    axis.text.x = element_text(angle = 90, hjust = 0, face = "italic")
  ) +
  facet_wrap(~id, strip.position = 'bottom', scales = 'fixed') +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 14, face = "bold"))+ 
  guides(size = guide_legend(title = "Contribution"))

# Plot 2 (Panel b)
hm5_plot2 <- ggplot(contribution_T, aes(x = taxon, forcats::fct_rev(Function))) +
  geom_point(aes(size = value, fill = taxon), shape = 21, stroke = 0.6) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(labels = c("N2 fix. - nifDK||vnfDKG||nifH" = expression(N[2]~"fix."~-italic("nifDK|vnfDKG|nifH")),
                              "Sulfur oxidation - sdo" = expression("Sulfur ox."~-italic("sdo")),
                              "Sulfite reduction - asrABC"  = expression("Sulfur reduction"~-italic("asrABC"))
  )) +
  scale_fill_manual(values = my_colors, guide = 'none') +
  labs(x = NULL, y = NULL, title = NULL, fill = NULL, size = 'Contribution') +
  theme_void(base_size = 15) +
  scale_size_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), range = c(1, 7), guide = guide_legend()) +
  theme(
    legend.position = "right", 
    panel.grid.major = element_blank(),
    axis.text.y = element_text(size = 15, face = "bold", hjust = 1),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 14),
    axis.text.x = element_text(angle = 90, hjust = 0, face = "italic")
  ) +
  facet_wrap(~id, strip.position = 'bottom', scales = 'fixed') +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 14, face = "bold"))+ 
  guides(size = guide_legend(title = "Contribution"))

# Combine the two plots into one figure with shared legend
combined_plot <- (hm5_plot1 / hm5_plot2) + 
  plot_layout(ncol = 1, heights = c(1, 1), guides = "collect") +  # Shared legend
  plot_annotation(tag_levels = 'a')+ 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())+ 
  guides(size = guide_legend(title = "Contribution"))

# Display the combined plot
print(combined_plot)
```

![](Siljancode_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

#### MetaT supplemental

``` r
eggnog_data <- read_excel("eggnog.xlsx")

eggnog_data <- eggnog_data %>%
  dplyr::rename(
    KO = kegg_ko,
    Gene =  preferred_name,
    orf = query
  )

Metabolic <- read_excel("Metabolic_genes.xlsx")

target_genes <- Metabolic$KO 

species_pathways <- eggnog_data %>%
  filter(KO %in% target_genes) %>%
  left_join(Metabolic, by = "KO") 
```

    ## Warning in left_join(., Metabolic, by = "KO"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 57 of `x` matches multiple rows in `y`.
    ## ℹ Row 118 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
# Aggregate per pathway and bin
species_pathways<- species_pathways %>%
  filter(KO %in% target_genes) %>%
  group_by(KO, bin,Pathway, Name) %>%
  
  mutate(bin = recode(bin,
                      'MEGAHIT-MaxBin2Refined-VK-3516-media-gw-a_S11_L002.001' = 'Can. Methanogranum gryphiswaldense',
                      'MEGAHIT-MaxBin2Refined-VK-3516-media-gw-oil-b_S38_L002.006_sub' = 'Acetobacterium',
                      'MEGAHIT-MaxBin2Refined-VK-3516-media-gw-oil-b_S38_L002.007' = 'Proteiniclasticum',
                      'MEGAHIT-MaxBin2Refined-VK-3516-media-gw-oil-b_S38_L002.011_sub' = 'Sedimentibacter',
                      'MEGAHIT-MetaBAT2Refined-VK-3516-media-gw-oil-b_S38_L002.3' = 'UBA1547',
                      'MEGAHIT-MetaBAT2Refined-VK-3516-media-gw-oil-b_S38_L002.4' = 'Paludibacter sp018054805',
                      'MEGAHIT-MetaBAT2Refined-VK-3516-media-oil-b_S28_L002.19' = 'Sphaerochaeta',
                      'MEGAHIT-MetaBAT2Refined-VK-3516-methanol-d_S36_L002.5' = 'Humidesulfovibrio',
                      'MEGAHIT-MetaBAT2Refined-VK-3516-methanol-a_S14_L002.1' = 'Acetobacterium KB-1',
                      .default = bin  # Keeps other values unchanged
  )) 


# Convert KO (pathway) to a factor for distinct colors
species_pathways$Pathway <- as.factor(species_pathways$Pathway)

species_pathways <- species_pathways %>%
  mutate(Name = forcats::fct_reorder(Name, as.numeric(factor(Pathway)), .fun = mean))

ggplot(species_pathways, aes(x = bin, y = Name, fill = Pathway)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +  # White borders, smaller tiles
  scale_fill_viridis_d(option = "plasma") +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic", size= 14, color = "black"), 
    axis.text.y = element_text(face = "italic", size = 14, color = "black"),  
    panel.grid = element_blank(), 
    axis.title = element_blank(),
    legend.title = (element_text(size=14)),
    legend.text = element_text(size = 13),
    axis.ticks = element_blank() ,
    plot.background = element_rect(fill = "white", color = NA)
  )
```

![](Siljancode_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
#ggsave('MetaTgenes.png', width=20, height= 37, units = 'cm')
```

#### Suplemental figures 1 and 2
```{r}

df1 <- Timeseries1
df1 <- df1[which(df1$Treatment == 'oil' | df1$Treatment=="medium"| df1$Treatment=="unfiltered"| df1$Treatment=="medium_without_e"| df1$Treatment=="control"),]
df2 <- Timeseries2
df2 <- df2[which(df2$Treatment == 'oil' | df2$Treatment=="medium"| df2$Treatment=="methanol"| df2$Treatment=="acetate"| df2$Treatment=="yeast"| df2$Treatment=="casein"| df2$Treatment=="control o"| df2$Treatment=="control y"| df2$Treatment=="control c"| df2$Treatment=="control m"| df2$Treatment=="control a"| df2$Treatment=="control"),]

high_oil_reps <- c("c", "d")  
low_oil_reps  <- c("a", "b")  

df1_split <- df1 %>%
  mutate(
    Treatment = case_when(
      Treatment == "oil" & Replicate %in% high_oil_reps ~ "oil_high",
      Treatment == "oil" & Replicate %in% low_oil_reps  ~ "oil_low",
      TRUE ~ Treatment
    )
  )

# Random intercept for each replicate
model_split <- lmer(total_ppm ~ Treatment * day_factor + (1 | Replicate), data = df1_split)

# Calculate estimated marginal means for Treatment at each day
emm_time <- emmeans(model_split, ~ Treatment | day_factor)

# Pairwise comparisons between treatments within each day
pairs_time <- pairs(emm_time, adjust = "tukey")

pairs_split <- emmeans(model_split, ~ Treatment | day_factor)
control_contrasts <- contrast(pairs_split, method = "trt.vs.ctrl", ref = "control")
summary(control_contrasts, adjust = "tukey")
pairs_time

pairs_df <- as.data.frame(pairs_time)

control_comparisons <- as.data.frame(pairs_time) %>%
  filter(!is.na(estimate)) %>% 
  filter(grepl("control", contrast)) %>% 
  arrange(day_factor) 

control_comparisons <- control_comparisons %>%
  mutate(
    # Identify which one is the non-control treatment
    Treatment = ifelse(grepl("^control", contrast),
                       sub("control - ", "", contrast),
                       sub(" - control$", "", contrast)),
    # Make sure the estimate is always Treatment minus control
    estimate = ifelse(grepl("^control", contrast), -estimate, estimate),
    # Add significance flag
    significant = p.value < 0.05
  ) %>%
  select(day_factor, Treatment, estimate, SE, p.value, significant)

#create figure
(SuppFigure1 <- ggplot(control_comparisons, 
                   aes(x = as.numeric(as.character(day_factor)),
                       y = estimate, color = Treatment)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 3) +
    geom_text(
      data = subset(control_comparisons, significant),
      aes(label = "*"), 
      vjust = -0.8,  # adjust vertical position
      color = "black",
      size = 6
    ) +
    facet_wrap(~ Treatment, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "Difference from control over time",
      y = "Treatment - Control methane (ppm)",
      x = "Day"
    ))

model2 <- lmer(total_ppm ~ Treatment * day_factor + (1 | Replicate), data = df2)
emm_time2 <- emmeans(model2, ~ Treatment | day_factor)

# Pairwise comparisons between treatments within each day
pairs_time2 <- pairs(emm_time2, adjust = "tukey")

pairs_df2 <- as.data.frame(pairs_time2)

# Remove rows with non-estimable results (nonEst)
pairs_clean2 <- pairs_df2 %>%
  filter(!is.na(estimate))  # nonEst rows have NA estimates

control_comparisons2 <- as.data.frame(pairs_time2) %>%
  filter(!is.na(estimate)) %>%   # remove nonEst
  filter(grepl("control", contrast)) %>%  # only contrasts with control
  arrange(day_factor)  # sort by day

control_comparisons2 <- control_comparisons2 %>%
  mutate(
    # Identify which one is the non-control treatment
    Treatment = ifelse(grepl("^control", contrast),
                       sub("control - ", "", contrast),
                       sub(" - control$", "", contrast)),
    # Make sure the estimate is always Treatment minus control
    estimate = ifelse(grepl("^control", contrast), -estimate, estimate),
    # Add significance flag
    significant = p.value < 0.05
  ) %>%
  select(day_factor, Treatment, estimate, SE, p.value, significant)

#Create figure 2
(SuppFig2 <- ggplot(control_comparisons2, 
                   aes(x = as.numeric(as.character(day_factor)),
                       y = estimate, color = Treatment)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 3) +
  geom_text(
    data = subset(control_comparisons2, significant),
    aes(label = "*"), 
    vjust = -0.4,  # adjust vertical position
    color = "black",
    size = 6
  ) +
  facet_wrap(~ Treatment, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Difference from control over time",
    y = "Treatment - Control methane (ppm)",
    x = "Day"
  ))

```


####Species Richness

```{r}

# Load and clean data
Abundance <- read_excel("C:/Users/fevaaa/OneDrive - Linnéuniversitetet/Dokument/Siljan/Manuscript2/Data Manuscript 2.xlsx", sheet= 'Samples')
Abundance <- Abundance[c(2:13)]
colnames(Abundance) <- c('Genome','Media_a','Media_b','Media_d',
                         'Oil_b','Oil_c','Oil2_b',
                         'Methanol_a','Methanol_c','Methanol_d',
                         'Total','Classification')

# Filter rows where Total > 0.1
Abundance <- Abundance %>% filter(Total > 0.1)

# Create treatment-specific data frames (drop Genome)
media_data <- Abundance %>% select(Media_a, Media_b, Media_d)
oil_data <- Abundance %>% select(Oil_b, Oil_c, Oil2_b)
methanol_data <- Abundance %>% select(Methanol_a, Methanol_c, Methanol_d)

# Transpose so rows = samples, columns = taxa
media_t <- as.data.frame(t(media_data))
oil_t <- as.data.frame(t(oil_data))
methanol_t <- as.data.frame(t(methanol_data))

media_t <- round(media_t)
oil_t <- round(oil_t)
methanol_t <- round(methanol_t)

# Calculate richness for each treatment
media_richness <- data.frame(
  Treatment = "Media",
  Sample = rownames(media_t),
  Observed = specnumber(media_t),
  Chao1 = estimateR(media_t)[2, ]
)

oil_richness <- data.frame(
  Treatment = "Oil",
  Sample = rownames(oil_t),
  Observed = specnumber(oil_t),
  Chao1 = estimateR(oil_t)[2, ]
)

methanol_richness <- data.frame(
  Treatment = "Methanol",
  Sample = rownames(methanol_t),
  Observed = specnumber(methanol_t),
  Chao1 = estimateR(methanol_t)[2, ]
)

# Combine results
richness_all <- bind_rows(media_richness, oil_richness, methanol_richness)

# View the results
print(richness_all)

library(ggplot2)

ggplot(richness_all, aes(x = Treatment, y = Observed)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.2, size = 3) +
  labs(title = "Observed Species Richness by Treatment",
       y = "Observed Richness",
       x = "") +
  theme_minimal()

ggplot(richness_all, aes(x = Treatment, y = Chao1)) +
  geom_boxplot(fill = "lightgreen") +
  geom_jitter(width = 0.2, size = 3) +
  labs(title = "Chao1 Estimated Richness by Treatment",
       y = "Chao1 Richness",
       x = "") +
  theme_minimal()

# Test normality within each treatment for Observed richness
by(richness_all$Observed, richness_all$Treatment, shapiro.test)
# Test variance across groups
library(car)
leveneTest(Observed ~ Treatment, data = richness_all)

kruskal.test(Observed ~ Treatment, data = richness_all)

```

