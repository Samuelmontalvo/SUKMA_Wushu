library(readr)
Df <- read_csv("Project-Session-09_13_22-Session-09_13_22-AveragesAndTests_Countermovement_Jump.csv")
View(Df)

library(dplyr)
library(ggplot2)
library(PupillometryR)
library(ggprism)
library(flextable)
library(officer)
library(table1)
library(janitor) #convert spaces to _
library(interlimb)
library(caret)
library(ggpubr)
library(ggridges)
library(plotly)
## Reduce jump trials to average
Df_average <- Df %>% filter(TestId == "AVERAGE")
View(Df_average)

## import participant details
library(readxl)
Df_details <- read_excel("Athletes details SUKMA XX.xlsx")
View(Df_details)

# change capital name letters so that data sets match
Df_details <- Df_details %>% rename(Name = NAME)

# re name males and females
Df_details$Sex <- recode_factor(Df_details$Sex,  F = "Female",
                                M = "Male")
# merge data sets by Name
Df <- inner_join(x=Df_details,y=Df_average,
                 by = "Name")
View(Df)

## Subset NA
Df <- Df |>
  subset(!is.na(`Jump Height`))
View(Df)

# convert spaces to _
Df <- clean_names(Df)

#remove names
Df <- Df %>% select(-c(name))

## Save as a new csv file
write.csv(Df,"C:\\Users\\Samuel\\OneDrive\\Documents\\R\\SUKMA_Wushu\\SUKMA_Wushu_Data.csv", row.names = FALSE)


## QQ PLOTS

Df %>% ggplot(aes(y=jump_height,group=sex,fill=sex)) +
  geom_density_ridges(alpha=.5) +
  ggtitle("CMJ") +
  xlab("sec") + ylab("") + geom_density_ridges(quantile_lines=TRUE,quantile_fun=function(x,...)mean(x),
                                               alpha=.2) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.05, 0.95),
                      alpha = 0.2) + theme_prism()
ggsave("qqplot_FP.png")





p1 <- Df_female %>% ggplot(y=jump_height,x=zscore) +
  geom_point(aes(y=jump_height,x=zscore,colour=Description),
             position = position_jitter(width = .25), size = 3, shape=20) +
   scale_color_manual(values = c("#125016", "#1b7821", "#24a02c",
                                "#125016", "#000000", "#e7b416",
                                "#CC3232","#a32828","#7a1e1e")) +
  theme_bw() + labs(title ="Females", y="Jump Height (m)")

ggplotly(p1)

p2 <- Df_male %>% ggplot(y=jump_height,x=zscore) +
  geom_point(aes(y=jump_height,x=zscore,colour=Description,shape = Classification),
             position = position_jitter(width = .2), size = 3) +
  scale_color_manual(values = c("#125016", "#1b7821", "#24a02c",
                                "#2dc937", "#000000", "#e7b416",
                                "#CC3232","#a32828","#7a1e1e")) +
  theme_bw() + labs(title ="Male",y="Jump Height (m)")

ggplotly(p2)

library(ggpubr)
ggarrange(p1,p2)
## Average Jump height by Sex
Df %>% ggplot(aes(x = sex, y = jump_height, fill = sex)) +
  geom_flat_violin(aes(fill = sex),
      position = position_nudge(x =.1, y=0), adjust = 1.5,trim = F, alpha =.5) +
  geom_point(aes(x=sex, y=jump_height,colour=sex),
             position = position_jitter(width = .05), size = 2.5, shape = 20)+
  geom_boxplot(aes(x = sex, y = jump_height, fill = sex),outlier.shape
               = NA, alpha = .5, width = .15, colour = "black")+
  coord_cartesian(ylim=c(0.22,0.72)) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+ theme_bw() +
  ylab('Vertical Jump Height (m)')+
  xlab('Group') + theme_prism()

ggsave("Vertical_jump_height.png")



Df %>% ggplot(aes(x = events, y = jump_height, fill = sex)) +
  geom_flat_violin(aes(fill = sex),
                   position = position_nudge(x =.1, y=0), adjust = 1.5,trim = F, alpha =.5) +
  geom_point(aes(x=events, y=jump_height,colour=sex),
             position = position_jitter(width = .05), size = 2.5, shape = 20)+
  geom_boxplot(aes(x = events, y = jump_height, fill = sex),outlier.shape
               = NA, alpha = .5, width = .15, colour = "black")+
  coord_cartesian(ylim=c(0.22,0.75)) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+ theme_bw() +
  ylab('Vertical Jump Height (m)')+
  xlab('Group') + theme_prism()
ggsave("Vertical_jump_height_bygroup_bysex.png")


Df %>% ggplot(aes(x = events, y = jump_height, fill = events)) +
  geom_flat_violin(aes(fill = events),
                   position = position_nudge(x =.1, y=0), adjust = 1.5,trim = F, alpha =.5) +
  geom_point(aes(x=events, y=jump_height,colour=events),
             position = position_jitter(width = .05), size = 2.5, shape = 20)+
  geom_boxplot(aes(x = events, y = jump_height, fill = events),outlier.shape
               = NA, alpha = .5, width = .15, colour = "black")+
  coord_cartesian(ylim=c(0.22,0.8)) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+ theme_bw() +
  ylab('Vertical Jump Height (m)')+
  xlab('Group') + theme_prism()
ggsave("Vertical_jump_height_bygroup.png")


library(table1)

table1(~jump_height | events*sex, overall=FALSE,
        data=Df)

################## Function needed for tables in APA format###################
apa_theme <- function (ft)  {
  ft %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = 12, part = "all") %>%
    flextable::align(align = "left", part = "body") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::rotate(rotation = "lrtb", align = "top", part = "body") %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = officer::fp_border(width = 2),
                         part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2),
                            part = "all") %>%
    flextable::hline(i = 1, border = officer::fp_border(width = 1), part = "header") %>%
    flextable::set_table_properties(layout = "autofit")}




# table with mean and SD

Table_Jump_Height <- Df  %>% group_by(Sex) %>%
  mutate(count=n()) %>%
  summarise_at(vars(`Jump Height`),
               list( n=length, Mean = mean, SD = sd)) %>%
  mutate_if(is.numeric, round, 2,) %>% flextable() %>% autofit()

# Save as word .docx
Table_Jump_Height %>% apa_theme
save_as_docx(Table_Jump_Height,
             path = "Table_Jump_Height.docx",
             pr_section =
               prop_section(page_size = page_size(orient = "portrait"),
                            type = "continuous"))


#Asymmetry Force bilateral
## dl - ndl / dl + ndl * 100 (for wushu it is assumed all are right dominant)

Df <- Df %>% mutate(Force_asymmetry_at_peak_porpulsive_force =
            ((right_force_at_peak_propulsive_force- left_force_at_peak_braking_force) /
           (right_force_at_peak_propulsive_force+ left_force_at_peak_braking_force) *100))


Df


label(Df$jump_height)  <- "Jump Height (m)"
label(Df$propulsive_net_impulse) <- "Propulsive Net Impulse (N-s)"
label(Df$right_force_at_peak_propulsive_force) <- "Right Limb Force at Peak Propulsive Force (n)"
label(Df$left_force_at_peak_braking_force) <- "Left Limb Force at Peak Propulsive Force (n)"
label(Df$IL_asymmetry_peak_propulsive_Force) <- "Inter-limb Asymmetry at Peak Propulsive Force"


Df <- Df %>% mutate(HL_peak_propulsive_force =
                      pmax(right_force_at_peak_propulsive_force,
                             left_force_at_peak_braking_force))

Df <- Df %>% mutate(LL_peak_propulsive_force =
                      pmin(right_force_at_peak_propulsive_force,
                           left_force_at_peak_braking_force))

Df <- Df %>% mutate(IL_asymmetry_peak_propulsive_Force =
                      ((HL_peak_propulsive_force - LL_peak_propulsive_force) /
                   (HL_peak_propulsive_force +LL_peak_propulsive_force) * 100))




table1(~ jump_height + propulsive_net_impulse +
         right_force_at_peak_propulsive_force +left_force_at_peak_braking_force +
         IL_asymmetry_peak_propulsive_Force|
         events*sex, data=Df)


one.way <- aov(jump_height ~ sex*events, data = Df)

summary(one.way)



Df %>% ggplot(aes(x = IL_asymmetry_peak_propulsive_Force,
                  y = jump_height, color=events)) +  geom_point()+
  geom_smooth(method=lm) + stat_cor(label.x=0, label.y=0.7, aes(label=
                paste(..rr.label.., ..p.label.., sep = "~','~"))) +
  stat_regline_equation(label.x=3, label.y=0.7) +
  theme_prism()


