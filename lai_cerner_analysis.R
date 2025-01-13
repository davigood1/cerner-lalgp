if (!require("pacman")) install.packages("pacman")
if (!requireNamespace("ggplot2", versionCheck = "3.4.0", quietly = TRUE)) {
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
}

library("pacman")
p_load(tidyverse, lubridate, stringr, tidylog, skimr, #Data wrangling
       IRdisplay, kableExtra, gtsummary) #table making

#R.Version()

## define custom test for tables
fisher.test.simulate.p.values <- function(data, variable, by, ...) {
  result <- list()
  test_results <- stats::fisher.test(data[[variable]], data[[by]], simulate.p.value = TRUE)
  result$p <- test_results$p.value
  result$test <- test_results$method
  result
}

#options(repr.matrix.max.cols=150, repr.matrix.max.rows=200)

theme_gtsummary_journal(journal = "jama")
#> Setting theme `JAMA`
theme_gtsummary_compact()
#> Setting theme `Compact`

p_load(mice, miceadds, estimatr, broom, naniar, howManyImputations)
#p_load_gh("cran/survey")
p_load(survey, MatchThem, cobalt)
#Load files
df.central <- read_csv("df_central.csv")

df.central %>% names()
#Usage statistics
df.central %>%
    filter(`Long acting glycopeptide (yes/no)` == "Yes") %>%
    select(`Bacteremia, isolated` = isolated_bacteremia,
        Osteomyelitis = dx_Osteomyelitis, 
        `Septic arthritis` = `dx_Septic arthritis`,
        Endocarditis = dx_Endocarditis,
        #Piomyositis = dx_Myositis,
          glyco_name_short) %>%
    tbl_summary( )  



df.central %>%
    filter(`Long acting glycopeptide (yes/no)` == "Yes") %>%
    select(`Bacteremia, isolated` = isolated_bacteremia,
        Osteomyelitis = dx_Osteomyelitis, 
        `Septic arthritis` = `dx_Septic arthritis`,
        Endocarditis = dx_Endocarditis,
        #Piomyositis = dx_Myositis, 
          glyco_name_short) %>%
    tbl_summary(by = glyco_name_short,
               percent = "column")  %>%
    add_p() %>%
    add_overall() 

df.central %>%
    filter(`Long acting glycopeptide (yes/no)` == "Yes") %>%
    select(`Bacteremia, isolated` = isolated_bacteremia,
        Osteomyelitis = dx_Osteomyelitis, 
        `Septic arthritis` = `dx_Septic arthritis`,
        Endocarditis = dx_Endocarditis,
        #Piomyositis = dx_Myositis, 
        `Any SUD or drug use`) %>%
    tbl_summary(by = `Any SUD or drug use`,
               percent = "column")  %>%
    add_p() %>%
    add_overall() 

#Geographic
df.central %>%
    filter(`Any SUD or drug use` == "Yes") %>%
    select(
        `Long acting glycopeptide (yes/no)`,
      `One digit zip code` = zip_code) %>%
    tbl_summary(by = c(`Long acting glycopeptide (yes/no)`),
               percent = "row")  %>%
    add_p() %>%
    add_overall() 

df.central %>%
    filter(`Any SUD or drug use` == "No") %>%
    select(
        `Long acting glycopeptide (yes/no)`,
      `One digit zip code` = zip_code) %>%
    tbl_summary(by = c(`Long acting glycopeptide (yes/no)`),
               percent = "row")  %>%
    add_p() %>%
    add_overall() 
#Select variables for table 1
df.table.1 <- df.central %>%
    select(
        `Age, years` = age,
        Gender = gender,
        Race = race2,
        Ethnicity = ethnicity,
        Insurance = insurance,
        `Alcohol abuse`,
        `Blood loss anemia`,
        `Chronic peptic ulcer disease`,
        `Chronic pulmonary disease`,
        `Coagulation deficiency`,
        `Congestive heart failure`,
        `Deficiency anemias`,
        `Depression`,
        `Diabetes with chronic complications`,
        `Diabetes without chronic complications`,
        #`Drug abuse`,
        `Fluid and electrolyte disorders`,
        `HIV and AIDS` = `HIV and AIDS (Acquired immune deficiency syndrome)`,
        `Hypertension, complicated`,
        `Hypertension, uncomplicated`,
        `Hypothyroidism`,
        `Liver disease`,
        `Lymphoma`,
        `Metastatic cancer`,
        `Obesity`,
        `Other neurological disorders`,
        `Paralysis`,
        `Peripheral vascular disease`,
        `Psychoses`,
        `Pulmonary circulation disorders`,
        `Renal failure`,
        `Rheumatoid arthritiscollagen vascular diseases`,
        `Solid tumor without metastasis`,
        `Valvular disease`,
        `Weight loss`,
        `Elixhauser (AHRQ), index` = ElixhauserAHRQ,
        `Elixhauser, number of conditions` = Elixhauser_conditions_num,
        `Elixhauser, number of conditions (grouped)` = Elixhauser_conditions_groups,
        `History of opioid use or disorder`,
        `History of cocaine use or disorder`,
        `History of methamphetamine use or disorder`,
        `Injection drug use` = idu,
        `Any SUD or drug use`,
        MOUD = moud,
        #`Length of stay, index admission` = los_index,  # Add the missing backtick here
        #Discharge = discharge_disposition,
        `Bed size` = bed_size,
        `One digit zip code` = zip_code,
        `Bacteremia, isolated` = isolated_bacteremia,
        Osteomyelitis = dx_Osteomyelitis, 
        `Septic arthritis` = `dx_Septic arthritis`,
        Endocarditis = dx_Endocarditis,
        #Piomyositis = dx_Myositis, 
        `Long acting glycopeptide (yes/no)`
        )
#Display table 1 using gtsummary for PWUD
tbl.1.pwud <- df.table.1 %>% 
    filter(`Any SUD or drug use` == "Yes") %>%
    select(-c(`Any SUD or drug use`)) %>%
    tbl_summary(by = `Long acting glycopeptide (yes/no)`,
               sort = list(everything() ~ "frequency",
                           `Elixhauser, number of conditions (grouped)` ~ "alphanumeric"),
               statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
    add_p(test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                     all_continuous() ~ "t.test")) %>%
    modify_caption("Table 1. Patient characteristics for PWUD") %>%
    bold_labels() #%>% 
#    add_overall()
               

tbl.1.pwud
#Display table 1 using gtsummary non PWUD
tbl.1.non.pwud <- df.table.1 %>% 
    filter(`Any SUD or drug use` == "No") %>% 
    select(-c(`History of opioid use or disorder`,
        `History of cocaine use or disorder`,
        `History of methamphetamine use or disorder`,
        `Injection drug use`,
        `Any SUD or drug use`,
        MOUD)) %>%
    tbl_summary(by = `Long acting glycopeptide (yes/no)`,
               sort = list(everything() ~ "frequency",
                           `Elixhauser, number of conditions (grouped)` ~ "alphanumeric"),                           
               statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
    add_p(test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                     all_continuous() ~ "t.test")) %>%
    #add_p(test.args = all_tests("fisher.test") ~ list(workspace=2e9)) %>%
    modify_caption("Table 1. Patient characteristics for non-PWUD") %>%
    bold_labels() #%>% 
#    add_overall()
               

tbl.1.non.pwud 
#Merge tables
tbl.1.merged <- tbl_merge(
    tbls = list(tbl.1.pwud , tbl.1.non.pwud ),
    tab_spanner = c("**PWUD**", "**non-PWUD**")
  ) %>%
    modify_caption("Table 1. Patient characteristics by receipt of long acting glycopeptide")

tbl.1.merged 
dir.create("~/results", showWarnings = FALSE, recursive = TRUE)

#Save table
tbl.1.merged %>% 
    as_kable() %>%
    kable_styling("striped") %>%
    save_kable(paste0("results/table_1_", Sys.Date(), ".html"))
p_load(rcartocolor, devtools, ggsci, ggmosaic, ggcharts, cowplot, RColorBrewer) 

if (!requireNamespace("ggdist", versionCheck = "3.0", quietly = TRUE)) {
  install.packages("ggdist", repos = "http://cran.us.r-project.org")
}
if (!requireNamespace("ggpubr", versionCheck = "0.3", quietly = TRUE)) {
  install.packages("ggpubr", repos = "http://cran.us.r-project.org")
}

p_load(ggsignif, ggpubr)

theme_set(theme_minimal(base_size = 20))
options(repr.plot.width=20, repr.plot.height=10)
#Age
p.age <- df.table.1 %>%
ggplot(aes(x = `Long acting glycopeptide (yes/no)`, y = `Age, years`, fill = `Any SUD or drug use`))  + 
  ggdist::stat_halfeye(fill = "blue", alpha = 0.5,
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_point(aes(),
    size = 0.1,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  scale_y_continuous(limits = c(15,80)) +
  scale_x_discrete(position = "bottom") +
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  scale_color_lancet() +
  scale_fill_lancet() +
  labs(title = "Age distribution by laLGP and PWUD status") +
  facet_wrap(~ `Any SUD or drug use`, nrow = 1,   strip.position = "top", 
              labeller = label_both) +
  labs(x = "Long acting glycopeptide (yes/no)", y = "Age") +
  #theme_minimal()  +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(panel.spacing.y = unit(0, "lines"))

p.age

ggsave("results/plots/plot_age.pdf", width = 10, height = 10, dpi = 300)
#Gender, Race, Ethnicity, Insurance
df.gender.plot <- df.table.1 %>%
    select(Gender, `Any SUD or drug use`,
        `Long acting glycopeptide (yes/no)`) %>%
    mutate(value = 1) %>%
    group_by(`Any SUD or drug use`, `Long acting glycopeptide (yes/no)`, Gender) %>%
    summarise(value = sum(value)) %>%
    mutate(percentage = value / sum(value) * 100) %>%
    ungroup() %>%
    filter(!Gender == "Unknown")

df.race.plot <- df.table.1 %>%
    select(Race, `Any SUD or drug use`,
        `Long acting glycopeptide (yes/no)`) %>%
    mutate(value = 1) %>%
    group_by(`Any SUD or drug use`, `Long acting glycopeptide (yes/no)`, Race) %>%
    summarise(value = sum(value)) %>%
    mutate(percentage = value / sum(value) * 100) %>%
    ungroup() 

df.ethnicity.plot <- df.table.1 %>%
    select(Ethnicity, `Any SUD or drug use`,
        `Long acting glycopeptide (yes/no)`) %>%
    mutate(value = 1) %>%
    group_by(`Any SUD or drug use`, `Long acting glycopeptide (yes/no)`, Ethnicity) %>%
    summarise(value = sum(value)) %>%
    mutate(percentage = value / sum(value) * 100) %>%
    ungroup() 

df.insurance.plot <- df.table.1 %>%
    select(Insurance, `Any SUD or drug use`,
        `Long acting glycopeptide (yes/no)`) %>%
    mutate(value = 1) %>%
    group_by(`Any SUD or drug use`, `Long acting glycopeptide (yes/no)`, Insurance) %>%
    summarise(value = sum(value)) %>%
    mutate(percentage = value / sum(value) * 100) %>%
    ungroup() 
p.gender <- ggplot(df.gender.plot, 
                      aes(x = percentage, y = fct_rev(Gender), fill = `Long acting glycopeptide (yes/no)`)) +
  geom_col(position = "dodge") +
  scale_x_continuous(limits = c(0, 100)) +
    labs(x = "Percentage", y = "Gender", title = "Gender") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right") +
    facet_wrap(~`Any SUD or drug use`, nrow = 1, strip.position = "top", 
              labeller = label_both) 

p.race <- ggplot(df.race.plot, 
                      aes(x = percentage, y = fct_rev(Race), fill = `Long acting glycopeptide (yes/no)`)) +
  geom_col(position = "dodge") +
  scale_x_continuous(limits = c(0, 100)) +
    labs(x = "Percentage", y = "Race") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right") +
    facet_wrap(~`Any SUD or drug use`, nrow = 1, strip.position = "top", 
              labeller = label_both)

p.ethnicity <- ggplot(df.ethnicity.plot, 
                      aes(x = percentage, y = fct_rev(Ethnicity), fill = `Long acting glycopeptide (yes/no)`)) +
  geom_col(position = "dodge") +
  scale_x_continuous(limits = c(0, 100)) +
    labs(x = "Percentage", y = "Ethnicity") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right") +
    facet_wrap(~`Any SUD or drug use`, nrow = 1, strip.position = "top", 
              labeller = label_both)

p.insurance <- ggplot(df.insurance.plot, 
                      aes(x = percentage, y = fct_rev(Insurance), fill = `Long acting glycopeptide (yes/no)`)) +
  geom_col(position = "dodge") +
  scale_x_continuous(limits = c(0, 100)) +
    labs(x = "Percentage", y = "Insurance") +
    theme_minimal() +
    theme(legend.position = "right", text = element_text(size = 14)) +
    facet_wrap(~`Any SUD or drug use`, nrow = 1, strip.position = "top", 
              labeller = label_both)

list.plots <- list(p.gender, p.race, p.ethnicity, p.insurance)

list.plots

# Create a PDF file
pdf("results/plots/plot_demographics.pdf", width = 10, height = 10)

# Loop through the list of plots and print each plot to the PDF
for (i in 1:length(list.plots)) {
  print(list.plots[[i]])
}

# Close the PDF device
dev.off()
df.comorbidity.plot <- df.table.1 %>%
    select(
        `Alcohol abuse`,
        `Blood loss anemia`,
        `Chronic peptic ulcer disease`,
        `Chronic pulmonary disease`,
        `Coagulation deficiency`,
        `Congestive heart failure`,
        `Deficiency anemias`,
        `Depression`,
        `Diabetes with chronic complications`,
        `Diabetes without chronic complications`,
        #`Drug abuse`,
        `Fluid and electrolyte disorders`,
        #`HIV and AIDS (Acquired immune deficiency syndrome)`,
        `Hypertension, complicated`,
        `Hypertension, uncomplicated`,
        `Hypothyroidism`,
        `Liver disease`,
        `Lymphoma`,
        `Metastatic cancer`,
        `Obesity`,
        `Other neurological disorders`,
        `Paralysis`,
        `Peripheral vascular disease`,
        `Psychoses`,
        `Pulmonary circulation disorders`,
        `Renal failure`,
        `Rheumatoid arthritiscollagen vascular diseases`,
        `Solid tumor without metastasis`,
        `Valvular disease`,
        `Weight loss`,
        #`ElixhauserAHRQ`,
        `Any SUD or drug use`,
        `Long acting glycopeptide (yes/no)`) %>%
    group_by(`Any SUD or drug use`, `Long acting glycopeptide (yes/no)`) %>%
    pivot_longer(
        cols = `Alcohol abuse`:`Weight loss`,
        names_to = "Comorbidity",
        values_to = "value",
    ) %>%
    mutate(Present = if_else(value == 1, "Yes", "No"),
          value = 1) %>%
    group_by(`Any SUD or drug use`, `Long acting glycopeptide (yes/no)`, Comorbidity, Present) %>%
    summarise(value = sum(value)) %>%
    mutate(percentage = value / sum(value) * 100) %>%
    ungroup()
p.comorbidities <- ggplot(df.comorbidity.plot %>% filter(Present == "Yes"), 
                      aes(x = percentage, y = fct_rev(Comorbidity), fill = `Long acting glycopeptide (yes/no)`)) +
    geom_col(position = "dodge") +
    scale_x_continuous(limits = c(0, 50)) +
    labs(x = "Percentage", y = "Comorbidity") +
    theme_minimal() +
    theme(legend.position = "right", 
         text = element_text(size = 20)) +
    facet_wrap(~`Any SUD or drug use`, nrow = 1, 
               strip.position = "top", 
              labeller = label_both)

p.comorbidities 

ggsave("results/plots/plot_comorbidities.pdf", width = 10, height = 10, dpi = 300)
df.comorbidity.condensed <- df.comorbidity.plot %>% 
    filter(Present == "Yes") %>%
    filter(Comorbidity %in% 
           c("Alcohol abuse", "Chronic pulmonary disease", "Congestive heart failure", "Depression",
             "Diabetes with chronic complications", "Diabetes without chronic complications", 
             "HIV and AIDS (Acquired immune deficiency syndrome)", "Liver disease", 
             "Chronic kidney disease", "Metastatic cancer", "Solid tumor without metastasis",
             "Obesity", "Psychoses", "Valvular disease", "Any SUD or drug use", "Long acting glycopeptide (yes/no)"
            )) %>%
    mutate(Comorbidity = case_match(
                Comorbidity,
                "HIV and AIDS (Acquired immune deficiency syndrome)" ~ "HIV/AIDS",
                "Diabetes with chronic complications" ~ "Diabetes w/ complications", 
                "Diabetes without chronic complications" ~ "Diabetes w/o complications",
                .default = Comorbidity
    ))

# Create a data frame with the p-values for each comparison.
p_values.pwud <- df.comorbidity.condensed %>%
  filter(`Any SUD or drug use` == "Yes") %>%
  group_by(Comorbidity) %>%
  summarize(p_value = wilcox.test(percentage, alternative = "two.sided")$p.value)


alpha = 1
brewerpal = "Paired"

p.comorbidities.condensed.pwud <- ggplot(
        df.comorbidity.condensed %>% filter(`Any SUD or drug use` == "Yes"),
        aes(x = percentage, y = fct_rev(Comorbidity), fill = `Long acting glycopeptide (yes/no)`)) +
    geom_col(position = "dodge", alpha = alpha) +
    labs(x = "Percentage", y = "") +
    scale_fill_brewer(palette = brewerpal) +
    theme_minimal() +
    theme(legend.position = "right", 
         text = element_text(size = 20, face = "bold")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_reverse(limits = c(40, 0)) +
    scale_y_discrete(labels = NULL) +
    theme(
        panel.grid.major = element_line(color = "gray", linetype = "solid"),
        panel.grid.minor = element_blank()  # If you don't want minor gridlines
  ) 

#p.comorbidities.condensed.pwud

p.comorbidities.condensed.non.pwud <- ggplot(
        df.comorbidity.condensed %>% filter(`Any SUD or drug use` == "No"),
        aes(x = percentage, y = fct_rev(Comorbidity), fill = `Long acting glycopeptide (yes/no)`)) +
    geom_col(position = "dodge", alpha = alpha) +
    scale_x_continuous(limits = c(0, 40)) +
    labs(x = "Percentage", y = "") +
    scale_fill_brewer(palette = brewerpal) +
    theme_minimal() +
    theme(legend.position = "right", 
         text = element_text(size = 20, face = "bold")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_y_discrete(labels = NULL) +
    theme(axis.text.y = element_text(hjust = 0.5)) +
    theme(
        panel.grid.major = element_line(color = "gray", linetype = "solid"),
        panel.grid.minor = element_blank()  # If you don't want minor gridlines
  )

#p.comorbidities.condensed.non.pwud

p.comorbidities.condensed.axis <- ggplot(
        df.comorbidity.condensed %>% filter(`Any SUD or drug use` == "Yes"),
        aes(x = percentage, y = fct_rev(Comorbidity), fill = `Long acting glycopeptide (yes/no)`)) +
    geom_col(position = "dodge", alpha = alpha) +
    scale_x_continuous(limits = c(0,1)) +
    labs(x = "", y = "") +
    scale_fill_brewer(palette = brewerpal) +
    theme_minimal() +
    theme(legend.position = "right", 
         text = element_text(size = 20, face = "bold")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(axis.text.y = element_text(hjust = 0.5)) 

p.comorbidities.condensed.axis <- get_y_axis(p.comorbidities.condensed.axis, position = c("left"))
#p.comorbidities.condensed.legend <- get_legend(p.comorbidities.condensed.axis)


ggarrange(p.comorbidities.condensed.pwud, 
          ggdraw() + 
              draw_grob(p.comorbidities.condensed.axis, scale = 1,   hjust = 0,  vjust = 0),
          p.comorbidities.condensed.non.pwud, 
          ncol = 3, 
          align = "h", 
          widths = 1,  heights = 1,  
          hjust = -0.5,  vjust = 1,
          common.legend = T,
          font.label = list(size = 20, color = "black", face = "bold", family = "Helvetica"),
          labels = c("PWUD", "", "non-PWUD"),
          legend = "top"
         )


ggsave("results/plots/plot_comorbidities_condensed.pdf", width = 10, height = 10, dpi = 300)
ggsave("results/plots/plot_comorbidities_condensed.png", width = 10, height = 10, dpi = 300)
#Substance use
df.substance.plot <- df.table.1 %>% 
    filter(`Any SUD or drug use` == "Yes") %>%
    select(
        `History of opioid use or disorder`,
        `History of cocaine use or disorder`,
        `History of methamphetamine use or disorder`,
        `Injection drug use`,
        `Long acting glycopeptide (yes/no)`) %>% 
    pivot_longer(
        cols = `History of opioid use or disorder`:`Injection drug use`,
        names_to = "Substance use",
        values_to = "value",
    ) %>%
    mutate(Present = if_else(value == 1, "Yes", "No"),
          value = 1) %>%
    group_by(`Long acting glycopeptide (yes/no)`, `Substance use`, Present) %>%
    summarise(value = sum(value)) %>%
    mutate(percentage = value / sum(value) * 100) %>%
    ungroup()
p.substance <- ggplot(df.substance.plot %>% filter(Present == "Yes"), 
                      aes(x = percentage, y = fct_rev(`Substance use`), fill = `Long acting glycopeptide (yes/no)`)) +
  geom_col(position = "dodge") +
  scale_x_continuous(limits = c(0, 75)) +
    labs(x = "Percentage", y = "Substance use") +
    theme_minimal() +
    theme(legend.position = "right",
            text = element_text(size = 14))

p.substance

ggsave("results/plots/plot_substances.pdf", width = 10, height = 10, dpi = 300)
df.table.2 <- df.central %>%
    select(
        `Long acting glycopeptide (yes/no)`,
        `Long acting glycopeptide doses` = glyco_dose,
        `Terminal antibiotics` = antibiotics,
        `Days, terminal antibiotics` = abx_days,
        `Days, total number of antibiotic days` = abx_days_total,
        `Days, prior to laLGP` = laLGP_prior,
        `Any SUD or drug use`,
        MOUD = moud,
        `Bacteremia, isolated` = isolated_bacteremia,
        Osteomyelitis = dx_Osteomyelitis, 
        `Septic arthritis` = `dx_Septic arthritis`,
        Endocarditis = dx_Endocarditis#,
        #Piomyositis = dx_Myositis, 
    )

tbl.2.overall.non.pwud <- df.table.2 %>% 
    filter(`Any SUD or drug use` == "No") %>%                
    select(
        `Long acting glycopeptide (yes/no)`,
        `Terminal antibiotics`,
        `Days, terminal antibiotics`,
        `Days, total number of antibiotic days`,
        `Days, prior to laLGP`,
        `Long acting glycopeptide doses`,
        #`Any SUD or drug use`,
        #MOUD
        ) %>%
    #mutate(`Long acting glycopeptide doses` = as.numeric(`Long acting glycopeptide doses`)) %>%
    tbl_summary(by = `Long acting glycopeptide (yes/no)`,
               sort = everything() ~ "frequency",
               #statistic = list(all_continuous() ~ "{mean} ({sd})"),
               type = list(`Long acting glycopeptide doses` = "continuous",
                          `Days, prior to laLGP` = "continuous")) %>%
    add_p(test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                     all_continuous() ~ "t.test")) %>%
    #add_p(test.args = all_tests("fisher.test") ~ list(workspace=2e9)) %>%
    modify_caption("Table 2. Antibiotic characteristics for non-PWUD") %>%
    bold_labels() %>% 
    add_overall()
                    
tbl.2.overall.non.pwud
#Stratified Non-pwud table 2
list.dx <- c("Bacteremia, isolated", "Osteomyelitis", "Septic arthritis", "Endocarditis")#, "Piomyositis")

tbl.2.non.pwud <- purrr::maplist.dx, ~ df.table.2 %>% 
    filter(`Any SUD or drug use` == "No") %>%
    filter(.data[[.x]] == 1) %>%                    
    select(
        `Long acting glycopeptide (yes/no)`,
        #`Long acting glycopeptide doses`,
        `Terminal antibiotics`,
        `Days, terminal antibiotics`,
        `Days, total number of antibiotic days`,
        `Days, prior to laLGP`,
        `Long acting glycopeptide doses`,
        #`Any SUD or drug use`,
        #MOUD
        ) %>%
    #mutate(`Long acting glycopeptide doses` = as.numeric(`Long acting glycopeptide doses`)) %>%
    tbl_summary(by = `Long acting glycopeptide (yes/no)`,
               sort = everything() ~ "frequency",
               #statistic = list(all_continuous() ~ "{mean} ({sd})"),
               type = list(`Long acting glycopeptide doses` = "continuous",
                          `Days, prior to laLGP` = "continuous")) %>%
    add_p(test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                     all_continuous() ~ "t.test")) %>%
    #add_p(test.args = all_tests("fisher.test") ~ list(workspace=2e9)) %>%
    modify_caption("Table 2. Antibiotic characteristics for non-PWUD") %>%
    bold_labels() %>% 
    add_overall()
                      )
#Merge tables
tbl.2.non.pwud.merged <- tbl_merge(
    tbls = list(tbl.2.non.pwud[[1]], tbl.2.non.pwud[[2]],
               tbl.2.non.pwud[[4]], tbl.2.non.pwud[[3]]),#,
               #tbl.2.non.pwud[[5]]),
    tab_spanner = c("**Bacteremia**", "**Osteomyelitis**", 
                    "**Septic arthritis**", "**Endocarditis**")) %>%
    modify_caption("Table 2. Antibiotic characteristics by receipt of long acting glycopeptide - non-PWUD")

tbl.2.non.pwud.merged
tbl.2.overall.pwud <- df.table.2 %>% 
    filter(`Any SUD or drug use` == "Yes") %>%
    select(
        `Long acting glycopeptide (yes/no)`,
        #`Long acting glycopeptide doses`,
        `Terminal antibiotics`,
        `Days, terminal antibiotics`,
        `Days, total number of antibiotic days`,
        `Days, prior to laLGP`,
        #`Any SUD or drug use`,
        MOUD) %>%
    #mutate(`Long acting glycopeptide doses` = as.numeric(`Long acting glycopeptide doses`)) %>%
    tbl_summary(by = `Long acting glycopeptide (yes/no)`,
               sort = everything() ~ "frequency",
               #statistic = list(all_continuous() ~ "{mean} ({sd})"),
               type = list(`Long acting glycopeptide doses` = "continuous",
                          `Days, prior to laLGP` = "continuous")) %>%
    add_p(test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                     all_continuous() ~ "t.test")) %>%
    #add_p(test.args = all_tests("fisher.test") ~ list(workspace=2e9)) %>%
    modify_caption("Table 2. Antibiotic characteristics for PWUD") %>%
    bold_labels() %>% 
    add_overall()
                  
tbl.2.overall.pwud
list.dx <- c("Bacteremia, isolated", "Osteomyelitis", "Septic arthritis", "Endocarditis")#, "Piomyositis")

tbl.2.pwud <- purrr::maplist.dx, ~ df.table.2 %>% 
    filter(`Any SUD or drug use` == "Yes") %>%
    filter(.data[[.x]] == 1) %>%
    select(
        `Long acting glycopeptide (yes/no)`,
        `Terminal antibiotics`,
        `Days, terminal antibiotics`,
        `Days, total number of antibiotic days`,
        `Days, prior to laLGP`,
        `Long acting glycopeptide doses`,
        #`Any SUD or drug use`,
        MOUD) %>%
    #mutate(`Long acting glycopeptide doses` = as.numeric(`Long acting glycopeptide doses`)) %>%
    tbl_summary(by = `Long acting glycopeptide (yes/no)`,
               sort = everything() ~ "frequency",
               #statistic = list(all_continuous() ~ "{mean} ({sd})"),
               type = list(`Long acting glycopeptide doses` = "continuous",
                          `Days, prior to laLGP` = "continuous")) %>%
    add_p(test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                     all_continuous() ~ "t.test")) %>%
    #add_p(test.args = all_tests("fisher.test") ~ list(workspace=2e9)) %>%
    modify_caption("Table 2. Antibiotic characteristics for non-PWUD") %>%
    bold_labels() %>% 
    add_overall()
                      )
#Merge tables
tbl.2.pwud.merged <- tbl_merge(
    tbls = list(tbl.2.pwud[[1]], tbl.2.pwud[[2]],
               tbl.2.pwud[[4]], tbl.2.pwud[[3]]),
               #tbl.2.pwud[[5]]),
    tab_spanner = c("**Bacteremia**", "**Osteomyelitis**", 
                    "**Septic arthritis**", "**Endocarditis**") 
                    #**Piomyositis**")
  ) %>%
    modify_caption("Table 2. Antibiotic characteristics by receipt of long acting glycopeptide - PWUD")

tbl.2.pwud.merged
#Save table
tbl.2.pwud.merged %>% 
    as_kable() %>%
    kable_styling("striped") %>%
    save_kable(paste0("results/table_2_pwud_", Sys.Date(), ".html"))

#Save table
tbl.2.non.pwud.merged %>% 
    as_kable() %>%
    kable_styling("striped") %>%
    save_kable(paste0("results/table_2_non_pwud_", Sys.Date(), ".html"))
df.outcomes <- df.central %>%
    select(
        personid,
        `Long acting glycopeptide (yes/no)`,
        `Any SUD or drug use`,
        `Follow-up time, days` = censure_time,
        `Length of stay, index admission` = los_index,  # Add the missing backtick here
        Discharge = discharge_disposition,
        #`Readmission, 30 days` = readmissions_30d, 
        `Readmission, 90 days` = readmissions_90d,         
        #`Readmission, 365 days` = readmissions_365d, 
        #`Number of readmission, 365 days` = num_readmissions_365d, 
        #`ED visit, 30 days` = edvisit_30d, 
        `ED visit, 90 days` = edvisit_90d,         
        #`ED visit, 365 days` = edvisit_365d, 
        #`Number of ED visits, 365 days` = num_edvisits_365d, 
        #`Reinfection, 30 days` = reinfection_30d,
        #`Reinfection, 90 days` = reinfection_90d,
        #`Reinfection, 365 days` = reinfection_365d,
        #`Hospice, 30 days` = hospice_30d,
        #`Hospice, 90 days` = hospice_90d,
        #`Hospice, 365 days` = hospice_365d,
        #`Death, 30 days` = death_30d,
        #`Death, 90 days` = death_90d,   
        #`Death, 365 days` = death_365d,
        #`Death or hospice, 30 days` = death_or_hospice_30d,
        `Death or hospice, 90 days` = death_or_hospice_90d,   
        #`Death or hospice, 365 days` = death_or_hospice_365d,
        `Composite: Readmission, ED visit, or death/hospice, 90 days` = composite
    ) %>%
    mutate(
        retention_90_days = if_else(
            `Follow-up time, days` >= 90 | 
            `Readmission, 90 days` == 1 | 
            `ED visit, 90 days` == 1 | 
            `Death or hospice, 90 days` == 1, 
            1, 0
        )
    ) %>%
    mutate(
        loss_to_follow_up_90_days = if_else(
            `Follow-up time, days` < 90 & 
            `Readmission, 90 days` == 0 & 
            `ED visit, 90 days` == 0 & 
            `Death or hospice, 90 days` == 0, 
            1,  # Mark as lost to follow-up
            0   # Not lost to follow-up
        )
    )

#Outcomes table for PWUD
tbl.outcomes.pwud <- df.outcomes %>%
    filter(`Any SUD or drug use` == "Yes") %>% 
    select(-personid, -`Any SUD or drug use`) %>%
    tbl_summary(by = `Long acting glycopeptide (yes/no)`,
                sort = everything() ~ "frequency",
               statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
    add_p() %>%
    modify_caption("Table outcomes by glycopeptides use / pwud") %>%
    bold_labels() %>% 
    add_overall()

tbl.outcomes.pwud
#Outcomes table for non-PWUD
tbl.outcomes.non.pwud <- df.outcomes %>%
    filter(`Any SUD or drug use` == "No") %>% 
    select(-personid, -`Any SUD or drug use`) %>%
    tbl_summary(by = `Long acting glycopeptide (yes/no)`,
                sort = everything() ~ "frequency",
               statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
    add_p(test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                     all_continuous() ~ "t.test")) %>%
    modify_caption("Table outcomes by glycopeptides use / non-pwud") %>%
    bold_labels() %>% 
    add_overall()

tbl.outcomes.non.pwud
#Merge tables
tbl.outcomes.merged <-
  tbl_merge(
    tbls = list(tbl.outcomes.pwud , tbl.outcomes.non.pwud ),
    tab_spanner = c("**PWUD**", "**non-PWUD**")
  )

tbl.outcomes.merged
#Save table
tbl.outcomes.merged %>% 
    as_kable() %>%
    kable_styling("striped") %>%
    save_kable(paste0("results/table_outcomes_", Sys.Date(), ".html"))
df.outcomes.plot <- df.outcomes %>%
    select(
        `Long acting glycopeptide (yes/no)`,
        `Any SUD or drug use`,
        `Readmission, 30 days`,
        `Readmission, 90 days`,
        `Readmission, 365 days`,
        `ED visit, 30 days`,
        `ED visit, 90 days`,
        `ED visit, 365 days`,
        `Reinfection, 30 days`,
        `Reinfection, 90 days`,
        `Reinfection, 365 days`,
        `Death or hospice, 30 days`,
        `Death or hospice, 90 days`,
        `Death or hospice, 365 days`,
        `Composite: Readmission, ED visit, or death/hospice, 90 days`
    ) %>%
    pivot_longer(
        cols = `Readmission, 30 days`:`Composite: Readmission, ED visit, or death/hospice, 90 days`,
        names_to = "Outcome",
        values_to = "value",
    ) %>%
    mutate(Present = if_else(value == 1, "Yes", "No"),
          value = 1) %>%
    group_by(`Any SUD or drug use`, `Long acting glycopeptide (yes/no)`, `Outcome`, Present) %>%
    summarise(value = sum(value)) %>%
    mutate(percentage = value / sum(value) * 100) %>%
    ungroup()

#Set up factors for plotting
df.outcomes.plot <- df.outcomes.plot %>%
    mutate(Outcome = 
              factor(Outcome, levels =
                       c('Readmission, 30 days', 'Readmission, 90 days', 'Readmission, 365 days',
                         'ED visit, 30 days', 'ED visit, 90 days', 'ED visit, 365 days',
                         'Reinfection, 30 days', 'Reinfection, 90 days', 'Reinfection, 365 days', 
                         'Death or hospice, 30 days', 'Death or hospice, 90 days', 'Death or hospice, 365 days',
                        'Composite: Readmission, ED visit, or death/hospice, 90 days'))
          )

options(repr.plot.width=20, repr.plot.height=10)
p.outcomes <- ggplot(
    df.outcomes.plot %>% filter(Present == "Yes"), 
    aes(x = percentage, y = fct_rev(`Outcome`), fill = `Long acting glycopeptide (yes/no)`)) +
    geom_col(position = "dodge") +
    scale_fill_npg() +
    scale_x_continuous(limits = c(0, 100)) +
    labs(x = "Percentage", y = "Outcome", fill = "Long acting\nglycopeptide") +
    theme_minimal() +
    theme(legend.position = "right",
          text = element_text(size = 14)) +
    guides(fill = guide_legend(reverse = TRUE)) +
    facet_wrap(~`Any SUD or drug use`, nrow = 1, 
               strip.position = "top", 
              labeller = label_both)

p.outcomes

#ggsave("results/plots/plot_outcomes.pdf", width = 10, height = 10, dpi = 300)
#Outcomes (condensed)
options(repr.plot.width=20, repr.plot.height=10)

df.outcomes.condensed <- df.outcomes.plot %>% 
    filter(Present == "Yes") %>%
    filter(Outcome %in% 
           c('Readmission, 90 days', 
             'ED visit, 90 days',
             'Reinfection, 90 days',
             'Death or hospice, 90 days', 
             'Composite: Readmission, ED visit, or death/hospice, 90 days'
            )) %>%
    mutate(Outcome = case_match(
                Outcome,
                "Composite: Readmission, ED visit, or death/hospice, 90 days" ~ "Composite",
                .default = Outcome
    )) %>%
    mutate(Outcome = factor(Outcome, levels = c(
    'Composite', 'Death or hospice, 90 days', 'Reinfection, 90 days', 'ED visit, 90 days', 'Readmission, 90 days'
    )))

alpha = 1
brewerpal = "Paired"
manualpal = c('#FB9A99', '#E31A1C')

#Create plots
p.outcomes.condensed.pwud <- ggplot(
       df.outcomes.condensed  %>% filter(`Any SUD or drug use` == "Yes"),
        aes(x = percentage, y = fct_rev(Outcome), fill = `Long acting glycopeptide (yes/no)`)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = manualpal) + 
    labs(x = "Percentage", y = "") +
    theme_minimal() +
    theme(legend.position = "right", 
         text = element_text(size = 20, face = "bold")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_reverse(limits = c(60, 0)) +
    scale_y_discrete(labels = NULL) +
    theme(
        panel.grid.major = element_line(color = "gray", linetype = "solid"),
        panel.grid.minor = element_blank()  # If you don't want minor gridlines
  )

#p.outcomes.condensed.pwud

p.outcomes.condensed.non.pwud <- ggplot(
       df.outcomes.condensed  %>% filter(`Any SUD or drug use` == "No"),
        aes(x = percentage, y = fct_rev(Outcome), fill = `Long acting glycopeptide (yes/no)`)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = manualpal) + 
    scale_x_continuous(limits = c(0, 60)) +
    labs(x = "Percentage", y = "") +
    theme_minimal() +
    theme(legend.position = "right", 
         text = element_text(size = 20, face = "bold")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_y_discrete(labels = NULL) +
    theme(axis.text.y = element_text(hjust = 0.5)) +
    theme(
        panel.grid.major = element_line(color = "gray", linetype = "solid"),
        panel.grid.minor = element_blank()  # If you don't want minor gridlines
  )

#p.outcomes.condensed.non.pwud

p.outcomes.condensed.axis <- ggplot(
       df.outcomes.condensed  %>% filter(`Any SUD or drug use` == "Yes"),
        aes(x = percentage, y = fct_rev(Outcome), fill = `Long acting glycopeptide (yes/no)`)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = manualpal) + 
    scale_x_continuous(limits = c(0,1)) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "right", 
         text = element_text(size = 20, face = "bold")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(axis.text.y = element_text(hjust = 0.5)) +
    theme(
        panel.grid.major = element_line(color = "gray", linetype = "solid"),
        panel.grid.minor = element_blank()  # If you don't want minor gridlines
  )

p.outcomes.condensed.axis <- get_y_axis(p.outcomes.condensed.axis, position = c("left"))
#p.outcomes.condensed.legend <- get_legend(p.outcomes.condensed.axis)


ggarrange(p.outcomes.condensed.pwud, 
          ggdraw() + 
              draw_grob(p.outcomes.condensed.axis, scale = 1,   hjust = 0,  vjust = 0),
          p.outcomes.condensed.non.pwud, 
          ncol = 3, 
          align = "h", 
          widths = 1,  heights = 1,  
          hjust = -0.5,  vjust = 1,
          common.legend = T,
          font.label = list(size = 20, color = "black", face = "bold", family = "Helvetica"),
          labels = c("PWUD", "", "non-PWUD"),
          legend = "top")


ggsave("results/plots/plot_outcomes_condensed.pdf", width = 10, height = 10, dpi = 300)
ggsave("results/plots/plot_outcomes_condensed.png", width = 10, height = 10, dpi = 300)


p_load(maps, mapdata, wesanderson, RColorBrewer, ggpubr)
#Create 1-zip code df
df.states <- map_data("state")


df.zip <- data.frame(
  state = c(
    "Connecticut", "Massachusetts", "Maine", "New Hampshire", "New Jersey", "Puerto Rico", "Rhode Island", "Vermont",
    "Delaware", "New York", "Pennsylvania",
    "District of Columbia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia",
    "Alabama", "Florida", "Georgia", "Mississippi", "Tennessee",
    "Indiana", "Kentucky", "Michigan", "Ohio",
    "Iowa", "Minnesota", "Montana", "North Dakota", "South Dakota", "Wisconsin",
    "Illinois", "Kansas", "Missouri", "Nebraska",
    "Arkansas", "Louisiana", "Oklahoma", "Texas",
    "Arizona", "Colorado", "Idaho", "New Mexico", "Nevada", "Utah", "Wyoming",
    "Alaska", "California", "Hawaii", "Oregon", "Washington"
  ),
  zip_number = c(
    0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1,
    2, 2, 2, 2, 2, 2,
    3, 3, 3, 3, 3,
    4, 4, 4, 4,
    5, 5, 5, 5, 5, 5,
    6, 6, 6, 6,
    7, 7, 7, 7,
    8, 8, 8, 8, 8, 8, 8,
    9, 9, 9, 9, 9
  )
) %>%
    mutate(zip_number = as.factor(zip_number)) %>%
    mutate(region = str_to_lower(state))

df.zip <- df.states %>% left_join(df.zip, by = c("region" = "region"))

df.map.lalgp <- df.central %>%
  select(`Any SUD or drug use`, `Long acting glycopeptide (yes/no)`, 
      `One digit zip code` = zip_code) %>%
  group_by(`Any SUD or drug use`, `One digit zip code`, `Long acting glycopeptide (yes/no)`) %>%
  mutate(value = 1) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(`Any SUD or drug use`, `One digit zip code`) %>%
  mutate(row_total = sum(value)) %>%
  mutate(percentage = (value / row_total) * 100) %>%
  ungroup() %>%
    filter(`Long acting glycopeptide (yes/no)` == "Yes") %>%
  mutate(zip_number = as.character(str_extract(`One digit zip code`, "\\d+"))) %>%
    left_join(df.zip, by = c("zip_number" = "zip_number")) %>%
    mutate(percentage_groups = case_when(
            percentage <5 ~ "Less than 5%",
            percentage >= 5 & percentage <10 ~ "5 - 9.9%",
            percentage >= 10 & percentage <15 ~ "10 - 14.9%",
            percentage >= 15 & percentage <20 ~ "15 - 19.9%",
            percentage >= 20 ~ "20% or more",
    ), 
           percentage_groups = 
               factor(percentage_groups, 
                          levels = c(
                              "Less than 5%", "5 - 9.9%", "10 - 14.9%", "15 - 19.9%", "20% or more"
                          )
                          )
           )
            
df.map.lalgp

#In groups

pal <- wes_palette("Zissou1", 5, type = "discrete")


p.map.pwud <- ggplot(data = df.map.lalgp %>% 
           filter(`Any SUD or drug use` == "Yes"), aes(x = long, y = lat, fill = percentage_groups, group = group)) + 
  geom_polygon(color = "white", alpha = 0.8) + 
  scale_fill_manual(values = pal) +  # Use scale_fill_manual with your color palette
  #  scale_fill_brewer(palette = "YlOrBr") +
  theme_void(base_size = 20) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  labs(title = 'PWUD', 
      fill = "Percentage\nlaLGP") + 
  coord_fixed(1.3)

p.map.non.pwud <- ggplot(data=df.map.lalgp %>% 
           filter(`Any SUD or drug use` == "No"), aes(x = long, y = lat, fill = percentage_groups, group = group)) + 
  geom_polygon(color = "white", alpha = 0.8) + 
  scale_fill_manual(values = pal) +
  theme_void(base_size = 20) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  labs(title = 'non-PWUD', 
      fill = "Percentage\nlaLGP") + 
  coord_fixed(1.3)

ggarrange(p.map.pwud, p.map.non.pwud, 
          ncol = 1,  
          common.legend = T,
          #font.label = list(size = 20, color = "black", face = "bold", family = "Helvetica"),
          #labels = c("PWUD", "", "non-PWUD"),
          legend = "top")

ggsave("results/plots/plot_maps.pdf", width = 10, height = 10, dpi = 300)
ggsave("results/plots/plot_maps.png", width = 10, height = 10, dpi = 300)


#In groups

pal <- wes_palette("Zissou1", type = "continuous")


p.map.pwud <- ggplot(data = df.map.lalgp %>% 
           filter(`Any SUD or drug use` == "Yes"), aes(x = long, y = lat, fill = percentage, group = group)) + 
  geom_polygon(color = "white", alpha = 0.8) + 
  scale_fill_gradient(low = pal[1], high = pal[length(pal)]) + 
  theme_void(base_size = 14) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  labs(title = 'PWUD', 
      fill = "Percentage\nlaLGP") + 
  coord_fixed(1.3)

p.map.non.pwud <- ggplot(data=df.map.lalgp %>% 
           filter(`Any SUD or drug use` == "No"), aes(x = long, y = lat, fill = percentage, group = group)) + 
  geom_polygon(color = "white", alpha = 0.8) + 
  scale_fill_gradient(low = pal[1], high = pal[length(pal)]) + 
  theme_void(base_size = 14) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  labs(title = 'non-PWUD', 
      fill = "Percentage\nlaLGP") + 
  coord_fixed(1.3)

p.map.both <- ggarrange(p.map.pwud, p.map.non.pwud, 
          ncol = 1,  
          common.legend = T,
          #font.label = list(size = 20, color = "black", face = "bold", family = "Helvetica"),
          #labels = c("PWUD", "", "non-PWUD"),
          legend = "top")

p.map.both

ggsave("results/plots/plot_maps_continous.pdf", width = 10, height = 10, dpi = 300)
ggsave("results/plots/plot_maps_continous.png", width = 10, height = 10, dpi = 300)

p_load(ggstream, ggtext, ggsci, ggnewscale, cowplot, scales)  
options(repr.plot.width=20, repr.plot.height=10)

#Stacking order
order <- rev(c("Endocarditis", "Septic arthritis", "Bacteremia", "Osteomyelitis"))


df.diagnosis.trend <- df.central %>%
  filter(`Long acting glycopeptide (yes/no)` == "Yes") %>%
  select(personid, glyco_start_date, "dx_Osteomyelitis", "dx_Bacteremia", 
         "dx_Septic arthritis", "dx_Endocarditis") %>%
  pivot_longer(
    cols = c("dx_Osteomyelitis", "dx_Bacteremia", 
             "dx_Septic arthritis", "dx_Endocarditis"),
    names_to = "Diagnosis",
    values_to = "Value"
  ) %>%
    filter(Value == 1) %>%
  mutate(Diagnosis = str_remove(Diagnosis, "dx_")) %>%
  mutate(glyco_start_date = mdy(glyco_start_date)) %>%
  arrange(glyco_start_date) %>%
  group_by(Diagnosis) %>%
  mutate(month = format(glyco_start_date, "%Y-%m")) %>%
    mutate(month = ym(month)) %>%
  mutate(count = row_number()) %>%
  group_by(Diagnosis, month) %>%
  summarize(cumulative_count = max(count)) %>%
  ungroup() %>%
        mutate(Diagnosis = factor(Diagnosis, levels = order)) %>%
    group_by(Diagnosis) %>%
  complete(month = seq(min(month), ymd("2024-01-01"), by = "months")) %>%
  fill(cumulative_count, .direction = "up") %>%
  fill(cumulative_count, .direction = "down") %>%
    ungroup()

df.diagnosis.trend





p.diagnosis.trend <- ggplot(df.diagnosis.trend , 
           aes(x = month,
               y = cumulative_count, 
               color = Diagnosis, 
               label = Diagnosis,
               fill = Diagnosis)) +
    geom_area(alpha = 0.8) +
    scale_y_continuous(limits = c(0,1200)) +
    scale_color_jco() +
    scale_fill_jco() +
    labs(x = "Date", y = "Number of cases") +
    theme_minimal() + 
    theme(text = element_text(size = 14, face = "bold")) +
    theme(legend.position = c(.35, .9)) +
    scale_x_date(limits = c(as.Date("2015-10-01"), as.Date("2022-10-01")))

p.diagnosis.trend
p.diagnosis.trend.line <- ggplot(df.diagnosis.trend , 
           aes(x = month,
               y = cumulative_count, 
               color = Diagnosis, 
               label = Diagnosis,
               fill = Diagnosis)) +
    geom_line(alpha = 0.8) +
    scale_y_continuous(limits = c(0,1200)) +
    scale_color_jco() +
    scale_fill_jco() +
    labs(x = "Date", y = "Number of cases") +
    theme_minimal() + 
    theme(text = element_text(size = 14, face = "bold")) +
    theme(legend.position = c(.35, .9)) +
    scale_x_date(limits = c(as.Date("2015-10-01"), as.Date("2022-10-01")))

p.diagnosis.trend.line
ggplot(df.diagnosis.trend, 
       aes(x = month,
           y = cumulative_count, 
           group = Diagnosis,
           color = Diagnosis, 
           label = Diagnosis,
           fill = Diagnosis)) +
  geom_line(size = 1) +  # Use line plot for trends
  geom_point(size = 2) +  # Optional: Add points to make it clearer
  labs(title = "Cumulative Count of Diagnoses Over Time",
       x = "Month",
       y = "Cumulative Count") +
  theme_minimal() +
  theme(legend.position = "top")
options(repr.plot.width=20, repr.plot.height=10)

# Create a data frame with all months
all_months <- expand.grid(
  month = unique(format(dmy(df.central$glyco_start_date), "%Y-%m")),
  `Long acting glycopeptide` = unique(df.central$glyco_name_short)
)


#Drug plot over time
df.glyco.trend <- df.central %>%
  filter(!is.na(glyco_name_short)) %>%
  mutate(glyco_start_date = mdy(glyco_start_date)) %>%
  arrange(glyco_start_date) %>%
  group_by(glyco_name_short) %>%
  mutate(count = row_number()) %>%
  rename(`Long acting glycopeptide` = glyco_name_short) %>%
  mutate(month = format(glyco_start_date, "%Y-%m")) %>%
  group_by(`Long acting glycopeptide`, month) %>%
  summarize(cumulative_count = max(count)) %>%
  ungroup() %>%
  right_join(all_months, by = c("month", "Long acting glycopeptide")) %>%
  arrange(`Long acting glycopeptide`, month) %>%
  fill(cumulative_count, .direction = "up") %>%
    mutate(month = ym(month)) %>%
    filter(!`Long acting glycopeptide` == "No glycopeptide") %>%
filter(!is.na(month)) %>%
mutate(cumulative_count = if_else(is.na(cumulative_count), 101, cumulative_count))

df.glyco.trend
p.glyco.trend <- ggplot(df.glyco.trend, aes(x = month , 
               y = cumulative_count, 
               #color = `Long acting glycopeptide`, 
               #label = `Long acting glycopeptide`,
               fill = `Long acting glycopeptide`)) +
    geom_area(alpha = 0.8, position = "stack") +
    scale_y_continuous(limits = c(0,1000)) +
    scale_color_startrek() +
    scale_fill_startrek() +
    labs(x = "Date", y = "Number of cases", fill = "Lipoglycopeptide") +
    theme_minimal() + 
    theme(text = element_text(size = 14, face = "bold")) +
    theme(legend.position = c(.35, .93)) +
  scale_x_date(limits = c(as.Date("2015-10-01"), as.Date("2022-10-01")))

p.glyco.trend
#Drug plot over time
df.glyco.trend.density <- df.central %>%
  filter(!is.na(glyco_name_short)) %>%
  arrange(glyco_start_date) %>%
  group_by(glyco_name_short) %>%
  mutate(count = row_number()) %>%
    mutate(value = 1) %>%
  rename(`Long acting glycopeptide` = glyco_name_short) %>%
 filter(!`Long acting glycopeptide` == "No glycopeptide") %>%
  mutate(glyco_start_date = as.Date(glyco_start_date)) %>%
  mutate(month = format(glyco_start_date, "%Y-%m")) 


df.glyco.trend.density 

plot_grid(p.glyco.trend, p.diagnosis.trend, p.map.both, 
          labels = c("A", "B", "C"),
         nrow = 1)


ggsave("results/plots/plot_trends.pdf", width = 10, height = 8, dpi = 300)
ggsave("results/plots/plot_trends.png", width = 10, height = 8, dpi = 300)

#Subset data
df.iptw <- df.central %>%
    select(
        age,
        gender,
        race = race2,
        ethnicity,
        insurance,
        alcohol = `Alcohol abuse`,
        bloss = `Blood loss anemia`,
        pud = `Chronic peptic ulcer disease`,
        copd = `Chronic pulmonary disease`,
        coag = `Coagulation deficiency`,
        chf = `Congestive heart failure`,
        anem = `Deficiency anemias`,
        dep = `Depression`,
        diabeteswcomp = `Diabetes with chronic complications`,
        diabeteswocopm = `Diabetes without chronic complications`,
        #`Drug abuse`,
        fluid = `Fluid and electrolyte disorders`,
        hiv = `HIV and AIDS (Acquired immune deficiency syndrome)`,
        hypert = `Hypertension, complicated`,
        hypertun = `Hypertension, uncomplicated`,
        hypothy = `Hypothyroidism`,
        liver = `Liver disease`,
        lymph = `Lymphoma`,
        cancmeta = `Metastatic cancer`,
        obe = `Obesity`,
        neuro = `Other neurological disorders`,
        para = `Paralysis`,
        pvd = `Peripheral vascular disease`,
        psycho = `Psychoses`,
        pcd = `Pulmonary circulation disorders`,
        rf = `Renal failure`,
        collavd =  `Rheumatoid arthritiscollagen vascular diseases`,
        cancwometa = `Solid tumor without metastasis`,
        valve = `Valvular disease`,
        weight = `Weight loss`,
        #`Elixhauser (AHRQ), index` = ElixhauserAHRQ,
        #`Elixhauser, number of conditions` = Elixhauser_conditions_num,
        #`Elixhauser, number of conditions (grouped)` = Elixhauser_conditions_groups,
        oud = `History of opioid use or disorder`,
        cud = `History of cocaine use or disorder`,
        mud = `History of methamphetamine use or disorder`,
        idu,
        `Any SUD or drug use`,
        MOUD = moud,
        Bacteremia = isolated_bacteremia,
        Osteomyelitis = dx_Osteomyelitis, 
        separthritis = `dx_Septic arthritis`,
        Endocarditis = dx_Endocarditis,
        #Piomyositis = dx_Myositis, 
        #los_index,  # Add the missing backtick here
        #discharge_disposition,
        `Long acting glycopeptide (yes/no)`,
        zip_code,
        bed_size,
        #`Hospital type` = speciality, 
        soc_antibiotics = antibiotics,
        censure_time,
        #`Readmission, 30 days` = readmissions_30d, 
        readmissions_90d,         
        #`Readmission, 365 days` = readmissions_365d, 
        #`Number of readmission, 365 days` = num_readmissions_365d, 
        #`ED visit, 30 days` = edvisit_30d, 
        edvisit_90d,         
        #`ED visit, 365 days` = edvisit_365d, 
        #`Number of ED visits, 365 days` = num_edvisits_365d, 
        #`Hospice, 30 days` = hospice_30d,
        #`Hospice, 90 days` = hospice_90d,
        #`Hospice, 365 days` = hospice_365d,
        #`Death, 30 days` = death_30d,
        #`Death, 90 days` = death_90d,   
        #`Death, 365 days` = death_365d,
        #`Death or hospice, 30 days` = death_or_hospice_30d,
        death_or_hospice_90d,   
        #`Death or hospice, 365 days` = death_or_hospice_365d,
        composite
        ) %>%
    mutate( lalgp = 
               if_else(`Long acting glycopeptide (yes/no)` == "No", 0, 1),
           Bacteremia = 
               if_else(is.na(Bacteremia), 0, Bacteremia)
           )

df.iptw.pwud <- df.iptw %>%
    filter(`Any SUD or drug use` == "Yes") %>%
    select(-c(`Any SUD or drug use`, `Long acting glycopeptide (yes/no)`)) %>%  
    mutate_all(~ if_else(. == "Unknown", NA, .)) %>%
  mutate_if(is.character, as.factor)

df.iptw.non.pwud <- df.iptw %>%
    filter(`Any SUD or drug use` == "No") %>%
    select(-c(MOUD, `Any SUD or drug use`, `Long acting glycopeptide (yes/no)`, oud, cud, mud,
        idu)) %>%  
    mutate_all(~ if_else(. == "Unknown", NA, .)) %>%
  mutate_if(is.character, as.factor)
#Check unknown
df.iptw.pwud %>% skim()
df.iptw.non.pwud %>% skim()

gg_miss_var(df.iptw.pwud, show_pct = TRUE)
gg_miss_var(df.iptw.non.pwud, show_pct = TRUE)
#Multiple imputation of missing values - takes very long
df.iptw.pwud.imputed <-  mice(
    df.iptw.pwud, m = 10, maxit = 10, seed = 20200516)

#Save multiple imputed datasets
df.iptw.pwud.imputed %>% saveRDS("data/pwud_mi.rds")
#Multiple imputation of missing values - takes very long
df.iptw.non.pwud.imputed <- mice(
    df.iptw.non.pwud, m = 10, maxit = 10, seed = 20200516)

#Save multiple imputed datasets
df.iptw.non.pwud.imputed %>% saveRDS("data/non_pwud_mi.rds")

#Read multiple imputed datasets
df.iptw.non.pwud.imputed <- readRDS("data/non_pwud_mi.rds")
df.iptw.pwud.imputed <- readRDS("data/pwud_mi.rds")
#Create formulas
pwud.formula <- paste("lalgp ~ ", 
                      paste(names(df.iptw.pwud %>% 
                                  select(-c(lalgp, composite, censure_time,
                                           death_or_hospice_90d, readmissions_90d, edvisit_90d, soc_antibiotics))), 
                            collapse = " + ")
                      )


non.pwud.formula <- paste("lalgp ~ ", 
                      paste(names(df.iptw.non.pwud %>% 
                                  select(-c(lalgp, composite, censure_time,
                                           death_or_hospice_90d, readmissions_90d, edvisit_90d, soc_antibiotics))), 
                            collapse = " + ")
                      )

as.formula(pwud.formula)
as.formula(non.pwud.formula)
df.iptw.pwud.matched <- matchthem(as.formula(pwud.formula),
                              datasets = df.iptw.pwud.imputed,
                              approach = 'within',
                              method = 'nearest',
                              caliper = 0.05,
                              ratio = 3)

df.iptw.non.pwud.matched <- matchthem(as.formula(non.pwud.formula),
                              datasets = df.iptw.non.pwud.imputed,
                              approach = 'within',
                              method = 'nearest',
                              caliper = 0.05,
                              ratio = 3)

#Descriptive for matched

#Complete the data over all imputations
df.iptw.pwud.matched.complete <- complete(df.iptw.pwud.matched , action = "long", all = FALSE)

#df.iptw.pwud.matched.complete

#Dummy code categorical variables to create means
list_categorical <- c("gender", "race", "ethnicity", "insurance", "MOUD",
                     #"discharge_disposition", 
                      "zip_code", "bed_size")

# Create binary dummy variables using pivot_wider for each variable
df_binary.pwud <- list_categorical %>% 
  purrr::map(~ df.iptw.pwud.matched.complete %>%
         mutate(Value = 1) %>%
         pivot_wider(
           names_from = .x,
           values_from = Value,
           values_fill = 0,
           names_prefix = paste0(.x, "_")
         )# %>%
         #rename_with(~paste0(.x, "_", .), everything())  # Rename all columns
  ) %>% 
  reduce(full_join)  

#Create list of all variables to summarize
list_continuous <- c("age", 
                     "gender_Female", "gender_Male",
                     "race_Black or African American", "race_Other race", "race_White or Caucasian",
                     "ethnicity_Not Hispanic or Latino",
                     "insurance_Commerical insurance", 
                     "insurance_Medicaid",
                     "insurance_Medicare", "insurance_Other",
                     "alcohol", "bloss", "pud", "copd", "coag", "chf", "anem", "dep",
                     "diabeteswcomp", "diabeteswocopm", "fluid", "hiv", "hypert",
                     "hypertun", "hypothy", "liver", "lymph", "cancmeta", "obe", "neuro",
                     "para", "pvd", "psycho", "pcd", "rf", "collavd", "cancwometa",
                     "valve", "weight", "oud", "cud", "mud", "idu", "Bacteremia",
                     "Osteomyelitis", "separthritis", "Endocarditis", #"Piomyositis", "los_index",
                     "MOUD_No MOUD",  "MOUD_Buprenorphine", "MOUD_Methadone", 
                    "bed_size_>=1000", "bed_size_500-999", "bed_size_<500",
                     #"discharge_disposition_Home",
                    #"discharge_disposition_Patient directed discharge", "discharge_disposition_Other", 
                    #"discharge_disposition_Nursing facility or rehabilitation", 
                     "zip_code_3 - Alabama, Florida, Georgia, Mississippi, Tennessee", 
                    "zip_code_2 - District of Columbia, Maryland, North Carolina, South Carolina, Virginia, West Virginia", 
                     "zip_code_0 - Connecticut, Massachusetts, Maine, New Hampshire, New Jersey, Puerto Rico, Rhode Island", 
                     "zip_code_5 - Iowa, Minnesota, Montana, North Dakota, South Dakota, Wisconsin", 
                     "zip_code_9 - Alaska, California, Hawaii, Oregon, Washington", 
                     "zip_code_1 - Delaware, New York, Pennsylvania", "zip_code_8 - Arizona, Colorado, Idaho, New Mexico, Nevada, Utah, Wyoming", 
                     "zip_code_6 - Illinois, Kansas, Missouri, Nebraska", "zip_code_7 - Arkansas, Louisiana, Oklahoma, Texas", 
                     "zip_code_4 - Indiana, Kentucky, Michigan, Ohio")


#Calculate means
matched.descriptive.result.pwud <- map_dfr(list_continuous, ~ {
  variable <- .x
  df_filtered_1 <- df_binary.pwud[df_binary.pwud$lalgp == 1, c("lalgp", variable, ".imp")]
  df_filtered_0 <- df_binary.pwud[df_binary.pwud$lalgp == 0, c("lalgp", variable, ".imp")]

  means_1 <- tapply(df_filtered_1[[variable]], df_filtered_1$.imp, mean)
  variances_1 <- tapply(df_filtered_1[[variable]], df_filtered_1$.imp, sd)
  means_0 <- tapply(df_filtered_0[[variable]], df_filtered_0$.imp, mean)
  variances_0 <- tapply(df_filtered_0[[variable]], df_filtered_0$.imp, sd)

  data.frame(variable = variable, mean_lalgp_1 = mean(means_1), mean_lalgp_0 = mean(means_0))
})

matched.descriptive.result.pwud <- matched.descriptive.result.pwud %>%
  mutate(across(starts_with("mean"), ~if_else(!variable %in% c("age", "los_index"), . * 100, .))) %>%
  mutate(across(c(-variable), ~round(., 2)))

#matched.descriptive.result.pwud

options(repr.plot.width=20, repr.plot.height=10)
#Assess balance - pwud
pwud.bal.tab.matched <- bal.tab(
    df.iptw.pwud.matched, 
    un = TRUE,
    stats = c('m', 'ks'),
    imp.fun = 'mean')

#Create table
table.matched.descriptives.pwud <- pwud.bal.tab.matched$Balance.Across.Imputations %>% 
    rownames_to_column(var = "variable") %>%
    select(-Type) %>%
    left_join(matched.descriptive.result.pwud) %>%
    relocate(mean_lalgp_0, mean_lalgp_1, .after = "variable") %>%
    mutate(across(starts_with("Mean."), ~round(., 3))) %>%
    mutate(variable = c(
  "Distance",
  "Age",
  "Male",
  "Female",
  "Black or African American",
  "Other race",
  "White or Caucasian",
  "Not Hispanic or Latino",
  "Commerical insurance",
  "Medicaid",
  "Medicare",
  "Other insurance",
  "Alcohol abuse",
  "Blood loss anemia",
  "Chronic peptic ulcer disease",
  "Chronic pulmonary disease",
  "Coagulation deficiency",
  "Congestive heart failure",
  "Deficiency anemias",
  "Depression",
  "Diabetes with chronic complications",
  "Diabetes without chronic complications",
  "Fluid and electrolyte disorders",
  "HIV and AIDS",
  "Hypertension, complicated",
  "Hypertension, uncomplicated",
  "Hypothyroidism",
  "Liver disease",
  "Lymphoma",
  "Metastatic cancer",
  "Obesity",
  "Other neurological disorders",
  "Paralysis",
  "Peripheral vascular disease",
  "Psychoses",
  "Pulmonary circulation disorders",
  "Renal failure",
  "Rheumatoid arthritiscollagen vascular diseases",
  "Solid tumor without metastasis",
  "Valvular disease",
  "Weight loss",
  "History of opioid use or disorder",
  "History of cocaine use or disorder",
  "History of methamphetamine use or disorder",
  "Injection drug use",
        "Buprenorphine",
        "Methadone",
"No MOUD",
  "Bacteremia",
  "Osteomyelitis",
  "Septic arthritis",
  "Endocarditis",
  #"Piomyositis",
  #"Length of stay, index admission",
  #"Home",
  #"Nursing facility or rehabilitation",
  #"Other discharge disposition",
  #"Patient directed discharge",
  "0 - Connecticut, Massachusetts, Maine, New Hampshire, New Jersey, Puerto Rico, Rhode Island",
  "1 - Delaware, New York, Pennsylvania",
  "2 - District of Columbia, Maryland, North Carolina, South Carolina, Virginia, West Virginia",
  "3 - Alabama, Florida, Georgia, Mississippi, Tennessee",
  "4 - Indiana, Kentucky, Michigan, Ohio",
  "5 - Iowa, Minnesota, Montana, North Dakota, South Dakota, Wisconsin",
  "6 - Illinois, Kansas, Missouri, Nebraska",
  "7 - Arkansas, Louisiana, Oklahoma, Texas",
  "8 - Arizona, Colorado, Idaho, New Mexico, Nevada, Utah, Wyoming",
  "9 - Alaska, California, Hawaii, Oregon, Washington",
  "bed_size_<500",
  "bed_size_500-999"
  "bed_size_>=1000",
))


new_var_name0 <- paste0("SOC, n = ", pwud.bal.tab.matched$Observations[4, 1])
new_var_name1 <- paste0("laLGP, n = ", pwud.bal.tab.matched$Observations[4, 2])

table.matched.descriptives.pwud <- table.matched.descriptives.pwud %>%
  mutate(across(c(mean_lalgp_0, mean_lalgp_1), ~round(., 1))) %>%
  #mutate(across(starts_with("Mean."), ~.*100)) %>%
  rename_with(~new_var_name0, mean_lalgp_0) %>%
  rename_with(~new_var_name1, mean_lalgp_1) %>%
    rename(`SMD, unmatched` = Mean.Diff.Un,
           `KS, unmatched` = Mean.KS.Un,
           `SMD, matched` = Mean.Diff.Adj,
           `KS, matched` = Mean.KS.Adj) #%>%
  
#  kable() %>%
#  kable_styling("striped") %>%
#  save_kable(paste0("results/table_descriptives_matched_pwud_", Sys.Date(), ".html"))

tbl.1.pwud %>% as.tibble() %>% 
    mutate( `**Characteristic**` = 
           ifelse( `**Characteristic**` == "Other", "Other insurance",  `**Characteristic**`)) %>%
    mutate(
        `**Characteristic**` = str_remove_all(`**Characteristic**`, "__"),
        `**Characteristic**` = gsub("\\(SD\\)", "", `**Characteristic**`),
        `**Characteristic**` = str_remove_all(`**Characteristic**`, ', years, Mean'),
        `**Characteristic**` = str_remove_all(`**Characteristic**`, ', Mean'),
        `**Characteristic**` = trimws(str_remove_all(`**Characteristic**`, ", n"))) %>%
    mutate(
        `**Characteristic**` = gsub("\\(.*?%\\)", "", `**Characteristic**`),
        `**Characteristic**` = trimws(`**Characteristic**`)) %>%
    left_join(
        table.matched.descriptives.pwud, 
        by = c("**Characteristic**" = "variable")) %>%
    mutate_all(~ifelse(is.na(.), "", as.character(.))) %>%
    mutate_all(~ifelse(. == "Unknown", "Missing", .)) %>%
    #filter(!`**Characteristic**` %in% c("Female", "Hispanic or Latino", 
    #          "Blood loss anemia", "Chronic peptic ulcer disease", 
    #          "Coagulation deficiency", "Fluid and electrolyte disorders", 
    #          "Hypothyroidism", "Hypertension, complicated", "Hypertension, uncomplicated", 
    #          "Obesity", "Other neurological disorders", "Paralysis", 
    #          "Peripheral vascular disease", "Pulmonary circulation disorders", 
    #          "Rheumatoid arthritiscollagen vascular diseases", 
    #          "Weight loss")) %>%
    relocate(`SMD, unmatched`, .after = "**p-value**") %>%
    select(-c(`KS, matched`,  `KS, unmatched`  )) %>%
    kable() %>%
    kable_styling("striped") %>%
    save_kable(paste0("results/table_descriptives_matched_pwud_", Sys.Date(), ".html"))


tbl.1.pwud %>% as.tibble() %>% 
    mutate( `**Characteristic**` = 
           ifelse( `**Characteristic**` == "Other", "Other insurance",  `**Characteristic**`)) %>%
    mutate(
        `**Characteristic**` = str_remove_all(`**Characteristic**`, "__"),
        `**Characteristic**` = gsub("\\(SD\\)", "", `**Characteristic**`),
        `**Characteristic**` = str_remove_all(`**Characteristic**`, ', years, Mean'),
        `**Characteristic**` = str_remove_all(`**Characteristic**`, ', Mean'),
        `**Characteristic**` = trimws(str_remove_all(`**Characteristic**`, ", n"))) %>%
    mutate(
        `**Characteristic**` = gsub("\\(.*?%\\)", "", `**Characteristic**`),
        `**Characteristic**` = trimws(`**Characteristic**`)) %>%
    left_join(
        table.matched.descriptives.pwud %>% 
        mutate(variable = 
           if_else(`variable` == "Bacteremia", "Bacteremia, isolated", `variable`)), 
        by = c("**Characteristic**" = "variable")) %>%
    mutate_all(~ifelse(is.na(.), "", as.character(.))) %>%
    mutate_all(~ifelse(. == "Unknown", "Missing", .)) %>%
    #filter(!`**Characteristic**` %in% c("Female", "Hispanic or Latino", 
    #          "Blood loss anemia", "Chronic peptic ulcer disease", 
    #          "Coagulation deficiency", "Fluid and electrolyte disorders", 
    #          "Hypothyroidism", "Hypertension, complicated", "Hypertension, uncomplicated", 
    #          "Obesity", "Other neurological disorders", "Paralysis", 
    #          "Peripheral vascular disease", "Pulmonary circulation disorders", 
    #          "Rheumatoid arthritiscollagen vascular diseases", 
    #          "Weight loss")) %>%
    relocate(`SMD, unmatched`, .after = "**p-value**") %>%
    select(-c(`KS, matched`,  `KS, unmatched`  ))
#Descriptive for matched

#Complete the data over all imputations
df.iptw.non.pwud.matched.complete <- complete(df.iptw.non.pwud.matched , action = "long", all = FALSE)

#df.iptw.pwud.matched.complete

#Assess balance - non-pwud
#Dummy code categorical variables to create means
list_categorical <- c("gender", "race", "ethnicity", "insurance", #"MOUD",
                     #"discharge_disposition", 
                      "zip_code", "bed_size")

# Create binary dummy variables using pivot_wider for each variable
df_binary.non.pwud <- list_categorical %>% 
  purrr::map(~ df.iptw.non.pwud.matched.complete %>%
         mutate(Value = 1) %>%
         pivot_wider(
           names_from = .x,
           values_from = Value,
           values_fill = 0,
           names_prefix = paste0(.x, "_")
         )# %>%
         #rename_with(~paste0(.x, "_", .), everything())  # Rename all columns
  ) %>% 
  reduce(full_join)  

dput(names(df_binary.non.pwud))

#Create list of all variables to summarize
list_continuous <- c("age", 
                     "gender_Female", "gender_Male",
                     "race_Black or African American", "race_Other race", "race_White or Caucasian",
                     "ethnicity_Not Hispanic or Latino",
                     "insurance_Commerical insurance", 
                     "insurance_Medicaid",
                     "insurance_Medicare", "insurance_Other",
                     "alcohol", "bloss", "pud", "copd", "coag", "chf", "anem", "dep",
                     "diabeteswcomp", "diabeteswocopm", "fluid", "hiv", "hypert",
                     "hypertun", "hypothy", "liver", "lymph", "cancmeta", "obe", "neuro",
                     "para", "pvd", "psycho", "pcd", "rf", "collavd", "cancwometa",
                     "valve", "weight", 
                     #"oud", "cud", "mud", "idu", 
                     "Bacteremia",
                     "Osteomyelitis", "separthritis", "Endocarditis", #"Piomyositis", "los_index",
                     #"MOUD_No MOUD",  "MOUD_Buprenorphine", "MOUD_Methadone", 
                    "bed_size_>=1000", "bed_size_500-999", "bed_size_<500",
                    # "discharge_disposition_Home",
                    #"discharge_disposition_Patient directed discharge", "discharge_disposition_Other", 
                    #"discharge_disposition_Nursing facility or rehabilitation", 
                     "zip_code_3 - Alabama, Florida, Georgia, Mississippi, Tennessee", 
                    "zip_code_2 - District of Columbia, Maryland, North Carolina, South Carolina, Virginia, West Virginia", 
                     "zip_code_0 - Connecticut, Massachusetts, Maine, New Hampshire, New Jersey, Puerto Rico, Rhode Island", 
                     "zip_code_5 - Iowa, Minnesota, Montana, North Dakota, South Dakota, Wisconsin", 
                     "zip_code_9 - Alaska, California, Hawaii, Oregon, Washington", 
                     "zip_code_1 - Delaware, New York, Pennsylvania", "zip_code_8 - Arizona, Colorado, Idaho, New Mexico, Nevada, Utah, Wyoming", 
                     "zip_code_6 - Illinois, Kansas, Missouri, Nebraska", "zip_code_7 - Arkansas, Louisiana, Oklahoma, Texas", 
                     "zip_code_4 - Indiana, Kentucky, Michigan, Ohio")


#Calculate means
matched.descriptive.result.non.pwud <- map_dfr(list_continuous, ~ {
  variable <- .x
  df_filtered_1 <- df_binary.non.pwud[df_binary.non.pwud$lalgp == 1, c("lalgp", variable, ".imp")]
  df_filtered_0 <- df_binary.non.pwud[df_binary.non.pwud$lalgp == 0, c("lalgp", variable, ".imp")]

  means_1 <- tapply(df_filtered_1[[variable]], df_filtered_1$.imp, mean)
  variances_1 <- tapply(df_filtered_1[[variable]], df_filtered_1$.imp, sd)
  means_0 <- tapply(df_filtered_0[[variable]], df_filtered_0$.imp, mean)
  variances_0 <- tapply(df_filtered_0[[variable]], df_filtered_0$.imp, sd)

  data.frame(variable = variable, mean_lalgp_1 = mean(means_1), mean_lalgp_0 = mean(means_0))
})

matched.descriptive.result.non.pwud <- matched.descriptive.result.non.pwud %>%
  mutate(across(starts_with("mean"), ~if_else(!variable %in% c("age", "los_index"), . * 100, .))) %>%
  mutate(across(c(-variable), ~round(., 2)))

matched.descriptive.result.non.pwud



non.pwud.bal.tab.matched <- bal.tab(
    df.iptw.non.pwud.matched, 
    un = TRUE,
    disp = c("means", "sds"),
    stats = c('m', 'ks'),
    imp.fun = 'mean')

#Create table
table.matched.descriptives.non.pwud <- non.pwud.bal.tab.matched$Balance.Across.Imputations %>% 
    rownames_to_column(var = "variable") %>%
    select(-Type) %>%
    left_join(matched.descriptive.result.non.pwud) %>%
    relocate(mean_lalgp_0, mean_lalgp_1, .after = "variable") %>%
    mutate(across(starts_with("Mean."), ~round(., 3))) %>%
    mutate(variable = c(
  "Distance",
  "Age",
  "Male",
  "Black or African American",
  "Other race",
  "White or Caucasian",
  "Not Hispanic or Latino",
  "Commerical insurance",
  "Medicaid",
  "Medicare",
  "Other insurance",
  "Alcohol abuse",
  "Blood loss anemia",
  "Chronic peptic ulcer disease",
  "Chronic pulmonary disease",
  "Coagulation deficiency",
  "Congestive heart failure",
  "Deficiency anemias",
  "Depression",
  "Diabetes with chronic complications",
  "Diabetes without chronic complications",
  "Fluid and electrolyte disorders",
  "HIV and AIDS",
  "Hypertension, complicated",
  "Hypertension, uncomplicated",
  "Hypothyroidism",
  "Liver disease",
  "Lymphoma",
  "Metastatic cancer",
  "Obesity",
  "Other neurological disorders",
  "Paralysis",
  "Peripheral vascular disease",
  "Psychoses",
  "Pulmonary circulation disorders",
  "Renal failure",
  "Rheumatoid arthritis/collagen vascular diseases",
  "Solid tumor without metastasis",
  "Valvular disease",
  "Weight loss",
  #"History of opioid use or disorder",
  #"History of cocaine use or disorder",
  #"History of methamphetamine use or disorder",
  #"History of injection drug use",
  "Bacteremia",
  "Osteomyelitis",
  "Septic arthritis",
  "Endocarditis",
  #"Piomyositis",
  #"Length of stay, index admission",
  #"Home",
  #"Nursing facility or rehabilitation",
  #"Other discharge disposition",
  #"Patient directed discharge",
  "0 - Connecticut, Massachusetts, Maine, New Hampshire, New Jersey, Puerto Rico, Rhode Island",
  "1 - Delaware, New York, Pennsylvania",
  "2 - District of Columbia, Maryland, North Carolina, South Carolina, Virginia, West Virginia",
  "3 - Alabama, Florida, Georgia, Mississippi, Tennessee",
  "4 - Indiana, Kentucky, Michigan, Ohio",
  "5 - Iowa, Minnesota, Montana, North Dakota, South Dakota, Wisconsin",
  "6 - Illinois, Kansas, Missouri, Nebraska",
  "7 - Arkansas, Louisiana, Oklahoma, Texas",
  "8 - Arizona, Colorado, Idaho, New Mexico, Nevada, Utah, Wyoming",
  "9 - Alaska, California, Hawaii, Oregon, Washington",
  "bed_size_<500",
  "bed_size_>=1000",
  "bed_size_500-999"
))

new_var_name0 <- paste0("SOC, n = ", non.pwud.bal.tab.matched$Observations[4, 1])
new_var_name1 <- paste0("laLGP, n = ", non.pwud.bal.tab.matched$Observations[4, 2])

table.matched.descriptives.non.pwud <- table.matched.descriptives.non.pwud %>%
  mutate(across(c(mean_lalgp_0, mean_lalgp_1), ~round(., 1))) %>%
  #mutate(across(starts_with("Mean."), ~.*100)) %>%
  rename_with(~new_var_name0, mean_lalgp_0) %>%
  rename_with(~new_var_name1, mean_lalgp_1) %>%
    rename(`SMD, unmatched` = Mean.Diff.Un,
           `KS, unmatched` = Mean.KS.Un,
           `SMD, matched` = Mean.Diff.Adj,
           `KS, matched` = Mean.KS.Adj) #%>%
#  kable() %>%
#  kable_styling("striped") %>%
#  save_kable(paste0("results/table_descriptives_matched_pwud_", Sys.Date(), ".html"))

table.matched.descriptives.non.pwud
tbl.1.non.pwud %>% as.tibble() %>% 
    mutate( `**Characteristic**` = 
           ifelse( `**Characteristic**` == "Other", "Other insurance",  `**Characteristic**`)) %>%
  mutate(
    `**Characteristic**` = str_remove_all(`**Characteristic**`, "__"),
       `**Characteristic**` = gsub("\\(SD\\)", "", `**Characteristic**`),
          `**Characteristic**` = str_remove_all(`**Characteristic**`, ', years, Mean'),
                `**Characteristic**` = str_remove_all(`**Characteristic**`, ', Mean'),
      `**Characteristic**` = trimws(str_remove_all(`**Characteristic**`, ", n"))) %>%
    mutate(
    `**Characteristic**` = gsub("\\(.*?%\\)", "", `**Characteristic**`),
    `**Characteristic**` = trimws(`**Characteristic**`)
  ) %>%
  left_join(
    table.matched.descriptives.non.pwud, 
    by = c("**Characteristic**" = "variable")
  ) %>%
    mutate_all(~ifelse(is.na(.), "", as.character(.))) %>%
    mutate_all(~ifelse(. == "Unknown", "Missing", .)) %>%
    #filter(!`**Characteristic**` %in% c("Female", "Hispanic or Latino", 
    #          "Blood loss anemia", "Chronic peptic ulcer disease", 
    #          "Coagulation deficiency", "Fluid and electrolyte disorders", 
    #          "Hypothyroidism", "Hypertension, complicated", "Hypertension, uncomplicated", 
    #          "Obesity", "Other neurological disorders", "Paralysis", 
    #          "Peripheral vascular disease", "Pulmonary circulation disorders", 
    #          "Rheumatoid arthritiscollagen vascular diseases", 
    #          "Weight loss")) %>%
    relocate(`SMD, unmatched`, .after = "**p-value**") %>%
    select(-c(`KS, unmatched`, `KS, matched`)) %>%
    kable() %>%
 kable_styling("striped") %>%
 save_kable(paste0("results/table_descriptives_matched_non_pwud_", Sys.Date(), ".html"))



#Love plots - pwud

options(repr.plot.width=10, repr.plot.height=10)

love.plot.pwud <- love.plot(df.iptw.pwud.matched, binary = "raw",
          thresholds = c(m = .1),
          abs = TRUE, 
          #var.order = "unadjusted", 
          line = TRUE,
         stars = "raw") +
        labs(title = "Covariate balance - PWUD")



love.plot.non.pwud <- love.plot(df.iptw.non.pwud.matched, binary = "raw",
          thresholds = c(m = .1),
          abs = TRUE, 
          #var.order = "unadjusted", 
          line = TRUE, 
         stars = "raw") +
        labs(title = "Covariate balance - non-PWUD")


ggsave(plot = love.plot.pwud, 
       "results/plots/plot_love_pwud.pdf", width = 10, height = 10, dpi = 300)
ggsave(plot = love.plot.pwud, 
       "results/plots/plot_love_pwud.png", width = 30, height = 10, dpi = 300)

ggsave(plot = love.plot.non.pwud, 
       "results/plots/plot_love_non_pwud.pdf", width = 10, height = 10, dpi = 300)
ggsave(plot = love.plot.non.pwud, 
       "results/plots/plot_love_non_pwud.png", width = 10, height = 10, dpi = 300)

#Love plots - non pwud
bal.plot.pwud <- bal.plot(df.iptw.pwud.matched, var.name = "distance",
        which = "both") + 
    labs("Distributiona Balance for Propensity Score - PWUD", x = "Propensity Score") +
    scale_fill_discrete(labels = c("SOC", "laLGP"))

bal.plot.non.pwud <- bal.plot(df.iptw.non.pwud.matched, var.name = "distance",
        which = "both") +
    labs("Distributiona Balance for Propensity Score - non-PWUD", x = "Propensity Score") +
    scale_fill_discrete(labels = c("SOC", "laLGP"))

ggsave(plot = bal.plot.pwud, 
       "results/plots/plot_balance_pwud.pdf", width = 10, height = 10, dpi = 300)
ggsave(plot = bal.plot.pwud, 
       "results/plots/plot_balance_pwud.png", width = 10, height = 10, dpi = 300)

ggsave(plot = bal.plot.non.pwud, 
       "results/plots/plot_balance_non_pwud.pdf", width = 10, height = 10, dpi = 300)
ggsave(plot = bal.plot.non.pwud, 
       "results/plots/plot_balance_non_pwud.png", width = 10, height = 10, dpi = 300)

#Matched analysis

#Weighted analysis
matched.models.pwud <- with(df.iptw.pwud.matched,
                            svyglm(composite ~ lalgp, family = quasibinomial()))

#Obtain results
matched.results.pwud <- pool(matched.models.pwud)
table.matched.pwud <- summary(matched.results.pwud, exponentiate = T, conf.int = TRUE)

#Weighted analysis
matched.models.non.pwud <- with(df.iptw.non.pwud.matched,
                                svyglm(composite ~ lalgp, family = quasibinomial()))

#Obtain results
matched.results.non.pwud <- pool(matched.models.non.pwud)
table.matched.non.pwud <- summary(matched.results.non.pwud, exponentiate = T, conf.int = TRUE)

#Merge results
table.matched.non.pwud <- 
    table.matched.non.pwud %>% 
    mutate(stratum = "non-PWUD") %>%
    relocate(stratum) %>%
    filter(!term == "(Intercept)") %>%
    mutate(term = "Overall")

table.matched.pwud <-
table.matched.pwud %>% 
    mutate(stratum = "PWUD") %>%
    relocate(stratum) %>%
    filter(!term == "(Intercept)") %>%
    mutate(term = "Overall")


table.results.overall <- table.matched.non.pwud %>%
    bind_rows(table.matched.pwud)

#Get table
table.results.overall %>% mutate(across(where(is.numeric), ~round(., 3)))

#PWUD
matched.models.readmission.pwud <- with(df.iptw.pwud.matched,
                svyglm(readmissions_90d ~ lalgp, family = quasibinomial()))
matched.models.edvisit.pwud <- with(df.iptw.pwud.matched,
                svyglm(edvisit_90d ~ lalgp, family = quasibinomial()))
matched.models.death.pwud <- with(df.iptw.pwud.matched,
                svyglm(death_or_hospice_90d ~ lalgp, family = quasibinomial()))

matched.results.readmission.pwud <- pool(matched.models.readmission.pwud)
table.matched.readmission.pwud <- summary(matched.results.readmission.pwud, exponentiate = T, conf.int = TRUE)

matched.results.edvisit.pwud <- pool(matched.models.edvisit.pwud)
table.matched.edvisit.pwud <- summary(matched.results.edvisit.pwud, exponentiate = T, conf.int = TRUE)

matched.results.death.pwud <- pool(matched.models.death.pwud)
table.matched.death.pwud <- summary(matched.results.death.pwud, exponentiate = T, conf.int = TRUE)

#Other outcomes Non pwud
list.outcomes <- c("readmissions_90d", "edvisit_90d", "death_or_hospice_90d")


matched.models.readmission.non.pwud <- with(df.iptw.non.pwud.matched,
                svyglm(readmissions_90d ~ lalgp, family = quasibinomial()))
matched.models.edvisit.non.pwud <- with(df.iptw.non.pwud.matched,
                svyglm(edvisit_90d ~ lalgp, family = quasibinomial()))
matched.models.death.non.pwud <- with(df.iptw.non.pwud.matched,
                svyglm(death_or_hospice_90d ~ lalgp, family = quasibinomial()))

matched.results.readmission.non.pwud <- pool(matched.models.readmission.non.pwud)
table.matched.readmission.non.pwud <- summary(matched.results.readmission.non.pwud, exponentiate = T, conf.int = TRUE)

matched.results.edvisit.non.pwud <- pool(matched.models.edvisit.non.pwud)
table.matched.edvisit.non.pwud <- summary(matched.results.edvisit.non.pwud, exponentiate = T, conf.int = TRUE)

matched.results.death.non.pwud <- pool(matched.models.death.non.pwud)
table.matched.death.non.pwud <- summary(matched.results.death.non.pwud, exponentiate = T, conf.int = TRUE)

#merge other outcome tables
table.matched.sec.outcomes.pwud <- table.matched.readmission.pwud %>%
    bind_rows(table.matched.edvisit.pwud,
              table.matched.death.pwud) %>% 
    mutate(stratum = "PWUD") %>%
    relocate(stratum) %>%
    filter(!term == "(Intercept)") %>%
    mutate(term = c("Readmission", "ED visit", "Death or hospice"))



table.matched.sec.outcomes.non.pwud <- table.matched.readmission.non.pwud %>%
    bind_rows(table.matched.edvisit.non.pwud,
              table.matched.death.non.pwud) %>% 
    mutate(stratum = "non-PWUD") %>%
    relocate(stratum) %>%
    filter(!term == "(Intercept)") %>%
    mutate(term = c("Readmission", "ED visit", "Death or hospice"))

#Get table
table.matched.sec.outcomes.pwud %>% 
    bind_rows(table.matched.sec.outcomes.non.pwud) %>% 
    mutate(across(where(is.numeric), ~round(., 3)))
options(scipen=999)

#Save table
table.matched.sec.outcomes.pwud %>% 
    bind_rows(table.matched.sec.outcomes.non.pwud) %>% 
    relocate(p.value, std.error, statistic, df, 
             .after = last_col()) %>%
    mutate(across(where(is.numeric), ~round(., 3))) %>%
    kable() %>%
    kable_styling("striped") %>%
    save_kable(paste0("results/table_secondary_outcomes_", Sys.Date(), ".html"))
#Multiple imputation of missing values - takes very long
list.infection <- c("Bacteremia", "Osteomyelitis", "separthritis", "Endocarditis")#, "Piomyositis")

df.iptw.pwud.imputed.stratified <-  
    purrr::map(list.infection, ~ 
        mice(
            df.iptw.pwud %>% filter(.data[[.x]] == 1), 
            m = 10, maxit = 10, seed = 20200516)
        )

#Save multiple imputed datasets
df.iptw.pwud.imputed.stratified %>% saveRDS("data/pwud_mi_stratified2.rds")

#Read multiple imputed datasets
df.iptw.pwud.imputed.stratified <- readRDS("data/pwud_mi_stratified2.rds")

#Create formulas
pwud.formula.stratified <- 
    paste("lalgp ~ ", 
          paste(names(df.iptw.pwud %>%                                      
                      select(-c(lalgp, composite, 
                               Bacteremia, Osteomyelitis, separthritis, 
                                Endocarditis, #Piomyositis, 
                               censure_time,
                                           death_or_hospice_90d, readmissions_90d, edvisit_90d, soc_antibiotics))), 
                            collapse = " + ")
                      )


as.formula(pwud.formula.stratified)

#Match
df.iptw.pwud.matched.stratfied <- 
    purrr::map(df.iptw.pwud.imputed.stratified[1:4], ~
        matchthem(as.formula(pwud.formula.stratified),
                  datasets = .x,
                  approach = 'within',
                  method = 'nearest',
                  caliper = 0.05,
                  ratio = 3)
        )


#Assess balance
options(repr.plot.width=20, repr.plot.height=10)

pwud.bal.tab.matched.stratified <- 
    purrr::map(df.iptw.pwud.matched.stratfied, ~
        bal.tab(
    .x, 
    un = TRUE,
    stats = c('m', 'ks'),
    imp.fun = 'max')
        )

pwud.love.plot.matched.stratified <- 
    purrr::map(df.iptw.pwud.matched.stratfied, ~
        love.plot(.x, 
                  binary = "raw",
          thresholds = c(m = .1),
          abs = TRUE, 
          #var.order = "unadjusted", 
          line = TRUE,
         stars = "raw")
        )

#Weighted analysis
matched.models.pwud.stratified <- 
    purrr::map(df.iptw.pwud.matched.stratfied, ~
        with(.x,
         svyglm(composite ~ lalgp, family = quasibinomial()))
                           )

matched.results.pwud.stratified <- 
        purrr::map(matched.models.pwud.stratified, ~
            pool(.x)
            )


table.matched.pwud.stratified <- map_dfr(matched.results.pwud.stratified, ~
    summary(.x, exponentiate = T, conf.int = TRUE)
    )

#Multiple imputation of missing values - takes very long
list.infection <- c("Bacteremia", "Osteomyelitis", "separthritis", "Endocarditis")#, "Piomyositis")

df.iptw.non.pwud.imputed.stratified <-  
    purrr::map(list.infection, ~ 
        mice(
            df.iptw.non.pwud %>% filter(.data[[.x]] == 1), 
            m = 10, maxit = 10, seed = 20200516)
        )

df.iptw.non.pwud.imputed.stratified %>% saveRDS("data/non_pwud_mi_stratified2.rds")
df.iptw.non.pwud.imputed.stratified <- readRDS("data/non_pwud_mi_stratified2.rds")

non.pwud.formula.stratified <- 
    paste("lalgp ~ ", 
          paste(names(df.iptw.non.pwud %>%                                      
                      select(-c(lalgp, composite, 
                               Bacteremia, Osteomyelitis, separthritis, 
                                Endocarditis, #Piomyositis, 
                               censure_time,
                                           death_or_hospice_90d, readmissions_90d, edvisit_90d, soc_antibiotics))), 
                            collapse = " + ")
                      )

as.formula(non.pwud.formula.stratified)

#Match
df.iptw.non.pwud.matched.stratfied <- 
    purrr::map(df.iptw.non.pwud.imputed.stratified[1:4], ~
        matchthem(as.formula(non.pwud.formula.stratified),
                  datasets = .x,
                  approach = 'within',
                  method = 'nearest',
                  caliper = 0.05,
                  ratio = 3)
        )

#Assess balance
options(repr.plot.width=20, repr.plot.height=10)

non.pwud.bal.tab.matched.stratified <- 
    purrr::map(df.iptw.non.pwud.matched.stratfied, ~
        bal.tab(
    .x, 
    un = TRUE,
    stats = c('m', 'ks'),
    imp.fun = 'max')
        )

non.pwud.love.plot.matched.stratified <- 
    purrr::map(df.iptw.non.pwud.matched.stratfied, ~
        love.plot(.x, 
                  binary = "raw",
          thresholds = c(m = .1),
          abs = TRUE, 
          #var.order = "unadjusted", 
          line = TRUE,
         stars = "raw")
        )

#Weighted analysis
matched.models.non.pwud.stratified <- 
    purrr::map(df.iptw.non.pwud.matched.stratfied, ~
        with(.x,
         svyglm(composite ~ lalgp, family = quasibinomial()))
                           )

matched.results.non.pwud.stratified <- 
        purrr::map(matched.models.non.pwud.stratified, ~
            pool(.x)
            )

table.matched.non.pwud.stratified <- map_dfr(matched.results.non.pwud.stratified, ~
    summary(.x, exponentiate = T, conf.int = TRUE)
    )
table.matched.non.pwud.stratified <- 
    table.matched.non.pwud.stratified %>% 
    mutate(stratum = "non-PWUD") %>%
    relocate(stratum) %>%
    filter(!term == "(Intercept)") %>%
    mutate(term = 
               ifelse(row_number() <= 4, 
                      c("Bacteremia, isolated", "Osteomyelitis", 
                        "Septic arthritis", "Endocarditis"), 
                      term)
          )

table.matched.pwud.stratified <-
table.matched.pwud.stratified %>% 
    mutate(stratum = "PWUD") %>%
    relocate(stratum) %>%
    filter(!term == "(Intercept)") %>%
    mutate(term = 
               ifelse(row_number() <= 4, 
                      c("Bacteremia, isolated", "Osteomyelitis", 
                        "Septic arthritis", "Endocarditis"), 
                      term)
          )


table.stratified.all <- table.matched.non.pwud.stratified %>%
    bind_rows(table.matched.pwud.stratified)
table.results.all <- table.results.overall %>% 
    bind_rows(table.stratified.all) %>%
    group_by(stratum) %>%
    arrange(stratum) %>%
    relocate(p.value, std.error, statistic, df, 
             .after = last_col())

table.results.all %>% 
    mutate(across(where(is.numeric), ~round(., 3)))
#Save table
table.results.all %>% 
  mutate(across(where(is.numeric), ~round(., 3))) %>%
    kable() %>%
    kable_styling("striped") %>%
    save_kable(paste0("results/table_overall_stratified_", Sys.Date(), ".html"))
#Multiple imputation of missing values - takes very long
list.antibiotics <- c("Vancomycin", "Cefazolin")

df.iptw.pwud.imputed.abx <-  
    purrr::map(list.antibiotics, ~ 
        mice(
            df.iptw.pwud %>% filter(soc_antibiotics == .x |
                                   lalgp == 1), 
            m = 10, maxit = 10, seed = 20200516)
        )

#Save multiple imputed datasets
df.iptw.pwud.imputed.abx %>% saveRDS("data/pwud_mi_abx.rds")
#Read multiple imputed datasets
df.iptw.pwud.imputed.abx <- readRDS("data/pwud_mi_abx.rds")

#Create formulas
pwud.formula.abx <- 
    paste("lalgp ~ ", 
          paste(names(df.iptw.pwud %>%                                      
                      select(-c(lalgp, composite, 
                               censure_time,
                                           death_or_hospice_90d, readmissions_90d, edvisit_90d, soc_antibiotics))), 
                            collapse = " + ")
                      )


as.formula(pwud.formula.abx)

#Match
df.iptw.pwud.matched.abx <- 
    purrr::map(df.iptw.pwud.imputed.abx[1:2], ~
        matchthem(as.formula(pwud.formula.abx),
                  datasets = .x,
                  approach = 'within',
                  method = 'nearest',
                  caliper = 0.05,
                  ratio = 3)
        )


#Assess balance
options(repr.plot.width=20, repr.plot.height=10)

pwud.bal.tab.matched.abx <- 
    purrr::map(df.iptw.pwud.matched.abx, ~
        bal.tab(
    .x, 
    un = TRUE,
    stats = c('m', 'ks'),
    imp.fun = 'max')
        )

pwud.love.plot.matched.abx <- 
    purrr::map(df.iptw.pwud.matched.abx, ~
        love.plot(.x, 
                  binary = "raw",
          thresholds = c(m = .1),
          abs = TRUE, 
          #var.order = "unadjusted", 
          line = TRUE,
         stars = "raw")
        )

#Weighted analysis
matched.models.pwud.abx <- 
    purrr::map(df.iptw.pwud.matched.abx, ~
        with(.x,
         svyglm(composite ~ lalgp, family = quasibinomial()))
                           )

matched.results.pwud.abx <- 
        purrr::map(matched.models.pwud.abx, ~
            pool(.x)
            )


table.matched.pwud.abx <- map_dfr(matched.results.pwud.abx, ~
    summary(.x, exponentiate = T, conf.int = TRUE)
    )


#Multiple imputation of missing values - takes very long
list.antibiotics <- c("Vancomycin", "Cefazolin")

df.iptw.non.pwud.imputed.abx <-  
    purrr::map(list.antibiotics, ~ 
        mice(
            df.iptw.non.pwud %>% filter(soc_antibiotics == .x |
                                   lalgp == 1), 
            m = 10, maxit = 10, seed = 20200516)
        )

#Save multiple imputed datasets
df.iptw.non.pwud.imputed.abx %>% saveRDS("data/non_pwud_mi_abx.rds")


#Read multiple imputed datasets
df.iptw.non.pwud.imputed.abx <- readRDS("data/non_pwud_mi_abx.rds")

#Create formulas
pwud.formula.abx <- 
    paste("lalgp ~ ", 
          paste(names(df.iptw.non.pwud %>%                                      
                      select(-c(lalgp, composite, 
                               censure_time,
                                           death_or_hospice_90d, readmissions_90d, edvisit_90d, soc_antibiotics))), 
                            collapse = " + ")
                      )


as.formula(pwud.formula.abx)

#Match
df.iptw.non.pwud.matched.abx <- 
    purrr::map(df.iptw.non.pwud.imputed.abx[1:2], ~
        matchthem(as.formula(pwud.formula.abx),
                  datasets = .x,
                  approach = 'within',
                  method = 'nearest',
                  caliper = 0.05,
                  ratio = 3)
        )


#Assess balance
options(repr.plot.width=20, repr.plot.height=10)

pwud.bal.tab.matched.abx <- 
    purrr::map(df.iptw.non.pwud.matched.abx, ~
        bal.tab(
    .x, 
    un = TRUE,
    stats = c('m', 'ks'),
    imp.fun = 'max')
        )

pwud.love.plot.matched.abx <- 
    purrr::map(df.iptw.non.pwud.matched.abx, ~
        love.plot(.x, 
                  binary = "raw",
          thresholds = c(m = .1),
          abs = TRUE, 
          #var.order = "unadjusted", 
          line = TRUE,
         stars = "raw")
        )

#Weighted analysis
matched.models.non.pwud.abx <- 
    purrr::map(df.iptw.non.pwud.matched.abx, ~
        with(.x,
         svyglm(composite ~ lalgp, family = quasibinomial()))
                           )

matched.results.non.pwud.abx <- 
        purrr::map(matched.models.non.pwud.abx, ~
            pool(.x)
            )


table.matched.non.pwud.abx <- map_dfr(matched.results.non.pwud.abx, ~
    summary(.x, exponentiate = T, conf.int = TRUE)
    )



table.matched.pwud.abx.abx <- 
    table.matched.pwud.abx %>% 
    mutate(stratum = "PWUD") %>%
    relocate(stratum) %>%
    filter(!term == "(Intercept)") %>%
    mutate(term = 
               ifelse(row_number() <= 2, 
                      c("Vancomycin", "Cefazolin"), 
                      term)
          )



table.matched.non.pwud.abx.abx <- 
    table.matched.non.pwud.abx %>% 
    mutate(stratum = "non-PWUD") %>%
    relocate(stratum) %>%
    filter(!term == "(Intercept)") %>%
    mutate(term = 
               ifelse(row_number() <= 2, 
                      c("Vancomycin", "Cefazolin"), 
                      term)
          )


table.matched.pwud.abx.abx %>%
    bind_rows(table.matched.non.pwud.abx.abx) %>%
    mutate(across(where(is.numeric), ~round(., 3)))

#Save table
table.matched.pwud.abx.abx %>%
    bind_rows(table.matched.non.pwud.abx.abx) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
    kable() %>%
    kable_styling("striped") %>%
    save_kable(paste0("results/table_overall_abx_", Sys.Date(), ".html"))