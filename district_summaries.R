# District Summaries
# Evan Kramer

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
setwd(str_c("N:/ORP_accountability/data/", year(now()), "_final_accountability_files"))

# Switches
data = F
analysis = T
output = F

# Data
if(data) {
  regions = read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/core_region_crosswalk.dta")
  school_assessment = read_csv("school_assessment_file.csv")
  district_assessment = read_csv("district_assessment_file.csv")
  state_assessment = read_csv("state_assessment_file.csv") 
  school_tvaas = readxl::read_excel(str_c("N:/ORP_accountability/data/", year(now()), "_tvaas/SAS-NIET School-Wide.xlsx")) %>% 
    janitor::clean_names() %>% 
    transmute(
      system = as.numeric(district_number),
      school = as.numeric(school_number),
      school_tvaas = school_wide_composite
    )
  school_tvaas_lag = readxl::read_excel(str_c("N:/ORP_accountability/data/", year(now()) - 1, "_tvaas/School Composite Level.xlsx")) %>% 
    janitor::clean_names() %>% 
    transmute(
      system = as.numeric(district_number),
      school = as.numeric(school_number),
      school_tvaas_lag = school_wide_composite
    )
  district_tvaas = readxl::read_excel(str_c("N:/ORP_accountability/data/", year(now()), "_tvaas/SAS-NIET District-Wide.xlsx")) %>% 
    janitor::clean_names() %>% 
    transmute(
      system = as.numeric(district_number),
      district_tvaas = system_wide_composite
    )
  district_tvaas_lag = readxl::read_excel(str_c("N:/ORP_accountability/data/", year(now()) - 1, "_tvaas/District Composite.xlsx")) %>% 
    janitor::clean_names() %>% 
    transmute(
      system = as.numeric(district_number),
      district_tvaas_lag = system_wide_composite
    )
  school_absenteeism = read_csv(str_c("N:/ORP_accountability/data/", year(now()), "_chronic_absenteeism/school_chronic_absenteeism_Jul25.csv"))
  district_absenteeism = read_csv(str_c("N:/ORP_accountability/data/", year(now()), "_chronic_absenteeism/district_chronic_absenteeism_Jul25.csv"))
  state_absenteeism = read_csv(str_c("N:/ORP_accountability/data/", year(now()), "_chronic_absenteeism/state_chronic_absenteeism_Jul25.csv"))
} else {
  rm(data)
}

# Analysis
if(analysis) {
  # Assessment
  assessment_summary = 
    # School
    filter(school_assessment, test %in% c("EOC", "TNReady") & subgroup == "All Students" & 
             grade == "All Grades") %>% 
    group_by(year, system, system_name, school, school_name) %>% 
    summarize(pct_on_mastered = round(100 * (sum(n_on_track, na.rm = T) + sum(n_mastered, na.rm = T)) / 
                                        sum(valid_tests, na.rm = T), 1)) %>% 
    ungroup() %>% 
    # District
    left_join(
      filter(district_assessment, test %in% c("EOC", "TNReady") & subgroup == "All Students" & 
               grade == "All Grades") %>% 
        group_by(year, system) %>% 
        summarize(pct_on_mastered_district = round(100 * (sum(n_on_track, na.rm = T) + sum(n_mastered, na.rm = T)) / 
                                                     sum(valid_tests, na.rm = T), 1)) %>% 
        ungroup(),
      by = c("year", "system")
    ) %>% 
    # Region
    left_join(
      transmute(regions, system, region),
      by = "system"
    ) %>%
    select(year, region, everything()) %>% 
    left_join(
      filter(district_assessment, test %in% c("EOC", "TNReady") & subgroup == "All Students" & 
               grade == "All Grades") %>% 
        left_join(
          transmute(regions, system, region),
          by = "system"
        ) %>% 
        group_by(year, region) %>% 
        summarize(pct_on_mastered_region = round(100 * (sum(n_on_track, na.rm = T) + sum(n_mastered, na.rm = T)) / 
                                                   sum(valid_tests, na.rm = T), 1)) %>% 
        ungroup(),
      by = c("year", "region")
    ) %>% 
    # State
    left_join(
      filter(state_assessment, test %in% c("EOC", "TNReady") & subgroup == "All Students" & 
               grade == "All Grades") %>% 
        group_by(year) %>% 
        summarize(pct_on_mastered_state = round(100 * (sum(n_on_track, na.rm = T) + sum(n_mastered, na.rm = T)) / 
                                                  sum(valid_tests, na.rm = T), 1)) %>% 
        ungroup(),
      by = "year"
    )
  
  # TVAAS
  tvaas_summary =
    # District 
    full_join(district_tvaas, district_tvaas_lag, by = "system") %>% 
      left_join(
        left_join(school_tvaas, school_tvaas_lag, by = c("system", "school")) %>% 
          group_by(system) %>% 
          summarize(
            pct_high_tvaas_district = round(100 * sum(school_tvaas >= 4, na.rm = T) / sum(!is.na(school_tvaas)), 1),
            pct_high_tvaas_2yr_district = round(100 * sum(school_tvaas >= 4 & school_tvaas_lag >= 4, na.rm = T) / 
                                         sum(!is.na(school_tvaas) & !is.na(school_tvaas_lag)), 1)
          ) %>% 
          ungroup(),
        by = "system"
      ) %>% 
    #Region
      left_join(transmute(regions, system, region), by = "system") %>% 
      select(region, everything()) %>% 
      left_join(
        left_join(school_tvaas, school_tvaas_lag, by = c("system", "school")) %>% 
          left_join(transmute(regions, system, region), by = "system") %>% 
          group_by(region) %>% 
          summarize(
            pct_high_tvaas_region = round(100 * sum(school_tvaas >= 4, na.rm = T) / sum(!is.na(school_tvaas)), 1),
            pct_high_tvaas_2yr_region = round(100 * sum(school_tvaas >= 4 & school_tvaas_lag >= 4, na.rm = T) / 
                                         sum(!is.na(school_tvaas) & !is.na(school_tvaas_lag)), 1)
          ) %>% 
          ungroup(),
        by = "region"
      ) %>% 
    # State 
      left_join(
        left_join(school_tvaas, school_tvaas_lag, by = c("system", "school")) %>% 
          mutate(
            pct_high_tvaas_state = round(100 * sum(school_tvaas >= 4, na.rm = T) / sum(!is.na(school_tvaas)), 1),
            pct_high_tvaas_2yr_state = round(100 * sum(school_tvaas >= 4 & school_tvaas_lag >= 4, na.rm = T) / 
                                                sum(!is.na(school_tvaas) & !is.na(school_tvaas_lag)), 1)
          ) %>% 
          distinct(system, pct_high_tvaas_state, pct_high_tvaas_2yr_state),
        by = "system"
      ) %>% 
    # Names
      left_join(transmute(assessment_summary, system, system_name), by = "system") %>% 
      select(region, starts_with("system"), everything())
      
  # Absenteeism
  absenteeism_summary = 
    # School
    # District
    # Region
    # State
} else {
  rm(analysis)
}

# Output 
if(output) {
  if(!"district" %in% ls()) {
    print("You sure you have the data for this?")
  } else {
    # Initiate loop for plots
    for(d in sort(unique(district$system))) {
      # Reference RMD file
      rmarkdown::render(
        str_c(
          "N:/ORP_accountability/projects/", 
          year(now()), 
          "_district_release/Code/knit_loop.Rmd"
        ),
        output_file = str_c(d, "_absenteeism_health_report.html"),
        output_dir = str_c(
          "N:/ORP_accountability/projects/", 
          year(now()), 
          "_district_release/Code/Output"
        )
      )
    }
  }
} else {
  rm(output)
}

