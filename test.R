# load packages 
packages  <- c("microdatasus", "dplyr", "purrr", "ggplot2","tidyr",  
               "lubridate","geobr", "stringr") 
invisible(install.packages(setdiff(packages, rownames(installed.packages())))) 
invisible(lapply(packages, function(pkg) suppressMessages(require(pkg, character.only = TRUE)))) 

# other options 
options(scipen = 999) 
options(timeout = 900) 

# load functions 
files.sources <-  list.files("R/") 
invisible(sapply(paste0("R/", files.sources), source)) 
# malariaData <- fetch_datasus(year_start = 2014, year_end = 2014, information_system = "SINAN-MALARIA") 
malariaData <- readRDS("data/mal14.rds") 
colnames(malariaData) 
malariaVarDefPath <- file.path(getwd(), 'workspace', 'Dictionary_Malaria_EN.csv') 
malariaVarDef <- read.csv(malariaVarDefPath) 
knitr::kable(malariaVarDef[14:20,1:5]) 
sel_var <- c("NU_IDADE_N","CS_SEXO", "CS_GESTANT", "CS_RACA", "AT_LAMINA", "AT_SINTOMA", "DT_NOTIFIC", "DT_SIN_PRI", "DT_DIGITA", "DEXAME", "DTRATA", "ID_MUNICIP",  "ID_MN_RESI",  "COMUNINF","RESULT")  

malariaData <- malariaData %>% select(all_of(sel_var)) 
NACount <- malariaData %>% map(~mean(is.na(.))) 
NACount <- data.frame(count=unlist(NACount), 
                      var=names(NACount)) 

ggplot(NACount[NACount$count != 0, ], aes(x = reorder(var, count), y = count)) + 
  geom_bar(stat = "identity", width = 0.9, position = position_dodge(width = 5)) + 
  ylab("Percentage of NAs in each column") + 
  xlab("Variables") + 
  coord_flip() 
NACount$var[NACount$count == 0] 
drop_variables <- NACount$var[NACount$count == 1] 
malariaData <- malariaData[, !(names(malariaData) %in% drop_variables)] 
malariaData <- malariaData %>% 
  mutate(RESULT = as.factor(case_when(RESULT == "1" ~ "Negative", 
                                      RESULT == "2" ~ "Falciparum", 
                                      RESULT == "3" ~ "F+Fg", 
                                      RESULT == "4" ~ "Vivax", 
                                      RESULT == "5" ~ "F+V", 
                                      RESULT == "6" ~ "V+Fg", 
                                      RESULT == "7" ~ "Fg", 
                                      RESULT == "8" ~ "Malariae", 
                                      RESULT == "9" ~ "F+M", 
                                      RESULT == "10" ~ "Ovale"))) 
ggplot(malariaData, aes(x = RESULT, fill=RESULT)) + 
  geom_bar() + 
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4, 
           label = paste0(sum(is.na(malariaData$RESULT)), " NA values"))+ 
  labs(title="Exam results during malaria cases in 2014", 
       y="Count", x="Exam Result", fill="Exam Results") + 
  geom_text(stat = 'count', position = position_stack(vjust = 0.5), 
            aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), "%"), 
                x = RESULT)) 
malariaData <- malariaData %>% 
  mutate(RES_EXAM = as.factor(case_when(RESULT == "Falciparum" | RESULT == "F+Fg" | RESULT == "Fg" | RESULT == "F+M" ~ "F", 
                                        RESULT == "Vivax" | RESULT == "V+Fg" ~ "V", 
                                        RESULT == "F+V"  ~  "V+F", 
                                        RESULT == "Malariae" | RESULT == "Ovale" ~ "other", 
                                        RESULT == "Negative" ~ "Negative"))) 
ggplot(malariaData, aes(x = RES_EXAM, fill=RES_EXAM)) + 
  geom_bar() + 
  labs(title="Exam results during malaria cases in 2014", 
       y="Count", x="Exam Result", fill="Exam Results") + 
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4, 
           label = paste0(sum(is.na(malariaData$RES_EXAM)), " NA values"))+ 
  geom_text(stat = 'count', position = position_stack(vjust = 0.5), 
            aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), "%"), 
                x = RES_EXAM)) 
malariaData %>% 
  mutate(epiweek = paste0(sprintf("%02d",epiweek(DT_NOTIFIC)))) %>% 
  group_by(epiweek,RES_EXAM) %>% 
  summarise(Count = n(), .groups = 'drop')%>% 
  pivot_wider(., id_cols = epiweek, names_from = RES_EXAM, values_from = Count) %>% 
  mutate(across(everything(), ~ replace_na(., 0)))%>% 
  ggplot(aes(x = epiweek)) + 
  labs(title="Malaria notification in 2014 per type of Malaria", 
       y="Number of cases", x="Week of the year", color = 'Malaria type') + 
  geom_line(aes(y = V, group = 1, color = "P.vivax")) + 
  geom_line(aes(y = F, group = 1, color = "P.falciparum")) + 
  geom_line(aes(y = `V+F`, group = 1, color = "Mixed malaria")) + 
  geom_line(aes(y = `other`, group = 1, color = "Other malaria")) 
malariaData <- malariaData[malariaData$RESULT != "Negative",] 
malariaData  <- malariaData %>% 
  mutate(AGE = case_when(NU_IDADE_N < 120 ~ as.numeric(NU_IDADE_N), # assumes that non-codified data means that the age in years was given 
                         NU_IDADE_N >= 120 & NU_IDADE_N < 1000 ~ NA_real_, 
                         NU_IDADE_N >= 1000 & NU_IDADE_N < 2366 ~ 0, 
                         NU_IDADE_N >= 2366 & NU_IDADE_N < 3000 ~ NA_real_, 
                         NU_IDADE_N >= 3000 & NU_IDADE_N < 3013 ~ 0, 
                         NU_IDADE_N >= 3013 & NU_IDADE_N < 4000 ~ NA_real_, 
                         NU_IDADE_N >= 4000 & NU_IDADE_N < 4120 ~ as.numeric(NU_IDADE_N - 4000), 
                         NU_IDADE_N >= 4120 ~ NA_real_, 
                         TRUE ~ NA_real_)) 
ggplot(malariaData, aes(x = AGE))+ 
  geom_histogram(binwidth = 1, fill = "lightblue",color="black", alpha = 0.7)+ 
  annotate("text", x = 100, y = Inf, label = paste0(sum(is.na(malariaData$AGE)), " NA values"), hjust = 1, vjust = 1, size = 4)+ 
  labs(x="Age [years]", y="Count", title="Age distribution")+ 
  theme_light() 

malariaData <- malariaData %>% 
  mutate(CS_SEXO = as.factor(case_when(CS_SEXO == "F" ~ "Female", 
                                       CS_SEXO == "M" ~ "Male", 
                                       CS_SEXO == "I" ~ "Ignored", 
                                       TRUE ~ "NA"))) 
ggplot(malariaData, aes(x = CS_SEXO, fill=CS_SEXO)) + 
  geom_bar() + 
  labs(title="Sex distribution of malaria cases in 2015", 
       x="Sex of the patient", y="Count", fill="Sex")+ 
  geom_text(stat = 'count', position = position_stack(vjust = 0.5), 
            aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), "%"), 
                x = CS_SEXO)) + 
  theme_light() 
pregnant <- malariaData %>% 
  filter((CS_GESTANT == 1 | CS_GESTANT == 2 | CS_GESTANT ==3 | CS_GESTANT == 4)) %>% 
  count() 

mpreg <- malariaData %>% 
  filter(CS_SEXO == "M" & (CS_GESTANT == 1 | CS_GESTANT == 2 | CS_GESTANT ==3 | CS_GESTANT == 4)) %>% 
  count() 

pover50 <- malariaData %>% 
  filter(AGE > 50 & (CS_GESTANT == 1 | CS_GESTANT == 2 | CS_GESTANT ==3 | CS_GESTANT == 4)) %>% 
  count() 

malariaData <- malariaData %>% 
  filter(!(CS_GESTANT %in% c("1", "2", "3", "4") & CS_SEXO == "M") &  
           !(CS_GESTANT %in% c("1", "2", "3", "4") & AGE > 50)) 
malariaData <- malariaData %>% 
  mutate(CS_RACA = as.factor(case_when(CS_RACA == 1 ~ "White", 
                                       CS_RACA == 2 ~ "Black", 
                                       CS_RACA == 3 ~ "Yellow", 
                                       CS_RACA == 4 ~ "Brown", 
                                       CS_RACA == 5 ~ "Indigenous", 
                                       CS_RACA == 9 ~ "Ignored", 
                                       TRUE ~ "NA"))) 

ggplot(malariaData, aes(x = CS_RACA, fill=CS_RACA)) + 
  geom_bar(stat = "count") + 
  labs(title="Race distribution of malaria cases in 2015", 
       x="Race of the patient", y="Count", fill="Race") + 
  geom_text(stat = 'count', 
            aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), "%"), 
                x = CS_RACA), 
            position = position_stack(vjust = 0.5)) + 
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4, 
           label = paste0(sum(is.na(malariaData$CS_RACA)), " NA values")) 

malariaData <- malariaData %>% 
  mutate(AT_LAMINA = as.factor(case_when(AT_LAMINA == 1 ~ "Active", 
                                         AT_LAMINA == 2 ~ "Passive", 
                                         AT_LAMINA == 3 ~ "LVC", 
                                         TRUE ~ "NA"))) 
ggplot(malariaData, aes(x = AT_LAMINA, fill=AT_LAMINA)) + 
  geom_bar() + 
  labs(title="Type of detections of malaria cases in 2014", 
       y="Count", x="Detection type", fill="Detection type")+ 
  geom_text(stat = 'count', 
            aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), "%"), 
                x = AT_LAMINA), 
            position = position_stack(vjust = 0.5)) + 
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4, 
           label = paste0(sum(is.na(malariaData$AT_LAMINA)), " NA values")) 
malariaData <- malariaData %>% 
  mutate(AT_SINTOMA = as.factor(case_when(AT_SINTOMA== "1" ~ "Yes", 
                                          AT_SINTOMA == "2" ~ "No", 
                                          TRUE ~ "NA"))) 

ggplot(data= malariaData, aes(x = AT_SINTOMA, fill=AT_SINTOMA)) + 
  geom_bar() + 
  labs(title="Presence of symptoms during malaria cases in 2014", 
       y="Count", x="Presence of symptoms", fill="Presence of symptoms") + 
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4, 
           label = paste0(sum(is.na(malariaData$AT_SINTOMA)), " NA values"))+ 
  geom_text(stat = 'count', position = position_stack(vjust = 0.5), 
            aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), "%"), 
                x = AT_SINTOMA)) 
malariaData %>% 
  mutate(EPIWEEK = floor_date(DT_NOTIFIC, "weeks")) %>% 
  group_by(EPIWEEK) %>% 
  summarise(Count = n()) %>% 
  ggplot() + 
  geom_line(aes(x = EPIWEEK, y = Count)) + 
  scale_x_date(date_labels = "%m-%d", date_breaks = "1 week") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
malariaData <- malariaData %>% 
  mutate(Symptoms = as.numeric(DT_SIN_PRI - DT_NOTIFIC), 
         Digitalization = as.numeric(DT_DIGITA - DT_NOTIFIC), 
         Exam = as.numeric(DEXAME - DT_NOTIFIC), 
         Treatment = as.numeric(DTRATA - DT_NOTIFIC)) 

malariaData %>% 
  select(Symptoms, Digitalization, Exam, Treatment) %>% 
  gather(TYPE, DAYS) %>% 
  ggplot(aes(x = TYPE, y = DAYS)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(-100, 100)) + 
  xlab("Event") + 
  ylab("Days from the notification day") 

muni <- read_municipality(code_muni = "all", year = 2014, showProgress = FALSE) %>%
  mutate(code_muni = str_sub(code_muni, end = 6)) %>%
  left_join(as.data.frame(table(malariaData$ID_MUNICIP, dnn = list("code_muni")), responseName = "noti_muni")) %>%
  left_join(as.data.frame(table(malariaData$COMUNINF, dnn = list("code_muni")), responseName = "infec_muni")) %>%
  left_join(as.data.frame(table(malariaData$ID_MN_RESI, dnn = list("code_muni")), responseName = "resi_muni"))
cols <- c("green", "yellow","orange","red", "black") #change these as needed
breaks <- c(0, 10, 100, 1000, 10000, 100000) #change these as needed

ggplot(muni) +
  ggtitle("Extra-Amazon malaria per municipality of notification") +
  geom_sf( aes(fill = noti_muni), size = .15, show.legend = TRUE) +
  scale_fill_stepsn(breaks = breaks,
                    colours = cols,
                    values = scales::rescale(breaks))

ggplot(muni) +
  ggtitle("Extra-Amazon malaria per municipality of probable infection") +
  geom_sf(aes(fill = infec_muni), size = .15, show.legend = TRUE)  +
  scale_fill_stepsn(breaks = breaks,
                    colours = cols,
                    values = scales::rescale(breaks))

ggplot(muni) +
  ggtitle("Extra-Amazon malaria per municipality of residence")+
  geom_sf( aes(fill = resi_muni), size = .15, show.legend = TRUE)  +
  scale_fill_stepsn(breaks = breaks,
                    colours = cols,
                    values = scales::rescale(breaks))
muni <- muni %>%
  left_join(as.data.frame(table(malariaData$ID_MUNICIP[malariaData$RES_EXAM == "V"], dnn = list("code_muni")), responseName = "vivax_muni")) %>%
  left_join(as.data.frame(table(malariaData$ID_MUNICIP[malariaData$RES_EXAM == "F"], dnn = list("code_muni")), responseName = "falci_muni")) %>%
  left_join(as.data.frame(table(malariaData$ID_MUNICIP[malariaData$RES_EXAM == "V+F"], dnn = list("code_muni")), responseName = "mixed_muni")) %>%
  left_join(as.data.frame(table(malariaData$ID_MUNICIP[malariaData$RES_EXAM == "other"], dnn = list("code_muni")), responseName = "other_muni"))

vivax_plot <- ggplot(muni) +
  geom_sf( aes(fill = vivax_muni), size = .15, show.legend = TRUE) +
  scale_fill_viridis_c() +
  labs(subtitle = paste0("Number of P. vivax cases in Brazil 2015"), size = 8) +
  theme_minimal()
vivax_plot

falci_plot <-  ggplot(muni) +
  geom_sf( aes(fill = falci_muni), size = .15, show.legend = TRUE) +
  scale_fill_viridis_c() +
  labs(subtitle=paste0("Number of P. falciparum cases in Brazil 2015"), size = 8) +
  theme_minimal()
falci_plot

mixed_plot <-  ggplot(muni) +
  geom_sf( aes(fill = mixed_muni), size = .15, show.legend = TRUE) +
  scale_fill_viridis_c() +
  labs(subtitle=paste0("Number of mixed cases in Brazil 2015"), size = 8) +
  theme_minimal()
mixed_plot
get_br_pop_data(2015, save=F)

muni <-  left_join(muni, pop_2015[c("code_muni","pop")], by="code_muni")
muni$pop <- as.numeric(muni$pop)
muni$noti_inc <- muni$noti_muni / muni$pop*1000
muni$infec_inc <- muni$infec_muni / muni$pop*1000
muni$resi_inc <- muni$resi_muni / muni$pop*1000

breaks2 <- c(0, 5, 25, 50, 100, 250)
noti_inci_plot <- ggplot(muni) +
  ggtitle("Incidence of malaria notifications in 2015")+
  geom_sf( aes(fill = noti_inc), size = .15, show.legend = TRUE)  +
  scale_fill_stepsn(breaks = breaks2,
                    colours = cols,
                    values = scales::rescale(breaks2))
noti_inci_plot

infe_inci_plot <- ggplot(muni) +
  ggtitle("Incidence of probable malaria infections in 2015")+
  geom_sf(aes(fill = infec_inc), size = .15, show.legend = TRUE)  +
  scale_fill_stepsn(breaks = breaks2,
                    colours = cols,
                    values = scales::rescale(breaks2))
infe_inci_plot

resi_inci_plot <- ggplot(muni) +
  ggtitle("Incidence of residents with malaria in 2015")+
  geom_sf(aes(fill = resi_inc), size = .15, show.legend = TRUE)  +
  scale_fill_stepsn(breaks = breaks2,
                    colours = cols,
                    values = scales::rescale(breaks2))
resi_inci_plot
muni$vivax_inc <- muni$vivax_muni / muni$pop*1000
muni$falci_inc <- muni$falci_muni / muni$pop*1000
muni$mixed_inc <- muni$mixed_muni / muni$pop*1000

vivax_inci_plot <- ggplot(muni) +
  geom_sf(aes(fill = vivax_inc), size = .15, show.legend = TRUE) +
  scale_fill_viridis_c() +
  labs(subtitle = paste0("Vivax incidence in Brazil 2015"), size = 8) +
  theme_minimal()
vivax_inci_plot

falci_inci_plot<- ggplot(muni) +
  geom_sf( aes(fill = falci_inc), size = .15, show.legend = TRUE) +
  scale_fill_viridis_c()+
  labs(subtitle = paste0("Falciparum incidence in Brazil in 2015"), size = 8) +
  theme_minimal()
falci_inci_plot

mixed_inci_plot<- ggplot(muni) +
  geom_sf( aes(fill = mixed_inc), size = .15, show.legend = TRUE) +
  scale_fill_viridis_c()+
  labs(subtitle = paste0("Mixed incidence in Brazil in 2015"), size = 8) +
  theme_minimal()
mixed_inci_plot