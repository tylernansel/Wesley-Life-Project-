#Clean CPS
# read in cps data as cps
cps <- read.csv('Data/cps_00006.csv', stringsAsFactors = TRUE)

#each row of csp is an INDIVIDUAL within a family
cps = cps %>% 
  mutate(SEX = SEX - 1,
         CHILD = ifelse(AGE < 18, 1, 0),
         ELDERLY = ifelse(AGE > 64, 1, 0),
         BLACK = ifelse(RACE == 200, 1, 0),
         HISPANIC = ifelse(HISPAN>0, 1, 0),
         EDUC = as.integer(EDUC %in%  c(91,92,111,123,124,125)),
         EMP = as.integer(EMPSTAT %in%  c(1,10,12)),
         MARRIED = as.integer(MARST %in% c(1,2)),
         DIFF = ifelse(DIFFANY == 2,1,0),
         COUNTY = as.factor(COUNTY))

#currently one row of CPS = one individual
#however, we want to make prediction on the family level

#aggregate to the family level - this is where we choose FAMILY-LEVEL traits
#that we want to calculate. For example, househild size is equal to the
#number of rows for that faily
cps_data = cps %>% 
  group_by(CPSID = as.factor(CPSID)) %>% 
  summarise(COUNTY = first(COUNTY),
            #family level weight
            weight = first(HWTFINL),
            
            #household size
            hhsize = n(),
            
            #Y variables - i.e., measures of hunger
            #see CPS website for details
            #FSSTATUS, etc. is the same for each member - 
            #just take first value for each family
            FSTOTXPNC_perpers=FSTOTXPNC/hhsize, #Inperpersonterms
            FSSTATUS= first(FSSTATUS),
            FSSTATUSMD= first(FSSTATUSMD),
            FSFOODS= first(FSFOODS),
            FSWROUTY= first(FSWROUTY),
            FSBAL= first(FSBAL),
            FSRAWSCRA= first(FSRAWSCRA),
            FSTOTXPNC= first(FSTOTXPNC),
            FSSTATUS=first(FSSTATUS),
            #count of familymembers invarious categories
            female= sum(SEX),
            hispanic=sum(HISPANIC),
            black= sum(BLACK),
            kids= sum(CHILD),
            elderly= sum(ELDERLY),
            education= sum(EDUC),
            FamInc = first(FAMINC),
            married= sum(MARRIED))%>%ungroup()



#each row of cps_data is a Fthat AMILY
#note: we just calculated the number of people in each family that belong
#to the above groups. Perhaps isn't the best way? Would proportions be good
#in addition or instead of sums?!

#Summary(cps_data) #see extremes for food secruity variable
cps_data<-cps_data %>%
  mutate(FSSTATUS=ifelse(FSSTATUS%in%c(98,99),NA,FSSTATUS),
         FSSTATUSMD= ifelse(FSSTATUSMD%in%c(98,99),NA,FSSTATUSMD),
         FSFOODS= ifelse(FSFOODS%in%c(98,99),NA,FSFOODS),
         FSWROUTY=ifelse(FSWROUTY %in%c(96,97,98,99),NA,FSWROUTY),
         FSBAL= ifelse(FSBAL %in%c(96,97,98,99),NA,FSBAL),
         FSRAWSCRA=ifelse(FSRAWSCRA%in%c(98,99),NA,FSRAWSCRA),#rawscore
         FSTOTXPNC=ifelse(FSTOTXPNC%in%c(999),NA,FSTOTXPNC)) %>%
  mutate(FSSTATUS=ifelse(FSSTATUS> 1,1,0),
         FSSTATUSMD= ifelse(FSSTATUSMD > 1,1,0),
         FSFOODS= ifelse(FSFOODS >1,1,0),
         FSWROUTY=ifelse(FSWROUTY> 1,1,0),#moremissings
         FSBAL = ifelse(FSBAL > 1, 1, 0),
         FSRAWSCRA=ifelse(FSRAWSCRA > 1, 1, 0))
str(cps_data)
summary(cps_data)
#Note: many of our y variables contain some NA values.
#Do not use complete.cases or na.omit on the whole dataset


cps_data <- cps_data %>%
  mutate(
    FamInc = case_when(
      FamInc == 100 ~ "Under $5,000",
      FamInc == 110 ~ "Under $1,000",
      FamInc == 111 ~ "Under $500",
      FamInc == 112 ~ "$500 - 999",
      FamInc == 120 ~ "$1,000 - 1,999",
      FamInc == 121 ~ "$1,000 - 1,499",
      FamInc == 122 ~ "$1,500 - 1,999",
      FamInc == 130 ~ "$2,000 - 2,999",
      FamInc == 131 ~ "$2,000 - 2,499",
      FamInc == 132 ~ "$2,500 - 2,999",
      FamInc == 140 ~ "$3,000 - 3,999",
      FamInc == 141 ~ "$3,000 - 3,499",
      FamInc == 142 ~ "$3,500 - 3,999",
      FamInc == 150 ~ "$4,000 - 4,999",
      FamInc == 200 ~ "$5,000 - 7,999",
      FamInc == 210 ~ "$5,000 - 7,499",
      FamInc == 220 ~ "$5,000 - 5,999",
      FamInc == 230 ~ "$6,000 - 7,999",
      FamInc == 231 ~ "$6,000 - 7,499",
      FamInc == 232 ~ "$6,000 - 6,999",
      FamInc == 233 ~ "$7,000 - 7,499",
      FamInc == 234 ~ "$7,000 - 7,999",
      FamInc == 300 ~ "$7,500 - 9,999",
      FamInc == 310 ~ "$7,500 - 7,999",
      FamInc == 320 ~ "$8,000 - 8,499",
      FamInc == 330 ~ "$8,500 - 8,999",
      FamInc == 340 ~ "$8,000 - 8,999",
      FamInc == 350 ~ "$9,000 - 9,999",
      FamInc == 400 ~ "$10,000 - 14,999",
      FamInc == 410 ~ "$10,000 - 10,999",
      FamInc == 420 ~ "$11,000 - 11,999",
      FamInc == 430 ~ "$10,000 - 12,499",
      FamInc == 440 ~ "$10,000 - 11,999",
      FamInc == 450 ~ "$12,000 - 12,999",
      FamInc == 460 ~ "$12,000 - 14,999",
      FamInc == 470 ~ "$12,500 - 14,999",
      FamInc == 480 ~ "$13,000 - 13,999",
      FamInc == 490 ~ "$14,000 - 14,999",
      FamInc == 500 ~ "$15,000 - 19,999",
      FamInc == 510 ~ "$15,000 - 15,999",
      FamInc == 520 ~ "$16,000 - 16,999",
      FamInc == 530 ~ "$17,000 - 17,999",
      FamInc == 540 ~ "$15,000 - 17,499",
      FamInc == 550 ~ "$17,500 - 19,999",
      FamInc == 560 ~ "$18,000 - 19,999",
      FamInc == 600 ~ "$20,000 - 24,999",
      FamInc == 700 ~ "$25,000 - 49,999",
      FamInc == 710 ~ "$25,000 - 29,999",
      FamInc == 720 ~ "$30,000 - 34,999",
      FamInc == 730 ~ "$35,000 - 39,999",
      FamInc == 740 ~ "$40,000 - 49,999",
      FamInc == 800 ~ "$50,000 and over",
      FamInc == 810 ~ "$50,000 - 74,999",
      FamInc == 820 ~ "$50,000 - 59,999",
      FamInc == 830 ~ "$60,000 - 74,999",
      FamInc == 840 ~ "$75,000 and over",
      FamInc == 841 ~ "$75,000 - 99,999",
      FamInc == 842 ~ "$100,000 - 149,999",
      FamInc == 843 ~ "$150,000 and over",
      FamInc == 995 ~ "Missing",
      FamInc == 996 ~ "Refused",
      FamInc == 997 ~ "Don't know",
      FamInc == 999 ~ "Blank",
      TRUE ~ "Unknown"
    )
  )

cps_data <- cps_data %>%
  mutate(FamInc_numeric = case_when(
    FamInc == "Under $5,000" ~ 5000,
    FamInc == "Under $1,000" ~ 1000,
    FamInc == "Under $500" ~ 500,
    FamInc == "$500 - 999" ~ 750,
    FamInc == "$1,000 - 1,999" ~ 1500,
    FamInc == "$1,000 - 1,499" ~ 1250,
    FamInc == "$1,500 - 1,999" ~ 1750,
    FamInc == "$2,000 - 2,999" ~ 2500,
    FamInc == "$2,000 - 2,499" ~ 2250,
    FamInc == "$2,500 - 2,999" ~ 2750,
    FamInc == "$3,000 - 3,999" ~ 3500,
    FamInc == "$3,000 - 3,499" ~ 3250,
    FamInc == "$3,500 - 3,999" ~ 3750,
    FamInc == "$4,000 - 4,999" ~ 4500,
    FamInc == "$5,000 - 7,999" ~ 6000,
    FamInc == "$5,000 - 7,499" ~ 6250,
    FamInc == "$5,000 - 5,999" ~ 5500,
    FamInc == "$6,000 - 7,999" ~ 7000,
    FamInc == "$6,000 - 7,499" ~ 7250,
    FamInc == "$6,000 - 6,999" ~ 6500,
    FamInc == "$7,000 - 7,499" ~ 7500,
    FamInc == "$7,000 - 7,999" ~ 8000,
    FamInc == "$7,500 - 9,999" ~ 8750,
    FamInc == "$7,500 - 7,999" ~ 7750,
    FamInc == "$8,000 - 8,499" ~ 8250,
    FamInc == "$8,500 - 8,999" ~ 8750,
    FamInc == "$8,000 - 8,999" ~ 8500,
    FamInc == "$9,000 - 9,999" ~ 9500,
    FamInc == "$10,000 - 14,999" ~ 12500,
    FamInc == "$10,000 - 10,999" ~ 10500,
    FamInc == "$11,000 - 11,999" ~ 11500,
    FamInc == "$10,000 - 12,499" ~ 11500,
    FamInc == "$10,000 - 11,999" ~ 11000,
    FamInc == "$12,000 - 12,999" ~ 12500,
    FamInc == "$12,000 - 14,999" ~ 13000,
    FamInc == "$12,500 - 14,999" ~ 13750,
    FamInc == "$13,000 - 13,999" ~ 13500,
    FamInc == "$14,000 - 14,999" ~ 14500,
    FamInc == "$15,000 - 19,999" ~ 17500,
    FamInc == "$15,000 - 15,999" ~ 15500,
    FamInc == "$16,000 - 16,999" ~ 16500,
    FamInc == "$17,000 - 17,999" ~ 17000,
    FamInc == "$15,000 - 17,499" ~ 16250,
    FamInc == "$17,500 - 19,999" ~ 17500,
    FamInc == "$18,000 - 19,999" ~ 18500,
    FamInc == "$20,000 - 24,999" ~ 22500,
    FamInc == "$25,000 - 49,999" ~ 37500,
    FamInc == "$25,000 - 29,999" ~ 27500,
    FamInc == "$30,000 - 34,999" ~ 32500,
    FamInc == "$35,000 - 39,999" ~ 37500,
    FamInc == "$40,000 - 49,999" ~ 45000,
    FamInc == "$50,000 and over" ~ 50000,
    FamInc == "$50,000 - 74,999" ~ 62500,
    FamInc == "$50,000 - 59,999" ~ 55000,
    FamInc == "$60,000 - 74,999" ~ 67500,
    FamInc == "$75,000 and over" ~ 75000,
    FamInc == "$75,000 - 99,999" ~ 87500,
    FamInc == "$100,000 - 149,999" ~ 125000,
    FamInc == "$150,000 and over" ~ 150000,
    TRUE ~ NA_real_
  ))

# Replace FamInc with FamInc_numeric and drop the original FamInc column
cps_data <- cps_data %>%
  select(-FamInc) %>%       # Remove the original FamInc column
  rename(FamInc = FamInc_numeric)  # Rename FamInc_numeric to FamInc



