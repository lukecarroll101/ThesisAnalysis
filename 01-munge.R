# Data manipulations
# Highlight and run command below do load project without munging.
# This is useful when debugging data manipulation code.
# rm(list = ls()); library(ProjectTemplate); load.project(list(munging=FALSE)) 

meta.pwb <- meta.pwb[!is.na(meta.pwb$ryff42itemnumber), ]

# Variable sets
v <- list()

# Item Lists
v$demographic_items <- c("gender", "age_estimate",
    "maritalstatus", "children_any", "children_number", "children_youngest", 
    "born_australia", "reside_australia", "reside_state", "english_first_language", 
    "education", "employed", "number_of_jobs", "time_since_last_job", 
    "employment_status", "org_size", "industry", "income", "jobtype", 
    "supervisor", "subordinate_count")
v$wfh_demographics <- c("wfhprecovid", "wfhcovid", "wfhnextyear", "wfhjobsuited")

v$cwb_items <- meta.cwb$id
v$ocb_items <- meta.ocb$id
v$eng_items <- meta.eng$id
v$commit_items <- meta.commit$id
v$workcriteria_items <- c(v$cwb_items, v$ocb_items, v$eng_items, v$commit_items)

v$swl_items <- meta.swl$id
v$panas_items <- meta.panas$id
v$pwb_items <- meta.pwb$id
v$wellbeing_items <- c(v$swl_items, v$panas_items, v$pwb_items)

v$criteria_items <- c(v$workcriteria_items, v$wellbeing_items)    

v$wfh_allitems <- meta.wfh$id
# reduced
meta.fwfh <- meta.wfh[!is.na(meta.wfh$scale6), ]
meta.fwfh$subscale <- paste0("wfh_", meta.fwfh$scale6)
meta.fwfh$overall <- "wfh_overall"

v$fwfh_items <- meta.fwfh$id

v$survey_items <- c(v$criteria_items, v$wfh_allitems)

v$hexaco_items <- meta.hexaco$id
v$spv_items <- meta.spv$id

v$psychometric_items <- c(v$hexaco_items, v$spv_items)


v$workcriteria_scales <- c("cwb", 
                           "ocbtotal", "ocbinterpersonal", "ocborganisational",
                           "engagement", 
                           "affective", "normative", "continuance")
v$swb_scales <- c("swl", "pa", "na")

v$pwb_scales <- unique(meta.pwb$subscale)
v$wellbeing_scales <- c(v$swb_scales, v$pwb_scales)

dput(unique(meta.fwfh$subscale))
v$wfh_subscales <- c("wfh_generalpreference", 
                    "wfh_flexibility",  "wfh_socialconnection",  
                    "wfh_careerdevelopment", "wfh_homedistractions" )
v$wfh_scales <-  v$wfh_subscales # c(unique(meta.fwfh$overall), v$wfh_subscales)

v$hexaco_factors <- c("honestyhumility", "emotionality", "extraversion",  
               "agreeableness", "conscientiousness", "openness")

v$hexaco_facets <- c("sincerity", "fairness", "greedavoidance",  "modesty", 
              "fearfulness", "anxiety", "dependence",  "sentimentality", 
              "socialselfesteem", "socialboldness",  "sociability", "liveliness", 
              "forgiveness",  "gentleness", "flexibility", "patience", 
             "organization", "diligence", "perfectionism",  "prudence", 
              "aestheticappreciation", "inquisitiveness",  "creativity", "unconventionality", 
             "altruism")
v$spv_basic <- c("selfdirection", "stimulation",  "hedonism",  "achievement",
                 "power", "security","conformity","tradition",  "benevolence",
                 "universalism") 
v$spv_ipsbasic <- paste0("ips_", v$spv_basic) # ipsative version
v$ability_subscales <- c("verbal", "abstract", "numeric")
v$intelligence <- "intelligence"


# There is almost no missing data; so tidy up with median
sapply(rcases[,v$survey_items], function(X) sum(is.na(X)))
rcases[,v$survey_items] <- sapply(rcases[,v$survey_items], function(X) ifelse(is.na(X), median(X, na.rm = TRUE), X))


# Clean data

# Too quick: suggests skipping
# hist(rcases$duration[rcases$duration < 100], 100); abline(v = 15)
# 320 * 2.5 / 60 + 1 # 
rcases$problematic_tooquick <- rcases$duration < 15

# almost no one did the survey in 2022; so remove the 1 case to make date of completion clear
# table(substr(rcases$sureydate, 1, 4))
rcases$problematic_toolate <- substr(rcases$startdate, 1, 4) == "2022"

rcases$wfh_sd <- apply(rcases[,v$wfh_allitems], 1, sd)
rcases$commit_sd <- apply(rcases[,v$commit_items], 1, sd)
rcases$pwb_sd <- apply(rcases[,v$pwb_items], 1, sd)
rcases$panas_sd <- apply(rcases[,v$panas_items], 1, sd)
rcases$composite_sd <- rcases$wfh_sd + rcases$commit_sd + rcases$pwb_sd + rcases$panas_sd
# hist(rcases$composite_sd, 50)
# hist(rcases$wfh_sd, 50)
# hist(rcases$commit_sd, 50)
# hist(rcases$pwb_sd, 50)
# hist(rcases$panas_sd, 50)
rcases$problematic_lowsd <- rcases$composite_sd < 2.5 | rcases$wfh_sd < 0.3 | 
    rcases$commit_sd < 0.3 | rcases$pwb_sd < 0.3 | rcases$panas_sd < 0.3

#
rcases$notemployedcount <- (rcases$employed == "No") +  
        (rcases$wfhprecovid == "Not applicable (not working)") + 
    (rcases$wfhcovid == "Not applicable (not working)") + 
    (rcases$wfhnextyear == "Not applicable (not working)")
rcases$problematic_notemployed <- rcases$notemployedcount %in% 4
rcases$problematic_unsuitablejob <- rcases$wfhjobsuited %in% c("Unsuited", "Extremely unsuited")


v$problematic <- c("problematic_tooquick", "problematic_lowsd", "problematic_notemployed", "problematic_unsuitablejob",
                   "problematic_toolate")
rcases$valid <- apply(rcases[,v$problematic], 1, function(X) !any(X))
ccases <- rcases[rcases$valid, ]


# Score tests
scored <- list()
scored$cwb <- score_test(meta.cwb, ccases, subscale_name = "subscale", reverse="reverse")
scored$ocboverall <- score_test(meta.ocb, ccases, subscale_name = "overall", reverse="reverse")
scored$ocbsubscales <- score_test(meta.ocb, ccases, subscale_name = "subscale", reverse="reverse")
scored$engagement <- score_test(meta.eng, ccases, subscale_name = "subscale", reverse="reverse")
scored$commit <- score_test(meta.commit, ccases, subscale_name = "subscale", reverse="reverse")
scored$swl <- score_test(meta.swl, ccases, subscale_name = "subscale", reverse="reverse")
scored$panas <- score_test(meta.panas, ccases, subscale_name = "subscale", reverse="reverse")
scored$pwb <- score_test(meta.pwb, ccases, subscale_name = "subscale", reverse="reverse")
scored$hexacofacets <- score_test(meta.hexaco, ccases, subscale_name = "facet", reverse="reversed")
scored$hexacofactors <- score_test(meta.hexaco, ccases, subscale_name = "factor", reverse="reversed")

scored$wfh_subscales <- score_test(meta.fwfh, ccases, subscale_name = "subscale", reverse="reverse")
scored$wfh_overall <- score_test(meta.fwfh, ccases, subscale_name = "overall", reverse="reverse")

scores <- lapply(scored, function(X) X$scores)
names(scores) <- NULL
scores <- data.frame(do.call(cbind, scores))
ccases[,names(scores)] <- scores

# spv basic
scored$spvbasic <- score_test(meta.spv, ccases, subscale_name = "basic", reverse="reversed")
ccases <- scored$spvbasic$data

# ipsatise spv
ccases$spv_mean <- apply(ccases[,v$spv_basic], 1, mean)
ccases[,v$spv_ipsbasic] <- sapply(ccases[,v$spv_basic], function(X) X - ccases$spv_mean)


# demographic coding

ccases$isfemale <- as.numeric(ccases$gender == "Female")

ccases$livewithparnter <- as.numeric(ccases$maritalstatus %in% c("Married", "De facto relationship", "Registered domestic relationship"))

ccases$issupervisor <- as.numeric(ccases$supervisor %in% "Yes")

ccases$income_estimate <- as.numeric(as.character(factor(ccases$income, c("Negative or Zero Income", 
"$1 ‐ $9,999 per year ($1 ‐ $189 per week)", 
"$10,000 ‐ $19,999 per year ($190 ‐ $379 per week)",
"$20,000 ‐ $29,999 per year ($380 ‐ $579 per week)", 
"$30,000 ‐ $39,999 per year ($580 ‐ $769 per week)", 
"$40,000 ‐ $49,999 per year ($770 ‐ $959 per week)", 
"$50,000 ‐ $59,999 per year ($960 ‐ $1149 per week)", 
"$60,000 ‐ $79,999 per year ($1150 ‐ $1529 per week)", 
"$80,000 ‐ $99,999 per year ($1530 ‐ $1919 per week)", 
"$100,000 ‐ $124,999 per year ($1920 ‐ $2399 per week)", 
"$125,000 ‐ $149,999 per year ($2400 ‐ $2879 per week)", 
"$150,000 ‐ $199,999 per year ($2880 ‐ $3839 per week)", 
"$200,000 or more per year ($3840 or more per week)"),
c(0, 5000, 15000, 25000, 35000, 45000, 55000, 70000, 90000, 112500, 137500, 175000, 250000))))

ccases$education_numeric <- as.numeric(as.character(factor(ccases$education,
    c("Did not complete High school",
      "Completed High school",
      "Apprenticeship, Diploma, TAFE certificate, etc", 
      "Bachelor's degree",
    "Master's degree",  
    "Doctorate, PhD"),
    c(1,2,3,4,5,6))))

ccases$dependentagechildren <- as.numeric(ccases$children_youngest %in%  
    c("0 - 4 years", "5 - 9 years", "15 - 18 years", "10 - 14 years"))
     
ccases$isenglish_first_language <- as.numeric(ccases$english_first_language %in% "Yes")

ccases$isfulltime <- as.numeric(ccases$employment_status %in% "Working full-time")

ccases$reside_victoria <- ccases$reside_state %in% "Victoria"


# dput(names(ccases))
v$demographic_numeric <- c("isfemale", "age_estimate", "livewithparnter", 
     "dependentagechildren", "reside_victoria", "isenglish_first_language", 
     "education_numeric", "income_estimate", 
    "isfulltime",  "issupervisor")

dput(unique(ccases$age))
ccases$age_estimate <- as.numeric(as.character(factor(ccases$age,
      c("18 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79",  "80+"),
      c((18+29)/2, (30+39)/2, (40+49)/2, (50+59)/2, (60+69)/2, (70+79)/2, 85))))


ccases[,v$intelligence] <- as.numeric(scale(apply(sapply(ccases[,v$ability_subscales], function(X) as.numeric(scale(X))), 1, sum)))

