# dat = rio::import('data/LLCP2017.XPT')
# diabete = dat[dat$DIABETE3 == '1',]
# dage = diabete[diabete$`_AGE65YR` == '2',]
# data.table::fwrite(dage, 'old_diabetes_brfss2017.csv')

pacman::p_load(tidyverse, survey,data.table,Hmisc)
dat = rio::import('data/old_diabetes_brfss2017.csv') %>%
  filter(!is.na(`_LLCPWT`)) %>%
  as.data.table()

##########################################
###### OUTCOME: self-care behaviors ######
##########################################
# Blood glucose check: BLDSUGAR
# Feet Check: FEETCHK2
# Eye Check: EYEEXAM
# Physical Activity

# Diabetes Education: DIABEDU
# Physical Activity: EXERANY2
# Feet Check: FEETCHK2
# Eye Check: EYEEXAM
# Blood Glucose Check: BLDSUGAR

# 1.Diabetes Education: DIABEDU -> Out_diab_edu
describe(dat$DIABEDU)
dat[,Out_diab_edu:=DIABEDU]
dat[Out_diab_edu%in%1, Out_diab_edu:=1]
dat[Out_diab_edu%in%2, Out_diab_edu:=0]
dat[Out_diab_edu%in%c(7, 9), Out_diab_edu:=NA]
describe(dat$Out_diab_edu)
table(dat$Out_diab_edu, exclude = NULL)

# 2. Physical Activity: EXERANY2 -> Out_physic_activity
describe(dat$EXERANY2)
dat[,Out_physic_activity:=EXERANY2]
dat[Out_physic_activity%in%1, Out_physic_activity:=1]
dat[Out_physic_activity%in%2, Out_physic_activity:=0]
dat[Out_physic_activity%in%c(7, 9), Out_physic_activity:=NA]
describe(dat$Out_physic_activity)
table(dat$Out_physic_activity, exclude = NULL)

# 3. Feet Check: FEETCHK -> Out_feet_check
describe(dat$FEETCHK2)
dat[,Out_feet_check:=FEETCHK2]
dat[Out_feet_check <= 499, Out_feet_check:=1]
dat[Out_feet_check%in%c(555, 888), Out_feet_check:=0]
dat[Out_feet_check%in%c(777, 999), Out_feet_check:=NA]
describe(dat$Out_feet_check)
table(dat$Out_feet_check, exclude = NULL)

# 4. Eye Check: EYEEXAM
describe(dat$EYEEXAM)
dat[,Out_eye_exam:=EYEEXAM]
dat[Out_eye_exam <= 4, Out_eye_exam:=1]
dat[Out_eye_exam %in% 8, Out_eye_exam:=0]
dat[Out_eye_exam %in% c(7, 9), Out_eye_exam:=NA]
describe(dat$Out_eye_exam)
table(dat$Out_eye_exam, exclude = NULL)

# 5. Blood Glucose Check: BLDSUGAR
describe(dat$BLDSUGAR)
dat[,Out_blood_glucose:=BLDSUGAR]
dat[Out_blood_glucose <= 499, Out_blood_glucose:=1]
dat[Out_blood_glucose%in%c(555, 888), Out_blood_glucose:=0]
dat[Out_blood_glucose%in%c(777, 999), Out_blood_glucose:=NA]
describe(dat$Out_blood_glucose)
table(dat$Out_blood_glucose, exclude = NULL)

# OUTCOME: N_selfcare
dat[,N_selfcare := Out_diab_edu + Out_physic_activity + Out_feet_check +
                       Out_eye_exam + Out_blood_glucose]
table(dat$N_selfcare, exclude = NULL)
histogram(dat$N_selfcare)

dat[N_selfcare%in%0,N_selfcare_cat:="None"]
dat[N_selfcare%in%1,N_selfcare_cat:="One"]
dat[N_selfcare%in%2,N_selfcare_cat:="Two"]
dat[N_selfcare%in%3,N_selfcare_cat:="Three"]
dat[N_selfcare%in%4,N_selfcare_cat:="Four"]
dat[N_selfcare%in%5,N_selfcare_cat:="Five"]
dat[,N_selfcare_cat:=factor(N_selfcare_cat, levels =
                              c("None", "One", "Two", "Three", "Four", "Five"))]
describe(dat$N_selfcare_cat)

##########################################
######     Independent variables    ######
##########################################
describe(dat$`_IMPRACE`)
dat[,race:=factor(`_IMPRACE`)]
dat[race%in%1,race:="Non-Hispanic White"]
dat[race%in%2,race:="Non-Hispanic Black"]
dat[race%in%5,race:="Hispanic"]
dat[race%in% c(3,4,6),race:="Other"]
dat[,race:=factor(race, levels =
                    c("Non-Hispanic White",
                      "Non-Hispanic Black",
                      "Hispanic", "Other"))]
describe(dat$race)


describe(dat$SEX)
dat[,gender:=factor(SEX)]
dat[gender%in%1,gender:="Male"]
dat[gender%in%2,gender:="Female"]
dat[gender%in%9,gender:=NA]
dat[,gender:=factor(gender, levels = c("Female", "Male"))]
describe(dat$gender)


describe(dat$`_AGEG5YR`)
dat[,age:=as.factor(`_AGEG5YR`)]
dat[age%in%10,age:="65-69yr"]
dat[age%in%11,age:="70-74yr"]
dat[age%in%12,age:="75-79yr"]
dat[age%in%13,age:=">80yr"]
dat[,age:=factor(age, levels =
                   c("65-69yr", "70-74yr", "75-79yr", ">80yr"))]
describe(dat$age)


describe(dat$MARITAL)
dat[,marital:= factor(MARITAL)]
dat[marital%in%1,marital:="Married"]
dat[marital%in% c(2,3,4,6),
    marital:="Divorced/Widowed/Separated"]
dat[marital%in%5,marital:="Never Married"]
dat[marital%in%9,marital:=NA]
dat[,marital:=factor(marital, levels =
                       c("Married", "Divorced/Widowed/Separated",
                         "Never Married"))]
describe(dat$marital)


describe(dat$`_EDUCAG`)
dat[,education:=factor(`_EDUCAG`)]
dat[education%in%1,education:="Less than High School"]
dat[education%in%2,education:="High School Graduate"]
dat[education%in%3,education:="Some College"]
dat[education%in%4,education:="College Graduate"]
dat[education%in%9,education:=NA]
dat[,education:=factor(education,
                       levels = c("Less than High School", "High School Graduate",
                                  "Some College", "College Graduate"))]
describe(dat$education)


describe(dat$`_INCOMG`)
dat[,income:=factor(`_INCOMG`)]
dat[income%in%1,income:="Less than 15000"]
dat[income%in%2,income:= "15000 to 25000"]
dat[income%in%3,income:= "25000 to 35000"]
dat[income%in%4,income:= "35000 to 50000"]
dat[income%in%5,income:= "More than 50000"]
dat[income%in%9,income:= NA]
dat[,income:=factor(income, levels =
                      c("Less than 15000", "15000 to 25000",
                        "25000 to 35000", "35000 to 50000", "More than 50000"))]
describe(dat$income)


describe(dat$`_BMI5CAT`)
dat[,bmi:=as.factor(`_BMI5CAT`)]
dat[bmi%in%c(1, 2),bmi:="Under/Normal Weight"]
dat[bmi%in%3,bmi:= "Overweight"]
dat[bmi%in%4,bmi:= "Obese"]
dat[,bmi:=factor(bmi, levels =
                   c("Under/Normal Weight", "Overweight", "Obese"))]
describe(dat$bmi)


describe(dat$HLTHPLN1)
dat[,insurance:=as.factor(HLTHPLN1)]
dat[insurance%in%1,insurance:= "Yes"]
dat[insurance%in%2,insurance:= "No"]
dat[insurance%in%c(7, 9),insurance:= NA]
dat[,insurance:=factor(insurance, levels =
                   c("No", "Yes"))]
describe(dat$insurance)


dat[,provider:=as.factor(PERSDOC2)]
dat[provider%in%c(1,2),provider:= "Yes"]
dat[provider%in%3,provider:= "No"]
dat[provider%in%c(7,9),provider:= NA]
dat[,provider:=factor(provider, levels =
                         c("No", "Yes"))]
describe(dat$provider)


dat[,health:=as.factor(GENHLTH)]
dat[health%in%c(1,2),health:= "Excellent/Very good"]
dat[health%in%3,health:= "Good"]
dat[health%in%c(4,5),health:= "Fair Poor"]
dat[health%in%c(6,7),health:= NA]
dat[,health:=factor(health, levels =
                      c("Fair Poor", "Good", "Excellent/Very good"))]
describe(dat$health)


dat[,insulin:=as.factor(INSULIN)]
dat[insulin%in%1,insulin:= "Yes"]
dat[insulin%in%2,insulin:= "No"]
dat[insulin%in%9,insulin:= NA]
dat[,insulin:=factor(insulin, levels =
                      c("No", "Yes"))]
describe(dat$insulin)


dat[,smoking:=as.factor(`_SMOKER3`)]
dat[smoking%in%c(1,2),smoking:= "Yes"]
dat[smoking%in%c(3, 4),smoking:= "No"]
dat[smoking%in%9,smoking:= NA]
dat[,smoking:=factor(smoking, levels =
                       c("No", "Yes"))]
describe(dat$smoking)


describe(dat$CVDCRHD4)
dat[,chronic_coronary_heart:=as.factor(CVDCRHD4)]
dat[chronic_coronary_heart%in%1,chronic_coronary_heart:= "Yes"]
dat[chronic_coronary_heart%in%2,chronic_coronary_heart:= "No"]
dat[chronic_coronary_heart%in%c(7,9),chronic_coronary_heart:= NA]
dat[,chronic_coronary_heart:=factor(chronic_coronary_heart, levels =
                                      c("No", "Yes"))]
describe(dat$chronic_coronary_heart)


describe(dat$CHCKIDNY)
dat[,chronic_kidney:=as.factor(CHCKIDNY)]
dat[chronic_kidney%in%1,chronic_kidney:= "Yes"]
dat[chronic_kidney%in%2,chronic_kidney:= "No"]
dat[chronic_kidney%in%c(7,9),chronic_kidney:= NA]
dat[,chronic_kidney:=factor(chronic_kidney, levels =
                       c("No", "Yes"))]
describe(dat$chronic_kidney)


describe(dat$CVDINFR4)
dat[,chronic_heart_attack:=as.factor(CVDINFR4)]
dat[chronic_heart_attack%in%1,chronic_heart_attack:= "Yes"]
dat[chronic_heart_attack%in%2,chronic_heart_attack:= "No"]
dat[chronic_heart_attack%in%c(7,9),chronic_heart_attack:= NA]
dat[,chronic_heart_attack:=factor(chronic_heart_attack, levels =
                                    c("No", "Yes"))]
describe(dat$chronic_heart_attack)



dat[,nofeet:=factor(FEETCHK2)]
dat[!(nofeet%in%555),nofeet:="Has feet"]
dat[nofeet%in%555,nofeet:="No feet"]
dat[,nofeet:=factor(nofeet, levels =
                      c("Has feet", "No feet"))]
describe(dat$nofeet)

describe(dat$TOLDHI2)
dat[,cholesterol:=factor(TOLDHI2)]
dat[cholesterol%in%1,cholesterol:="Yes"]
dat[cholesterol%in%2,cholesterol:="No"]
dat[cholesterol%in%c(7,9),cholesterol:=NA]
dat[,cholesterol:=factor(cholesterol, levels =
                      c("No", "Yes"))]
describe(dat$cholesterol)

describe(dat$BPHIGH4)
dat[,bldpressure:=factor(BPHIGH4)]
dat[bldpressure%in%1,bldpressure:="Yes"]
dat[bldpressure%in%3,bldpressure:="No"]
dat[bldpressure%in%c(2,4,7,9),bldpressure:=NA]
dat[,bldpressure:=factor(bldpressure, levels =
                           c("No", "Yes"))]
describe(dat$bldpressure)

describe(dat$CVDCRHD4)
dat[,cvd:=factor(CVDCRHD4)]
dat[cvd%in%1,cvd:="Yes"]
dat[cvd%in%2,cvd:="No"]
dat[cvd%in%c(7,9),cvd:=NA]
dat[,cvd:=factor(cvd, levels =
                           c("No", "Yes"))]
describe(dat$cvd)

dat_model = dat[,.(`_STSTR`, `_LLCPWT`, N_selfcare_cat, N_selfcare, Out_diab_edu,
                   Out_physic_activity, Out_feet_check, Out_eye_exam, Out_blood_glucose,
                   race, gender, age, marital, education, income, bmi,
                   insurance, provider, health, insulin, smoking,
                   chronic_coronary_heart, chronic_kidney,
                   chronic_heart_attack, nofeet,
                   cholesterol,bldpressure,cvd)]


##########################################
######            TABLE 1           ######
##########################################
# Survey analysis in R
# https://www.cdc.gov/brfss/annual_data/2017/pdf/Complex-Smple-Weights-Prep-Module-Data-Analysis-2017-508.pdf
options(survey.lonely.psu = "adjust")

brfssdsgn <- svydesign(
  id=~1,
  strata = ~ `_STSTR`,
  weights = ~ `_LLCPWT`,
  data = dat_model)

##### Dependent variables: #####
tab1_1 = svymean(~N_selfcare_cat + factor(Out_diab_edu) + factor(Out_physic_activity) +
                                                                      factor(Out_feet_check) + factor(Out_eye_exam) + factor(Out_blood_glucose),
        brfssdsgn,
        na.rm = TRUE) %>%
  as.data.frame() %>%
  round(3)%>%
  tibble::rownames_to_column(var="var_name") %>%
  filter(!grepl("0", var_name))

##### Independent variable: #####
tab1_2 = svymean(~race + gender + age + marital + education + income + bmi +
          insurance + provider + health + insulin + smoking + #nofeet+
          chronic_coronary_heart + chronic_kidney +
          chronic_heart_attack +cholesterol+ bldpressure,
        brfssdsgn,
        na.rm = TRUE) %>%
  as.data.frame() %>%
  round(3) %>%
  tibble::rownames_to_column(var="var_name")


tab1_1
tab1_2
readr::write_csv(tab1_1, "data/tab1_1.csv")
readr::write_csv(tab1_2, "data/tab1_2.csv")


##########################################
######            TABLE 2           ######
##########################################
# svyglm(y ~ x, design = brfssdsgn, family=quasibinomial())
poi_fit = svyglm(N_selfcare ~ race + gender + age + marital + education + income + bmi +
                    insurance + provider + health + insulin + smoking + #nofeet+
                    chronic_coronary_heart + chronic_kidney +
                    chronic_heart_attack +cholesterol+ bldpressure,
                 design = brfssdsgn, family=quasipoisson())
# cvd deleted because it is the same as chronic_coronary_heart

tab2 = summary(poi_fit)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var="var_name") %>%
  mutate(IRR = round(exp(Estimate), 3),
         CI_left = round(exp(Estimate - 1.96*`Std. Error`), 3),
         CI_right = round(exp(Estimate + 1.96*`Std. Error`), 3))
tab2
readr::write_csv(tab2, "data/tab2_Miao.csv")

tab2mat<-
  print(tab2,smd = T,test = F,showAllLevels = T,explain = F)
write.csv(tab2mat,file ='data/tab2mat.csv')


# Ordinal logistics
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)),
    'Y>=5' = qlogis(mean(y >= 5)))
}

(s <-
    with(dat, summary(as.numeric(N_selfcare) ~ race + gender + age + marital + education + income + bmi +
                          insurance + provider + health + insulin + smoking +
                          chronic_coronary_heart + chronic_kidney +
                          chronic_heart_attack + nofeet+cholesterol+ bldpressure + cvd, fun=sf)))
as.numeric(N_selfcare)     N= 18447 , 14499 Missing

#+----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |                      |                          |N    |Y>=1    |Y>=2    |Y>=3     |Y>=4       |Y>=5      |
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |race                  |Non-Hispanic White        |14289|7.263960|3.923652|2.1453569| 0.66088126|-0.9212929|
#  |                      |Non-Hispanic Black        | 2331|     Inf|3.927896|2.5944252| 0.80938151|-0.8725533|
#  |                      |Hispanic                  |  906|6.807935|3.790985|1.9999774| 0.36606468|-1.1736099|
#  |                      |Other                     |  921|5.723585|3.579065|1.9671742| 0.58272336|-0.9117349|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |gender                |Female                    |10317|7.294959|4.025352|2.2418469| 0.66881931|-0.9352299|
#  |                      |Male                      | 8124|7.055806|3.753670|2.0978521| 0.64860578|-0.9145681|
#  |                      |Missing                   |    6|     Inf|     Inf|1.6094379| 1.60943791|-1.6094379|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |age                   |65-69yr                   | 5858|7.576439|4.160271|2.3761192| 0.81531064|-0.7167987|
#  |                      |70-74yr                   | 5279|7.184440|4.010230|2.3185250| 0.84252157|-0.8326373|
#  |                      |75-79yr                   | 3657|6.817010|3.852664|2.0999074| 0.59383207|-1.0301390|
#  |                      |>80yr                     | 3653|7.103870|3.500747|1.8185223| 0.25152300|-1.3551054|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |marital               |Married                   | 8493|7.660232|4.220225|2.3418058| 0.81769951|-0.7759160|
#  |                      |Divorced/Widowed/Separated| 8837|6.888459|3.686678|2.0487473| 0.52818476|-1.0849289|
#  |                      |Never Married             | 1057|6.962243|3.604138|2.0272921| 0.55701501|-0.9349083|
#  |                      |Missing                   |   60|     Inf|     Inf|2.9444390| 0.61903921|-0.2682640|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |education             |Less than High School     | 2020|6.222576|3.419774|1.8604255| 0.13883644|-1.5661056|
#  |                      |High School Graduate      | 5960|6.745740|3.742145|2.0781209| 0.50867855|-1.1457011|
#  |                      |Some College              | 5134|7.850104|4.108501|2.3353749| 0.82992059|-0.7806492|
#  |                      |College Graduate          | 5291|8.573574|4.176500|2.2957436| 0.90125682|-0.6439316|
#  |                      |Missing                   |   42|     Inf|2.995732|1.4469190| 0.09531018|-1.0360919|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |income                |Less than 15000           | 1915|7.556951|3.773563|2.0888756| 0.39676940|-1.2363883|
#  |                      |15000 to 25000            | 3699|6.828441|3.680049|2.1240034| 0.57611518|-1.1302225|
#  |                      |25000 to 35000            | 2227|     Inf|4.166665|2.2570125| 0.66765955|-0.9748689|
#  |                      |35000 to 50000            | 2520|6.444131|4.152855|2.2745358| 0.73631935|-0.8228514|
#  |                      |More than 50000           | 4773|7.777164|4.084593|2.2872407| 0.88540250|-0.6269225|
#  |                      |Missing                   | 3313|7.006091|3.686407|2.0200444| 0.54104526|-1.0512779|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |bmi                   |Under/Normal Weight       | 2930|7.288928|3.681211|2.0041171| 0.54703364|-1.0017337|
#  |                      |Overweight                | 6298|7.648899|3.941258|2.2039467| 0.72860849|-0.8026295|
#  |                      |Obese                     | 8314|6.827388|3.918422|2.2261572| 0.65620556|-0.9999871|
#  |                      |Missing                   |  905|     Inf|4.228517|2.1314507| 0.59845986|-0.9031783|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |insurance             |No                        |  284|5.645447|3.211577|1.6698011| 0.37037379|-1.2550280|
#  |                      |Yes                       |18127|7.239491|3.913262|2.1878813| 0.66653606|-0.9201169|
#  |                      |Missing                   |   36|     Inf|3.555348|1.4213857|-0.11122564|-1.8245493|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |provider              |No                        |  670|6.505784|3.170308|1.5740360| 0.19765592|-1.4922644|
#  |                      |Yes                       |17717|7.216597|3.936960|2.2083747| 0.68140172|-0.9058046|
#  |                      |Missing                   |   60|     Inf|4.077537|1.4939250|-0.06669137|-1.6094379|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |health                |Fair Poor                 | 7349|8.208900|3.824479|2.1466991| 0.52820851|-1.1993204|
#  |                      |Good                      | 6994|6.772222|3.958703|2.2464813| 0.75908607|-0.7610618|
#  |                      |Excellent/Very good       | 4038|6.916219|3.967268|2.1297291| 0.75167851|-0.7528160|
#  |                      |Missing                   |   66|     Inf|2.740840|1.4053426|-0.12136086|-1.6094379|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |insulin               |No                        |12554|6.871809|3.578374|1.9037930| 0.43629619|-1.0988247|
#  |                      |Yes                       | 5871|8.677610|5.538361|3.1296447| 1.21440923|-0.5928245|
#  |                      |Missing                   |   22|     Inf|1.845827|0.9808293|-0.18232156|-1.2237754|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |smoking               |No                        |16793|7.243096|4.000143|2.2208022| 0.70245124|-0.8956646|
#  |                      |Yes                       | 1540|6.645091|3.154634|1.8077598| 0.26650125|-1.2460963|
#  |                      |Missing                   |  114|     Inf|4.025352|1.6094379| 0.10536052|-1.5475625|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |chronic_coronary_heart|No                        |14695|7.029428|3.837369|2.1277292| 0.64072194|-0.9128159|
#  |                      |Yes                       | 3335|8.111928|4.247887|2.4662145| 0.77063468|-0.9486300|
#  |                      |Missing                   |  417|     Inf|3.706228|1.8843110| 0.48405538|-1.2481440|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |chronic_kidney        |No                        |15952|7.111575|3.845038|2.1395058| 0.64426597|-0.9201995|
#  |                      |Yes                       | 2360|7.765993|4.287029|2.4703079| 0.77763181|-0.9566852|
#  |                      |Missing                   |  135|     Inf|4.897840|2.0065348| 0.53062825|-1.1284653|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |chronic_heart_attack  |No                        |15039|7.132697|3.894470|2.1636196| 0.66900654|-0.9064901|
#  |                      |Yes                       | 3245|7.391107|3.906068|2.2586366| 0.64044312|-1.0105506|
#  |                      |Missing                   |  163|     Inf|3.976562|1.8061482| 0.25910870|-1.1233049|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |nofeet                |Has feet                  |18339|7.176964|3.910856|2.1983763| 0.67236150|-0.9180854|
#  |                      |No feet                   |  108|     Inf|2.669210|0.2231436|-1.42138568|      -Inf|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |cholesterol           |Yes                       |11884|7.184966|3.938712|2.1947960| 0.68042580|-0.9131710|
#  |                      |No                        | 6147|7.336774|3.883059|2.1769639| 0.65386940|-0.9261048|
#  |                      |Missing                   |  416|6.028279|3.218876|1.7233333| 0.20261339|-1.3447454|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |bldpressure           |Yes                       |14678|6.954093|3.947362|2.2046652| 0.66740167|-0.9369851|
#  |                      |No                        | 3555|     Inf|3.733645|2.0568559| 0.62429831|-0.8898575|
#  |                      |Missing                   |  214|     Inf|3.545779|2.3285606| 0.76409892|-0.8075575|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |cvd                   |Yes                       | 3335|8.111928|4.247887|2.4662145| 0.77063468|-0.9486300|
#  |                      |No                        |14695|7.029428|3.837369|2.1277292| 0.64072194|-0.9128159|
#  |                      |Missing                   |  417|     Inf|3.706228|1.8843110| 0.48405538|-1.2481440|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+
#  |Overall               |                          |18447|7.182840|3.897200|2.1761410| 0.66015049|-0.9262921|
#  +----------------------+--------------------------+-----+--------+--------+---------+-----------+----------+