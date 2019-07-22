#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Title: HIV Care Continuum Model
# Author: Eric Hall
# Date: 5/4/2019

library(shiny)
library(shinydashboard)
library(rsconnect)
library(shinythemes)
library(ggplot2)
library(plyr)
library(operators)
library(scales)
library(shinyjs)



##### 1) DATA INPUTS #####
  # Input years
  FYR<-2010
  LYR<-2030
  
  ## Number of diagnoses and estimated number of infections per state--------------------------------------------
  # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-supplemental-report-vol-24-1.pdf
  # Table 13
  diagnoses<- list(
    c(828482,	854067,	879368,	903203,	927664,	952500,	977883),	#United States
    c(10705,	11195,	11492,	11882,	12308,	12099,	12549),	#Alabama
    c(645,	663,	650,	642,	619,	646,	682),	#Alaska
    c(12253,	12627,	13200,	13708,	14392,	14998,	15538),	#Arizona
    c(4333,	4427,	4600,	4778,	5071,	5222,	5470),	#Arkansas
    c(105876,	108916,	112285,	115048,	117880,	121592,	125416),	#California
    c(9890,	10192,	10478,	10683,	10995,	11401,	12142),	#Colorado
    c(10080,	10067,	10100,	10334,	9963,	10131,	10106),	#Connecticut
    c(2975,	3054,	3098,	3089,	3075,	3097,	3145),	#Delaware
    c(14039,	14102,	14722,	14722,	14192,	14248,	14374),	#DistrictofColumbia
    c(88073,	90304,	94436,	96707,	100685,	103908,	106577),	#Florida
    c(40295,	41640,	43778,	47365,	47292,	49248,	50913),	#Georgia
    c(2430,	2434,	2435,	2493,	2540,	2606,	2604),	#Hawaii
    c(873,	915,	958,	1006,	1003,	1032,	1085),	#Idaho
    c(30736,	31782,	32603,	33332,	34153,	34846,	35415),	#Illinois
    c(8719,	9049,	9378,	9718,	10067,	10561,	10886),	#Indiana
    c(1974,	2046,	2150,	2217,	2317,	2391,	2534),	#Iowa
    c(2601,	2678,	2711,	2746,	2812,	2780,	2901),	#Kansas
    c(5293,	5506,	5713,	6210,	6429,	6582,	6778),	#Kentucky
    c(15758,	16567,	16783,	17594,	18524,	19227,	19937),	#Louisiana
    c(1249,	1264,	1280,	1320,	1401,	1427,	1529),	#Maine
    c(27370,	28380,	28908,	29375,	30576,	32167,	32020),	#Maryland
    c(16899,	17574,	18282,	18709,	19074,	19423,	19743),	#Massachusetts
    c(13277,	13644,	14121,	14497,	14496,	14299,	15232),	#Michigan
    c(6489,	6727,	7019,	7209,	7414,	7684,	7965),	#Minnesota
    c(7880,	8269,	8425,	8565,	8846,	9147,	9360),	#Mississippi
    c(10096,	10478,	10811,	11050,	11392,	11756,	12021),	#Missouri
    c(419,	477,	486,	525,	540,	568,	591),	#Montana
    c(1739,	1791,	1821,	1857,	1936,	1995,	2059),	#Nebraska
    c(6881,	7099,	7326,	7599,	8172,	8663,	9135),	#Nevada
    c(1071,	1092,	1111,	1125,	1179,	1136,	1154),	#NewHampshire
    c(33314,	33422,	33810,	34563,	34794,	34864,	34868),	#NewJersey
    c(2875,	2972,	2950,	2920,	3061,	3187,	3266),	#NewMexico
    c(115986,	117448,	118745,	119704,	122631,	123603,	125266),	#NewYork
    c(24183,	25041,	26278,	27077,	28084,	29028,	29981),	#NorthCarolina
    c(199,	213,	240,	251,	293,	329,	369),	#NorthDakota
    c(16865,	17494,	18133,	18773,	19516,	20265,	21093),	#Ohio
    c(4830,	4993,	5160,	5355,	5531,	5708,	5876),	#Oklahoma
    c(5663,	5868,	6042,	6076,	6183,	6532,	6706),	#Oregon
    c(30133,	30904,	31717,	32228,	32898,	33515,	35591),	#Pennsylvania
    c(2056,	2066,	2099,	2132,	2236,	2274,	2356),	#RhodeIsland
    c(13941,	16490,	15364,	15349,	15664,	15999,	16547),	#SouthCarolina
    c(356,	378,	400,	435,	473,	498,	514),	#SouthDakota
    c(14662,	15148,	15758,	16021,	15800,	16262,	16047),	#Tennessee
    c(64768,	67946,	71091,	74344,	77858,	81230,	84135),	#Texas
    c(2136,	2232,	2282,	2467,	2555,	2630,	2711),	#Utah
    c(455,	493,	553,	597,	633,	640,	652),	#Vermont
    c(18031,	18445,	19466,	19769,	20450,	20907,	21321),	#Virginia
    c(10420,	10610,	10990,	11410,	11875,	12297,	12653),	#Washington
    c(1557,	1657,	1728,	1707,	1749,	1712,	1755),	#WestVirginia
    c(4874,	5038,	5148,	5663,	5775,	5856,	6008),	#Wisconsin
    c(260,	250,	254,	257,	262,	284,	307)	#Wyoming
  )
  
  
  total.infections<- list(
    c(1000200,	1024100,	1048000,	1071000,	1093900,	1117000,	1140400),	#United States
    c(13000,	13300,	13600,	13900,	14200,	14500,	14900),	#Alabama
    c(720,	740,	750,	770,	790,	800,	830),	#Alaska
    c(15500,	15900,	16400,	16900,	17400,	17900,	18500),	#Arizona
    c(5600,	5700,	5800,	5900,	6100,	6300,	6500),	#Arkansas
    c(124900,	128400,	131900,	135200,	138800,	142300,	145900),	#California
    c(12300,	12500,	12800,	13100,	13300,	13500,	13900),	#Colorado
    c(10800,	10900,	11000,	11100,	11200,	11300,	11400),	#Connecticut
    c(3400,	3500,	3500,	3500,	3500,	3600,	3600),	#Delaware
    c(15000,	15300,	15700,	16100,	16300,	16500,	16700),	#DistrictofColumbia
    c(109300,	112100,	114800,	117500,	120100,	122900,	125900),	#Florida
    c(51600,	53200,	54900,	56300,	57600,	59200,	61000),	#Georgia
    c(2700,	2700,	2800,	2800,	2900,	3000,	3000),	#Hawaii
    c(1100,	1100,	1100,	1100,	1100,	1100,	1100),	#Idaho
    c(35800,	36700,	37700,	38600,	39500,	40400,	41200),	#Illinois
    c(11100,	11300,	11600,	11900,	12300,	12800,	13100),	#Indiana
    c(2600,	2700,	2800,	2800,	2800,	2900,	3000),	#Iowa
    c(3000,	3100,	3100,	3200,	3200,	3300,	3400),	#Kansas
    c(6900,	7100,	7300,	7500,	7600,	7800,	8000),	#Kentucky
    c(21000,	21500,	22000,	22500,	23200,	23800,	24400),	#Louisiana
    c(1600,	1600,	1600,	1600,	1700,	1800,	1800),	#Maine
    c(33300,	34000,	34600,	35200,	35800,	36400,	37200),	#Maryland
    c(20300,	20800,	21300,	21800,	22200,	22600,	23000),	#Massachusetts
    c(15800,	16200,	16700,	17100,	17600,	18000,	18500),	#Michigan
    c(8000,	8200,	8500,	8700,	8900,	9100,	9300),	#Minnesota
    c(9900,	10100,	10200,	10300,	10500,	10600,	10700),	#Mississippi
    c(12300,	12500,	12800,	13100,	13400,	13600,	13900),	#Missouri
    c(620,	640,	650,	660,	670,	680,	690),	#Montana
    c(2100,	2200,	2200,	2300,	2300,	2400,	2400),	#Nebraska
    c(9300,	9600,	9800,	10100,	10400,	10700,	11100),	#Nevada
    c(1200,	1200,	1200,	1300,	1300,	1300,	1300),	#NewHampshire
    c(36500,	36800,	37200,	37600,	38000,	38300,	38700),	#NewJersey
    c(3400,	3400,	3500,	3600,	3700,	3800,	3900),	#NewMexico
    c(131300,	133300,	135100,	136700,	138300,	139900,	141300),	#NewYork
    c(30300,	31200,	32000,	32800,	33600,	34300,	35100),	#NorthCarolina
    c(360,	370,	380,	400,	420,	420,	470),	#NorthDakota
    c(21000,	21600,	22200,	22800,	23400,	24000,	24500),	#Ohio
    c(6100,	6200,	6400,	6600,	6700,	6800,	7000),	#Oklahoma
    c(7000,	7200,	7300,	7400,	7500,	7600,	7700),	#Oregon
    c(36500,	36900,	37500,	37800,	38000,	38200,	38500),	#Pennsylvania
    c(2400,	2500,	2600,	2600,	2700,	2700,	2800),	#RhodeIsland
    c(17400,	17800,	18100,	18500,	18800,	19200,	19600),	#SouthCarolina
    c(510,	510,	520,	530,	540,	550,	570),	#SouthDakota
    c(16500,	16900,	17400,	17800,	18200,	18500,	18900),	#Tennessee
    c(84400,	87400,	90500,	93600,	96500,	99600,	102600),	#Texas
    c(2700,	2700,	2800,	2900,	3000,	3100,	3200),	#Utah
    c(650,	670,	680,	690,	700,	700,	690),	#Vermont
    c(21400,	22000,	22500,	23100,	23600,	24300,	24800),	#Virginia
    c(13000,	13300,	13600,	13900,	14200,	14500,	14700),	#Washington
    c(1900,	1900,	1900,	1900,	2000,	2000,	2000),	#WestVirginia
    c(6200,	6300,	6500,	6600,	6800,	6900,	7100),	#Wisconsin
    c(310,	320,	330,	340,	340,	350,	360)	#Wyoming
  )
  
  deaths<-list(
    c(17031,	16528,	16200,	16048,	16257,	16025),	#United States
    c(274,	257,	283,	243,	277,	282),	#Alabama
    c(16,	13,	11,	16,	12,	12),	#Alaska
    c(203,	195,	215,	221,	222,	216),	#Arizona
    c(116,	115,	90,	124,	125,	114),	#Arkansas
    c(1558,	1623,	1559,	1616,	1643,	1700),	#California
    c(142,	126,	116,	116,	149,	124),	#Colorado
    c(218,	223,	201,	177,	190,	186),	#Connecticut
    c(77,	71,	79,	73,	90,	68),	#Delaware
    c(261,	279,	231,	203,	210,	256),	#DistrictofColumbia
    c(2036,	2021,	1982,	1995,	1972,	2092),	#Florida
    c(918,	827,	845,	848,	937,	838),	#Georgia
    c(38,	42,	50,	39,	42,	32),	#Hawaii
    c(15,	16,	24,	14,	21,	14),	#Idaho
    c(672,	571,	558,	514,	527,	547),	#Illinois
    c(175,	199,	166,	179,	180,	178),	#Indiana
    c(28,	37,	43,	43,	55,	36),	#Iowa
    c(49,	47,	44,	57,	47,	52),	#Kansas
    c(121,	111,	118,	119,	118,	123),	#Kentucky
    c(463,	475,	474,	417,	422,	427),	#Louisiana
    c(24,	25,	19,	30,	19,	20),	#Maine
    c(621,	586,	638,	620,	594,	555),	#Maryland
    c(284,	266,	271,	272,	264,	300),	#Massachusetts
    c(316,	310,	341,	282,	290,	303),	#Michigan
    c(79,	91,	86,	77,	97,	99),	#Minnesota
    c(208,	210,	200,	228,	235,	221),	#Mississippi
    c(211,	184,	192,	187,	171,	173),	#Missouri
    c(11,	8,	5,	14,	10,	14),	#Montana
    c(31,	24,	32,	33,	29,	28),	#Nebraska
    c(126,	134,	161,	147,	129,	128),	#Nevada
    c(18,	14,	23,	15,	14,	29),	#NewHampshire
    c(813,	770,	657,	740,	666,	653),	#NewJersey
    c(64,	52,	58,	48,	67,	75),	#NewMexico
    c(2272,	2196,	2055,	1996,	1939,	1834),	#NewYork
    c(587,	543,	527,	533,	550,	562),	#NorthCarolina
    c(6,	4,	3,	6,	7,	8),	#NorthDakota
    c(290,	318,	352,	313,	343,	306),	#Ohio
    c(109,	121,	115,	125,	140,	146),	#Oklahoma
    c(99,	101,	112,	110,	102,	110),	#Oregon
    c(704,	666,	586,	643,	654,	593),	#Pennsylvania
    c(38,	47,	44,	35,	37,	26),	#RhodeIsland
    c(340,	330,	318,	322,	330,	293),	#SouthCarolina
    c(12,	7,	10,	9,	9,	7),	#SouthDakota
    c(380,	324,	342,	333,	331,	343),	#Tennessee
    c(1359,	1291,	1314,	1231,	1351,	1314),	#Texas
    c(24,	24,	27,	45,	22,	33),	#Utah
    c(17,	5,	6,	9,	3,	5),	#Vermont
    c(321,	317,	337,	324,	310,	282),	#Virginia
    c(155,	171,	151,	166,	173,	148),	#Washington
    c(28,	38,	39,	42,	37,	30),	#WestVirginia
    c(96,	98,	88,	93,	91,	86),	#Wisconsin
    c(8,	5,	2,	6,	4,	4)	#Wyoming
  )
  
  state.names<-c("United States" = 1,"Alabama" = 2,"Alaska" = 3,"Arizona" = 4,"Arkansas" = 5,"California" = 6,"Colorado" = 7,
    "Connecticut" = 8,"Delaware" = 9,"District of Columbia" = 10,"Florida" = 11,"Georgia" = 12,"Hawaii" = 13,"Idaho" = 14,
    "Illinois" = 15,"Indiana" = 16,"Iowa" = 17,"Kansas" = 18,"Kentucky" = 19,"Louisiana" = 20,"Maine" = 21, "Maryland" = 22,
    "Massachusetts" = 23,"Michigan" = 24, "Minnesota" = 25, "Mississippi" = 26, "Missouri" = 27, "Montana" = 28, "Nebraska" = 29,
    "Nevada" = 30, "New Hampshire" = 31, "New Jersey" = 32, "New Mexico" = 33, "New York" = 34, "North Carolina" = 35, "North Dakota" = 36,
    "Ohio" = 37, "Oklahoma" = 38, "Oregon" = 39, "Pennsylvania" = 40, "Rhode Island" = 41, "South Carolina" = 42, "South Dakota" = 43,
    "Tennessee" = 44, "Texas" = 45, "Utah" = 46, "Vermont" = 47, "Virginia" = 48, "Washington" = 49, "West Virginia" = 50, "Wisconsin" = 51, "Wyoming" = 52)

  # https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/2130723
  skarbinski.rates<-array(c(6.5523/100,5.3078/100,2.064078/100,0.3916/100), 
                          dimnames = list(c("Undiagnosed", "Not in Care | Diagnosis", "Not Suppressed | In Care", "Suppressed")))
  
  # https://www.cdc.gov/mmwr/volumes/68/wr/mm6811e1.htm
  vitalsigns.rates<-array(c((1500+13000)/(9600+154400),16500/249700,7700/125300,0), 
                          dimnames = list(c("Undiagnosed", "Not in Care | Diagnosis", "Not Suppressed | In Care", "Suppressed")))
  
  

##### 2) INITIAL CALCULATIONS #####
  # Use linear trend to estimate 2017 and 2018
  diagnosis.pcts.linear<-array(dim = c(length(diagnoses),9))
  for (j in 1:length(diagnoses)){
    for (i in 1:7){
      diagnosis.pcts.linear[j,i]<-diagnoses[[j]][[i]]/total.infections[[j]][[i]]
    }
    diagnosis.pcts.linear[j,8]<-(((diagnoses[[j]][[7]]/total.infections[[j]])[[7]]-(diagnoses[[j]][[1]]/total.infections[[j]])[[1]])/6)+(diagnoses[[j]][[7]]/total.infections[[j]])[[7]]
    diagnosis.pcts.linear[j,9]<-(((diagnoses[[j]][[7]]/total.infections[[j]])[[7]]-(diagnoses[[j]][[1]]/total.infections[[j]])[[1]])/6)+diagnosis.pcts.linear[j,8]
    diagnosis.pcts.linear[j,8]<-ifelse(diagnosis.pcts.linear[j,8]>1,0.99999,diagnosis.pcts.linear[j,8]) # if any are over 1, set them to 0.99999
    diagnosis.pcts.linear[j,9]<-ifelse(diagnosis.pcts.linear[j,9]>1,0.99999,diagnosis.pcts.linear[j,9])
  }
  # Convert to percent and round
  diagnosis.pcts.linear<-round(diagnosis.pcts.linear*100, 1)
  
  
  # Use linear trend to estimate 2017 and 2018
  diagnoses.linear<-array(dim = c(length(diagnoses),9))
  for (j in 1:length(diagnoses)){
    for (i in 1:7){
      diagnoses.linear[j,i]<-diagnoses[[j]][[i]]
    }
    diagnoses.linear[j,8]<-((diagnoses[[j]][[7]]-diagnoses[[j]][[1]])/6)+diagnoses[[j]][[7]]
    diagnoses.linear[j,9]<-((diagnoses[[j]][[7]]-diagnoses[[j]][[1]])/6)+diagnoses.linear[j,8]
  }
  
  ###### NEED TO PLUG THESE IN AS PRESET VALUES FOR CONTINUUM TOTALS #####
  # Calcualte national care continuum values for pre-populating relevant fields
  # 2010-2013 are from Bradley AIDS paper
  # 2014-2018 are linear trends
  national.continuum<-array(dim = c(length(2010:2018), 4), 
                            dimnames = list(c(2010:2018),
                                            c("Percent Diagnosed", "Percent Retained | Diagnosed", "Percent Suppressed | In Care",
                                              "Percent Suppressed | Diagnosed")))
  national.continuum[1:4,2]<-c(556645/826907, 613220/855714, 595807/882993, 612966/904040)
  national.continuum[1:4,3]<-c(387856/556645, 441107/613220, 441619/595807, 473693/612966)
  for (j in 2:3){
    for (i in 5:9){
      national.continuum[i,j]<-((national.continuum[4,j]-national.continuum[1,j])/3)*(i-4)+national.continuum[4,j]
    }
  }
  for (i in 1:9){
    national.continuum[i,4]<-national.continuum[i,2]*national.continuum[i,3]
  }
  national.continuum<-round(national.continuum*100,1)
  


###### 3) SERVER #####
  shinyServer(function(input, output, session) {
    
    # Reset button
    observeEvent(input$reset_input, {
      reset("treatment_inputs")
      reset("prevention_inputs")
      reset("target_inputs")
      reset("advanced_inputs")
      reset("years_inputs")
      
    # Diagnosis inputs are reactive to jurisdiction selection  
      for(i in 1:length(diagnoses.linear[1,])){
        # Number of diagnoses - update 2010-2016 values to numbers from table 13
        diagnoses.update <- diagnoses.linear[as.numeric(input$jurisdiction),i]
        updateNumericInput(session, paste0("diagnosed_infections_",(i+2009)),
                           value = round_any(diagnoses.update, 1))
      } # End of for loop
      
      for(i in 1:length(diagnosis.pcts.linear[1,])){
        # Percent diagnosed
        diagnosed.pct.update <- diagnosis.pcts.linear[as.numeric(input$jurisdiction),i]
        updateNumericInput(session, paste0("pct_diag_",(i+2009)),
                           value = diagnosed.pct.update)
      }
    })
    
    
    ## Update initial numbers for each jurisdiction selection
    observe({
      for(i in 1:length(diagnoses.linear[1,])){
        # Number of diagnoses - update 2010-2016 values to numbers from table 13
        diagnoses.update <- diagnoses.linear[as.numeric(input$jurisdiction),i]
        updateNumericInput(session, paste0("diagnosed_infections_",(i+2009)),
                           value = round_any(diagnoses.update, 1))
      } # End of for loop
      
      for(i in 1:length(diagnosis.pcts.linear[1,])){
        # Percent diagnosed
        diagnosed.pct.update <- diagnosis.pcts.linear[as.numeric(input$jurisdiction),i]
        updateNumericInput(session, paste0("pct_diag_",(i+2009)),
                           value = diagnosed.pct.update)
      }
      
    }) # End observe
    
    # Force data inputs to stay within the specified ranges
    lapply(1:9, function(i){
      observe({
        
        # Force diagnosis percentage data to be between 0 and 1
        if(input[[paste0("pct_diag_",i+2009)]] > 100){
          updateNumericInput(session, paste0("pct_diag_",i+2009), value = 100)
        }
        
        if (input[[paste0("pct_diag_",i+2009)]] < 0){
          updateNumericInput(session, paste0("pct_diag_",i+2009), value = 0.1)
        }
        # Diagnoses must be greater than 1
        if(input[[paste0("diagnosed_infections_",i+2009)]] < 1){
          updateNumericInput(session, paste0("diagnosed_infections_",i+2009), value = 1)
        }
        
        # Continuum percentages must be between 0 and 1
        if(input[[paste0("continuum_2_",i+2009)]] > 100){
          updateNumericInput(session, paste0("continuum_2_",i+2009), value = 100)
        }
        
        if(input[[paste0("continuum_4_",i+2009)]] > 100){
          updateNumericInput(session, paste0("continuum_4_",i+2009), value = 100)
        }
        
        # Force percent retained to be greater than percent suppressed
        if(input[[paste0("continuum_2_",i+2009)]] < input[[paste0("continuum_4_",i+2009)]]){
          updateNumericInput(session, paste0("continuum_2_",i+2009), value = input[[paste0("continuum_4_",i+2009)]])
        }
      }) #end observe
    }) #end lapply
    
    lapply(10:21, function(i){
      observe({
        if(input[[paste0("intervention_pct_",i+2009)]] > 100){
          updateNumericInput(session, paste0("intervention_pct_",i+2009), value = 100)
        }
      }) #end observe
    }) #end lapply
    
    observe({
      if(input$target_pct_diagnosed > 100){
        updateNumericInput(session, "target_pct_diagnosed", value = 100)
      }
    }) #end observe
    
    observe({
      if(input$target_pct_retained > 100){
        updateNumericInput(session, "target_pct_retained", value = 100)
      }
    }) #end observe
    
    observe({
      if(input$target_pct_suppressed > 100){
        updateNumericInput(session, "target_pct_suppressed", value = 100)
      }
    }) #end observe
        
    
    ## Model and calculations
    model<-reactive({
      
      # Determine years of data entered
      checkbox.numeric<-as.numeric(input$data_years)
      # Determine latest year of data entry
      max.data.year<-max(checkbox.numeric)+2009 
      
      ### Create an array for reactive continuum values
      continuum.reactive<-array(dim = c(length(FYR:LYR), 4), 
                                dimnames = list(c(FYR:LYR),
                                                c("Percent Diagnosed", "Percent Retained | Diagnosed", "Percent Suppressed | In Care",
                                                  "Percent Suppressed | Diagnosed")))
      
      continuum.reactive.nochange<-array(dim = c(length(FYR:LYR), 4), 
                                         dimnames = list(c(FYR:LYR),
                                                         c("Percent Diagnosed", "Percent Retained | Diagnosed", "Percent Suppressed | In Care",
                                                           "Percent Suppressed | Diagnosed")))
      
      
      # Pull in percent diagnosed from 2010-2018
      continuum.reactive[1:9,1]<-c(input$pct_diag_2010,
                                input$pct_diag_2011,
                                input$pct_diag_2012,
                                input$pct_diag_2013,
                                input$pct_diag_2014,
                                input$pct_diag_2015,
                                input$pct_diag_2016,
                                input$pct_diag_2017,
                                input$pct_diag_2018)/100
      
      # Pull in percent Retained in Care | Diagnosis from 2010-2018
      continuum.reactive[1:9,2]<-c(input$continuum_2_2010,
                                   input$continuum_2_2011,
                                   input$continuum_2_2012,
                                   input$continuum_2_2013,
                                   input$continuum_2_2014,
                                   input$continuum_2_2015,
                                   input$continuum_2_2016,
                                   input$continuum_2_2017,
                                   input$continuum_2_2018)/100
      
      # Pull in Percent Suppressed | Diagnosis from 2010-2018
      continuum.reactive[1:9,4]<-c(input$continuum_4_2010,
                                   input$continuum_4_2011,
                                   input$continuum_4_2012,
                                   input$continuum_4_2013,
                                   input$continuum_4_2014,
                                   input$continuum_4_2015,
                                   input$continuum_4_2016,
                                   input$continuum_4_2017,
                                   input$continuum_4_2018)/100
      
      # Calculate Percent Suppressed | Retained in Care from 2010-2018
      for (i in 1:9){
        continuum.reactive[i,3]<-continuum.reactive[i,4]/continuum.reactive[i,2]
      }
      
  
      
      # Pull in Percent averted by intervetion from 2019-2030
      intervention.reactive<-c(rep(0,21))
      intervention.reactive[10:21]<-c(input$intervention_pct_2019,
                                     input$intervention_pct_2020,
                                     input$intervention_pct_2021,
                                     input$intervention_pct_2022,
                                     input$intervention_pct_2023,
                                     input$intervention_pct_2024,
                                     input$intervention_pct_2025,
                                     input$intervention_pct_2026,
                                     input$intervention_pct_2027,
                                     input$intervention_pct_2028,
                                     input$intervention_pct_2029,
                                     input$intervention_pct_2030)/100
    
      ### Create an array for reactive totals
      outcomes.reactive<-array(dim = c(length(FYR:LYR), 6), 
                      dimnames = list(c(FYR:LYR),
                                      c("Year", "End of Year Prevalence", "Diagnosed", "Annual Incidence", "Annual Mortality", "Cumulative Incidence")))
      
      outcomes.reactive.nochange<-array(dim = c(length(FYR:LYR), 6), 
                                        dimnames = list(c(FYR:LYR),
                                                        c("Year", "End of Year Prevalence", "Diagnosed", "Annual Incidence", "Annual Mortality", "Cumulative Incidence")))
      
      
      outcomes.reactive[,1]<-FYR:LYR
      
     
      
      # Pull in number of diagnoses
      diagnoses.list.reactive<-c(input$diagnosed_infections_2010,
                                 input$diagnosed_infections_2011,
                                 input$diagnosed_infections_2012,
                                 input$diagnosed_infections_2013,
                                 input$diagnosed_infections_2014,
                                 input$diagnosed_infections_2015,
                                 input$diagnosed_infections_2016,
                                 input$diagnosed_infections_2017,
                                 input$diagnosed_infections_2018)
      outcomes.reactive[1:max(checkbox.numeric),3]<-diagnoses.list.reactive[1:max(checkbox.numeric)]
      
          # If the user does not enter diagnoses data from 2010-2018, enter the average for that time period
          avg.input.prev<-mean(outcomes.reactive[checkbox.numeric,3])
          for (i in 1:max(checkbox.numeric)){
            outcomes.reactive[i,3]<-ifelse(i %in% checkbox.numeric, outcomes.reactive[i,3], avg.input.prev)
            continuum.reactive[i,1]<-ifelse(i %in% checkbox.numeric, continuum.reactive[i,1], diagnosis.pcts.linear[as.numeric(input$jurisdiction),i]/100)
          }
          
          
     
      # Calculate total number of infections from percent diagnosed and diagnosed infections
      for (i in 1:max(checkbox.numeric)){
        outcomes.reactive[i,2]<-outcomes.reactive[i,3]/continuum.reactive[i,1]
        
      }
      
          # Define continuum reactive no change for first time
          continuum.reactive.nochange<-continuum.reactive
          
          # Define outcomes reactive for first time
          outcomes.reactive.nochange<-outcomes.reactive
      
      ### Continuum targets ###
      ## No change in national continuum ##
      if (input$continuum_targets_check=="1"){
        
        continuum.reactive[(max(checkbox.numeric)+1):21,1]<-continuum.reactive[max(checkbox.numeric),1]
        for (i in 2:4){
          continuum.reactive[(max(checkbox.numeric)+1):21,i]<-continuum.reactive[max(checkbox.numeric),i]
        }
      }else if (input$continuum_targets_check=="2"){
        ## Percent Diagnosed
        # Calculate percent increase need for each year to meet target by target year
        pct.diagnosed.increase<-(input$target_pct_diagnosed/100-continuum.reactive[max(checkbox.numeric),1])/(as.numeric(input$target_year)-2009-max(checkbox.numeric))
        for (i in 1:(as.numeric(input$target_year)-2009-max(checkbox.numeric))){
          continuum.reactive[(max(checkbox.numeric)+i),1]<-continuum.reactive[max(checkbox.numeric),1]+(pct.diagnosed.increase*i)
        }
        continuum.reactive[is.na(continuum.reactive[,1]),1]<-input$target_pct_diagnosed/100
        
        ## Percent Retained
        # Calculate percent increase need for each year to get from 2018 estimate to target by target year
        pct.retained.increase<-(input$target_pct_retained/100-continuum.reactive[9,2])/(as.numeric(input$target_year)-2018)
        for (i in 10:(as.numeric(input$target_year)-2009)){
          continuum.reactive[i,2]<-continuum.reactive[9,2]+(pct.retained.increase*(i-9))
        }
        continuum.reactive[is.na(continuum.reactive[,2]),2]<-input$target_pct_retained/100
        
        ## Percent Suppressed
        # Calculate percent increase need for each year to get from 2018 estimate to target by target year
        pct.suppressed.increase<-(input$target_pct_suppressed/100-continuum.reactive[9,3])/(as.numeric(input$target_year)-2018)
        for (i in 10:(as.numeric(input$target_year)-2009)){
          continuum.reactive[i,3]<-continuum.reactive[9,3]+(pct.suppressed.increase*(i-9))
        }
        continuum.reactive[is.na(continuum.reactive[,3]),3]<-input$target_pct_suppressed/100
        
      }
       
          # No change  
          continuum.reactive.nochange[(max(checkbox.numeric)+1):21,1]<-continuum.reactive.nochange[max(checkbox.numeric),1]
          for (i in 2:4){
            continuum.reactive.nochange[(max(checkbox.numeric)+1):21,i]<-continuum.reactive.nochange[max(checkbox.numeric),i]
          }
      
      ### Select transmission rates to use ###
      transmission.rates<-if(input$transmission_rates_check=="1"){skarbinski.rates} else {vitalsigns.rates}
      # Include a scale factor for Skarbinski rates (set as 1 for Vital Signs rates)
      incidence_scale<-ifelse(input$transmission_rates_check=="1", 1.31647079368293, 1)
      
      ### Death rates ###
        death.rates<-c(deaths[[as.numeric(input$jurisdiction)]][[2]]/outcomes.reactive[1,2],
                       deaths[[as.numeric(input$jurisdiction)]][[3]]/outcomes.reactive[2,2],
                       deaths[[as.numeric(input$jurisdiction)]][[4]]/outcomes.reactive[3,2],
                       deaths[[as.numeric(input$jurisdiction)]][[5]]/outcomes.reactive[4,2],
                       deaths[[as.numeric(input$jurisdiction)]][[6]]/outcomes.reactive[5,2],
                       rep(NA,16))
        
        # Calculate average death rates from 2011-2015 and use national death decay to get up to 2030
        national.death.rates<-array(dim=c(length(deaths[[1]])))
        national.death.decay<-array(dim=c((length(deaths[[1]])-1)))
        for (i in 1:length(deaths[[1]])){national.death.rates[i]<-deaths[[1]][[i]]/total.infections[[1]][[i]]}
        for (i in 1:(length(deaths[[1]]))-1){national.death.decay[i]<-national.death.rates[i+1]/national.death.rates[i]}
        average.death.decay<-mean(national.death.decay)
        for (i in 6:21){
          death.rates[i]<-death.rates[i-1]*average.death.decay
        }
        ## Death for first year is in loop below
      
      # Define empty arrays
      continuum<-array(dim = c(length(FYR:LYR), 4), 
                       dimnames = list(c(FYR:LYR),
                                       c("Undiagnosed", "Not in Care|Diagnosis", "Not Suppressed|In-Care", "Suppressed")))
      
      continuum.nochange<-array(dim = c(length(FYR:LYR), 4), 
                       dimnames = list(c(FYR:LYR),
                                       c("Undiagnosed", "Not in Care|Diagnosis", "Not Suppressed|In-Care", "Suppressed")))
      
      transmissions<-array(dim = c(length(FYR:LYR), 4), 
                           dimnames = list(c(FYR:LYR),
                                           c("Undiagnosed", "Not in Care|Diagnosis", "Not Suppressed|In-Care", "Suppressed")))
      
      transmissions.nochange<-array(dim = c(length(FYR:LYR), 4), 
                           dimnames = list(c(FYR:LYR),
                                           c("Undiagnosed", "Not in Care|Diagnosis", "Not Suppressed|In-Care", "Suppressed")))
      
      
      ### Loop over years ###
      for (i in 1:length(FYR:LYR)){
        
        # Calculate number in each state of the continuum
        continuum[i,1]<-outcomes.reactive[i,2]-outcomes.reactive[i,3]
        continuum[i,2]<-outcomes.reactive[i,3]*(1-continuum.reactive[i,2])
        continuum[i,4]<-outcomes.reactive[i,3]*continuum.reactive[i,2]*continuum.reactive[i,3]
        continuum[i,3]<-outcomes.reactive[i,2]-(sum(continuum[i,c(1,2,4)]))
        
        continuum.nochange[i,1]<-outcomes.reactive.nochange[i,2]-outcomes.reactive.nochange[i,3]
        continuum.nochange[i,2]<-outcomes.reactive.nochange[i,3]*(1-continuum.reactive.nochange[i,2])
        continuum.nochange[i,4]<-outcomes.reactive.nochange[i,3]*continuum.reactive.nochange[i,2]*continuum.reactive.nochange[i,3]
        continuum.nochange[i,3]<-outcomes.reactive.nochange[i,2]-(sum(continuum.nochange[i,c(1,2,4)]))
        
        # Calculate number of new infections for year
        for (j in 1:length(transmission.rates)){
          transmissions[i,j]<-continuum[i,j]*transmission.rates[j]
          transmissions.nochange[i,j]<-continuum.nochange[i,j]*transmission.rates[j]
        }
        
        # Avert a defined proportion of infections that occur from 2019 on
        if (i <=9){
          outcomes.reactive[i,4]<-sum(transmissions[i,])*incidence_scale
        }else{
          outcomes.reactive[i,4]<-sum(transmissions[i,])*incidence_scale*(1-intervention.reactive[i])
        }
        outcomes.reactive.nochange[i,4]<-sum(transmissions.nochange[i,])*incidence_scale
        
        # Calculate cumulative incidence
        outcomes.reactive[i,6]<-sum(outcomes.reactive[1:i,4])
        outcomes.reactive.nochange[i,6]<-sum(outcomes.reactive.nochange[1:i,4])
        
        # Calculate number of deaths
        outcomes.reactive[1,5]<-deaths[[as.numeric(input$jurisdiction)]][[1]]
        outcomes.reactive.nochange[1,5]<-deaths[[as.numeric(input$jurisdiction)]][[1]]
        if (i <= length(FYR:LYR)-1){
          outcomes.reactive[i+1,5]<-outcomes.reactive[i,2]*death.rates[i+1]
          outcomes.reactive.nochange[i+1,5]<-outcomes.reactive.nochange[i,2]*death.rates[i+1]
          
          if (i >= max(checkbox.numeric)){
            # Calculate end of year prevalence
            outcomes.reactive[i+1,2]<-outcomes.reactive[i,2]+outcomes.reactive[i,4]-outcomes.reactive[i+1,5]
            outcomes.reactive.nochange[i+1,2]<-outcomes.reactive.nochange[i,2]+outcomes.reactive.nochange[i,4]-outcomes.reactive.nochange[i+1,5]
            # Calculate Percent Diagnosed
            outcomes.reactive[i+1,3]<-outcomes.reactive[i+1,2]*continuum.reactive[i+1]
            outcomes.reactive.nochange[i+1,3]<-outcomes.reactive.nochange[i+1,2]*continuum.reactive.nochange[i+1]
          }
        }
        
      }
      
  
      ### Output results as a data frame ###
      outcomes.out<<-as.data.frame(outcomes.reactive)
      outcomes.out.nochange<<-as.data.frame(outcomes.reactive.nochange)
      
      # Set up label for plots
      target.label<-if (input$continuum_targets_check=="1"){
        paste0("Maintain care continuum + intervention (avg: ", round(100*mean(intervention.reactive[10:21]),1), "% per year)")
      } else {paste0("Achieve ", input$target_pct_diagnosed,"% - ",input$target_pct_retained,"% - ",
                     input$target_pct_suppressed,"% by ",input$target_year,
                     " + intervention (avg: ", round(100*mean(intervention.reactive[10:21]),1), "% per year)")}
      
      outcomes.out$group<-paste0(target.label)
      outcomes.out.nochange$group<-"Maintain national care continuum"
      plot.data<<-rbind(outcomes.out, outcomes.out.nochange)
    })
    
    output$view <- renderTable({
      model()
      format(round(outcomes.out, 0)) # round numbers
    })
    
    # Plot of annual incidence
    output$plot.annual.incidence<-renderPlot({
      model()
      ggplot(plot.data,aes(x=Year,y=`Annual Incidence`, col=group, lty=group))+geom_line(size=2)+
        ggtitle(paste0("Estimated Incidence: ",names(state.names)[state.names == input$jurisdiction]))+
        scale_color_manual(values=c('#67a9cf','#ef8a62'))+
        xlab("Year")+ylab("Number of New HIV Infections")+
        scale_x_continuous(expand = c(0, 0), breaks=seq(2010,2030,2))+
        scale_y_continuous(labels = comma)+
        theme_bw()+
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 14),
              axis.title = element_text(face="bold", size = 14),
              legend.justification=c(0,1),
              legend.position = c(0.05, 0.95),
              legend.title = element_blank(),
              legend.text = element_text(size=12)
        )
      })
    
    # Plot of cummulative incidence
    output$plot.cumulative.incidence<-renderPlot({
      model()
      ggplot(plot.data,aes(x=Year,y=`End of Year Prevalence`, col=group, lty=group))+geom_line(size=2)+
        ggtitle(paste0("Estimated Prevalence: ", names(state.names)[state.names == input$jurisdiction]))+
        scale_color_manual(values=c('#67a9cf','#ef8a62'))+
        xlab("Year")+ylab("Number of Prevalent HIV Infections")+
        scale_x_continuous(expand = c(0, 0), breaks=seq(2010,2030,2))+
        scale_y_continuous(labels = comma)+
        theme_bw()+
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 14),
              axis.title = element_text(face="bold", size = 14),
              legend.justification=c(0,1),
              legend.position = c(0.05, 0.95),
              legend.title = element_blank(),
              legend.text = element_text(size=12)
        )
    })
    
  })
