#' Fisheriescape get herring phone survey data for herring CEU calculations
#'
#' @param start.year The oldest year you want. 
#' @param one.rope Logical. Do you want to return the one.rope table? Default is TRUE.
#' @import dplyr
#' @import readr
#' @importFrom stringr str_sub str_split
#' @export
fs_get_herring_phone_survey<-function(start.year=2013,one.rope=TRUE){
  
  
  M=NULL
  S=NULL
  season_code=NULL
  district=NULL
  season=NULL
  year=NULL
  area=NULL
  type1=NULL
  type2=NULL
  telsurvey_area=NULL
  nets=NULL
  hours=NULL
  hauls=NULL
  
  
  
  yr_global<-start.year
  
  
  ##//////////////////////////////////----
  # Locate files -----
  path<-'//ENT.dfo-mpo.ca/dfo-mpo/group/glf/mon/FES/Science/Hd2/herring/phone/'
  
  files_vector <- list.files(path = path, pattern = ("*.de$"), ignore.case = T, full.names = T)
  
  year_fct <- function(i){
    ifelse((as.numeric(as.character(stringr::str_sub(files_vector[[i]], -5, -4))) >= 86),
           (1900 + as.numeric(as.character(stringr::str_sub(files_vector[[i]], -5, -4)))),
           (2000 + as.numeric(as.character(stringr::str_sub(files_vector[[i]], -5, -4))))
    )
  }
  
  # remove file younger than yr_global
  for (i in files_vector) {
    no <- which(files_vector == i)
    if (!((stringr::str_sub(files_vector[[no]], -9, -4)) =="19FALL")) {
      if(year_fct(i = no) < yr_global) {
        files_vector <- files_vector[-no]
      }
    }
  }
  
  
  #//////////////////////////////////----
  # Setting reading parameters for survey -----
  #------------------------------------------------------------------------------#
  ### for 1986 only ----
  col_id_86 <- c('district',          # Home Statistical District 'stdist'
                 'resp',              # Resp. I.D. -- confidential (usually not available)
                 'season_code',       # Record for spring (1) or fall (2)
                 'fished88',          # Question 1
                 'owns',              # Question 2
                 'sfished',           # Question 3/22
                 'ffished',
                 'telsurvey_area',    # Question 4/23
                 'totdays',           # Question 5/24
                 'peak',              # Question 6/25
                 'peakdays',          # Question 7/26
                 'nnpkdays',
                 'peaknets',          # Question 8/27
                 'restnets',          # Question 12/31
                 'npeaknts',          # Question 10/29
                 'hauls',             # Question 14/33
                 'mesh1',             # Question 16/35 mesh size (in)
                 'nets1',             # Question 16/35 nets
                 'length1',           # Question 15/34
                 'depth1',
                 'mesh2',             # if fishermen use several gear...
                 'nets2',             # ...
                 'length2',           # ...
                 'depth2',
                 'mesh3',             # ...
                 'nets3',             # ...
                 'length3',           # ...
                 'depth3',
                 'catch',             # Question 17/36
                 'pctkept',           # Question 18/37a
                 'pctsold',           # Question 18b
                 'pctdump',           # Question 18c
                 'noyears',           # Question 19/38
                 'comp87',            # Question 20/39
                 # 0:much less, 2: little less, 5:about the same,
                 # 7: little more or 10: much more (Note: old SAS code stops here)
                 'compall',           # Question 21/40
                 'cfv',               # CFV = VRN
                 'limit_day',         # Question 41/44 Y/N
                 'catch_limit_day',   # Question 41/44 lbs
                 'market',            # Question 42/45
                 # 0: don't know, 1:good, 2:average,
                 # 3:bad ($value/lbs) ***Since 1994
                 'depth',             # 47/51 meshes deep ***Since 2004
                 'days_nocatch',      # Question 55/56 ***Since 2006
                 'limit_week',        # Question 57/58 Y/N
                 'catch_limit_week'   # Question 57/58 lbs ***Since 2011
  )
  
  col_spec_86 <- readr::cols(district = readr::col_character(),  # stdist
                             resp = readr::col_character(),
                             season_code = readr::col_integer(),
                             fished88 = readr::col_character(),
                             owns = readr::col_integer(),
                             sfished = readr::col_character(),
                             ffished = readr::col_character(),
                             #fseason = readr::col_character(),  # missing
                             telsurvey_area = readr::col_integer(),
                             totdays = readr::col_double(),
                             peak = readr::col_character(),
                             peakdays = readr::col_double(),
                             nnpkdays = readr::col_double(),
                             peaknets = readr::col_double(),
                             restnets = readr::col_double(),
                             npeaknts = readr::col_double(),
                             hauls = readr::col_double(),
                             mesh1 = readr::col_character(), #readr::col_double(),
                             nets1 = readr::col_double(),
                             length1 = readr::col_double(),
                             depth1 = readr::col_double(),
                             mesh2 = readr::col_character(), #readr::col_double(),
                             nets2 = readr::col_double(),
                             length2 = readr::col_double(),
                             depth2 = readr::col_double(),
                             mesh3 = readr::col_character(), #readr::col_double(),
                             nets3 = readr::col_double(),
                             length3 = readr::col_double(),
                             depth3 = readr::col_double(),
                             catch = readr::col_integer(),
                             pctkept = readr::col_double(),
                             pctsold = readr::col_double(),
                             pctdump = readr::col_double(),
                             noyears = readr::col_integer(),
                             comp87 = readr::col_double(),
                             compall = readr::col_double(),
                             cfv = readr::col_integer(),         # not in data, but create NA columns
                             limit_day = readr::col_character(), # idem for rest ...
                             catch_limit_day = readr::col_double(),
                             market = readr::col_integer(),
                             depth = readr::col_integer(),
                             days_nocatch = readr::col_double(),
                             limit_week = readr::col_character(),
                             catch_limit_week = readr::col_double()
  )
  
  
  ### for all other years ----
  col_start <- c(1, 3, 6, 7, 8, 12, 13, 14, 19, 20, 25, 29, 34, 38, 43, 47, 52, 56, 61, 66, 71, 72, 77, 82,
                 83, 88, 93, 94, 104, 108, 112, 116, 120, 123, 126, 132, 138, 143, 144, 149, 152, 156)
  col_end <- c(2, 5, 6, 7, 11, 12, 13, 18, 19, 24, 28, 33, 37, 42, 46, 51, 55, 60, 65, 70, 71, 76,
               81, 82, 87, 92, 93, 103, 107, 111, 115, 119, 122, 125, 131, 132, 142, 143, 146, 151, 152, 161)
  col_id <- c( 'district',         # Home Statistical District 'stdist'
               'resp',             # Resp. I.D. -- confidential (usually not available)
               'season_code',      # Record for spring (1) or fall (2)
               'fished88',         # Question 1 - commercial fishing : Yes - No
               'owns',             # Question 2 - total number of gillnets owns
               'fseason',          # Question 3/22 - did you fish in spring yes - no
               'telsurvey_area',   # Question 4/23 - where did you fish
               'totdays',          # Question 5/24 - how many days (at each location)
               'peak',             # Question 6/25 - really good days in spring year period or not
               'peakdays',         # Question 7/26 - if yes, how many days
               'peaknets',         # Question 8/27 - how many net set in the peak
               'peakhrs',          # Question 9/28 - how many hours you set net during these peak days
               'restnets',         # Question 12/31 - how many net set if no peak was identified in spring
               'resthrs',          # Question 13/32 - how many hours set net in avg
               'npeaknts',         # Question 10/29 - how many set net during non peak days
               'npeakhrs',         # Question 11/30 - how many hours you set net during these non peak days
               'hauls',            # Question 14/33 - how many time you empty your nets each day
               'netlngth',         # Question 15/34 - net length in fm in avg
               'mesh1',            # Question 16/35 mesh size (in)
               'nets1',            # Question 16/35 nets - number net set (both side grounded)
               'type1',            # Question 16/35 type (set/modified) number modified net set
               'mesh2',            # if fishermen use several gear...
               'nets2',            # ...
               'type2',            # ...
               'mesh3',            # ...
               'nets3',            # ...
               'type3',            # ...
               'catch',            # Question 17/36 - in lbs
               'pctkept',          # Question 18/37a - lbs or % keep for you or bait
               'pctsold',          # Question 18b - lbs or % sold
               'pctdump',          # Question 18c -  lbs or % dump
               'noyears',          # Question 19/38 -  year of experience of the fishermen
               'comp87',           # Question 20/39
               # 0:much less, 2: little less,5:about the same, 7: little more or
               # 10: much more - variation in quantity of fish in the present year
               'compall',          # Question 21/40 - rate of the fisheries (good or not good in the present year)
               'cfv',              # CFV = VRN (Note: old sas code stops here)
               'limit_day',        # Question 41/44 Y/N
               'catch_limit_day',  # Question 41/44 lbs
               'market',           # Question 42/45
               # 0: don't know, 1:good, 2:average, 3:bad ($value/lbs) ***Since 1994
               'depth',            # 47/51 meshes deep ***Since 2004
               'days_nocatch',     # Question 55/56 ***Since 2006 - how many days with no catch including no set nets
               'limit_week',       # Question 57/58 Y/N
               'catch_limit_week'  # Question 57/58 lbs ***Since 2011
  )
  
  col_spec <- readr::cols(district = readr::col_character(),
                          resp = readr::col_character(),
                          season_code = readr::col_integer(),
                          fished88 = readr::col_character(),
                          owns = readr::col_integer(),
                          fseason = readr::col_character(),
                          telsurvey_area = readr::col_integer(),
                          totdays = readr::col_double(),
                          peak = readr::col_character(),
                          peakdays = readr::col_double(),
                          peaknets = readr::col_double(),
                          peakhrs = readr::col_double(),
                          restnets = readr::col_double(),
                          resthrs = readr::col_double(),
                          npeaknts = readr::col_double(),
                          npeakhrs = readr::col_double(),
                          hauls = readr::col_double(),
                          netlngth = readr::col_double(),
                          mesh1 = readr::col_character(), #readr::col_double(),
                          nets1 = readr::col_double(),
                          type1 = readr::col_character(),
                          mesh2 = readr::col_character(), #readr::col_double(),
                          nets2 = readr::col_double(),
                          type2 = readr::col_character(),
                          mesh3 = readr::col_character(), #readr::col_double(),
                          nets3 = readr::col_double(),
                          type3 = readr::col_character(),
                          catch = readr::col_integer(),
                          pctkept = readr::col_double(),
                          pctsold = readr::col_double(),
                          pctdump = readr::col_double(),
                          noyears = readr::col_integer(),
                          comp87 = readr::col_double(),
                          compall = readr::col_double(),
                          cfv = readr::col_integer(),
                          limit_day = readr::col_character(),
                          catch_limit_day = readr::col_double(),
                          market = readr::col_integer(),
                          depth = readr::col_integer(),
                          days_nocatch = readr::col_double(),
                          limit_week = readr::col_character(),
                          catch_limit_week = readr::col_double()
  )
  
  #//////////////////////////////////----
  # Load files  -----
  #------------------------------------------------------------------------------#
  ## import loop 1986 to current year ----
  telsurvey_list <- list()
  
  for (i in 1:length(files_vector)) {
    
    #### for 2019, use only "19FALL"
    # if ((str_sub(files_vector[[i]], -5, -4)) == "19") {next}
    
    ### for 1986 ----
    if ((stringr::str_sub(files_vector[[i]], -5, -4)) == "86") {
      
      temp <- readr::read_log(files_vector[i],
                              col_names = col_id_86,
                              col_types = col_spec_86,
                              progress = readr::show_progress()
      )
      temp$year <- 1986
      cat(paste('\n', 'load', temp$year[1], 'phone survey data', '\n'))
      telsurvey_list[[i]] <- temp
      names(telsurvey_list)[i] <- paste(temp$year[1], sep = "") # rename the list element for easy access
      
    } # end of 1986 load
    
    
    ## for 1987 ----
    if ((stringr::str_sub(files_vector[[i]], -5, -4)) == "87") {
      
      temp <- readr::read_log( files_vector[i],
                               col_names = col_id,
                               col_types = col_spec,
                               progress = readr::show_progress()
      )
      temp$year <- 1987
      cat(paste('\n', 'load', temp$year[1], 'phone survey data', '\n'))
      telsurvey_list[[i]] <- temp
      names(telsurvey_list)[i] <- paste(temp$year[1], sep = "") # rename the list element for easy access
      
    } # end of 1987 load
    
    
    ## for others, but exception for 2019 fall ----
    if (!(stringr::str_sub(files_vector[[i]], -5, -4)) %in% c("86","87")) {
      
      # 2019 fall exception
      if ((stringr::str_sub(files_vector[[i]], -9, -4)) =="19FALL") {
        
        temp <- suppressWarnings(readr::read_fwf(files_vector[i],
                                guess_max = 5000,
                                progress = readr::show_progress(),
                                col_positions = readr::fwf_positions( start = col_start,
                                                                      end = col_end,
                                                                      col_names = col_id),
                                col_types = col_spec
        ))
        
        temp$year <- 2019
        cat(paste('\n', 'load', temp$year[1], 'phone survey data (19FALL)', '\n'))
        telsurvey_list[[i]] <- temp
        names(telsurvey_list)[i] <- paste(temp$year[1], "f", sep = "") # rename the list element for easy access
      }
      
      # others
      if (!((stringr::str_sub(files_vector[[i]], -9, -4)) =="19FALL")) {
        temp <- suppressWarnings(readr::read_fwf(files_vector[i],
                                guess_max = 5000,
                                progress = readr::show_progress(),
                                col_positions = readr::fwf_positions( start = col_start,
                                                                      end = col_end,
                                                                      col_names = col_id),
                                col_types = col_spec
        ))
        
        temp$year <- year_fct(i)
        cat(paste('\n', 'load', temp$year[1], 'phone survey data', '\n'))
        telsurvey_list[[i]] <- temp
        names(telsurvey_list)[i] <- paste(temp$year[1], sep = "") # rename the list element for easy access
      }
    } # end of others load
    
    
  } # end of the loop
  
  
  
  ##### reorder as in SAS ----
  telsurvey_list <- telsurvey_list[order(names(telsurvey_list))]
  
  #//////////////////////////////////----
  # Corrections -----
  #------------------------------------------------------------------------------#
  for (i in 1:length(telsurvey_list)) {
    
    output_i <- telsurvey_list[[i]]
    
    ### add number of rows for sas comparison ----
    output_i$row_no = 1:nrow(output_i)
    
    
    # initialize variable because some year haven't net_length_fm information (new correction) ----
    if (("netlngth" %in% names(output_i)) == FALSE){
      output_i$netlngth <- output_i$length1 # not in SAS
      
    }
    
    
    # import error in R, not present in SAS ----
    output_ii <- output_i[!(output_i$resp %in% c("C14", "C15", "C16")), ]
    
    
    # raw data correction as in effind.sas (year correction from 1986 to 1995) -----
    # year = 1986;
    if (output_ii$year[1] == 1986){
      output_ii$peakdays[is.na(output_ii$peakdays) & output_ii$year == 1986] <- 0
      output_ii$telsurvey_area[output_ii$year == 1986] <- output_ii$telsurvey_area[output_ii$year == 1986] + 1
    }
    
    # year = 1993;
    if (output_ii$year[1] == 1993){
      output_ii$resp[output_ii$year == 1993 &
                       output_ii$district == '65' &
                       is.na(output_ii$resp)] <- '151'
      
      output_ii$resp[output_ii$year == 1993 &
                       output_ii$district == '75' &
                       is.na(output_ii$resp)] <- '266'
      
      output_ii$district[output_ii$resp == '130' &
                           !is.na(output_ii$resp)] <- '64'
    }
    
    # year = 1994;
    if (output_ii$year[1] == 1994){
      output_ii$district[output_ii$year == 1994 &
                           output_ii$cfv == 150521 &
                           is.na(output_ii$cfv)] <- '80'
      
      output_ii$district[output_ii$year == 1994 &
                           (output_ii$cfv == 152008 | output_ii$cfv == 13378) &
                           is.na(output_ii$cfv)] <- '65'
      
      output_ii$district[is.na(output_ii$resp) &
                           output_ii$telsurvey_area == 5 &
                           output_ii$owns == 33 &
                           output_ii$year == 1994] <- '80'
      
      output_ii$district[is.na(output_ii$resp) &
                           output_ii$telsurvey_area == 8 &
                           output_ii$owns == 6 &
                           output_ii$year == 1994] <- '92'
      
      output_ii <- output_ii[!(output_ii$year == 1994 &
                                 output_ii$resp == '18' &
                                 output_ii$telsurvey_area == 6),]
    }
    
    # year = 1995
    if (output_ii$year[1] == 1995){
      output_ii$district[output_ii$resp == 288 & output_ii$year == 1995] <- '73'
      output_ii$district[output_ii$resp == 349 | output_ii$resp == 351 & output_ii$year == 1995] <- '80'
      output_ii$district[output_ii$resp == 402 & output_ii$year == 1995] <- '88'
      output_ii$district[output_ii$resp == 232 & output_ii$year == 1995] <- '66'
      output_ii$district[output_ii$resp == 394 & output_ii$year == 1995] <- '87'
    }
    
    
    output_iii <- output_ii 
    
    output_iii <- subset(output_iii, season_code %in% c(1, 2))
    
    output_iii <- subset(output_iii, telsurvey_area %in% c(1:8)) # in the by area steps in SAS (dmp or telsurvey part).
    
    output_iii$season <- ifelse(output_iii$season_code == 1, "spring",
                                ifelse(output_iii$season_code == 2, "fall", NA)) # remove one line (in tel3 steps in SAS)
    
    # adjust resp variable ----
    output_iii$resp = as.character(output_iii$resp)
    output_iii$x = stringr::str_sub(output_iii$resp, 1, 1)
    output_iii$y = stringr::str_sub(output_iii$resp, 1, 2)
    output_iii$z1 = stringr::str_sub(output_iii$resp, 2, 3)
    output_iii$z2 = stringr::str_sub(output_iii$resp, -1, -1)
    output_iii$resp = ifelse(output_iii$y == "00", output_iii$z2,
                             ifelse(output_iii$x == "0", output_iii$z1, output_iii$resp))
    
    output_iii <- output_iii[,!(names(output_iii) %in% c("x", "y", "z1", "z2"))]
    
    
    # add NAs : (peak, fished88, type1, type2, type3, limit_day, ffished, sfished) ----
    output_iii$fished88 <- ifelse(is.na(output_iii$fished88)| output_iii$fished88 %in% c("", "."), NA, output_iii$fished88)
    output_iii$peak <- ifelse(is.na(output_iii$peak)| output_iii$peak %in% c("", "."), NA, output_iii$peak)
    output_iii$limit_day <- ifelse(is.na(output_iii$limit_day)| output_iii$limit_day %in% c("", "."), NA, output_iii$limit_day)
    
    if (output_iii$year[1] != 1986){
      output_iii$type1 <- ifelse(is.na(output_iii$type1)| output_iii$type1 %in% c("", "."), NA, output_iii$type1)
      output_iii$type2 <- ifelse(is.na(output_iii$type2)| output_iii$type2 %in% c("", "."), NA, output_iii$type2)
      output_iii$type3 <- ifelse(is.na(output_iii$type3)| output_iii$type1 %in% c("", "."), NA, output_iii$type3)
    }
    
    if (output_iii$year[1] == 1986){
      output_iii$ffished <- ifelse(is.na(output_iii$ffished)| output_iii$ffished %in% c("", "."), NA, output_iii$ffished)
      output_iii$sfished <- ifelse(is.na(output_iii$sfished)| output_iii$sfished %in% c("", "."), NA, output_iii$sfished)
    }
    
    
    # mesh corrections -----
    # mesh1
    output_iii$mesh1 <- 
      ifelse(output_iii$mesh1 == "21/16", "2.0625",
             ifelse(output_iii$mesh1 %in% c("23/16", "2316"), "2.1875",
                    ifelse(output_iii$mesh1 %in% c("25/16", "2516"), "2.3125",
                           ifelse(output_iii$mesh1 == "29/16", "2.5625",
                                  ifelse(output_iii$mesh1 %in% c("21/8", "218"), "2.125",
                                         ifelse(output_iii$mesh1 %in% c("23/8", "238"), "2.375",
                                                ifelse(output_iii$mesh1 %in% c("25/8", "258"), "2.625",
                                                       ifelse(output_iii$mesh1 %in% c("27/8", "278"), "2.875",
                                                              ifelse(output_iii$mesh1 == "25/6", "2.83333",
                                                                     ifelse(output_iii$mesh1 %in% c("21/4", "22/8", "214") , "2.25",
                                                                            ifelse(output_iii$mesh1 %in% c("21/2", "22/4", "212", "24/8") , "2.5",
                                                                                   ifelse(output_iii$mesh1 %in% c("23/9"), "2.33",
                                                                                          ifelse(output_iii$mesh1 %in% c("27/16"), "2.4375",
                                                                                                 ifelse(output_iii$mesh1 %in% c("21/5"), "2.2",
                                                                                                        ifelse(output_iii$mesh1 %in% c("23/4", "234"), "2.75",
                                                                                                               ifelse(output_iii$mesh1 == ".", NA,
                                                                                                                      output_iii$mesh1
                                                                                                               ))))))))))))))))
    
    output_iii$mesh1 <- 
      ifelse(output_iii$mesh1 == "11/16", "1.0625",
             ifelse(output_iii$mesh1 == "13/16", "1.1875",
                    ifelse(output_iii$mesh1 == "15/16", "1.3125",
                           ifelse(output_iii$mesh1 == "19/16", "1.5625",
                                  ifelse(output_iii$mesh1 == "11/8", "1.125",
                                         ifelse(output_iii$mesh1 == "13/8", "1.375",
                                                ifelse(output_iii$mesh1  %in% c("15/8", "158"), "1.625",
                                                       ifelse(output_iii$mesh1  %in% c("17/8", "178"), "1.875",
                                                              ifelse(output_iii$mesh1 == "15/6", "1.83333",
                                                                     ifelse(output_iii$mesh1 %in% c("11/4", "12/8", "114") , "1.25",
                                                                            ifelse(output_iii$mesh1 %in% c("11/2", "12/4") , "1.5",
                                                                                   ifelse(output_iii$mesh1 %in% c("13/4", "134"), "1.75",
                                                                                          output_iii$mesh1
                                                                                   ))))))))))))
    
    output_iii$mesh1 <- 
      ifelse(output_iii$mesh1 == "31/4", "3.25",
             ifelse(output_iii$mesh1 == "51/2", "5.5",
                    ifelse(output_iii$mesh1 == "1/2", "0.5",
                           ifelse(output_iii$mesh1 == "53/4", "5.75",
                                  ifelse(output_iii$mesh1 == "300", "3",
                                         ifelse(output_iii$mesh1 == "53/8", "5.375",
                                                output_iii$mesh1
                                         ))))))
    
    # mesh2
    output_iii$mesh2 <- 
      ifelse(output_iii$mesh2 == "21/16", "2.0625",
             ifelse(output_iii$mesh2 %in% c("23/16", "2316"), "2.1875",
                    ifelse(output_iii$mesh2 %in% c("25/16", "2516"), "2.3125",
                           ifelse(output_iii$mesh2 == "29/16", "2.5625",
                                  ifelse(output_iii$mesh2 %in% c("21/8", "218"), "2.125",
                                         ifelse(output_iii$mesh2 %in% c("23/8", "238"), "2.375",
                                                ifelse(output_iii$mesh2 %in% c("25/8", "258"), "2.625",
                                                       ifelse(output_iii$mesh2 %in% c("27/8", "278"), "2.875",
                                                              ifelse(output_iii$mesh2 == "25/6", "2.83333",
                                                                     ifelse(output_iii$mesh2 %in% c("21/4", "22/8", "214") , "2.25",
                                                                            ifelse(output_iii$mesh2 %in% c("21/2", "22/4", "212", "24/8") , "2.5",
                                                                                   ifelse(output_iii$mesh2 %in% c("23/9"), "2.33",
                                                                                          ifelse(output_iii$mesh2 %in% c("27/16"), "2.4375",
                                                                                                 ifelse(output_iii$mesh2 %in% c("21/5"), "2.2",
                                                                                                        ifelse(output_iii$mesh2 %in% c("23/4", "234"), "2.75",
                                                                                                               ifelse(output_iii$mesh2 == ".", NA,
                                                                                                                      output_iii$mesh2
                                                                                                               ))))))))))))))))
    
    output_iii$mesh2 <- 
      ifelse(output_iii$mesh2 == "11/16", "1.0625",
             ifelse(output_iii$mesh2 == "13/16", "1.1875",
                    ifelse(output_iii$mesh2 == "15/16", "1.3125",
                           ifelse(output_iii$mesh2 == "19/16", "1.5625",
                                  ifelse(output_iii$mesh2 == "11/8", "1.125",
                                         ifelse(output_iii$mesh2 == "13/8", "1.375",
                                                ifelse(output_iii$mesh2  %in% c("15/8", "158"), "1.625",
                                                       ifelse(output_iii$mesh2  %in% c("17/8", "178"), "1.875",
                                                              ifelse(output_iii$mesh2 == "15/6", "1.83333",
                                                                     ifelse(output_iii$mesh2 %in% c("11/4", "12/8", "114") , "1.25",
                                                                            ifelse(output_iii$mesh2 %in% c("11/2", "12/4") , "1.5",
                                                                                   ifelse(output_iii$mesh2 %in% c("13/4", "134"), "1.75",
                                                                                          output_iii$mesh2
                                                                                   ))))))))))))
    
    output_iii$mesh2 <- 
      ifelse(output_iii$mesh2 == "31/4", "3.25",
             ifelse(output_iii$mesh2 == "51/2", "5.5",
                    ifelse(output_iii$mesh2 == "1/2", "0.5",
                           ifelse(output_iii$mesh2 == "53/4", "5.75",
                                  ifelse(output_iii$mesh2 == "300", "3",
                                         ifelse(output_iii$mesh2 == "53/8", "5.375",
                                                output_iii$mesh2
                                         ))))))
    
    # mesh3
    output_iii$mesh3 <- 
      ifelse(output_iii$mesh3 == "21/16", "2.0625",
             ifelse(output_iii$mesh3 %in% c("23/16", "2316"), "2.1875",
                    ifelse(output_iii$mesh3 %in% c("25/16", "2516"), "2.3125",
                           ifelse(output_iii$mesh3 == "29/16", "2.5625",
                                  ifelse(output_iii$mesh3 %in% c("21/8", "218"), "2.125",
                                         ifelse(output_iii$mesh3 %in% c("23/8", "238"), "2.375",
                                                ifelse(output_iii$mesh3 %in% c("25/8", "258"), "2.625",
                                                       ifelse(output_iii$mesh3 %in% c("27/8", "278"), "2.875",
                                                              ifelse(output_iii$mesh3 == "25/6", "2.83333",
                                                                     ifelse(output_iii$mesh3 %in% c("21/4", "22/8", "214") , "2.25",
                                                                            ifelse(output_iii$mesh3 %in% c("21/2", "22/4", "212", "24/8") , "2.5",
                                                                                   ifelse(output_iii$mesh3 %in% c("23/9"), "2.33",
                                                                                          ifelse(output_iii$mesh3 %in% c("27/16"), "2.4375",
                                                                                                 ifelse(output_iii$mesh3 %in% c("21/5"), "2.2",
                                                                                                        ifelse(output_iii$mesh3 %in% c("23/4", "234"), "2.75",
                                                                                                               ifelse(output_iii$mesh3 == ".", NA,
                                                                                                                      output_iii$mesh3
                                                                                                               ))))))))))))))))
    
    output_iii$mesh3 <- 
      ifelse(output_iii$mesh3 == "11/16", "1.0625",
             ifelse(output_iii$mesh3 == "13/16", "1.1875",
                    ifelse(output_iii$mesh3 == "15/16", "1.3125",
                           ifelse(output_iii$mesh3 == "19/16", "1.5625",
                                  ifelse(output_iii$mesh3 == "11/8", "1.125",
                                         ifelse(output_iii$mesh3 == "13/8", "1.375",
                                                ifelse(output_iii$mesh3  %in% c("15/8", "158"), "1.625",
                                                       ifelse(output_iii$mesh3  %in% c("17/8", "178"), "1.875",
                                                              ifelse(output_iii$mesh3 == "15/6", "1.83333",
                                                                     ifelse(output_iii$mesh3 %in% c("11/4", "12/8", "114") , "1.25",
                                                                            ifelse(output_iii$mesh3 %in% c("11/2", "12/4") , "1.5",
                                                                                   ifelse(output_iii$mesh3 %in% c("13/4", "134"), "1.75",
                                                                                          output_iii$mesh3
                                                                                   ))))))))))))
    
    output_iii$mesh3 <- 
      ifelse(output_iii$mesh3 == "31/4", "3.25",
             ifelse(output_iii$mesh3 == "51/2", "5.5",
                    ifelse(output_iii$mesh3 == "1/2", "0.5",
                           ifelse(output_iii$mesh3 == "53/4", "5.75",
                                  ifelse(output_iii$mesh3 == "300", "3",
                                         ifelse(output_iii$mesh3 == "53/8", "5.375",
                                                output_iii$mesh3
                                         ))))))
    
#    if (output_iii$year[1] == yr_global){
#      cat("The following mesh values are from : ", yr_global, ", if you see a character or wrong value return to 'telsurvey_load.R'", "\n",
#          "mesh1: ", unique(output_iii$mesh1), "\n",
#          "mesh2: ", unique(output_iii$mesh2), "\n",
#          "mesh3: ", unique(output_iii$mesh3), "\n")
#    }
    
    output_iii$mesh1 <- as.numeric(as.character(output_iii$mesh1))
    output_iii$mesh2 <- as.numeric(as.character(output_iii$mesh2))
    output_iii$mesh3 <- as.numeric(as.character(output_iii$mesh3))
    
    telsurvey_list[[i]] <- output_iii
  } # end of corrections
  
  rm(temp, output_i, output_ii, output_iii, col_spec, col_spec_86, col_end,
     col_id, col_id_86, col_start, files_vector, i, year_fct)
  
  
  
  #//////////////////////////////////----
  # Add variable to see  -----
  #    strange district (related to homeport) to telsurvey_area
  #    (related to port_landed) assignation
  #------------------------------------------------------------------------------#
  
  ## a- establish vector of districts by telsurvey_area with  the "areas" datafile -----
  #path_areas <- "K:/Data Analysis/Maps/"
  path_areas <- "//ENT.dfo-mpo.ca/dfo-mpo/group/glf/mon/EOS/Science/HerringM/Data Analysis/Maps/"
  list.files(path_areas)
  
  areas <- read.csv(paste0(path_areas, "areas.csv"))
  
  areas_dist_df <- areas |>
    dplyr::ungroup() |>
    dplyr::group_by(telsurvey_area) |>
    dplyr::summarise(area_dist = paste(unique(district), collapse = ' '))
  
  areas_dist_df$area_dist <- stringr::str_split(areas_dist_df$area_dist , " ")
  
  for(i in 1:nrow(areas_dist_df)){
    areas_dist_df$area_dist[i] <- list((as.numeric(trimws(areas_dist_df$area_dist[i][[1]]))))
  }
  
  
  
  ## loop -----
  for (e in 1:length(telsurvey_list)){
    
    #print(names(telsurvey_list[e]))
    
    output_i <- telsurvey_list[[e]]
    
    #print(table(output_i$telsurvey_area, output_i$district))
    
    
    # b- by telsurvey_area, create a logical variable "TRUE or FALSE" describing 
    #     if the district assignation in this particular telsurvey_area is good or not.
    dist_verif_list <- list()
    i = 1
    
    for (telarea in (1:8)){
      
      areas_dist_vec <- subset(areas_dist_df, telsurvey_area == telarea)
      
      #cat("telsurvey_area = ", telarea, "areas's district =", areas_dist_vec$area_dist[[1]], "\n")
      
      output_i_telarea <- subset(output_i, telsurvey_area == telarea)
      #output_i_telarea$nrow_yeararea_1 <- nrow(output_i_telarea)
      
      
      output_i_telarea$dist_in_telarea <- 
        ifelse(output_i_telarea$district %in% areas_dist_vec$area_dist[[1]], TRUE, FALSE)
      
      ## c- remove the rows where the logical variable is FALSE.
      #output_i_telarea <- subset(output_i_telarea, dist_in_telarea == TRUE)
      #output_i_telarea$nrow_yeararea_2 <- nrow(output_i_telarea)
      
      
      dist_verif_list[[i]] <- output_i_telarea
      names(dist_verif_list)[i] <- telarea # rename the list element for easy access
      
      i = i + 1
    }
    rm(i)
    
    output_ii <- dplyr::bind_rows(dist_verif_list)
    
    #print(table(output_ii$telsurvey_area, output_ii$district))
    
    telsurvey_list[[e]] <- output_ii
    
  } # end of loop - to see strange district-telsurvey assignation
  
  telsurvey<-as.data.frame(dplyr::bind_rows(telsurvey_list))
  
  if(isFALSE(one.rope)){return(telsurvey)}
  
  
  # format one.rope----
  if(isTRUE(one.rope)){
    df<-telsurvey
    
    keep.cols<-c('season','year','telsurvey_area','cfv',
                 'peak','peakdays' ,'peaknets' ,'peakhrs' ,
                 'restnets', 'resthrs', 'npeaknts' ,'npeakhrs', 
                 'hauls','type1','type2')
    
    df2<-df[,keep.cols]

    df2$area<-NA
    df2[which(df2$telsurvey_area==1),"area"]<-'16D'
    df2[which(df2$telsurvey_area==2),"area"]<-'16B'
    df2[which(df2$telsurvey_area==3),"area"]<-'16B'
    df2[which(df2$telsurvey_area==4),"area"]<-'16C'
    df2[which(df2$telsurvey_area==5),"area"]<-'16E'
    df2[which(df2$telsurvey_area==6),"area"]<-'16F'
    df2[which(df2$telsurvey_area==7),"area"]<-'16G'
    df2[which(df2$telsurvey_area==8),"area"]<-'16E'
  
    df2[which(is.na(df2$type1)),'type1']<-'S'
    
    ## ropes ----
    tmp1<-df2|>
      dplyr::group_by(season,year,area)|>
      dplyr::count(type1)|>
      tidyr::pivot_wider(names_from=type1,values_from=n)
    
    tmp2<-df2|>
      dplyr::group_by(season,year,area)|>
      dplyr::count(type2)|>
      tidyr::pivot_wider(names_from=type2,values_from=n)
    
    tmp3<-dplyr::bind_rows(tmp1,tmp2[,1:5])

    tmp3<-tmp3|>
      dplyr::group_by(season,year,area)|>
      dplyr::summarise(M=sum(M,na.rm=TRUE),
                S=sum(S,na.rm=TRUE))
  
    tmp3$tot<-tmp3$M+tmp3$S
    tmp3$one.rope<-tmp3$M/tmp3$tot
    ropes<-tmp3[,c(1:3,7)]
    ropes<-as.data.frame(ropes)

    ## gear and hours ----
    rm(tmp1,tmp2,tmp3)
    
    tmp1<-df2
    tmp1$nets<-NA
    for(i in 1:nrow(tmp1)){
      tmp1$nets[i]<-mean(c(tmp1[i,'peaknets'],tmp1[i,'npeaknts'],tmp1[i,'restnets']),na.rm=TRUE)
    }

    tmp1$hours<-NA
    for(i in 1:nrow(tmp1)){
      tmp1$hours[i]<-mean(c(tmp1[i,'peakhrs'],tmp1[i,'npeakhrs'],tmp1[i,'resthrs']),na.rm=TRUE)
    }

    phone<-tmp1|>
      dplyr::group_by(season,year,area)|>
      dplyr::summarise(nets=mean(nets,na.rm=TRUE),
                hours=mean(hours,na.rm=TRUE),
                hauls=mean(hauls,na.rm=TRUE))
    

    
    
    if(nrow(phone)==nrow(ropes)){
    phone<-dplyr::left_join(phone,ropes)
    }

    tmp<-phone[which(phone$area=='16B'),]
    tmp$area<-'16A'
    
    phone<-dplyr::bind_rows(phone,tmp)
   
    return(phone) 
  }
  
}#END FUNCTION ----

#test<-fs_get_herring_phone_survey(2013,one.rope=TRUE)
#head(test)
