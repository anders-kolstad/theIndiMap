journals <- c(
 "ECOLOGICAL INDICATORS",
 "SCIENCE OF THE TOTAL ENVIRONMENT",
 "ENVIRONMENTAL MONITORING AND ASSESSMENT",
 "PLOS ONE",
 "ENVIRONMENTAL SCIENCE AND POLLUTION RESEARCH",
 "HYDROBIOLOGIA",
 "MARINE POLLUTION BULLETIN",
 "JOURNAL OF ENVIRONMENTAL MANAGEMENT",
 "FOREST ECOLOGY AND MANAGEMENT",
 "CHEMOSPHERE",
 "WATER",
 "PALAEOGEOGRAPHY PALAEOCLIMATOLOGY PALAEOECOLOGY",
 "ENVIRONMENTAL MANAGEMENT",
 "WATER RESEARCH",
 "SCIENTIFIC REPORTS",
 "ECOLOGICAL MODELLING",
 "ENVIRONMENTAL TOXICOLOGY AND CHEMISTRY",
 "MARINE ECOLOGY PROGRESS SERIES",
 "ENVIRONMENTAL POLLUTION",
 "ENVIRONMENTAL SCIENCE & TECHNOLOGY",
 "ESTUARINE COASTAL AND SHELF SCIENCE",
 "ECOTOXICOLOGY AND ENVIRONMENTAL SAFETY",
 "REMOTE SENSING",
 "AQUACULTURE",
 "FRESHWATER BIOLOGY",
 "FRESENIUS ENVIRONMENTAL BULLETIN",
 "FRONTIERS IN MARINE SCIENCE",
 "ECOLOGICAL APPLICATIONS",
 "JOURNAL OF PALEOLIMNOLOGY",
 "AGRICULTURE ECOSYSTEMS & ENVIRONMENT"
  )

scale1 <- c(unknown          = "0 - unknown",
            global           = "1 - global",
            continent        = "2 - continent",
            'multi-national' = "3 - multi-national",
            country          = "4 - country",
            region           = "5 - region",
            local            = "6 - local",
            'project area'   = "7 - project-area")

origin <- c("The original systematic search results"                             = "Systematic search",
            "The SEEA EA maintaind list of ECAs"                                 = "SEEA EA list",
            "Unsystematic search or the publications was previously known to me" = "Opportunistic")

refStates <- c("Undisturbed or minimally-disturbed condition" = "UND - Undisturbed or minimally-disturbed condition",
               "Historical condition"                         = "HIS - Historical condition",
               "Least-disturbed condition"                    = "LDI - Least-disturbed condition",
               "Contemporary condition"                       = "CON - Contemporary condition",
               "Best-attainable condition"                    = "BAT - Best-attainable condition",
               "other"                                        = "OTH - other")

rescalingMethod <- c(linear       = "LIN - linear",
                     "non-linear" = "NLI - non-linear",
                     "two-sided"  = "TSI - two-sided",
                     unclear      = "UNC - unclear")

refValMethod <- c(
                  "Reference sites"                                       = "RS - Reference sites",
                  "Modelled reference condition"                          = "MRC - Modelled reference condition",
                  "Statistical approaches based on ambient distributions" = "SAAD - Statistical approaches based on ambient distributions",
                  "Historical observations and paleo-environmental data"  = "HOPED - Historical observations and paleo-environmental data",
                  "Contemporary data"                                     = "CD - Contemporary data",
                  "Prescribed levels"                                     = "PL - Prescribed levels",
                  "Expert opinion"                                        = "EO - Expert opinion",
                  "Natural scale limits"                                  = "NSL - Natural scale limits",
                  "Others or unknown"                                     = "OTH - Others or unknown")

ETs <- c(
  "1 - Settlements and other artificial areas",
  "2 - Cropland",
  "3 - Grassland",
  "4 - Forest and woodland",
  "5 - Heathland and shrub",
  "6 - Sparsely vegetated ecosystems",
  "7 - Inland wetlands"
)
ETlink <- c("unknown"               = "un - unknown",
            "Conseptual connection" = "cc - Conseptual connection",
            "Field observations"    = "fo - Field observations",
            "Spatial overlay"       = "so - Spatial overlay",
            "Derived from maps"     = "dm - Derived from maps",
            "Not applicable"        = "na - not applicable")

ECTs <- c(
  "A1 - Physical state characteristics",
  "A2 - Chemical state characteristics",
  "B1a - Compositional state characteristics - abundance",
  "B1b - Compositional state characteristics - diversity",
  "B2 - Structural state characteristics",
  "B3 - Functional state characteristics",
  "C1 - Landscape and seascape characteristics",
  "OT - Other (e.g. pre-aggregated indices)")

publicationTypes <- c("Peer-reviewed article", 
                      "Book", 
                      "Book chapter",
                      "Rapport",
                      "Web resource",
                      "Unpublished")

maps <- c("No", "Yes", "Not by itself, but as part of an aggregated index")
