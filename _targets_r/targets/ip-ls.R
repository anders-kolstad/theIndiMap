##| tar_interactive: FALSE
tar_target(ipls, 
           list.files("data/indicatorProfiles", full.names = T), 
           cue = tar_cue(mode = "thorough"), 
           format="file",
           packages = "tidyverse")
