##| tar_interactive: FALSE
tar_target(ppls, 
           list.files("data/publicationProfiles", full.names = T), 
           cue = tar_cue(mode = "thorough"), 
           format="file",
           packages = "tidyverse")
