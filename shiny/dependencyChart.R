

inputs <- c('localPub',
            'pubDrop',
            'header/sep/quote/disp',
            'publication_info')

reactives <- c('publicationList',
               'titleToPath',
               'uploadedPub',
               'pform',
               'pExport')

datasets <- c('publicationParameters')

updates <- c('update_pubDrop',
             'UUIDgenerate()')

outputs <- c('uploaded',   # the preview
             'downloadData'
             )

myNodes <- c(inputs, 
             reactives,
             datasets,
             updates,
             outputs)
              

n   <- length(myNodes)
ins <- length(inputs)
rea <- length(reactives)
dat <- length(datasets)
upd <- length(updates)
out <- length(outputs)

nodes <- data.frame(id = 1:n,
                    
                    # add labels on nodes
                    label = myNodes,
                    
                    # add groups on nodes 
                    group = c(rep("input",ins),
                              rep("reactive",rea),
                              rep("data", dat),
                              rep('updates', upd),
                              rep('output', out)),
                    
                    # size adding value
                    value = 1,          
                    
                    # control shape of nodes
                    shape = c(rep("box",ins),
                              rep("box",rea),
                              rep("star", dat),
                              rep('triangle', upd),
                              rep('dot', out)),
                    
                    # tooltip (html or character), when the mouse is above
                    #title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),
                    
                    # color
                    color = c(rep("orange",ins),
                              rep("lightgrey", rea),
                              rep("lightblue", dat),
                              rep("khaki", upd),
                              rep("darkgrey", out)),
                    
                    # shadow
                    shadow = TRUE
                    
                    )             


# add edges, from-nodes followed by to-nodes
edgesRaw <- data.frame(
  col = c(
  which(nodes$label=="localPub"),                   which(nodes$label=="publicationList"),
  which(nodes$label=="pubDrop"),                    which(nodes$label=="titleToPath"),
  which(nodes$label=="header/sep/quote/disp"),      which(nodes$label=="uploadedPub"),
  which(nodes$label=="titleToPath"),                which(nodes$label=="uploadedPub"),
  which(nodes$label=="publicationList"),            which(nodes$label=="titleToPath"),
  which(nodes$label=="uploadedPub"),                which(nodes$label=="pform"),
  which(nodes$label=="publicationParameters"),      which(nodes$label=="pform"),
  which(nodes$label=="update_pubDrop"),             which(nodes$label=="pubDrop"),
  which(nodes$label=="localPub"),                   which(nodes$label=="update_pubDrop"),
  which(nodes$label=="uploadedPub"),                which(nodes$label=="uploaded"),
  which(nodes$label=="pExport"),                    which(nodes$label=="downloadData"),
  which(nodes$label=="UUIDgenerate()"),             which(nodes$label=="pExport"),
  which(nodes$label=="publication_info"),           which(nodes$label=="pExport"),
  which(nodes$label=="pform"),           which(nodes$label=="pExport")
  #which(nodes$label==""), which(nodes$label==""),
  
  ))
edgesRaw$id <- seq(1, nrow(edgesRaw))


edges <- data.frame(edgesRaw$col[edgesRaw$id %% 2 == 1],
                    edgesRaw$col[edgesRaw$id %% 2 != 1])



names(edges) <- c("from", "to")
edges$arrows <- "middle"


visNetwork(nodes, edges, 
           height = "500px", 
           width = "100%"
           #hierarchical =T
           )
