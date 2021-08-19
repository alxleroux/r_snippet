consensus_clustering <- function(G, A){
    ## implmentation from fortunatoCommunityDetectionNetworks2016
    ## G = graph (igraph)
    ## A = list of clustering algo (igraph)
    ## return concensus membership 
    message("Apply Algos to Graphs")
    partitions <- lapply(A, function(x) membership(x(G)) )
    vertices <- V(G)$name
    if(is_partitions_identical(partitions)){ #if true stop here (transitivity if A == B , A == C then B == C) 
        message('All partitions are identicals, no consensus needed')
    }
    else{
        it = 0
        message("initialization")
        repeat{
            it = it + 1
            D <- consensus_matrix(partitions, vertices)
            D_g <- graph_from_adjacency_matrix(D,weighted = TRUE, mode = 'undirected') 
            partitions <- lapply(A, function(x) membership(x(D_g)) ) ## repartitions with each algo the conesensus matrix
            message(paste('iterations:', it))
            if(is_partitions_identical( partitions)){ # stop if identical otherwhise run again
                message("converged")
                break
            }
        }
    }
    return(partitions[[1]])
}
    
consensus_matrix <- function(partitions, vertices){
    ## consensus matrice (D) is a matrice counting the number of time the
    ## vertices i and j are assigned to same community ( divided by n partition)
    n_vertices <- length(vertices)
    D = matrix( 0,  nrow = n_vertices, ncol = n_vertices)
    rownames(D) <- vertices
    colnames(D) <- vertices
    for(p in partitions){
        for( i in vertices){
            for(j in vertices){
                if( j == i) next
                if(p[[j]] == p[[i]]){
                    D[i,j] <- D[i,j] + 1
                }
            }
        }
    }
    D <- D/length(partitions)
    return(D)
}

is_partitions_identical <- function(p){
    #for strict comparison all(sapply(partitions[2:length(partitions)], FUN = identical, partitions[[1]] )) like here https://stackoverflow.com/questions/30850400/using-identical-in-r-with-multiple-vectors
    try( if(length(p) < 2){
             stop("input need to have multiple elements")}
        )
    comparison <- sapply( p[2:length(p)], function(x) igraph::compare( p[[1]], x, method = 'vi'))
    comparison <- ifelse(sum(comparison) == 0, TRUE, FALSE)
    return(comparison)
}
