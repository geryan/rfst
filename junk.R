xx <- foreach(i = 1:3) %:%
        foreach(j = c(100, 1000, 10000)) %do% {
                i+j
        }