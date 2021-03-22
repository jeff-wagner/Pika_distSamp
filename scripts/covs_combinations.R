vars <- c('mst', 'dvc', 'lat', 'doy', 'asp')
combn(vars, m = 2)
combn(vars, m = 3)
combn(vars, m = 4)
combn(vars, m = 5)

5+
ncol(combn(vars, m = 2))+
ncol(combn(vars, m = 3)) +
ncol(combn(vars, m = 4))+
ncol(combn(vars, m = 5))
