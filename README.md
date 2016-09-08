# ttesdyn
Dia y Noche t de stuent


################################ Prueba T de studen partidos por dias
t.test(salida_clus1d, salida_clus1n)

ttabla = as.data.frame(matrix(ncol =  ncol(salida_clus3d)))
colnames(ttabla) = colnames(salida_clus3d)

for(i in  8: ncol(salida_clus3d)){
    if (((sd(salida_clus3n[,i], na.rm = T))!=0)&(sd(salida_clus3d[,i], na.rm = T)!=0)){
        ttabla[1,i] = T
    } else {
        ttabla[1,i] = F
        }
    
}
ttabla

aT = which(ttabla == T)

for(i in aT){
    a = t.test(salida_clus3n[,i], salida_clus3d[,i])$p.value
    ttabla[2,i] = a

}

ttabla

write.csv(ttabla, file = "t.tes_clus3.csv", na = "")


#################################
