library(tidyverse)
#Zoznam slov
s <- c('kokot',
       'pi�a',
       'chuj',
       'jeba�',
       'jebo',
       'jebko',
       'ojeb',
       '�ur�k',
       'dojeba�',
       'dojebal',
       'popi�i',
       'kurva',
       'chuj',
       'buzerant'
)


repfun2 <- function(s){
        #Znak ktorým chceme nahradiť písmená
        f <- function(x,y) {substr(x,y,y) <- "*"; x}
        g <- function(x) Reduce(f,x,s)
        
        #Pozície (okrem prvého a posledného písmena)
        out <- unlist(lapply(1:(nchar(s)-2),function(x) combn(2:(nchar(s)-1),x,g)))
        return(out)
}

#Vytvor tabuľku slov
censored<- lapply(s, FUN = repfun2) %>% 
        unlist() %>% 
        as.data.frame()

#Export
openxlsx::write.xlsx(censored, file = 'Telekom_censored.xlsx', overwrite = TRUE)
