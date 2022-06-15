library(tidyverse)
#Zoznam slov
s <- c('kokot',
       'pièa',
       'chuj',
       'jeba',
       'jebo',
       'jebko',
       'ojeb',
       'èurák',
       'dojeba',
       'dojebal',
       'popièi',
       'kurva',
       'chuj',
       'buzerant'
)


repfun2 <- function(s){
        #Znak ktorÃ½m chceme nahradiÅ¥ pÃ­smenÃ¡
        f <- function(x,y) {substr(x,y,y) <- "*"; x}
        g <- function(x) Reduce(f,x,s)
        
        #PozÃ­cie (okrem prvÃ©ho a poslednÃ©ho pÃ­smena)
        out <- unlist(lapply(1:(nchar(s)-2),function(x) combn(2:(nchar(s)-1),x,g)))
        return(out)
}

#Vytvor tabuÄ¾ku slov
censored<- lapply(s, FUN = repfun2) %>% 
        unlist() %>% 
        as.data.frame()

#Export
openxlsx::write.xlsx(censored, file = 'Telekom_censored.xlsx', overwrite = TRUE)
