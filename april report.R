library(googledrive)
library(data.table)
library(rio)
our.functions::`%+%`()
google_secret<-Sys.getenv("DRIVE_CREDS")
if(google_secret=="") stop("No google drive credentials")
google_id<-"1feEds9MZNYHfyO9WhHGDm2YIG1m1U_G1dPwiGdVYtss"

`%+%`<- function (x, y)
{
  if (length(x) == 0)
    x <- ""
  if (length(y) == 0)
    y <- ""
  if (is.na(x))
    x <- ""
  if (is.na(y))
    y <- ""
  paste0(x, y)
}
google_download<-function(object_id, path, google_auth_creds=.GlobalEnv$google_auth_creds)

{

  library(googledrive)
  if(!googledrive:::.auth$auth_active) {
    drive_auth(
      path = google_auth_creds,
      scopes = "https://www.googleapis.com/auth/drive",
      cache = F,
      use_oob = T,
      token = NULL
    )

  }
  for (i in 1:10) {
    if(is_dribble(tryCatch(download<-drive_download(as_id(object_id),overwrite = T,path = path),error=function(e){print(e)})))
    {print(paste("object ", object_id, " is downloaded"));break}
    print (i)
  }
}


get_xlsx_from_google<-function(google_id, object_name="file", folder=".")
{
  obj_path<-paste0(folder,"/",object_name, ".xlsx")
  d_file<- google_download(google_id,obj_path)
  data<-rio::import_list(obj_path,setclass = "data.table")
  if(length(data)==1) data<-data[[1]]
  return(data)
}

data<-get_xlsx_from_google(google_id)


incomes<-data$апр23доходы
expenses<-data$апр23расходы
setnames(expenses,c("day","source","amount","currency","din_amount","category","target","comment"))
expenses[is.na(target), target:=""]
expenses[is.na(comment), comment:=""]

exchanges<-data$апр23переводы
exchanges[is.na(комментарий), комментарий:=""]
exchanges_plus<-exchanges[,.(source=куда, day=дата, amount=`сумма пришла`, currency=`валюта пришла`, comment=paste(ifelse(`валюта пришла`==`валюта ушла`,"ПЕРЕВОД ДЕНЕГ из ", "ОБМЕН ВАЛЮТЫ из "), откуда))]
exchanges_minus<-exchanges[,.(source=откуда, day=дата, amount=-`сумма ушла`, currency=`валюта ушла`, comment=paste(ifelse(`валюта пришла`==`валюта ушла`,"ПЕРЕВОД ДЕНЕГ в ", "ОБМЕН ВАЛЮТЫ в "), куда))]
exchanges_results<-rbind(exchanges_plus,exchanges_minus)

incomes[is.na(комментарий), комментарий:=""]

опнастя<-incomes[кошелёк %in% c("настяДинары", "настяЕвро","настяРубли"), .(day=дата,amount=сумма,currency=валюта, comment=paste(источник,комментарий, sep=" "))]
опнастя2<-expenses[source %in% c("настяДинары", "настяЕвро","настяРубли"), .(day,amount=-amount,currency, comment=paste(category,target,comment, sep=" "))]
опнастя3<-data$апр23остатки[кошелёк %in% c("настяДинары", "настяЕвро","настяРубли"),.(day=as.POSIXct(as.Date("2023-04-01")),amount=`сумма на начало месяца`, currency="валюта", comment="ВХОДЯЩИЙ ОСТАТОК")]
опнастя4<-exchanges_results[source %in% c("настяДинары", "настяЕвро","настяРубли"), .(day,amount,currency,comment)]
настя_все<-rbindlist(list(опнастя, опнастя2, опнастя3,опнастя4))

настяДинары<-настя_все[currency=="динар"][order(day)]
настяДинары[,sum(amount)]
настяДинарыFin<-настя_все[currency=="динар"][order(day), .(fin=paste(day,amount,comment, sep="\t"))]

настяЕвро<-настя_все[currency=="евро"][order(day)]
настяЕвро[,sum(amount)]
настяЕвроFin<-настя_все[currency=="евро"][order(day), .(fin=paste(day,amount,comment, sep="\t"))]

настяРубли<-настя_все[currency=="рубль"][order(day)]
настяРубли[,sum(amount)]
настяРубFin<-настя_все[currency=="рубль"][order(day), .(fin=paste(day,amount,comment, sep="\t"))]






ag1<-incomes[кошелёк %in% c("аняГдинары", "аняГевро","аняГрубли"), .(day=дата,amount=сумма,currency=валюта, comment=paste(источник,комментарий, sep=" "))]
ag2<-expenses[source %in% c("аняГдинары", "аняГевро","аняГрубли"), .(day,amount=-amount,currency, comment=paste(category,target,comment, sep=" "))]
ag3<-data$апр23остатки[кошелёк %in% c("аняГдинары", "аняГевро","аняГрубли"),.(day=as.POSIXct(as.Date("2023-04-01")),amount=`сумма на начало месяца`, currency="валюта", comment="ВХОДЯЩИЙ ОСТАТОК")]
ag4<-exchanges_results[source %in% c("аняГдинары", "аняГевро","аняГрубли"), .(day,amount,currency,comment)]
ag<-rbindlist(list(ag1, ag2, ag3,ag4))


agd<-ag[currency=="динар"][order(day)]
agd[,sum(amount)]
agdFin<-ag[currency=="динар"][order(day), .(fin=paste(day,amount,comment, sep="\t"))]


age<-ag[currency=="евро"][order(day)]
age[,sum(amount)]
ageFin<-ag[currency=="евро"][order(day), .(fin=paste(day,amount,comment, sep="\t"))]



ang1<-incomes[кошелёк %in% c("анжДинары", "анжЕвро","анжРубли"), .(day=дата,amount=сумма,currency=валюта, comment=paste(источник,комментарий, sep=" "))]
ang2<-expenses[source %in% c("анжДинары", "анжЕвро","анжРубли"), .(day,amount=-amount,currency, comment=paste(category,target,comment, sep=" "))]
ang3<-data$апр23остатки[кошелёк %in% c("анжДинары", "анжЕвро","анжРубли"),.(day=as.POSIXct(as.Date("2023-04-01")),amount=`сумма на начало месяца`, currency="валюта", comment="ВХОДЯЩИЙ ОСТАТОК")]
ang4<-exchanges_results[source %in% c("анжДинары", "анжЕвро","анжРубли"), .(day,amount,currency,comment)]
ang<-rbindlist(list(ang1, ang2, ang3,ang4))


angd<-ang[currency=="динар"][order(day)]
angd[,sum(amount)]
angdFin<-ang[currency=="динар"][order(day), .(fin=paste(day,amount,comment, sep="\t"))]


ange<-ang[currency=="евро"][order(day)]
ange[,sum(amount)]
angeFin<-ang[currency=="евро"][order(day), .(fin=paste(day,amount,comment, sep="\t"))]

