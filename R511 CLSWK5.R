myfib4 <- function(thresh, printme = TRUE){
  fib <- c(0,1)
  while (tail(fib, 1) <= thresh){
    next_fib <- tail(fib,2) %% c(1,1)
    fib <- c(fib, next_fib)
  }
  if(printme){
    cat(fib[fib <= thresh], sep = ", ")
  } else {
    return(fib[fib <= thresh])
  }
}
myfib4(thresh = 350, printme = TRUE)

calc_v <- function(data){
  data <- as.numeric(data[!is.na(data)])
  if (length(data) == 0){
    cat("Dataset is not valid.")
    return(NULL)
  }
  cv <- sd(data) / mean(data)
  return(list(
    cv = cv,
    mean = mean(data, na.rm = TRUE),
    stde = sd(data, na.rm = TRUE),
    datasetname = deparse(substitute(data))
  ))
}
calc_v(Loblolly)
calc_v(Orange)
#question 3
calcfac <- function(mynum){
  if(mynum == 0){
    return(1)
  }
  if(mynum < 0){
    cat("Cannot compute factorial of a negative integer")
    return(NULL)
  } else {
    result <- 1
    for (i in 1:mynum){
      result <- result*i
    }
  }
  return(result)
}
calcfac(5)
calcfac(14)
calcfac(0)
tbl1 <- data.frame(
  employee_id = 1:5,
  employee_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623,515,611,729,843),
  startdate = as.Date(c('2012-01-01', '2013-09-23', '2014-11-15', '2014-05-11', '2015-03-27')),
  dept = c('IT','Operations', 'IT', 'HR', 'Finance'),
  stringsAsFactors = FALSE
)
tbl2 <- data.frame(
  employee_id = 6:8,
  employee_name = c("Rasmi","Pranab","Vil"),
  salary = c(578,72,632),
  startdate = as.Date(c("2013-05-21","2013-07-30","2014-06-17")),
  dept = c("IT","Operations","Finance"),
  stringAsFactor = FALSE
)
tbl3 <- data.frame(
  employee_id = 1:8,
  location = c('NY','NC','NV','ND','NH','NJ','NM','NH'),
  stringAsFactors = FALSE
)
merge_a <-full_join(tbl1,tbl2, by = "employee_name")
result_a <- merge(merge_a, tbl3, by.x= "employee_id", by.y = "employee_id")
