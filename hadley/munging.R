#' ---
#' title: "Data munging with **plyr** and **reshape2**"
#' author: "Aaron A. King"
#' output:
#'   html_document:
#'     toc: yes
#'     toc_depth: 4
#' bibliography: ../course.bib
#' csl: ../ecology.csl
#' ---
#' 
#' 
#' 
#' ## Reshaping data with **reshape2**
#' 
#' The **reshape2** package works with a metaphor of *melting* and *casting*.
#' 
#' ### Melting
#' 
#' Melting takes a wide data frame and makes it long.
#' Multiple columns are combined into one *value* column with a *variable* column keeping track of which column the different values came from.
#' Only the columns containing *measure* variables are reshaped;
#' those containing *identifier* variables are left alone.
#' 
## ------------------------------------------------------------------------
require(reshape2)
x <- data.frame(a=letters[1:10],b=1:10,
                c=sample(LETTERS[1:3],10,replace=TRUE),d=sample(1:10,10,replace=T))
x
melt(x,id.vars=c("a","b"))
melt(x,measure.vars=c("c","d")) -> y; y

#' 
#' ### Casting
#' 
#' Casting turns a long data frame into a wide one.
#' A single column (called the *value* column) is separated into multiple columns according to the specification given.
#' Use `dcast` or `acast` according to whether you want the result as a data frame or an array.
## ------------------------------------------------------------------------
dcast(y,a+b~variable) -> d1; d1
class(d1)
acast(y,b~variable) -> a1; a1
class(a1); dim(a1)
acast(y,a~b~variable) -> a2; a2
class(a2); dim(a2)

#' 
#' ## Split-apply-combine with **plyr**
#' 
#' ### Basic **plyr** functions
#' 
#' The following are the basic functions for manipulating data using **plyr**.
#' 
#' #### `arrange`
#' 
#' `arrange` sorts a data frame according to specifications.
#' 
## ------------------------------------------------------------------------
require(plyr)
x <- data.frame(a=letters[1:10],b=runif(10),c=sample(LETTERS[1:3],10,replace=TRUE))
arrange(x,a,b,c)
arrange(x,b,c,a)
arrange(x,c,b,a)

#' 
#' #### `count`
#' 
#' `count(x)` counts the combinations that occur and returns a data frame.
## ------------------------------------------------------------------------
count(x,~c)
count(x,~a+c)
count(x,vars=c('a','c'))

#' 
#' #### `summarise` and `summarize`
#' 
#' Given a data frame, `summarise` (synonym `summarize`), produces a new data frame.
## ------------------------------------------------------------------------
summarize(x,mean=mean(b),sd=sd(b),top=c[1])

## energy <- read.csv("http://kingaa.github.io/short-course/hadley/energy_production.csv",comment="#")
energy <- read.csv("./energy_production.csv",comment="#")
summarize(energy,tot=sum(TJ),n=length(TJ))
summarize(energy,range(year))
summarize(energy,min(year),max(year),interval=diff(range(year)))

#' 
#' #### `mutate`
#' Given a data frame, `mutate` modifies, adds, or removes variables.
## ------------------------------------------------------------------------
x <- mutate(x,d=2*b,c=tolower(c),e=b+d,a=NULL); x

#' 
#' #### `subset`
#' 
#' We've already encountered `subset`, which doesn't belong to **plyr**, but would if it didn't already exist in **base**.
#' This function allows you to choose a subset of rows and/or columns.
#' The `subset` argument specifies a logical condition: those rows that satisfy it are chosen.
#' The `select` argument picks out which columns to keep or throw away.
## ------------------------------------------------------------------------
subset(x,d>1.2)
subset(x,select=c(b,c))
subset(x,select=-c(d))
subset(x,d>1.2,select=-e)
subset(energy,year>2010,select=c(source,TJ))

#' 
#' #### `merge` and `join`
#' 
#' `merge` belongs to the `base` package; `join` belongs to `plyr`.
#' They both do versions of the database *join* operation.
#' 
#' #### Exercise
#'   Examine the following and determine what is happening in each case.
## ------------------------------------------------------------------------
x <- expand.grid(a=1:3,b=1:5)
y <- expand.grid(a=1:2,b=1:5,c=factor(c("F","G")))
m1 <- merge(x,y); m1
m2 <- merge(x,y,by='a'); m2
m3 <- merge(x,y,all=TRUE); m3
m4 <- merge(x,y,by='a',all=TRUE); m4

#' 
#' `join` is more general.
#' It can perform a *left join*, a *right join*, an *inner join*, or a *full join*.
#' Read the documentation `join` for explanations.
#' 
#' #### Exercise
#'   Examine each of the following, and compare with the results of `merge` from the last exercise.
## ------------------------------------------------------------------------
join(x,y,by=c('a','b'),type='left')
join(x,y,by=c('a','b'),type='right')
join(x,y,by=c('a','b'),type='inner')
join(x,y,by=c('a','b'),type='full')
join(x,y,by='a',type='full')
join(x,y,by='a',type='inner')

#' 
#' 
#' ### The `-ply` functions
#' 
#' `plyr` provides a systematic, intuitive, and regular expansion of base \R's `apply` family (`apply`, `lapply`, `sapply`, `tapply`, `mapply`).
#' Collectively, these functions implement the split-apply-combine pattern of computation.
#' They first split the data up according to some criterion, then apply some function, then combine the results.
#' The functions are all named according to the scheme `XYply`, where `X` tells about the class of the source object and `Y` the class of the desired target object.
#' 
#' #### `ddply`
#' 
#' I find this the most useful of the lot.
#' It splits a data frame according to some criterion, conveniently expressed as a formula involving the variables of the data frame, applies a specified function, and combines the results back into a data frame.
#' It is best to use a function that returns a data frame, but if the function returns something else, `ddply` will attempt to coerce the value into a data frame.
#' Here are some examples:
## ------------------------------------------------------------------------
x <- ddply(energy,~region+source,subset,TJ==max(TJ)); x
x <- ddply(energy,~region+source,summarize,TJ=mean(TJ)); x

#' Notice that only combinations of the variables that exist are included in the result by default.
#' 
#' #### `daply`
#' 
#' This one is very similar, except that (as the name implies), the result is returned as an array:
## ------------------------------------------------------------------------
daply(energy,~region,function(df) mean(df$TJ))

#' 
#' #### `dlply`
#' 
#' This splits the data according to the given specifications, applies the function, and returns each result (as its name implies) as a distinct element of a list.
## ------------------------------------------------------------------------
dlply(energy,~region,summarize,TJ=mean(TJ))

#' 
#' #### `adply`, `aaply`, `alply`
#' 
#' These take arrays and, like the `base` function `apply`, divide the array up into slices along specified directions.
#' They then apply a function to each slice and return the results in the desired form (if possible).
#' As an example, we first create an array from `dat`, then act on it with each of these.
## ------------------------------------------------------------------------
mutate(energy,time=year-min(year)) -> dat
daply(dat,~source+region,function(df) min(df$time)) -> A; A
aaply(A,1,max)

#' 
#' #### Exercise
#' Create some simple arrays and practice using these functions.
#' 
#' 
#' #### `llply`, `laply`, `ldply`
#' 
#' These functions are generalizations of `lapply` and `sapply`.
#' 
#' #### Exercise
#' Create a few simple lists and practice using these functions.
#' 
#' 
#' #### `mlply`, `maply`, `mdply`
#' 
#' #### Exercise
#' Create a simple data frame and practice using these functions.
#' 
#' 
#' 
#' ### Other functions
#' 
#' #### `rename`, `revalue`, `mapvalues`
#' 
#' `rename` helps one to change the names of a data frame.
## ------------------------------------------------------------------------
x <- rename(energy,c(TJ='energy',year="time")); head(x)

#' `revalue` allows you to change one or more of the levels of a factor without worrying about how the factors are coded.
#' `mapvalues` does the same, but works on vectors of any type.
## ------------------------------------------------------------------------
x <- mutate(energy,region=revalue(region,c(`Asia and Oceania`="Asia",
                                           `Central and South America`="Latin.America"))); 
head(x)
x <- mutate(energy,source=mapvalues(source,from=c("Coal","Gas","Oil"),
                                    to=c("Carbon","Carbon","Carbon")))
head(x)

#' 
#' ## The **magrittr** syntax
#' 
#' ![ceci n'est pas une pipe](MagrittePipe.png)  
#' Ren&eacute; Magritte, *La Trahison des Images*
#' 
#' 
#' **magrittr** gives a set of "pipe" operators.
#' These allow one to chain operations together.
#' 
#' ### The %>% operator
#' 
#' ```
#' f(g(data, a, b, c, ...), d, e, ...)
#' ```
#' 
#' is equivalent to
#' 
#' ```
#' data %>% g(a, b, c, ...) %>% f(d, e, ...)
#' ```
#' 
#' ### The %<>% operator
#' 
#' ```
#' x %>% f(a, b, c, ...) -> x
#' ```
#' is equivalent to
#' ```
#' x %<>% f(a, b, c, ...)
#' ```
#' ----------------------------
#' 
#' 
#' ## [Back to course homepage](http://kingaa.github.io/short-course)
#' 
#' ----------------------------
#' 
#' ## References
#' 
