data <- read.csv2("input-2020-8",sep = ' ',header = F)
# Part 1
run <- function(data) {
  acc = 0; ran = numeric(); row = 1
  repeat {
    inst = data[row,1]
    if (row %in% ran) break()
    ran = c(ran,row)
    if (inst=='nop') acc=acc+0
    if (inst=='acc') acc=acc+data[row,2]
    if (inst=='jmp') row=row+data[row,2] else row=row+1
      }
  print(acc)
} 
run(data)

# Part 2
run <- function(data) {
  acc = 0; ran = numeric(); row = 1
  repeat {
    inst=data[row,1]
    if (row %in% ran) break()
    ran=c(ran,row)
    if (inst=='nop') acc=acc+0
    if (inst=='acc') acc=acc+data[row,2]
    if (inst=='jmp') row=row+data[row,2] else row=row+1
    if (row==595) {print(c('end of prog',acc)); break()}
  } } 
loop <- function(data){
  for (i in 1:nrow(data)){
    current <- data
    if (current[i,1]=='nop') current[i,1]='jmp'
    if (current[i,1]=='jmp') current[i,1]='nop'
    run(current)
  } }
loop(data)
