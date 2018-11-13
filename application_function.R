#get product and due date and prioritize

#step one, have format to get data into

df = data.frame(product = c("product_1", "product_2", "product_3"),
                due_date = c("2018-11-15", "2018-11-11", "2018-11-14"),
                time_to_make = c(40, 30, 50),
                importance = c(.6, .7, .9))
hours_available = 140

#a good way might be to go through each one, and pick
#calculate start and finish time
current_date = Sys.Date()
difftime(today, df$due_date[1])
df$finish_shifts = round(df$time_to_make / 8)
df$finish_date = as.Date(current_date) + df$finish_shifts 
df$due_date_diff = difftime(df$finish_date,df$due_date)
df$scaled_diff = scale(df$due_date_diff)
df$final_score = df$importance + df$scaled_diff
df = df[order(df$final_score, decreasing = TRUE),] 


for (i in c(1:nrow(df))){
  if(i == 1){
    print("Here is your schedule!")
  }
  print(as.character(df$due_date_diff[i]))
}

today
diff.Date()
df$due_date[1] - today
schedule = c()
schedule