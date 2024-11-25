library(glue)

# Import 3NF tables
proj_df <- read.csv('./data/proj.csv')
assign_df <- read.csv('./data/assign.csv')
emp_df <- read.csv('./data/emp.csv')
job_df <- read.csv('./data/job.csv')

# Merge all tables
proj_assign_df <- merge(proj_df, assign_df, by='PROJ_NUM')
head(proj_assign_df)

proj_assign_emp_df <- merge(proj_assign_df, emp_df, by.x='EMP_NUM', by.y='EMP_NUM')
head(proj_assign_emp_df)

all_df <- merge(proj_assign_emp_df, job_df, by.x='JOB_CLASS', by.y='JOB_CLASS')
head(all_df)

#### Get the budget per project ####

# Iterate through project
for (proj_num in unique(all_df$PROJ_NUM)){
  
  # Get records associated with each project
  temp_proj <- all_df[all_df$PROJ_NUM == proj_num, ]
  
  # Multiply HOURS and Hourly Charges per Job Class
  temp_budget <- sum(temp_proj$HOURS * temp_proj$CHG_HOUR)
  
  # Get the name of the project
  temp_proj_name <- unique(temp_proj$PROJ_NAME)
  
  # Print the budget summary
  print(glue("{proj_num}({temp_proj_name}): ${temp_budget}"))
}

# Simper approach with aggregate()
all_df$PROJ_BUDGET <- all_df$HOURS * all_df$CHG_HOUR
aggregate(PROJ_BUDGET ~ PROJ_NAME, data = all_df, FUN = sum)


#### Calculate the working hours per employee
for (emp_num in unique(all_df$EMP_NUM)) {
  # Get records associate with each employee
  temp_emp <- all_df[all_df$EMP_NUM == emp_num, ]
  
  # Get the name of employee
  temp_emp_name <- unique(temp_emp$EMP_NAME)
  
  # Sum the working hours per employee
  temp_emp_hour <- sum(temp_emp$HOURS)
  
  print(glue("{emp_num}({temp_emp_name}) is working on for {temp_emp_hour} hours"))
}

# Simper approach with aggregate()
aggregate(HOURS ~ EMP_NAME, data = all_df, sum)