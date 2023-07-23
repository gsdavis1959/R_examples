# Load required packages
library(readxl)

# Replace 'shared_drive_path' with the actual path to the shared drive on your Windows 10 network
shared_drive_path <- "\\\\server-name\\shared-folder\\"

# Replace 'file_name.xlsx' with the name of your Excel file
file_name <- "file_name.xlsx"

# Construct the full file path to the Excel file on the shared drive
file_path <- file.path(shared_drive_path, file_name)

# Read the Excel file
excel_data <- read_excel(file_path)

# Print the first few rows of the Excel data
print(head(excel_data))

# if you need the username and password
# Load required packages
library(readxl)

# Replace 'shared_drive_path' with the actual path to the shared drive on your Windows 10 network
shared_drive_path <- "\\\\server-name\\shared-folder\\"

# Replace 'file_name.xlsx' with the name of your Excel file
file_name <- "file_name.xlsx"

# Replace 'your_username' and 'your_password' with your actual Windows login credentials
username <- "your_username"
password <- "your_password"

# Construct the full file path to the Excel file on the shared drive
file_path <- file.path(shared_drive_path, file_name)

# Create credentials list
creds <- readxl::excel_format(username = username, password = password)

# Read the Excel file with the provided credentials
excel_data <- readxl::read_excel_allsheets(file_path, credentials = creds)

# Print the first few rows of the Excel data from the first sheet
print(head(excel_data[[1]]))

