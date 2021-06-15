# 01/23/2021
/**
Was able to find an easy way to import email2.csv file into MySQL
Right click Marketing Campaign and use 'Table Importing Wizard'
Had to import send_date and open_date as char
Will learn how to convert them to date values at later time
**/
SELECT * FROM `Marketing Campaign`.email2;



# Displaying the emails that were opened on the same day that they were sent
# Note: This condition applies to 34,168 emails 
SELECT
	*
FROM
	email2
WHERE
	send_date = open_date;
    
# Create a table that shows the amount of emails sent per day
SELECT























