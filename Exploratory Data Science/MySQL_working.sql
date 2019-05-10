SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
select @@GLOBAL.sql_mode;

#setting the superstoresdb as Default DB
use superstoresdb;
show tables;

select * from cust_dimen;
# The above table has the database of customer names, geographical location as well as thier ID. 
# Cust_id is the primary key. There are no foreign keys.
select * from orders_dimen;
# The above table has the list of orders recieived along with date and prioritization.
# Order_ID is  the foreign key and Ord_ID is the primary key.
select * from prod_dimen;
# The above table has the list of products available for ordering by the customers.
# Prod_ID is the primary key. There are no foreign key.
select * from shipping_dimen;
# The table has the list of shipping information which was used for completing the order. 
#  Ship_ID is the primary key and Order_ID is the foreign key.
select * from market_fact;
# This is the master table which has the sales volume observed
# The foreign key linkages are Ord_ID, Prod_ID, Ship_ID and Cust_ID. There is no primary key.

select sum(sales) as 'Total_Sales', avg(sales) as 'Avg_Sales' from market_fact;
# Using the inbuilt function sum and avg to calculate the average and sum of sales.
# The Total_Sales and Avg_Sales are column names used for display

select count(Customer_Name) as Region_no_of_customers, region from cust_dimen
group by region
order by Region_no_of_customers desc;
# Storing the count of customers region wise in a new column - Region_no_of_customers
# and displaying the total customers / region in descending order through 'desc'

select count(*) as Region_count, region from cust_dimen
group by region
having Region_count >= All(select count(*) from cust_dimen group by region);
# counting number of customers per region and comparing against the entire set using All function
# which would display only entry which corresponds to max

select Customer_Name, market_fact.Order_Quantity as 'Number of tables purchased' from cust_dimen
inner join market_fact on market_fact.Cust_id = cust_dimen.Cust_id
inner join prod_dimen on prod_dimen.Prod_id = market_fact.Prod_id
where cust_dimen.Region = 'ATLANTIC' and prod_dimen.Product_Sub_Category = 'TABLES';
# joining tables based on unique customer_ID and Product_ID to map the table.
# Region and Product Subcategory column to identifying the Atlantic region customers ordering Tabels. 
# Using the order quantity column to identify the number of items
# purchased of the partitular product by the specific customer

select Product_Category, sum(Profit) as Total_Profit from market_fact
inner join prod_dimen on prod_dimen.Prod_id = market_fact.Prod_id
group by Product_Category
order by Total_Profit desc;
# Summarizing Profits for each Product category as Total_Profit Column
# and grouping profits for each product category to show the most profitable at top.

select Product_Category, Product_Sub_Category, sum(Profit) as Total_Profit from market_fact
inner join prod_dimen on prod_dimen.Prod_id = market_fact.Prod_id
group by Product_Category, Product_Sub_Category
order by Total_Profit;
# Approach similar to previous query but in this case displaying subcategory
# hence grouping by both Category and Sub-category.
# However in this case least profitable is shown at top.

select Region, sum(market_fact.Profit) as Region_wise_Profit from cust_dimen
inner join market_fact on market_fact.Cust_id = cust_dimen.Cust_id
inner join prod_dimen on prod_dimen.Prod_id = market_fact.Prod_id
where prod_dimen.Product_Sub_Category = 'TABLES'
group by Region
order by Region_wise_Profit desc;
# Since TABLES is  the least profitable, the query for data is hard-coded for TABLES
# Grouping by Region to provide region wise profits.
# Aggregating the profits for each customer to column Region_wise_Profit a
# and using desc to show the declining profits across regions.
