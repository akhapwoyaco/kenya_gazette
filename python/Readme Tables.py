#!/usr/bin/env python
# coding: utf-8

# In[ ]:





# In[1]:


def print_decades(start_year, end_year):
  """
  Prints a table of years by decade from start_year to end_year with "|" as delimiter 
  and each year as a Markdown link to a corresponding PDF file.

  Args:
    start_year: The starting year.
    end_year: The ending year.
  """
  for decade_start in range(start_year, end_year + 1, 10):
    decade_end = min(decade_start + 9, end_year) 
    row_str = "|"
    for year in range(decade_start, decade_end + 1):
      row_str += f"[{year}](./pdf_file/{year}.pdf) | " 
    print(row_str) 

# Print the table of decades
print_decades(1890, 2029)


# In[2]:


def print_year_month_table(start_year, end_year):
  """
  Prints a table of years with corresponding months (Jan, Feb, ...) 
  from start_year to end_year, separated by "|".

  Args:
    start_year: The starting year.
    end_year: The ending year.
  """
  months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
  for year in range(start_year, end_year + 1):
    row = f"|[{year}](./{year}/) |" 
    for month in months:
      row += f" [{month}](./{year}/{month}) |" 
    print(row)

# Print the table of years and months
print_year_month_table(1890, 2025)


# In[ ]:




