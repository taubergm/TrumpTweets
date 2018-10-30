import re
import csv

f = open("retweets.csv")
csv_reader = csv.reader(f, delimiter=',')
for row in csv_reader:
    text = row[1]
    
    m = re.search('@(.+?)\s+', text)
    if m:
        found = m.group(1)
        print(found)
