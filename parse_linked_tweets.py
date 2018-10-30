import re
import csv

f = open("linked_tweets.csv")
csv_reader = csv.reader(f, delimiter=',')
for row in csv_reader:
    text = row[1]
    
    #print(text)
    m = re.search('http://(.+?)\s+', text)
    if m:
        found = m.group(1)
        print(found)
