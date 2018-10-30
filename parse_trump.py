import re
import csv

i = 0
OUTFILE = "trump2.csv" 
output_file = open(OUTFILE, 'w')
keys = ['date', 'tweet', 'client']
dict_writer = csv.DictWriter(output_file, keys)
dict_writer.writeheader()

 
with open('trump.txt', 'r') as f:
    for line in f:
        #print i
        i = i + 1
        #x = re.search(".*[A|P]M", line)
        #date = x.group(0)
        #print line
        y = re.search("(.*[A|P]M)\s+(.+?)\s+(\[.*\])",line)
        
        print line  
        date = y.groups()[0]
        date = re.sub(",","",date)
        tweet = y.groups()[1]
        client = y.groups()[2]

        print "%s,\"%s\",%s" % (date,tweet,client)
        row = {}
        row['date'] = date
        row['tweet'] = tweet
        row['client'] = client


        
        dict_writer.writerow(row)


