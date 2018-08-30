#! /usr/bin/env nix-shell
#! nix-shell -i python3 shell.nix


from selenium import webdriver
from selenium import selenium
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from os.path import basename, splitext


from pyvirtualdisplay import Display

import sys
import os
import subprocess
from urllib.request import urlretrieve, urlopen
from bs4 import BeautifulSoup
import pickle


output_dir = sys.argv[1]


# World file is always in projection EPSG:4326
def write_world_file(filename, world_dict):
    world_filename = splitext(filename)[0] + ".jgw"
    f = open(os.path.join(output_dir, world_filename), "w")
    def write_line(key):
        f.write("%.30f\n" % world_dict[key])
    for index in ['A', 'D', 'B', 'E', 'C', 'F']:
        write_line(index)
    f.close()


# Get the list of available routegadgets from the main page
page = urlopen("http://www.routegadget.co.uk/")
soup = BeautifulSoup(page, 'html.parser').find(class_="nav__list")
urls_raw = [li.find('a').get('href') for li in soup.find_all('li')]
# They all end with rg2, stripping that off is more convenient.
urls = list(map((lambda x: x[:-3]), urls_raw))

print ("{} URLS fetched".format(len(urls)))

print ("Starting scraper")
display = Display(visible=0, size=(800, 600))
display.start()

meta = {}

"rg2-event-list"


for base_url in urls:
    driver = webdriver.Firefox()
    print ("Processing {}".format(base_url))
    driver.get(base_url + "rg2/")
    WebDriverWait(driver, 3).until(EC.presence_of_element_located((By.ID, 'rg2-event-list')))
    # Query information about the georeferenced events.
    v = driver.execute_script("return rg2.events.events.filter(x => x.worldfile.valid)")
    for event in v:
        mfn = event['mapfilename']
        club = event['club']
        name = event['name']

        file_name = (club  + "-" + mfn).replace("/","-").replace(" ","-")
        event['map_url'] = base_url + 'kartat/' + mfn

        # Strip the extension
        key = os.path.splitext(file_name)[0]
        meta[key] = event
        print(name, event['date'])

        out_filename = os.path.join(output_dir, file_name)
        # Don't redownload if we already have it
        if (os.path.isfile(out_filename)):
            print ("Using cached file: {}".format(out_filename))
        else:
            write_world_file(file_name, event['worldfile'])
            urlretrieve(base_url + 'kartat/' + mfn, out_filename)

    driver.close()

# Convert all .gif files to .jpg
pickle.dump(meta, open(os.path.join(output_dir, 'meta.pickle'), 'wb'))



