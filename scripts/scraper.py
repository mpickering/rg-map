#! /usr/bin/env nix-shell
#! nix-shell -i python3 shell.nix


from selenium import webdriver
from selenium import selenium
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from os.path import basename, splitext
import time


from pyvirtualdisplay import Display

import sys
import os
import subprocess
from urllib.request import urlretrieve, urlopen
from bs4 import BeautifulSoup
import pickle
import hashlib
import collections

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


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

def make_hash(o):

  return hashlib.sha256(repr(o).encode('utf-8')).hexdigest()

# Other instances of routegadgets
# Turned off for now due to https problem
#inj_urls = ["https://routegadget.fvo.org.uk/"]
inj_urls = ["http://www.sboc.routegadget.co.uk/"]

# Get the list of available routegadgets from the main page
page = urlopen("http://www.routegadget.co.uk/")
soup = BeautifulSoup(page, 'html.parser').find(class_="nav__list")
urls_raw = [li.find('a').get('href') for li in soup.find_all('li')]
# They all end with rg2, stripping that off is more convenient.
rg_urls = list(map((lambda x: x[:-3]), urls_raw))

urls = rg_urls + inj_urls

eprint ("{} URLS fetched".format(len(urls)))

eprint ("Starting scraper")
display = Display(visible=0, size=(800, 600))
display.start()



for base_url in urls:
    driver = webdriver.Firefox()
    eprint ("Processing {}".format(base_url))
    driver.get(base_url + "rg2/")
    time.sleep(10)
    WebDriverWait(driver, 3).until(EC.presence_of_element_located((By.ID, 'rg2-event-list')))
    # Query information about the georeferenced events.
    v = driver.execute_script("return rg2.events.events")
    for event in v:
        mfn = event['mapfilename']
        club = event['club']
        name = event['name']

        file_name = (club  + "-" + mfn).replace("/","-").replace(" ","-")
        event['map_url'] = base_url + 'kartat/' + mfn

        event['base_url'] = base_url

        # Use an ordered structure here so the serialisation is consistent.
        res = collections.OrderedDict([(k, event[k]) for k in ['kartatid', 'worldfile','mapid','date', 'map_url','mapfilename'
                                      , 'base_url', 'name', 'club', 'format', 'rawtype'] ])

        # Need this to be ordered as well so it always serialises in the same order
        res['worldfile'] = collections.OrderedDict(sorted(res['worldfile'].items()))

        h = make_hash([res['name'], res['date'], res['base_url'], sorted(res['worldfile'].items()), res['club']])

        pickle.dump(res, open(os.path.join(output_dir, "{}.pickle".format(h)), 'wb'))
        eprint(name, event['date'])


    driver.close()
