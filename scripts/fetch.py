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


output_dir = os.path.join(sys.argv[1])
metadata_path=sys.argv[2]

event = pickle.load(open(metadata_path, 'rb'))


# World file is always in projection EPSG:4326
def write_world_file(filename, world_dict):
    world_filename = splitext(filename)[0] + ".jgw"
    f = open(os.path.join(output_dir, world_filename), "w")
    def write_line(key):
        f.write("%.30f\n" % world_dict[key])
    for index in ['A', 'D', 'B', 'E', 'C', 'F']:
        write_line(index)
    f.close()

mfn = event['mapfilename']
club = event['club']
name = event['name']


_, file_ext = os.path.splitext(mfn)
file_hash, _ = os.path.splitext(os.path.basename(metadata_path))

file_name = file_hash + file_ext

out_filename = os.path.join(output_dir, file_name)
write_world_file(file_name, event['worldfile'])
urlretrieve(event['map_url'],  out_filename)
