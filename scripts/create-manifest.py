#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3

# Makes the manifest file for the georeferencer


from os.path import basename, splitext


import sys
import os
import subprocess
from urllib.request import urlretrieve, urlopen
import pickle
import glob
import json

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


output_dir = sys.argv[1]
metadata_path = sys.argv[2]
metadata = map(lambda x: pickle.load(open(x, 'rb')), glob.glob(os.path.join(metadata_path,'*.pickle')))

def formatEvent(ev):
    del ev['worldfile']
    del ev['kartatid']
    del ev['mapid']
    del ev['mapfilename']
    del ev['base_url']
    del ev['format']
    del ev['rawtype']
    return ev


manifest = [formatEvent(e) for e in metadata]

out_filename = os.path.join(output_dir, 'manifest.json')
json.dump(manifest, open(out_filename, 'w'))

