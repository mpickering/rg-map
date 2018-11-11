#! /usr/bin/env nix-shell
#! nix-shell -i python3 fetch.nix


from os.path import basename, splitext
import time
from shutil import copyfile



import sys
import os
import subprocess
from urllib.request import urlretrieve, urlopen
import pickle

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


output_dir = os.path.join(sys.argv[1])
metadata_path=sys.argv[2]
flag_raw=sys.argv[3]
try:
    world_file_path = sys.argv[4]
except IndexError:
    world_file_path = None

with open(metadata_path, 'rb') as md_f:
    event = pickle.load(md_f)


# World file is always in projection EPSG:4326
def write_world_file(world_filename, world_dict):
    f = open(os.path.join(output_dir, world_filename), "w")
    def write_line(key):
        f.write("%.30f\n" % world_dict[key])
    for index in ['A', 'D', 'B', 'E', 'C', 'F']:
        write_line(index)
    f.close()

if flag_raw == "1":
  flagged = True
elif flag_raw == "0":
  flagged = False
else:
  raise Exception("Unexpected flag value: {}".format(flag_raw))

mfn = event['mapfilename']
club = event['club']
name = event['name']

file_hash, _ = os.path.splitext(os.path.basename(metadata_path))

def write_proj(filename, proj):
    if proj == "3857":
        s = 'PROJCS["WGS 84 / Pseudo-Mercator",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Mercator_1SP"],PARAMETER["central_meridian",0],PARAMETER["scale_factor",1],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["X",EAST],AXIS["Y",NORTH],EXTENSION["PROJ4","+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"],AUTHORITY["EPSG","3857"]]'

    elif proj == "4326":
        s = 'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]]'
    with open(os.path.join(output_dir, filename), "w") as f:
        f.write(s)



def download_map():
    _, file_ext = os.path.splitext(mfn)

    file_name = file_hash + file_ext

    out_filename = os.path.join(output_dir, file_name)
    urlretrieve(event['map_url'],  out_filename)

    world_filename = splitext(file_name)[0] + ".jgw"
    proj_filename  = splitext(file_name)[0] + ".prj"

    if world_file_path:
        copyfile(world_file_path, world_filename)
        write_proj(proj_filename, "3857")
    else:
        write_world_file(world_filename, event['worldfile'])
        write_proj(proj_filename, "4326")
    eprint(club, name, mfn, out_filename)

def download_meta():
    file_name = file_hash + '.pickle'
    out_filename = os.path.join(output_dir, file_name)
    event['hash'] = file_hash
    event['flagged'] = flagged
    with open(out_filename, 'wb') as f:
        pickle.dump(event, f)


# Mark whether we are writing a world file as the computation
# will branch on the next step. Ultimately, we will try to fetch the world
# file from another bucket if it's not included but not yet..
if not flagged:
    if (event['worldfile']['valid'] or world_file_path):
        print(0)
        download_map()

    else:
        print(1)
        download_meta()
else:
    print(2)
    download_map()
    download_meta()

