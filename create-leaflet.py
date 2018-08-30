#! /usr/bin/env nix-shell
#! nix-shell -i python3 leaflet.nix

import affine
from PIL import Image

import os
import folium
from folium.plugins import ImageOverlay
from pyproj import Proj, transform
import glob
import pickle
from urllib.parse import urlsplit, urlunsplit
import html
import sys


def make_event_link(event):
    split_url = urlsplit(event['map_url'])
    map_url = urlunsplit((split_url.scheme, split_url.netloc, '/rg2/#{}'.format(event['kartatid']) , "", ""))
    return '<a href="{}">{} - {}</a>'.format(map_url, html.escape(event['name']), html.escape(event['date']))

icon_map = dict ([ ("I", "darkred"),
      ("N", "lightred"),
      ("R", "blue"),
      ("L", "lightblue"),
      ("T", "gray") ])


def make_icon(event):
    return folium.map.Icon(color=icon_map[event['rawtype']])


output_dir = sys.argv[1]
images = glob.glob(os.path.join(output_dir,'warped/*.jpg.vrt'))

print(len(images))
events = pickle.load(open(os.path.join(output_dir, 'meta.pickle'), 'rb'))


m = folium.Map([55.3781, -3.4360], zoom_start=10, tiles='OpenStreetMap')

for image_vrt in images:
#for image_vrt in ['/root/map-scraper/output/warped/BOK-129.jpg.vrt']:
    # This code should read the VRT directly
    print(image_vrt)
    key = os.path.basename(image_vrt)[:-8]
    print(key)
    if not (key in events):
        print("Skipping {}".format(key))
        continue
    image_world_file = os.path.join(output_dir, key + '.jgw')
    image_file = os.path.join(output_dir, key + '.jpg')
    im = Image.open(image_file)
    width, height = im.size

    event = events[key]


    with open(image_world_file, 'r') as fp:
       a = affine.loadsw(fp.read())
       print(a)
       xsize = width
       ysize = height
       # Process each combination of raster pixel space
       for (col, row) in [(0, 0), (xsize, 0), (xsize, ysize), (0, ysize)]:
           x, y = a * (col, row)
           print('(%.6f, %.6f) (%d, %d)' % (x, y, col, row))


    bs = [a*corner for corner in [(0,0), (xsize/2 ,ysize/2)]]
    x1,y1 = bs[0]
    x2,y2 = bs[1]

    folium.Marker([y2, x2], popup=make_event_link(event), icon=make_icon(event)).add_to(m)

tiles_loc = "https://s3-eu-west-1.amazonaws.com/rg-maps/{z}/{x}/{y}.png"

img = folium.map.TileLayer(tiles=tiles_loc, attr="RouteGadget")

img.add_to(m)

folium.LayerControl().add_to(m)

m.save(os.path.join('results', 'maps.html'))
