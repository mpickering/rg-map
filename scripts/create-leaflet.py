#! /usr/bin/env nix-shell
#! nix-shell -i python3 leaflet.nix

import os
import folium
import glob
import pickle
from urllib.parse import urlsplit, urlunsplit
import html
import sys
from osgeo import gdal, osr


vrt_dir = sys.argv[1]
metadata_dir = sys.argv[2]
zoom_level = int(sys.argv[3])
output_dir = sys.argv[4]


def make_event_link(event):
    split_url = urlsplit(event['map_url'])
    map_url = urlunsplit((split_url.scheme, split_url.netloc, '/rg2/#{}'.format(event['kartatid']) , "", ""))
    return '<a href="{}">{} - {}</a>'.format(map_url, event['name'].replace("'","\\'"), event['date'])

icon_map = dict ([ ("I", "darkred"),
      ("N", "lightred"),
      ("R", "blue"),
      ("L", "lightblue"),
      ("T", "gray") ])


def make_colour(event):
    return icon_map[event['rawtype']]

def GetCenter(gt,cols,rows):
    ''' Return list of corner coordinates from a geotransform

        @type gt:   C{tuple/list}
        @param gt: geotransform
        @type cols:   C{int}
        @param cols: number of columns in the dataset
        @type rows:   C{int}
        @param rows: number of rows in the dataset
        @rtype:    C{[float,...,float]}
        @return:   coordinates of each corner
    '''
    px = cols/2
    py = rows/2
    x=gt[0]+(px*gt[1])+(py*gt[2])
    y=gt[3]+(px*gt[4])+(py*gt[5])
    return [x, y]

def ReprojectCoords(coords,src_srs,tgt_srs):
    ''' Reproject a list of x,y coordinates.

        @type geom:     C{tuple/list}
        @param geom:    List of [[x,y],...[x,y]] coordinates
        @type src_srs:  C{osr.SpatialReference}
        @param src_srs: OSR SpatialReference object
        @type tgt_srs:  C{osr.SpatialReference}
        @param tgt_srs: OSR SpatialReference object
        @rtype:         C{tuple/list}
        @return:        List of transformed [[x,y],...[x,y]] coordinates
    '''
    trans_coords=[]
    transform = osr.CoordinateTransformation( src_srs, tgt_srs)
    for x,y in coords:
        x,y,z = transform.TransformPoint(x,y)
        trans_coords.append([x,y])
    return trans_coords

def GetCenterImage(raster):
    ds=gdal.Open(raster)
    gt=ds.GetGeoTransform()
    cols = ds.RasterXSize
    rows = ds.RasterYSize

    center = GetCenter(gt,cols,rows)

    src_srs=osr.SpatialReference()
    src_srs.ImportFromWkt(ds.GetProjection())
    tgt_srs=osr.SpatialReference()
    tgt_srs.ImportFromEPSG(4326)
    geo_ext=ReprojectCoords([center],src_srs,tgt_srs)

    return [geo_ext[0][1], geo_ext[0][0]]


images = glob.glob(os.path.join(vrt_dir,'*.jpg.vrt'))

print(len(images))


m = folium.Map([54.3781, -3.4360], zoom_start=6, tiles='OpenStreetMap'
              , prefer_canvas=True)

for image_vrt in images:
    print(image_vrt)
    key = os.path.splitext(os.path.splitext(os.path.basename(image_vrt))[0])[0]
    event = pickle.load(open(os.path.join(metadata_dir,'{}.pickle'.format(key)), 'rb'))
    print(key)

    center = GetCenterImage(image_vrt)
    popup = folium.map.Popup(html=make_event_link(event))
    folium.Circle( center, radius=100
                 , popup=popup
                 , color=make_colour(event)
                 , fill_color=make_colour(event)
                 , fill=True).add_to(m)

tiles_loc = "https://s3-eu-west-1.amazonaws.com/rg-maps/{z}/{x}/{y}.png"
tiles_loc_dev = "{z}/{x}/{y}.png"

img = folium.raster_layers.TileLayer(tiles=tiles_loc_dev
                                    , attr="RouteGadget"
                                    , name="RouteGadget"
                                    , tms=True
                                    , max_native_zoom=zoom_level
                                    , overlay=True
                                    , opacity=0.7)

img.add_to(m)

folium.LayerControl().add_to(m)

m.save(os.path.join(output_dir, 'maps.html'))
