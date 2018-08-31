#! /usr/bin/env nix-shell
#! nix-shell -i python3 merge-rasters.nix
from osgeo import gdal,ogr,osr
from shapely.geometry import Polygon
import sys
import glob
import os
import json
import pprint
import subprocess

output_dir=sys.argv[1]

def GetExtent(gt,cols,rows):
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
    ext=[]
    xarr=[0,cols]
    yarr=[0,rows]

    for px in xarr:
        for py in yarr:
            x=gt[0]+(px*gt[1])+(py*gt[2])
            y=gt[3]+(px*gt[4])+(py*gt[5])
            ext.append([x,y])
        yarr.reverse()
    return ext

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

def MakePolygon(raster):
    ds=gdal.Open(raster)
    gt=ds.GetGeoTransform()
    cols = ds.RasterXSize
    rows = ds.RasterYSize
    ext=GetExtent(gt,cols,rows)

    src_srs=osr.SpatialReference()
    src_srs.ImportFromWkt(ds.GetProjection())
    tgt_srs=osr.SpatialReference()
    tgt_srs.ImportFromEPSG(4326)

    geo_ext=ReprojectCoords(ext,src_srs,tgt_srs)

    # Approximately 2km buffer
    lat_buffer_factor = (2/111)
    long_buffer_factor = (2/73)

    return Polygon(geo_ext).buffer(lat_buffer_factor)



images = glob.glob(os.path.join(output_dir,'warped/*.jpg.vrt'))

polydict = dict([(fp, MakePolygon (fp)) for fp in images])

init_polygons = [{'fp': tuple([key]), 'poly': value} for key, value in polydict.items()]
truelen = len(init_polygons)

result = []

progress = True

# Iterate until no more groups are formed.
while progress:
    progress = False
    loop_result = []
    done = set()

    # Invariant here is that each fp should only appear once. As we iterate, we add
    # encountered fps to the "done" set.

    for p in init_polygons:
        # Enough to check if one is in the set as then they all will be.
        if p['fp'][0] in done: continue
        accum = p
        done.update(p['fp'])
        for idx, p_check in enumerate(init_polygons):
            if p_check['fp'][0] in done: continue
            if accum['poly'].intersects(p_check['poly']):
                progress = True
                accum['fp'] = accum['fp'] + p_check['fp']
                accum['poly'] = accum['poly'].union(p_check['poly'])
                done.update(p_check['fp'])
        loop_result.append(accum)

    reslen = sum([len(p['fp']) for p in loop_result])
    assert reslen == truelen

    print ("{} groups".format(len(loop_result)))

    init_polygons = loop_result

# Make VRTs
# TODO: Choose a sensible ordering of the layers, perhaps put the biggest ones at the bottom.

for ix, group in enumerate(loop_result):
    sorted_group = sorted(list(group['fp']), key=lambda fp: polydict[fp].area, reverse=True)
    path = os.path.join(output_dir, 'groups', "{}.vrt".format(ix))
    subprocess.run(["gdalbuildvrt", path] + sorted_group)




