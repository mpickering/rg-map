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

from branca.element import CssLink, Figure, JavascriptLink, MacroElement
from jinja2 import Template
import json


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
      ("T", "gray"),
      ("X", "gray") ])


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

class FlagControl(MacroElement):
    """
    Adds a measurem widget on the map.
    Parameters
    ----------
    position: location of the widget
        default is 'topright'.
    primary_length_unit and secondary_length_unit: length units
         defaults are 'meters' and 'miles' respectively.
    primary_area_unit and secondary_area_unit: ara units
        defaults are 'sqmeters' and 'acres' respectively.
    See https://github.com/ljagis/leaflet-measure for more information.
    """
    _template = Template("""
        {% macro script(this, kwargs) %}
            var {{this.get_name()}} = new L.easyButton({
                  states: [{
    stateName: 'set-flag',
    icon: 'fa-flag',
    title: 'Flag an incorrect map',
    onClick: function(control, map) {
      control.state('undo-flag');
    }
  }, {
    icon: 'fa-undo',
    stateName: 'undo-flag',
    onClick: function(control, map) {
      map.state = false;
      control.state('set-flag');
    },
    title: 'reset flag'
  }]})
    {{this._parent.get_name()}}.on('keypress', function(e) {
        if (e.originalEvent.charCode == 102){
            {{this._parent.get_name()}}.addControl({{this.get_name()}});
        }})
            {{this._parent.get_name()}}.flagControl = {{this.get_name()}};
        {% endmacro %}
        """)  # noqa


    def __init__(self):
        """Coordinate, linear, and area measure control"""
        super(FlagControl, self).__init__()
        self._name = 'FlagControl'

        options = {
        }
        self.options = json.dumps(options)

    def render(self, **kwargs):
        super(FlagControl, self).render()

        figure = self.get_root()
        assert isinstance(figure, Figure), ('You cannot render this Element '
                                            'if it is not in a Figure.')

        figure.header.add_child(
            JavascriptLink('https://cdn.jsdelivr.net/npm/leaflet-easybutton@2/src/easy-button.js'))  # noqa

        figure.header.add_child(
            CssLink('https://cdn.jsdelivr.net/npm/leaflet-easybutton@2/src/easy-button.css'))  # no})


images = glob.glob(os.path.join(vrt_dir,'*.jpg.vrt'))

print(len(images))


flagControl = FlagControl()
print(flagControl)
m = folium.Map([54.3781, -3.4360], zoom_start=6, tiles='OpenStreetMap'
              , prefer_canvas=True, flag_control=flagControl)
flagControl.add_to(m)


def flag_click(key):
    return Template(
        u"""function (e) {
            if ({{m}}.flagControl._currentState.stateName == "undo-flag"){
                jQuery.get(\"http://europe-west1-rg-maps-216117.cloudfunctions.net/flag_map?hash="""+key+u"""")
                {{m}}.flagControl.state("set-flag")
            }
        }""")




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
                 , fill=True
                 , onclick=flag_click(key)).add_to(m)

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
