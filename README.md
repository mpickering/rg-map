This pipeline generates the static site which has a map of all the orienteering
maps in the UK which have been uploaded to routegadget with georeferencing
information.

Usage:

1. Get the raw data, maps, meta information and world files.

```
./scraper.py
```

Warp the images, this step fixes the CRS and rotation of the images.

```
./do_warp
```

Make the tiles

```

./make_tiles

```

Make the site

```
./create-leaflet.py
```

Upload the tiles



```
./upload-tiles <bucket-name>
```
