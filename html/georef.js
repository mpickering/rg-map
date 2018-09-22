/** HTML5 Georeferencing Image on a map
*/


/** jQuery plugin : load an image as dataURL
*/
(function($){

jQuery.fn.loadDataURL = function (callback, scope)
{	return this.on ('change', function(e)
	{	// Loop through the FileList
		for (var i=0, f; f=e.target.files[i]; i++)
		{	// Only process image files.
			if (!f.type.match('image.*')) continue;

			var reader = new FileReader();

			// Closure to capture the file information.
			reader.onload = (function(file)
			{	return function(e)
				{	callback.call (scope, file.name, e.target.result);
				};
			})(f);

			// Read in the image file as a data URL.
			reader.readAsDataURL(f);
		}
	});
};
})(jQuery)


var pixelProjection = new ol.proj.Projection(
{	code: 'pixel',
	units: 'pixels',
	extent: [-100000, -100000, 100000, 100000]
});

$.postJSON = function(url, data, callback) {
    return jQuery.ajax({
        'method': "POST",
        'url': url,
        'contentType': 'application/json',
        'data': JSON.stringify(data),
        'dataType': 'json',
        'success': callback
    });
};



/** The webapp
*/
var wapp =
{	/** Synchronize maps
	*/
	synchro: true,

	/** Initialize webapp
	*/
	initialize: function()
	{	// Initialize loader
		$("#loader input[type=file]").loadDataURL( wapp.load, wapp );
		$("#loader button").click( function()
			{	var f = $("#loader input[type=text]").val();
				var n = f.split("/").pop();
				n = n.substr(0, n.lastIndexOf('.')) || n;
				wapp.load(n, $("#loader input[type=text]").val());
			});

    $("#submit-info").click( function()
        { var payload = { world_file: wapp.getWorldFile()
                        , hash: wapp.current.event['hash'] }
          $.ajax({
            url:"http://europe-west1-rg-maps-216117.cloudfunctions.net/upload_world_file",
            type:"POST",
            data:JSON.stringify(payload),
            contentType:"application/json; charset=utf-8",
            dataType:"json",
            complete: function(){
              wapp.clearAll()
              wapp.populateEvents()
            } })
           });

    $("#clear").click(function(){ wapp.clearAll()});

    $('#event-search').on('input', function(ev) {
      val = $("#event-search").val()
      wapp.setEvents(val)
    });

		// Set the maps
		this.setMap();
		this.setImageMap();
    this.populateEvents();

		// Decode source
		var p={}, hash = document.location.search;
		if (hash)
		{	hash = hash.replace(/(^#|^\?)/,"").split("&");
			for (var i=0; i<hash.length;  i++)
			{	var t = hash[i].split("=");
				p[t[0]] =decodeURIComponent(t[1]);
			}
		}
		if (p.lon && p.lat)
		{	wapp.map.getView().setCenterAtLonlat([Number(p.lon),Number(p.lat)]);
		}
		if (p.photo)
		{	var n = p.photo.split("/").pop();
			n = n.substr(0, n.lastIndexOf('.')) || n;
			wapp.load(n, p.photo);

			var d = wapp.distProj(0.001);
			var r = Number(p.res)*d/2.54;
			wapp.map.getView().setResolution(r);

			wapp.mapimg.getView().setRotation((180-Number(p.ori))*Math.PI/180)

		}
	}
};

(function(){
wapp.getTopLeft = function(source) {
  var center = source.getCenter();
  var scale = source.getScale();
  var rot = -source.getRotation();
  var width = source.getGeoImage().width * scale[0];
  var height = source.getGeoImage().height * scale[1];
  var tl = new ol.geom.Point([ center[0]-width/2, center[1]+height/2 ]);
  // The resulting polygon
  tl.rotate(rot,  center);
  return tl;
};

wapp.getWorldFile = function(){
  var source = wapp.current.destLayer.image.getSource();
  var tl = wapp.getTopLeft(source).getCoordinates()
  var scale = source.getScale()
  var world_file =
    { A: scale[0]
    , D: -(Math.tan(source.getRotation()) * scale[0])
    , B: -(Math.tan(source.getRotation()) * scale[1])
    , E: -scale[1]
    , C: tl[0]
    , F: tl[1]
  }
  return world_file }

/** Distance projetee par rapport au centre
*/
wapp.distProj = function(dist, c) {
	if (!c) c = this.map.getView().getCenter();
	var c2 = [c[0], c[1]+1]

	return (dist / l.sphere.getistance(
		ol.proj.transform(c, 'EPSG:3857', 'EPSG:4326'),
		ol.proj.transform(c2, 'EPSG:3857', 'EPSG:4326')));

	/* old version
	// Sphere pour le calcul des mesures geodesiques

	var wgs84Sphere = new ol.Sphere(6378137);
	if (!c) c = this.map.getView().getCenter();
	var c2 = [c[0], c[1]+1]

	return (dist / wgs84Sphere.haversineDistance(
		ol.proj.transform(c, 'EPSG:3857', 'EPSG:4326'),
		ol.proj.transform(c2, 'EPSG:3857', 'EPSG:4326')));
	*/
}
})();

filterEvent = function (key, ev){
   var key = key.toLowerCase()
   return (((ev.name.toLowerCase()).search(key)  != -1)
              || ((ev.club.toLowerCase()).search(key) != -1)
              || ((ev.date.toLowerCase()).search(key)) != -1)
}

$(document).ready(function(){ wapp.initialize(); });

wapp.populateEvents = function(){
  $.ajax( "https://www.googleapis.com/storage/v1/b/rg-maps-raw/o/manifest.json",
				{ dataType: "json"
				, type : 'GET'
				, error: function(err, s1, s3) { console.log(err, s1, s3) }
				, success: function( data ) {
				$.getJSON(data.mediaLink, function(data){
          $.getJSON("https://www.googleapis.com/storage/v1/b/rg-maps-final-world-files/o",
            function(de_data){
              $.getJSON("https://www.googleapis.com/storage/v1/b/rg-maps-world-files/o",
                function(we_data){
                  done_events = (de_data['items'].concat(we_data['items'])).map(e => e['name'].slice(0, -4))
                  wapp.events = data.filter(function(item){
                  return done_events.indexOf(item['hash']) === -1 })
              wapp.setEvents() })}) })
        }})}



wapp.setEvents =  function(key) {
        if (key) {
          data = wapp.events.filter(ev => filterEvent(key, ev))
        } else { data = wapp.events }

        data = data.sort(function(){
                          return 0.5 - Math.random()
                        }).slice(0,10)
        var items = []
        $("#event-num").html("Remaining Events: " + wapp.events.length.toString())
        $("#event-list").html("")
    		$.each( data, function( key, val ) {
        	items.push( "<button class='raised-box search-item' id='" + key + "'>" + val.name + "</button>" );
          	});

            $( "<div/>", {
                  "class": "my-new-list",
                      html: items.join( "" )
						}).appendTo( "#event-list" );
        $.each(data, function(key, val) {
          $("#" + key).click(function(_){wapp.loadEvent(data[key])})
        })

  }

wapp.clearAll = function (){
  if (wapp.current){
    $("#instructions").show()
    $("#img").hide()
    $("#submit-info").prop('disabled', true)
    $("#name").html("")
    $("#club").html("")
    this.mapimg.removeLayer(wapp.current.sourceLayer.image)
    this.mapimg.removeLayer(wapp.current.sourceLayer.vector)
    this.mapimg.removeInteraction(wapp.current.sourceLayer.iclick)
		this.mapimg.getView().setRotation(0);

    this.map.removeLayer(wapp.current.destLayer.vector)
    this.map.removeLayer(wapp.current.destLayer.image)
    this.map.removeInteraction(wapp.current.destLayer.iclick)
  }}

wapp.loadEvent = function(ev){
  wapp.clearAll()
  wapp.load("map", ev.map_url)
  $("#name").html(ev.name)
  $("#club").html(ev.club)
  wapp.current.event = ev
}


/** Define the reference map
*/
wapp.setMap = function()
{	// Layers to draw on the map
	var layers =
	[
  new ol.layer.Group(
		{	name:"Base Layers",
			layers: [
				// OSM
				new ol.layer.Tile(
				{	name: "OSM",
					source: new ol.source.OSM(),
					baseLayer: true,
					visible: true
				}),
				new ol.layer.Tile(
				{	name: "OSM-TOPO",
          source: new ol.source.XYZ({
            url: '//{a-c}.tile.opentopomap.org/{z}/{x}/{y}.png'
          }),
					baseLayer: true,
					visible: false
				}),

			],
			baseLayer: true,
			openInLayerSwitcher: true
		}),
				new ol.layer.Tile(
				{	name: "RG-maps",
          source: new ol.source.XYZ({
            url: '//storage.googleapis.com/rg-maps/{z}/{x}/{-y}.png'
          }),
          opacity: 0.5,
					visible: true
				}),
	];

	// New map
	var map = this.map = new ol.Map.Geoportail
		({	target: 'map',
			key: apiKey,
			view: new ol.View
			({	zoom: 5,
				center: [-410696, 7097063]
			}),
			controls: ol.control.defaults().extend
			([	new ol.control.LayerSwitcher()
			]),
			layers: layers
		});


  var geocoder = new Geocoder('nominatim', {
    provider: 'osm',
    lang: 'en-GB',
    placeholder: 'Search for ...',
    targetType: 'text-input',
    limit: 5,
    keepOpen: false,
    preventDefault: true
  });
  this.map.addControl(geocoder);

  geocoder.on('addresschosen', function(evt) {
    if (evt.bbox){
      map.getView().fit(evt.bbox, { duration: 500 });
    } else {
      resolution = 2.388657133911758;
      duration = 500;
      map.getView().animate(
        { duration, resolution },
        { duration, center: evt.coordinate }
  );
    }

  });

	var mousePositionControl = new ol.control.MousePosition(
	{	coordinateFormat: ol.coordinate.createStringXY(4),
		undefinedHTML: ""
	});
	this.map.addControl(mousePositionControl);
};


/** Add a map for the image
*/
wapp.setImageMap = function()
{	// Map for the image
	var map = this.mapimg = new ol.Map.Geoportail
		({	target: 'img',
			view: new ol.View
			({	projection: pixelProjection,
				zoom: 7,
				center: [0,0]
			}),
			controls: ol.control.defaults( { rotate:false,  attribution:false } ),
			interactions: ol.interaction.defaults( { altShiftDragRotate:false, pinchRotate:false } )
		});


	this.mapimg.addControl(new ol.control.Toggle(
		{	'className': "ol-fullpage",
			toggleFn: function(b)
			{	$("body").toggleClass("fullpage");
				wapp.map.updateSize();
				wapp.mapimg.updateSize();
				wapp.synchro = false;
			}
		}));

	this.mapimg.addControl(new ol.control.Toggle(
		{	'className': "ol-info",
			html: "&phi;",
			toggleFn: function(b)
			{	var info = $("#info .inner textarea");
				$("#info").removeClass("hidden");
				if (!wapp.current.destLayer.image)
				{	info.val("No georef yet!")
					return;
				}
				var source = wapp.current.destLayer.image.getSource();
				var options =
					{	url: /^data/.test(source.getGeoImage().src) ? undefined : source.getGeoImage().src ,
						imageCenter: source.getCenter(),
						imageRotate: source.getRotation(),
						imageScale: source.getScale(),
            imagePos: wapp.getTopLeft(source),
						//crop: source.getCrop(),
						imageMask: source.getMask()
					};
				var source = wapp.current.destLayer.image.getSource();
        var tl = wapp.getTopLeft(source).getCoordinates()
        var scale = source.getScale()
        var world_file =
          { A: scale[0]
          , D: source.getRotation()
          , B: source.getRotation()
          , E: -scale[1]
          , C: tl[0]
          , F: tl[1]
          }
				info.val(JSON.stringify(world_file).replace(/\,"/g,",\n\""))
			}
		}));

	// Synchronize views
	map.getView().on("change:center", function(e)
	{	if (wapp.synchro) return;
		wapp.synchro = true;
		var pt = wapp.current.transform(e.target.getCenter());
		if (pt) wapp.map.getView().setCenter(pt)
		wapp.synchro = false;
	});

	var mousePositionControl = new ol.control.MousePosition(
	{	coordinateFormat: function(xy)
		{	if (wapp.imglayer && wapp.imglayer.getSource().imageSize)
			{	var p = [	xy[0] + wapp.imglayer.getSource().imageSize[0]/2,
							wapp.imglayer.getSource().imageSize[1]/2 - xy[1]
						];
				return (Math.round(10*p[0])/10)
					+", "+ (Math.round(10*p[1])/10);
			}
			return "";
		},
		undefinedHTML: ""
	});
	this.mapimg.addControl(mousePositionControl);
}


/** Load a new file
*/
wapp.load = function (name, dataURL)
{
  $("#img").show()
  $("#instructions").hide()
  $(".dialog").addClass("hidden");
	this.current = new wapp.img(name, dataURL, this.mapimg, this.map);
	$("#loading").removeClass("hidden");
	$("#loading img").attr('src', dataURL);
	// console.log(dataURL)
	wapp.current.sourceLayer.image.getSource().once ("change", function()
	{	$("#loading").addClass("hidden");
    wapp.mapimg.updateSize()
	});
};


