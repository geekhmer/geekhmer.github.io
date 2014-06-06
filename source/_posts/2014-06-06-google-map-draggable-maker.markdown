---
layout: post
title: "Google Map Draggable Maker"
date: 2014-06-06 12:05
categories: Javascript
keywords: Google map, Google map draggable maker
---

<p>
  Google’s Maps API offer a vast range of functionality to interact with underlying maps. As opposed to statically setting markers (pins) programmatically sometimes you might want your users to be able to interact with the map by dragging existing markers around the map. Fortunately, Google’s JavaScript API offers this functionality out of the box. Below you’ll find an example based on Google Maps API V3.
</p>

<p> 
  <a class="fancybox" href="/images/google_map_draggable_maker.png"><img src="/images/google_map_draggable_maker.png" /></a>
</p>

<p>
  <strong>Practice</strong><br/>
</p>

<p>
  google_map_draggable_maker.js is a small library use to make google draggable easily:
</p>
{% codeblock google_map_draggable_maker.js lang:javascript %}
  MapDraggableMarker = function(element, lat, lng, zoom, coordsLenght, elementLat, elementLng) {
    this.element = element;
    this.lat = lat;
    this.lng = lng;
    this.zoom = zoom;
    this.coordsLenght = coordsLenght;
    this.elementLat = elementLat;
    this.elementLng = elementLng;

    this.map = new google.maps.Map(element, {
      zoom: this.zoom,
      center: new google.maps.LatLng(this.lat, this.lng),
      mapTypeId: google.maps.MapTypeId.ROADMAP
    });
    this.marker = new google.maps.Marker({
      position: new google.maps.LatLng(this.lat, this.lng),
      draggable: true
    });
  }

  MapDraggableMarker.prototype.addListenerToMarker = function() {
    var self = this;
    google.maps.event.addListener(this.marker, 'dragend', function(evt) {
      self.elementLat.val(evt.latLng.lat().toFixed(self.coordsLenght));
      self.elementLng.val(evt.latLng.lng().toFixed(self.coordsLenght));
    });
  }

  MapDraggableMarker.prototype.init = function() {
    this.addListenerToMarker();
    this.map.setCenter(this.marker.position);
    this.marker.setMap(this.map);
  }
{% endcodeblock %}

<p>
  index.html is the sample google map draggable:
</p>

{% codeblock index.html lang:javascript %}
<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <title></title>
  <link rel="stylesheet" href="styles.css" />
  <script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false"></script>
  <script type="text/javascript" src="jquery-1.11.1.min.js"></script>
  <script type="text/javascript" src="google_map_draggable.js"></script>
  <script type="text/javascript">
    $(document).ready(function() {
      var lat = ($("#latitude").val() == "") ? 11.558831 : $("#latitude").val();
      var lng = ($("#longitude").val() == "") ? 104.917445 : $("#longitude").val();
      var mapDraggableMarker = new MapDraggableMarker($("#canvas")[0], lat, lng, 15, 6, $("#latitude"), $("#longitude"));
      mapDraggableMarker.init();
    });
  </script>
</head>
<body>
  <div id="canvas" style="width: 635px; height: 300px;"></div><br />
  <label for="latitude">Latitude:</label>
  <input id="latitude" type="text" value="" />
  <label for="longitude">Longitude:</label>
  <input id="longitude" type="text" value="" />
</body>
</html>
{% endcodeblock %}

<p>
  You can download the <a href="https://github.com/Bunlong/google_map_draggable_maker" target="_blank">source code</a> and try it out.
</p>