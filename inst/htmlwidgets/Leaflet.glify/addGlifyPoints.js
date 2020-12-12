LeafletWidget.methods.addGlifyPoints = function(data, cols, popup, opacity, radius, group, layerId,
                                                hover, hoverWait, sensitivity, sensitivityHover, pane) {

  const map = this;

  // colors
  var clrs;
  if (cols.length === 1) {
    clrs = cols[0];
  } else {
    clrs = function(index, point) { return cols[index]; };
  }

  // radius
  var rad;
  if (typeof(radius) === "number") {
    rad = radius;
  } else {
    rad = function(index, point) { return radius[index]; };
  }

/*
  var pop;
  if (popup) {
      if (popup === true) {
        pop = function (e, feature) {
          var popUp = '<pre>'+JSON.stringify(feature.properties,null,' ').replace(/[\{\}"]/g,'')+'</pre>';
          if (map.hasLayer(pointslayer.layer)) {
            L.popup({ maxWidth: 2000 })
              .setLatLng(e.latlng)
              .setContent(popUp)
              .openOn(map);
          }
        };
      } else {
        pop = function (e, feature) {
          if (map.hasLayer(pointslayer.layer)) {
            L.popup({ maxWidth: 2000 })
              .setLatLng(e.latlng)
              .setContent(feature.properties[[popup]].toString())
              .openOn(map);
          }
        };
      }
  } else {
      pop = null;
  }

  var pointslayer = L.glify.points({
    map: map,
    click: pop,
    data: data,
    color: clrs,
    opacity: opacity,
    size: size,
    className: group
  });

  map.layerManager.addLayer(pointslayer.layer, null, null, group);
*/

  var mouse_event_pts = function(e, point, addpopup, popup, event) {
    var etype = event === "hover" ? "_glify_mouseover" : "_glify_click";
    if (map.hasLayer(pointslayer.layer)) {
      var idx = data.findIndex(k => k==point);
      var content = popup ? popup[idx].toString() : null;
      if (HTMLWidgets.shinyMode) {
        Shiny.setInputValue(map.id + etype, {
          id: layerId ? layerId[idx] : idx+1,
          lat: point[0],
          lng: point[1],
          data: content
        });
      }
      if (addpopup) {
        var pops = L.popup({ maxWidth: 2000 })
            .setLatLng(e.latlng)
            .setContent(content);
        map.layerManager.removeLayer("leafglpopups");
        map.layerManager.addLayer(pops, "popup", "leafglpopups");
      }
    }
  }
  var pop = function (e, point, xy) {
    mouse_event_pts(e, point, popup !== null, popup, "click");
  };
  var hov = function (e, point, xy) {
    mouse_event_pts(e, point, hover !== null, hover, "hover");
  };

  var pointslayer = L.glify.points({
    map: map,
    click: pop,
    hover: hov,
    hoverWait: hoverWait,
    sensitivityHover: sensitivityHover,
    sensitivity: sensitivity,
    data: data,
    color: clrs,
    opacity: opacity,
    size: rad,
    className: group,
    pane: pane
  });

  map.layerManager.addLayer(pointslayer.layer, "glify", layerId, group);
};


LeafletWidget.methods.removeGlPoints = function(layerId) {
  this.layerManager.removeLayer("glify", layerId);
};

LeafletWidget.methods.clearGlLayers = function() {
  this.layerManager.clearLayers("glify");
};