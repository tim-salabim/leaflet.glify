LeafletWidget.methods.addGlifyPolygons = function(data, cols, popup, opacity, group, layerId, border, hover, hoverWait, pane) {

  var map = this;

  var clrs;
  if (cols.length === 1) {
    clrs = cols[0];
  } else {
    clrs = function(index, feature) { return cols[index]; };
  }

  var mouse_event = function(e, feature, addpopup, popup, event) {
    var etype = event === "hover" ? "_glify_mouseover" : "_glify_click";
    if (map.hasLayer(shapeslayer.layer)) {
      var idx = data.features.findIndex(k => k==feature);
      if (HTMLWidgets.shinyMode) {
        Shiny.setInputValue(map.id + etype, {
          id: layerId ? layerId[idx] : idx+1,
          group: Object.values(shapeslayer.layer._eventParents)[0].groupname,
          lat: e.latlng.lat,
          lng: e.latlng.lng,
          data: feature.properties
        });
      }
      if (addpopup) {
        var content = popup === true ? '<pre>'+JSON.stringify(feature.properties,null,' ').replace(/[\{\}"]/g,'')+'</pre>' : popup[idx].toString();
        var pops = L.popup({ maxWidth: 2000 })
            .setLatLng(e.latlng)
            .setContent(content);
        map.layerManager.removeLayer("leafglpopups");
        map.layerManager.addLayer(pops, "popup", "leafglpopups");
      }
    }
  }
  var pop = function (e, feature) {
    mouse_event(e, feature, popup !== null, popup, "click");
  };
  var hov = function (e, feature) {
    mouse_event(e, feature, hover !== null, hover, "hover");
  };

  var shapeslayer = L.glify.shapes({
    map: map,
    click: pop,
    hover: hov,
    hoverWait: hoverWait,
    data: data,
    color: clrs,
    opacity: opacity,
    border: border,
    className: group,
    pane: pane
  });

  map.layerManager.addLayer(shapeslayer.layer, "glify", layerId, group);
};


LeafletWidget.methods.removeGlPolygons = function(layerId) {
  this.layerManager.removeLayer("glify", layerId);
};