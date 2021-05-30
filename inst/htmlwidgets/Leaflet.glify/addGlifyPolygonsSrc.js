LeafletWidget.methods.addGlifyPolygonsSrc = function(fillColor, fillOpacity, group, layerId, popup) {

  var map = this;

// FIX ME clrs, pop need to be layer specificly named!!!!!

  // color
  var clrs;
  if (fillColor === null) {
    clrs = function(index, feature) { return col[group][0][index]; };
  } else {
    clrs = fillColor;
  }

  // popup
  var pop;
  if (popup && popups[group]) {
    pop = function(index, feature) { return popups[group][0][index]; };
  }

  var shapeslayer = L.glify.shapes({
    map: map,
    click: function (e, feature) {
      if (map.hasLayer(shapeslayer.glLayer)) {
          var idx = data[group][0].features.findIndex(k => k==feature);
          if (HTMLWidgets.shinyMode) {
            Shiny.setInputValue(map.id + "_glify_click", {
              id: layerId ? (Array.isArray(layerId) ? layerId[idx] : layerId) : idx+1,
              group: Object.values(shapeslayer.glLayer._eventParents)[0].groupname,
              lat: e.latlng.lat,
              lng: e.latlng.lng,
              data: feature.properties
            });
          }
          if (pop !== undefined) {
            L.popup()
              .setLatLng(e.latlng)
              .setContent(popups[group][0][idx].toString())
              .openOn(map);
          }
      }
    },
    data: data[group][0],
    color: clrs,
    opacity: fillOpacity,
    className: group
  });

  map.layerManager.addLayer(shapeslayer.glLayer, "glify", null, group);

};
