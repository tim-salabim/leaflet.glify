LeafletWidget.methods.addGlifyPolylinesSrc = function(color, weight, opacity, group, layerId, popup) {

  var map = this;

  // color
  var clrs;
  if (color === null) {
    clrs = function(index, feature) { return col[group][0][index]; };
  } else {
    clrs = color;
  }

  // weight
  var wght;
  if (weight === null) {
    wght = function(index, feature) { return wgt[group][0][index]; };
  } else {
    wght = weight;
  }

  // popup
  var pop;
  if (popup && popups[group]) {
    pop = function(index, feature) { return popups[group][0][index]; };
  }


  var lineslayer = L.glify.lines({
    map: map,
    click: function (e, feature) {
      if (map.hasLayer(lineslayer.glLayer)) {
        var idx = data[group][0].features.findIndex(k => k==feature);
        if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue(map.id + "_glify_click", {
            id: layerId ? (Array.isArray(layerId) ? layerId[idx] : layerId) : idx+1,
            group: Object.values(lineslayer.glLayer._eventParents)[0].groupname,
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
    latitudeKey: 1,
    longitudeKey: 0,
    data: data[group][0],
    color: clrs,
    opacity: opacity,
    weight: wght,
    className: group
  });

  map.layerManager.addLayer(lineslayer.glLayer, "glify", null, group);

};
