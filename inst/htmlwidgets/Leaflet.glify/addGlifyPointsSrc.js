LeafletWidget.methods.addGlifyPointsSrc = function(fillColor, radius, fillOpacity, group, layerId, popup) {

  var map = this;

  // color
  var clrs;
  if (fillColor === null) {
    clrs = function(index, feature) { return col[group][0][index]; };
  } else {
    clrs = fillColor;
  }

  // radius
  var size;
  if (radius === null) {
    size = function(index, point) { return rad[group][0][index]; };
  } else {
    size = radius;
  }

  // popup
  var pop;
  if (popup && popups[group]) {
    pop = function(index, feature) { return popups[group][0][index]; };
  }

  var pointslayer = L.glify.points({
    map: map,
    click: function (e, point, xy) {
      if (map.hasLayer(pointslayer.glLayer)) {
        var idx = data[group][0].findIndex(k => k==point);
        if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue(map.id + "_glify_click", {
            id: layerId ? (Array.isArray(layerId) ? layerId[idx] : layerId) : idx+1,
            group: Object.values(pointslayer.glLayer._eventParents)[0].groupname,
            lat: e.latlng.lat,
            lng: e.latlng.lng,
            data: point
          });
        }
        if (pop !== undefined) {
          L.popup()
            .setLatLng(point)
            .setContent(popups[group][0][idx].toString())
            .openOn(map);
        }
      }
    },
    data: data[group][0],
    color: clrs,
    opacity: fillOpacity,
    size: size,
    className: group
  });

  map.layerManager.addLayer(pointslayer.glLayer, "glify", null, group);

};


/*
LeafletWidget.methods.addGlifyPointsSrc2 = function(group, opacity, size, layerId) {

  var map = this;
  var grp1 = group + "_1";
  var grp2 = group + "_2";
  //data[grp1][0] = data[grp1][0].concat(data[grp2][0]);
    var cols = col[group][0];
    var clrs;
    if (cols.length === 1) {
      clrs = cols[0];
    } else {
      clrs = function(index, feature) { return cols[index]; };
    }
    var pointslayer = L.glify.points({
      map: map,
      click: function (e, point, xy) {
        //var idx = data[group][0].indexOf(point);
        var idx = data[group][0].findIndex(k => k==point);
        //set up a standalone popup (use a popup as a layer)
        if (map.hasLayer(pointslayer.glLayer)) {
          L.popup()
            .setLatLng(point)
            .setContent(popup[group][0][idx].toString())
            .openOn(map);
        }

        console.log(point);

      },
      data: data[grp1][0],
      color: clrs,
      opacity: opacity,
      size: size,
      className: group
    });

  map.layerManager.addLayer(pointslayer.glLayer, "glify", layerId, group);

  function add() {
        if (typeof data[grp2] === 'undefined') {
            setTimeout(function () {
                add();
            }, 0.1);
        } else {
            var pointslayer2 = L.glify.points({
              map: map,
              click: function (e, point, xy) {
                //var idx = data[group][0].indexOf(point);
                var idx = data[group][0].findIndex(k => k==point);
                //set up a standalone popup (use a popup as a layer)
                if (map.hasLayer(pointslayer.glLayer)) {
                  L.popup()
                    .setLatLng(point)
                    .setContent(popup[group][0][idx].toString())
                    .openOn(map);
                }

                console.log(point);

              },
              data: data[grp2][0],
              color: clrs,
              opacity: opacity,
              size: size,
              className: group
            });

           map.layerManager.addLayer(pointslayer2.glLayer, null, null, group);
        }
    }

   add();

};
*/
