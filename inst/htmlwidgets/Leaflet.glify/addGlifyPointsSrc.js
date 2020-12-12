LeafletWidget.methods.addGlifyPointsSrc = function(fillColor, radius, fillOpacity, group, layerId,
                                                   hover, hoverWait, sensitivity, sensitivityHover, pane) {

  var map = this;

  // color
  var clrs;
  if (fillColor === null) {
    clrs = function(index, feature) { return col[layerId][0][index]; };
  } else {
    clrs = fillColor;
  }

  // radius
  var size;
  if (radius === null) {
    size = function(index, point) { return rad[layerId][0][index]; };
  } else {
    size = radius;
  }

  var mouse_event = function(e, point, addpopup, popup, event) {
    var etype = event === "hover" ? "_glify_mouseover" : "_glify_click"
    if (map.hasLayer(pointslayer.layer)) {
      var idx = data.findIndex(k => k==point);
      if (HTMLWidgets.shinyMode) {
        var content = popup ? popup[idx].toString() : null;
        Shiny.setInputValue(map.id + etype, {
          id: layerId ? layerId[idx] : idx+1,
          lat: point[0],
          lng: point[1],
          data: content
        });
      }
      if (addpopup) {
        var content = popup === true ? '<pre>'+JSON.stringify(point,null,' ').replace(/[\{\}"]/g,'')+'</pre>' : popup[idx].toString();
        var pops = L.popup({ maxWidth: 2000 })
            .setLatLng(e.latlng)
            .setContent(content);
        map.layerManager.removeLayer("leafglpopups");
        map.layerManager.addLayer(pops, "popup", "leafglpopups");
      }
    }
  }
  var pop = function (e, point, xy) {
    mouse_event(e, point, popup !== null, popup, "click");
  };
  var hov = function (e, point, xy) {
    mouse_event(e, point, hover !== null, hover, "hover");
  };

  var pointslayer = L.glify.points({
    map: map,
    click: pop,
    hover: hov,
    hoverWait: hoverWait,
    sensitivityHover: sensitivityHover,
    sensitivity: sensitivity,
    data: data[layerId][0],
    color: clrs,
    opacity: fillOpacity,
    size: size,
    className: group,
    pane: pane
  });

  map.layerManager.addLayer(pointslayer.layer, "glify", layerId, group);

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
        if (map.hasLayer(pointslayer.layer)) {
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

  map.layerManager.addLayer(pointslayer.layer, "glify", layerId, group);

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
                if (map.hasLayer(pointslayer.layer)) {
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

           map.layerManager.addLayer(pointslayer2.layer, null, null, group);
        }
    }

   add();

};
*/
