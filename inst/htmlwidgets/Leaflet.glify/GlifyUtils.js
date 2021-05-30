LeafletWidget.methods.clearGlLayers = function() {
  this.layerManager.clearLayers("glify");
};

LeafletWidget.methods.removeGlPolygons = function(layerId) {
  this.layerManager.removeLayer("glify", layerId);
};

LeafletWidget.methods.removeGlPolylines = function(layerId) {
  this.layerManager.removeLayer("glify", layerId);
};

LeafletWidget.methods.removeGlPoints = function(layerId) {
  this.layerManager.removeLayer("glify", layerId);
};
