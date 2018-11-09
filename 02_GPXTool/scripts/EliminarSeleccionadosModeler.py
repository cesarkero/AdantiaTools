##Modeler=group
##EliminarSeleccionadosModeler=name
##Layer=vector

layer = processing.getObject(Layer)
ids = [f.id() for f in layer.selectedFeatures()]
layer.dataProvider().deleteFeatures(ids)