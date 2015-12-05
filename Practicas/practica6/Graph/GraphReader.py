#!/usr/bin/env python
# -*- coding: utf-8 -*-

class GraphReader:
	"""
		Toma un archivo XML,JSON o CSV y regresa un objeto de la clase Graph 
		con el método toGraph.
	"""
	
	def toGraph(self,ruta):
		if ruta.endswith('.xml'):
			return self.__xmlToGraph(ruta)
		if ruta.endswith('.json'):
			return self.__jsonToGraph(ruta)
		if ruta.endswith('.csv'):
			return self.__csvToGraph(ruta)
		return None
	#end toGraph


	def __xmlToGraph(self, ruta):
		print 'xml'
		return None
	#end

	def __jsonToGraph(self, ruta):
		try:
            json_graph=open(arch)
            g = json.load(json_graph)
            vertix = []
            edg = {}
            for v in g['vertices']:
                vertix.append(v)
            for e in g['edges']:
                edg[j[0]]= [j[1], j[2]]
            json_graph.close()
            return Graph(vertix, edg, (int(g['direct']) == 1))
        except:
            print("El archivo JSON se encuentra corrupto")

	#end

	def __csvToGraph(self, ruta):
		print 'csv'
		return None
	#end

#end GraphReader

ruta = raw_input('Ruta del archivo: ')
g = GraphReader()
g.toGraph(ruta)
