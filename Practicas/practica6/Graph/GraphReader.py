#!/usr/bin/env python
# -*- coding: utf-8 -*-

class GraphReader:
	"""
		Toma un archivo XML,JSON o CSV y regresa un objeto de la clase Graph 
		con el método toGraph.
	"""
	
	def toGraph(self,ruta):
		print ruta
	#end toGraph

#end GraphReader

ruta = raw_input('Ruta del archivo: ')
g = GraphReader()
g.toGraph(ruta)
