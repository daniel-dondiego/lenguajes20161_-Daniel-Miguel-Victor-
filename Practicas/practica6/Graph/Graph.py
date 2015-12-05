#!/usr/bin/env python
# -*- coding: utf-8 -*-

class Graph:
	"""
		Modela una gráfica.
	"""
	def __init__(self, v, e, d):
		self.V = v
		self.E = e
		self.D = d
	#end init

	def directed(self):
		"""
			Nos dice si la gráfica es dirigida o no.
		"""
		return self.D
	#end directed

	def vertices(self):
		"""
			Regresa todos los vértices de la gráfica.
		"""
		return self.V
	#end vertices

	def edges(self):
		"""
			Regresa todas las aristas de la gráfica.
		"""
		return self.E
	#end edges

	def has_cycles(self):
		"""
		Nos dice si tiene ciclos la gráfica.
		"""
		return false
	#end has_cycles

	def printGraphData(self):
		grafica="Directed: " + str(self.D)+"\nVértices: "
		for v in self.V.values():
			grafica+= v.tag().encode('utf-8')+","
		grafica+="\nAristas:"
		for e in self.E:
			grafica+="("+e.svertex().encode('utf-8')+","+e.tvertex().encode('utf-8')+","+e.weight().encode('utf-8')+")"
		return grafica

#end Graph