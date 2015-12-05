#!/usr/bin/env python
# -*- coding: utf-8 -*-

import Vertex

class Edge:
	"""
		Modela una arista.
	"""
	
	def __init__(self, va, vb, peso):
		self.__verticeS = va
		self.__verticT = vb
		self.__weight = peso
	#end __init__

	def svertex(self):
		"""
			Regresa el vértice origen.
		"""
		return self.__verticeS
	#end svertex

	def tvertex(self):
		"""
			Regresa el vértice destino.
		"""
		return self.__verticT
	#end tvertex

	def weight(self):
		"""
			Regresa el peso de la arista.
		"""
		return self.__weight
	#end weight

#end Edges