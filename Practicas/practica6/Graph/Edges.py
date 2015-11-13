#!/usr/bin/env python
# -*- coding: utf-8 -*-

import Vertex

class Edges:
	"""
		Modela una arista.
	"""
	
	def __init__(self, va, vb, peso):
		__verticeS = va
		__verticT = vb
		__weight = peso
	#end __init__

	def svertex(self):
		"""
			Regresa el vértice origen.
		"""
		return __verticeS
	#end svertex

	def tvertex(self):
		"""
			Regresa el vértice destino.
		"""
		return __verticT
	#end tvertex

	def weight(self):
		"""
			Regresa el peso de la arista.
		"""
		return __weight
	#end weight

#end Edges