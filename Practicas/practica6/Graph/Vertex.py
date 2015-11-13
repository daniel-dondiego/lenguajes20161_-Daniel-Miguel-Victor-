#!/usr/bin/env python
# -*- coding: utf-8 -*-

class Vertex:
	"""
		Modela un vértice
	"""
	def __init__(self, t, n, g):
		__tag = t
		__vecinos = n
		__grado = g;
	#end __init__

	def neighbours(self):
		"""
			Regresa los vecinos del vértice
		"""
		return __vecinos
	#end neighbours

	def degree(self):
		"""
			Regresa el grado del vértice
		"""
		return __grado
	#end degree	

#end Vertex