#!/usr/bin/env python
# -*- coding: utf-8 -*-

class Vertex:
	"""
		Modela un vértice
	"""
	def __init__(self, t, n, g):
		self.__tag = t
		self.__vecinos = n
		self.__grado = g;
	#end __init__

	def tag(self):
		return self.__tag

	def neighbours(self):
		"""
			Regresa los vecinos del vértice
		"""
		return self.__vecinos
	#end neighbours

	def degree(self):
		"""
			Regresa el grado del vértice
		"""
		return self.__grado
	#end degree	

#end Vertex