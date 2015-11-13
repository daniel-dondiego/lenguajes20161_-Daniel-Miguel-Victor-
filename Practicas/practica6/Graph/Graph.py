#!/usr/bin/env python
# -*- coding: utf-8 -*-

class Graph:
	"""

	"""
	V = {}
	E = {}

	def __init__(self, v, e):
		V = v
		E = e
	#end init

	def directed(self):
		"""
			Nos dice si la gráfica es dirigida o no.
		"""
		return false
	#end directed

	def vertices(self):
		"""
			Regresa todos los vértices de la gráfica.
		"""
		return V
	#end vertices

	def edges(self):
		"""
			Regresa todas las aristas de la gráfica.
		"""
		return E
	#end edges


#end Graph