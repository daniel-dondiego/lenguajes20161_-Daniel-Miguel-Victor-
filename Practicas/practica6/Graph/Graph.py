#!/usr/bin/env python
# -*- coding: utf-8 -*-

class Graph:
	"""
		Modela una gráfica.
	"""
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

	def has_cycles(self):
		"""
		Nos dice si tiene ciclos la gráfica.
		"""
		return false
	#end has_cycles

#end Graph