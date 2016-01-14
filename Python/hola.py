#!/usr/bin/python
# -*- coding: utf-8 -*-

import math


def absValue(x):
  return -x if x<0 else x


def power(x, p):
  if p==0: return 1
  a = power(x, p/2)
  a *= a
  if p%2==1: a *= x
  return a


def isPrime(x):
  if x<=1: return False
  if x<=3: return True
  if x%2==0: return False
  for i in range(3, int(math.sqrt(x))+1, 2):
    if x%i==0: return False
  return True
  

def slowFib(n):
  if n==0: return 0
  elif n==1: return 1
  else: return slowFib(n-1)+slowFib(n-2)


def quickFib(n):
  a,b = 0,1
  for i in range(n):
    a,b = b,a+b
  return a







