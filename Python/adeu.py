#!/usr/bin/python
# -*- coding: utf-8 -*-

import math


def myLength(L):
  return len(L)



def myMaximum(L):
  return max(L)



def average(L):
  val = 0.0
  for x in L: val += x
  return val/len(L)



def buildPalindrome(L):
  return list(reversed(L)) + L



def remove(L1, L2):
  return [x for x in L1 if x not in L2]



def flatten(L):
  if isinstance(L, list):
    l = []
    for x in L: l += flatten(x)
    return l
  else: return [L]


def oddsNevens(L):
  o,e = [],[]
  for x in L:
    if x%2==0: e.append(x)
    else: o.append(x)
  return (o,e)


def isPrime(x):
  if x<=1: return False
  if x<=3: return True
  if x%2==0: return False
  for i in range(3, int(math.sqrt(x))+1, 2):
    if x%i==0: return False
  return True

def primeDivisors(n):
  divisors = [ d for d in range(2,n//2+1) if n%d==0 ]
  prime_divisors = [ d for d in divisors if isPrime(d) ]
  if isPrime(n): prime_divisors.append(n)
  return prime_divisors






