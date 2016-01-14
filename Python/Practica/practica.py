#!/usr/bin/python
# -*- coding: utf-8 -*-

import urllib
import xml.etree.ElementTree as ET
import re
import csv
import math
import webbrowser


# retorna el text del node apuntat pel path (o "" si no te text)
def text_str(text):              return "" if text == None else text.encode('utf-8').strip()
def find_text(node, path):       return text_str(node.find(path).text)
def find_attr(node, path, attr): return text_str(node.find(path).attrib[attr])


# adresa i (lat, lon)
class GeoLoc(object):
  # constructor
  def __init__(self, adresa, lat, lon):
    self.adresa = adresa.strip()
    self.lat = lat.strip()
    self.lon = lon.strip()
    
  # valida si te coordenades definides
  def valid(self): return self.lat!="" and self.lon!=""



# classe activitat
class Activitat(GeoLoc):  
  # constructor
  def __init__(self, act):
    # superclasse
    carrer = find_text(act, "./lloc_simple/adreca_simple/carrer")
    num = find_text(act, "./lloc_simple/adreca_simple/numero")
    cp = find_text(act, "./lloc_simple/adreca_simple/codi_postal")
    muni = find_text(act, "./lloc_simple/adreca_simple/municipi")
    adresa = carrer + ', ' + num + '<br/>(' + cp + ', ' + muni + ')'
    lat = find_attr(act, "./lloc_simple/adreca_simple/coordenades/googleMaps", "lat")
    lon = find_attr(act, "./lloc_simple/adreca_simple/coordenades/googleMaps", "lon")
    super(Activitat, self).__init__(adresa, lat, lon)
    # own stuff
    self.nom = find_text(act, "./nom")
    self.lloc = find_text(act, "./lloc_simple/nom")
    self.barri = find_text(act, "./lloc_simple/adreca_simple/barri")
    self.data = find_text(act, "./data/data_proper_acte")
  
  # return True if the activity satisfies the given expression
  def eval_expression(self, expr):
    if isinstance(expr, str):
      # base case
      search = lambda arg: re.search(expr, arg, re.IGNORECASE)
      return search(self.nom) or search(self.lloc) or search(self.barri)
    elif isinstance(expr, tuple):
      # AND expression
      return all(self.eval_expression(subexpr) for subexpr in expr)
    elif isinstance(expr, list):
      # OR expression
      return any(self.eval_expression(subexpr) for subexpr in expr)
    return False



# classe bicing
class Bicing(GeoLoc):  
  # constructor
  def __init__(self, bic):
    street = find_text(bic, "./street")
    street_num = find_text(bic, "./streetNumber")
    adresa = street + ', ' + street_num
    lat = find_text(bic, "./lat")
    lon = find_text(bic, "./long")
    super(Bicing, self).__init__(adresa, lat, lon)
    self.status = find_text(bic, "./status")
    self.slots = find_text(bic, "./slots")
    self.bikes = find_text(bic, "./bikes")
  
  def oberta(self):   return self.status == "OPN"
  def te_lloc(self):  return self.slots!="0"
  def te_bicis(self): return self.bikes!="0"



# classe transport (busos i metros i ferros i trams)
class Transport(GeoLoc):
  
  # constructor
  def __init__(self, line):
    super(Transport, self).__init__(line[8], line[4], line[5])
    ind = line[6].index('-')
    raw_tipus = line[6][0:ind].strip()
    if raw_tipus=="BUS" or raw_tipus=="NITBUS":
      self.tipus = raw_tipus
      self.linies = [ a for a in [ b.strip() for b in line[6][ind+1:].split('-') ] if a!="" ]
      self.sortides = []
    else:
      if raw_tipus=="TRAMVIA BLAU":
        self.tipus = raw_tipus
        self.linies = [ "Tram blau" ]
      else:
        sep = raw_tipus.index('(')
        self.tipus = raw_tipus[0:sep].strip()
        self.linies = [ a for a in [ b.strip() for b in raw_tipus[sep+1:len(raw_tipus)-1].split(',') ] if a!="" ]
      self.sortides = [ a for a in [ b.strip() for b in line[6][ind+1:].split('-') ] if a!="" ]
  
  # consultes tipus
  def bus_diurn(self):   return self.tipus=="BUS"
  def bus_nocturn(self): return self.tipus=="NITBUS"  
  def metro(self):       return self.tipus=="METRO"
  def fgc(self):         return self.tipus=="FGC"
  def tram(self):        return self.startswith("TRAMVIA")



# torna l'arbre xml que hi ha a una url
def get_xml(url):
  con = urllib.urlopen(url)
  xml = ET.fromstring(con.read())
  con.close()
  return xml

# torna la llista de transports d'un fitxer CSV
def get_transports(path):
  ifile  = open(path, 'r')
  reader = csv.reader(ifile, delimiter=';')
  reader.next()
  trans = [ t for t in map(Transport, reader) if t.valid() ]
  ifile.close()
  return trans


# graus a radians
DRAD = math.pi/180.0  # radians en 1 grau
ERAD = 6371000.0      # radi terra en metres
def deg2rad(deg): return deg*DRAD

# retorn a la distancia (en metres) entre 2 GeoLoc
def get_distance(geo_loc1, geo_loc2):
  lat1, lon1 = float(geo_loc1.lat), float(geo_loc1.lon)
  lat2, lon2 = float(geo_loc2.lat), float(geo_loc2.lon)
  phi1, phi2 = deg2rad(90.0-lat1), deg2rad(90.0-lat2)
  the1, the2 = deg2rad(lon1), deg2rad(lon2)
  cos = math.sin(phi1)*math.sin(phi2)*math.cos(the1-the2)+math.cos(phi1)*math.cos(phi2)
  return math.acos(cos)*ERAD


# given a GeoLoc 'act', and a list of GeoLoc objects
# returns a list of pairs [d, g], where d is the distance from 'act' to 'g' and is <= 500.0, and 'g' is the GeoLoc
def sorted_geo_loc(act, geo_locs):
  geo_locs_tmp = [ [ get_distance(act, g), g] for g in geo_locs ]
  return sorted([ a for a in geo_locs_tmp if a[0]<=500.0 ], key=lambda arg: arg[0])


# torna un anchor html amb el text donat, per accedir a la pagina de google maps que mostra les coordenades d'un geo_loc
def link_to(txt, geo_loc):
  gmaps_anchor = "<a href='http://www.google.com/maps/place/{},{}/@{},{},16z' target='_blank'>"
  return gmaps_anchor.format(geo_loc.lat, geo_loc.lon, geo_loc.lat, geo_loc.lon)+txt+"</a>"



# classe encarregada de construir la taula html, i escriure-la en un fitxer
class TaulaHtml(object):
  # constructor
  def __init__(self, acts, bicis, bus_dia, bus_nit, trens, altres):
    self.acts = acts
    self.bicis = bicis
    self.bus_dia = bus_dia 
    self.bus_nit = bus_nit
    self.trens = trens
    self.altres = altres

  # escriu una fila de la taula, corresponent a una activitat
  def escriu_fila_taula_html(self, act, ifile, even):
    
    # ordena les llistes per aquesta activitat
    bicis_act = sorted_geo_loc(act, self.bicis)
    bus_dia_act = sorted_geo_loc(act, self.bus_dia)
    bus_nit_act = sorted_geo_loc(act, self.bus_nit)
    trens_act = sorted_geo_loc(act, self.trens)
    altres_act = sorted_geo_loc(act, self.altres)
    
    # bicings amb lloc
    i, cnt = 0, 0
    bicis_lloc = []
    while i<len(bicis_act) and cnt<5:
      if bicis_act[i][1].te_lloc():
        bicis_lloc.append(bicis_act[i][1])
        cnt += 1
      i += 1
    # bicings amb bicis
    i, cnt = 0, 0
    bicis_bici = []
    while i<len(bicis_act) and cnt<5:
      if bicis_act[i][1].te_bicis():
        bicis_bici.append(bicis_act[i][1])
        cnt += 1
      i += 1
    
    # transports
    bus_dia = [ bus_dia_act[0][1] ] if len(bus_dia_act)>0 else []
    bus_nit = [ bus_nit_act[0][1] ] if len(bus_nit_act)>0 else []
    trens = [ trens_act[0][1] ] if len(trens_act)>0 else []
    altres = []
    # indices i contador
    i, j, k, l = len(bus_dia), len(bus_nit), len(trens), 0
    cnt = i+j+k
    # merge ben feo
    transport_invalid = lambda tra, lst: any([ b in t.linies for b in tra.linies for t in lst ])
    while (i<len(bus_dia_act) or j<len(bus_nit_act) or k<len(trens_act) or l<len(altres_act)) and cnt<10:
      # avançar indices
      while i<len(bus_dia_act) and transport_invalid(bus_dia_act[i][1], bus_dia): i += 1
      while j<len(bus_nit_act) and transport_invalid(bus_nit_act[j][1], bus_nit): j += 1
      while k<len(trens_act)   and transport_invalid(trens_act[k][1], trens):     k += 1
      while l<len(altres_act)  and transport_invalid(altres_act[l][1], altres):   l += 1
      # troba millor
      best, dis = -1, -1.0
      if i<len(bus_dia_act):                                         best, dis = 0, bus_dia_act[i][0]
      if j<len(bus_nit_act) and (best==-1 or dis>bus_nit_act[j][0]): best, dis = 1, bus_nit_act[j][0]
      if k<len(trens_act)   and (best==-1 or dis>trens_act[k][0]):   best, dis = 2, trens_act[k][0]
      if l<len(altres_act)  and (best==-1 or dis>altres_act[l][0]):  best = 3
      # afegeix millor
      if best==0:
        bus_dia.append(bus_dia_act[i][1])
        i += 1
      elif best==1:
        bus_nit.append(bus_nit_act[j][1])
        j += 1
      elif best==2:
        trens.append(trens_act[k][1])
        k += 1
      elif best==3:
        altres.append(altres_act[l][1])
        l += 1
      cnt += 1
    transports = [ t for sublist in [ bus_dia, bus_nit, trens, altres ] for t in sublist ]
    
    # escriure l'html 8P; primer les dades de l'activitat
    rows = max(max(max(1, len(bicis_lloc)), len(bicis_bici)), len(transports))
    bg_color = "#ddd" if even else "#eee"
    ifile.write("""      <tr style="background-color:{};border-bottom:1px solid black">
        <td rowspan="{}" style="border-bottom:1px solid black">{}</td>
        <td rowspan="{}" style="border-bottom:1px solid black">{}</td>
        <td rowspan="{}" style="border-bottom:1px solid black">{}</td>
      </tr>
""".format(bg_color, rows+1,act.nom, rows+1,link_to(act.adresa, act), rows+1,act.data))
    # i ara els bicings i els transports
    linies_str = lambda t: reduce(lambda x,y: x+", "+y, t.linies)
    pretty_tipus = lambda t: t if t=="FGC" else ("Tram" if t=="TRAMVIA" else ("Tram blau" if t=="TRAMVIA BLAU" else t[0]+t[1:].lower()))
    i = 0
    while i<rows:
      ifile.write("      <tr style='background-color:{}'>\n".format(bg_color if i%2==0 else "#fff"))
      ifile.write("        <td>{}</td>\n".format(link_to(bicis_lloc[i].adresa, bicis_lloc[i]) if i<len(bicis_lloc) else ""))
      ifile.write("        <td style='text-align:center'>{}</td>\n".format(bicis_lloc[i].slots if i<len(bicis_lloc) else ""))
      ifile.write("        <td>{}</td>\n".format(link_to(bicis_bici[i].adresa, bicis_bici[i]) if i<len(bicis_bici) else ""))
      ifile.write("        <td style='text-align:center'>{}</td>\n".format(bicis_bici[i].bikes if i<len(bicis_bici) else ""))
      ifile.write("        <td>{}</td>\n".format(pretty_tipus(transports[i].tipus) if i<len(transports) else ""))
      ifile.write("        <td style='text-align:center'>{}</td>\n".format(link_to(linies_str(transports[i]) if len(transports[i].linies)>0 else "T. blau", transports[i]) if i<len(transports) else ""))
      ifile.write("      </tr>\n")
      i += 1

  # escriu la taula html d'activitats en el fitxer 'table.html'
  def escriu(self):
    file_name = "table.html"
    ifile = open(file_name, "w")
    ifile.write("""<!DOCTYPE html>
<html>
  <head>
    <title>Practica Python LP - Activitats</title>
    <meta charset="UTF-8" />
    <style>
      html { font-family:Arial }
      table { border-top:1px solid black;border-bottom:1px solid black }
      th { font-size:20px;color:#fff;border-left:1px solid black;border-right:1px solid black;border-bottom:1px solid black }
      td { font-size:12px;border-left:1px solid black;border-right:1px solid black;padding:5px }
    </style>
  </head>
  <body>
    <table style="width:100%;border-collapse:collapse">
      <colgroup style="width:20%"></colgroup>
      <colgroup></colgroup>
      <colgroup></colgroup>
      <colgroup span="2"></colgroup>
      <colgroup span="2"></colgroup>
      <colgroup span="2"></colgroup>
      <tr style="background-color:#88f">
        <th rowspan="2" style="padding-top:20px;padding-bottom:20px">Nom activitat</th>
        <th rowspan="2">Adreça</th>
        <th rowspan="2">Data</th>
        <th colspan="2">Bicings amb lloc</th>
        <th colspan="2">Bicings amb bicis</th>
        <th colspan="2">Transports públics</th>
      </tr>
      <tr style="background-color:#88f">
        <th style="font-size:16px">Adreça</th>
        <th style="font-size:16px">Llocs</th>
        <th style="font-size:16px">Adreça</th>
        <th style="font-size:16px">Bicis</th>
        <th style="font-size:16px">Tipus</th>
        <th style="font-size:16px">Línies</th>
      </tr>
""")
    even = True
    for act in self.acts:
      self.escriu_fila_taula_html(act, ifile, even)
      even = not even
    ifile.write("""    </table>
  </body>
</html>""")
    ifile.close()
    webbrowser.open(file_name)
  
    

# funcio d'entrada si executem el fitxer com 1 programa 
if __name__ == '__main__':
  
  # llegir expressio
  from sys import argv
  from ast import literal_eval
  expr = literal_eval(argv[1])
  
  # fer la consulta d'activitats
  xml = get_xml("http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=199")
  acts = [ a for a in map(Activitat, xml.iter("acte")) if a.valid() and a.eval_expression(expr) ]
  
  # llegir bicings
  xml = get_xml("http://wservice.viabicing.cat/v1/getstations.php?v=1")
  bicis = [ b for b in map(Bicing, xml.iter("station")) if b.valid() and b.oberta() ]
  
  # llegir busos
  busos = get_transports("Documents/ESTACIONS_BUS.csv")
  bus_dia = [ b for b in busos if b.bus_diurn() ]
  bus_nit = [ b for b in busos if b.bus_nocturn() ]
  
  # llegir trens
  transports = get_transports("Documents/TRANSPORTS.csv")
  trens = [ b for b in transports if b.metro() or b.fgc() ]
  altres = [ b for b in transports if not (b.metro() or b.fgc()) ]
  
  # escriu html
  TaulaHtml(acts, bicis, bus_dia, bus_nit, trens, altres).escriu()





















