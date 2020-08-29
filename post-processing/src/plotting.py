from matplotlib import pyplot as plt
from functools import reduce

def plot_category(vs, key, plt):
  total = reduce(lambda acc, x: acc + x, vs, 0)

def plot_group(g, i):
  keys, vs = list(g.keys()), list(g.values(), range(max()))
  pairs = list(zip(keys, vs))

  []
  

def main(data):
  [plot_group(data[i], i) for i in range(len(data))]