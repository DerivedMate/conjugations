#!/usr/bin/python3

import json
from functools import reduce


def read_data(path: str) -> object:
    decoder = json.JSONDecoder()

    f = open(path)
    res = decoder.decode(f.read())

    f.close()
    return res


def processGroup(acc: object, group: object):
    idd = group['id']
    if not (idd in acc):
        acc[idd] = []

    members = group['data']['members']
    acc[idd].append(len(members))

    return acc


def mapper(data, key):
    return reduce(processGroup, data[key], {})


def main():
    data = read_data("../out4.2.json")
    return dict([(key, mapper(data, key))
                 for key in ["l2", "l1", "l0"]
                 ])


if __name__ == "__main__":
    encoder = json.JSONEncoder()
    encoded = encoder.encode(main())
    print(encoded)
