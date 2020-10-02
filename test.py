#!/usr/bin/python

import urllib.request, json, pyjq
#import checker
from pytezos.rpc import mainnet
import dateutil
from datetime import datetime

def btc_xtz():
    def extract_timestamp_and_rate(key, data):
        query = '.[] | select(.data.key.value == "%s") | {timestamp: .data.value.children[1].value, value: .data.value.children[5].value}' % (key)
        return pyjq.first(query,data)

    url = urllib.request.urlopen("https://api.better-call.dev/v1/bigmap/mainnet/68/keys")
    data = json.loads(url.read().decode())
    btc_usd = extract_timestamp_and_rate("BTC-USD", data)
    xtc_usd = extract_timestamp_and_rate("XTZ-USD", data)
    btc_xtz = int(int(btc_usd["value"])/int(xtc_usd["value"]) * 10000)
    unix_timestamp = int(datetime.timestamp(
        dateutil.parser.parse(xtc_usd["timestamp"].replace(" +0000 UTC", ""))))
    return { 'timestamp': unix_timestamp, 'value': btc_xtz }
