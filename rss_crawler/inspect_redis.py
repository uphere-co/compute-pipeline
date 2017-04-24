import time
import requests

from RSS.hash import sha256
from RSS.server import get_redis
from RSS.http import get

r = get_redis()

#List all existing keys
print r.keys()

known_links = r.smembers('known_nyt_links')
error_links = r.smembers('erratic_nyt_links')
processed_links = r.smembers('processed_nyt_links')


known_hashes = set([sha256(x) for x in known_links])
processed_hashes = set([sha256(x) for x in processed_links])

a=!ls /opt/NYT.dump

[x for x in a if not x in known_hashes]
[x for x in a if not x in processed_hashes]

