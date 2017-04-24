#! /usr/bin/env nix-shell
#! nix-shell -i python2.7 -p python27Packages.beautifulsoup4 -p python27Packages.requests -p python27Packages.redis

import time

import rss_urls
from RSS.url import URL
from RSS.logging import logging
from RSS.server import get_redis

redis = get_redis()

      
for link, link_type in rss_urls.rss_urls.iteritems():
    url = URL(link_type, link)
    if(url.get()):
        url.save_to_disk('/home/jihuni/word2vec/rss.dump/')
        redis.sadd('recent_rss_dumps', url.name)
    else:
        logging("Error to download %s RSS %s"%(link_type, link))
    time.sleep(2)

